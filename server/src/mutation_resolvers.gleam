/// Mutation Resolvers
///
/// Implements GraphQL mutation resolvers with AT Protocol integration.
/// These resolvers handle authentication, validation, and database operations.
import actor_validator
import atproto_auth
import backfill
import database
import dpop
import gleam/dynamic
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/int
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import lexicon
import sqlight
import swell/schema
import swell/value

/// Context for mutation execution
pub type MutationContext {
  MutationContext(
    db: sqlight.Connection,
    auth_base_url: String,
    plc_url: String,
    external_collection_ids: List(String),
  )
}

/// Convert GraphQL value to JSON string for AT Protocol
fn graphql_value_to_json(val: value.Value) -> String {
  case val {
    value.String(s) -> json.string(s) |> json.to_string
    value.Int(i) -> json.int(i) |> json.to_string
    value.Float(f) -> json.float(f) |> json.to_string
    value.Boolean(b) -> json.bool(b) |> json.to_string
    value.Null -> json.null() |> json.to_string
    value.Enum(e) -> json.string(e) |> json.to_string
    value.List(items) -> {
      json.array(items, graphql_value_to_json_value)
      |> json.to_string
    }
    value.Object(fields) -> {
      json.object(
        fields
        |> list.map(fn(field) {
          let #(key, val) = field
          #(key, graphql_value_to_json_value(val))
        }),
      )
      |> json.to_string
    }
  }
}

/// Convert GraphQL value to JSON value (not string)
fn graphql_value_to_json_value(val: value.Value) -> json.Json {
  case val {
    value.String(s) -> json.string(s)
    value.Int(i) -> json.int(i)
    value.Float(f) -> json.float(f)
    value.Boolean(b) -> json.bool(b)
    value.Null -> json.null()
    value.Enum(e) -> json.string(e)
    value.List(items) -> json.array(items, graphql_value_to_json_value)
    value.Object(fields) -> {
      json.object(
        fields
        |> list.map(fn(field) {
          let #(key, val) = field
          #(key, graphql_value_to_json_value(val))
        }),
      )
    }
  }
}

/// Create a resolver factory for create mutations
pub fn create_resolver_factory(
  collection: String,
  ctx: MutationContext,
) -> schema.Resolver {
  fn(resolver_ctx: schema.Context) -> Result(value.Value, String) {
    // Step 1: Extract auth token from context data
    let token = case resolver_ctx.data {
      option.Some(value.Object(fields)) -> {
        case list.key_find(fields, "auth_token") {
          Ok(value.String(t)) -> Ok(t)
          Ok(_) -> Error("auth_token must be a string")
          Error(_) ->
            Error(
              "Authentication required. Please provide Authorization header.",
            )
        }
      }
      _ ->
        Error("Authentication required. Please provide Authorization header.")
    }

    use token <- result.try(token)

    // Step 2: Get input and rkey from arguments
    let input_result = case schema.get_argument(resolver_ctx, "input") {
      option.Some(val) -> Ok(val)
      option.None -> Error("Missing required argument: input")
    }

    use input <- result.try(input_result)

    let rkey = case schema.get_argument(resolver_ctx, "rkey") {
      option.Some(value.String(r)) -> option.Some(r)
      _ -> option.None
    }

    // Step 3: Verify OAuth token and get AT Protocol session
    use user_info <- result.try(
      atproto_auth.verify_oauth_token(token, ctx.auth_base_url)
      |> result.map_error(fn(err) {
        case err {
          atproto_auth.UnauthorizedToken ->
            "Invalid or expired authentication token"
          atproto_auth.MissingAuthHeader -> "Missing authentication"
          atproto_auth.InvalidAuthHeader -> "Invalid authentication header"
          _ -> "Authentication failed"
        }
      }),
    )

    // Step 4: Ensure actor exists in database
    use is_new_actor <- result.try(actor_validator.ensure_actor_exists(
      ctx.db,
      user_info.did,
      ctx.plc_url,
    ))

    // If new actor, spawn backfill for external collections
    case is_new_actor {
      True -> {
        process.spawn_unlinked(fn() {
          backfill.backfill_external_collections_for_actor(
            ctx.db,
            user_info.did,
            ctx.external_collection_ids,
            ctx.plc_url,
          )
        })
        Nil
      }
      False -> Nil
    }

    use session <- result.try(
      atproto_auth.get_atproto_session(token, ctx.auth_base_url)
      |> result.map_error(fn(_) { "Failed to get AT Protocol session" }),
    )

    // Step 5: Convert input to JSON for validation and AT Protocol
    let record_json = graphql_value_to_json(input)

    // Step 6: Validate against lexicon
    use lexicon_records <- result.try(
      database.get_lexicon(ctx.db, collection)
      |> result.map_error(fn(_) { "Failed to fetch lexicon" }),
    )

    case lexicon_records {
      [lex, ..] -> {
        use _ <- result.try(
          lexicon.validate_record([lex.json], collection, record_json)
          |> result.map_error(fn(err) {
            "Validation failed: " <> lexicon.describe_error(err)
          }),
        )

        // Step 7: Call createRecord via AT Protocol
        // Omit rkey field when not provided to let PDS auto-generate TID
        let create_body =
          case rkey {
            option.Some(r) ->
              json.object([
                #("repo", json.string(user_info.did)),
                #("collection", json.string(collection)),
                #("rkey", json.string(r)),
                #("record", graphql_value_to_json_value(input)),
              ])
            option.None ->
              json.object([
                #("repo", json.string(user_info.did)),
                #("collection", json.string(collection)),
                #("record", graphql_value_to_json_value(input)),
              ])
          }
          |> json.to_string

        let pds_url =
          session.pds_endpoint <> "/xrpc/com.atproto.repo.createRecord"

        use response <- result.try(
          dpop.make_dpop_request("POST", pds_url, session, create_body)
          |> result.map_error(fn(_) { "Failed to create record on PDS" }),
        )

        // Step 8: Check HTTP status and parse response
        use #(uri, cid) <- result.try(case response.status {
          200 | 201 -> {
            // Parse successful response
            let response_decoder = {
              use uri <- decode.field("uri", decode.string)
              use cid <- decode.field("cid", decode.string)
              decode.success(#(uri, cid))
            }

            json.parse(response.body, response_decoder)
            |> result.map_error(fn(_) {
              "Failed to parse PDS success response. Body: " <> response.body
            })
          }
          _ -> {
            // Return actual PDS error
            Error(
              "PDS request failed with status "
              <> int.to_string(response.status)
              <> ": "
              <> response.body,
            )
          }
        })

        // Step 9: Index the created record in the database
        use _ <- result.try(
          database.insert_record(
            ctx.db,
            uri,
            cid,
            user_info.did,
            collection,
            record_json,
          )
          |> result.map_error(fn(_) { "Failed to index record in database" }),
        )

        // Step 10: Return the created record as a GraphQL value
        // Build the GraphQL object with all fields
        Ok(
          value.Object([
            #("uri", value.String(uri)),
            #("cid", value.String(cid)),
            #("did", value.String(user_info.did)),
            #("collection", value.String(collection)),
            #("indexedAt", value.String("")),
            // TODO: Add indexed_at from database query
          ]),
        )
      }
      [] -> Error("Lexicon not found for collection: " <> collection)
    }
  }
}

/// Create a resolver factory for update mutations
pub fn update_resolver_factory(
  collection: String,
  ctx: MutationContext,
) -> schema.Resolver {
  fn(resolver_ctx: schema.Context) -> Result(value.Value, String) {
    // Step 1: Extract auth token from context data
    let token = case resolver_ctx.data {
      option.Some(value.Object(fields)) -> {
        case list.key_find(fields, "auth_token") {
          Ok(value.String(t)) -> Ok(t)
          Ok(_) -> Error("auth_token must be a string")
          Error(_) ->
            Error(
              "Authentication required. Please provide Authorization header.",
            )
        }
      }
      _ ->
        Error("Authentication required. Please provide Authorization header.")
    }

    use token <- result.try(token)

    // Step 2: Get rkey (required) and input from arguments
    let rkey_result = case schema.get_argument(resolver_ctx, "rkey") {
      option.Some(value.String(r)) -> Ok(r)
      option.Some(_) -> Error("rkey must be a string")
      option.None -> Error("Missing required argument: rkey")
    }

    use rkey <- result.try(rkey_result)

    let input_result = case schema.get_argument(resolver_ctx, "input") {
      option.Some(val) -> Ok(val)
      option.None -> Error("Missing required argument: input")
    }

    use input <- result.try(input_result)

    // Step 3: Verify OAuth token and get AT Protocol session
    use user_info <- result.try(
      atproto_auth.verify_oauth_token(token, ctx.auth_base_url)
      |> result.map_error(fn(err) {
        case err {
          atproto_auth.UnauthorizedToken ->
            "Invalid or expired authentication token"
          atproto_auth.MissingAuthHeader -> "Missing authentication"
          atproto_auth.InvalidAuthHeader -> "Invalid authentication header"
          _ -> "Authentication failed"
        }
      }),
    )

    // Step 4: Ensure actor exists in database
    use is_new_actor <- result.try(actor_validator.ensure_actor_exists(
      ctx.db,
      user_info.did,
      ctx.plc_url,
    ))

    // If new actor, spawn backfill for external collections
    case is_new_actor {
      True -> {
        process.spawn_unlinked(fn() {
          backfill.backfill_external_collections_for_actor(
            ctx.db,
            user_info.did,
            ctx.external_collection_ids,
            ctx.plc_url,
          )
        })
        Nil
      }
      False -> Nil
    }

    use session <- result.try(
      atproto_auth.get_atproto_session(token, ctx.auth_base_url)
      |> result.map_error(fn(_) { "Failed to get AT Protocol session" }),
    )

    // Step 5: Convert input to JSON for validation and AT Protocol
    let record_json = graphql_value_to_json(input)

    // Step 6: Validate against lexicon
    use lexicon_records <- result.try(
      database.get_lexicon(ctx.db, collection)
      |> result.map_error(fn(_) { "Failed to fetch lexicon" }),
    )

    case lexicon_records {
      [lex, ..] -> {
        use _ <- result.try(
          lexicon.validate_record([lex.json], collection, record_json)
          |> result.map_error(fn(err) {
            "Validation failed: " <> lexicon.describe_error(err)
          }),
        )

        // Step 7: Call putRecord via AT Protocol
        let update_body =
          json.object([
            #("repo", json.string(user_info.did)),
            #("collection", json.string(collection)),
            #("rkey", json.string(rkey)),
            #("record", graphql_value_to_json_value(input)),
          ])
          |> json.to_string

        let pds_url = session.pds_endpoint <> "/xrpc/com.atproto.repo.putRecord"

        use response <- result.try(
          dpop.make_dpop_request("POST", pds_url, session, update_body)
          |> result.map_error(fn(_) { "Failed to update record on PDS" }),
        )

        // Step 8: Check HTTP status and parse response
        use #(uri, cid) <- result.try(case response.status {
          200 | 201 -> {
            // Parse successful response
            let response_decoder = {
              use uri <- decode.field("uri", decode.string)
              use cid <- decode.field("cid", decode.string)
              decode.success(#(uri, cid))
            }

            json.parse(response.body, response_decoder)
            |> result.map_error(fn(_) {
              "Failed to parse PDS success response. Body: " <> response.body
            })
          }
          _ -> {
            // Return actual PDS error
            Error(
              "PDS request failed with status "
              <> int.to_string(response.status)
              <> ": "
              <> response.body,
            )
          }
        })

        // Step 9: Update the record in the database
        use _ <- result.try(
          database.update_record(ctx.db, uri, cid, record_json)
          |> result.map_error(fn(_) { "Failed to update record in database" }),
        )

        // Step 10: Return the updated record as a GraphQL value
        Ok(
          value.Object([
            #("uri", value.String(uri)),
            #("cid", value.String(cid)),
            #("did", value.String(user_info.did)),
            #("collection", value.String(collection)),
            #("indexedAt", value.String("")),
            // TODO: Add indexed_at from database query
          ]),
        )
      }
      [] -> Error("Lexicon not found for collection: " <> collection)
    }
  }
}

/// Create a resolver factory for delete mutations
pub fn delete_resolver_factory(
  collection: String,
  ctx: MutationContext,
) -> schema.Resolver {
  fn(resolver_ctx: schema.Context) -> Result(value.Value, String) {
    // Step 1: Extract auth token from context data
    let token = case resolver_ctx.data {
      option.Some(value.Object(fields)) -> {
        case list.key_find(fields, "auth_token") {
          Ok(value.String(t)) -> Ok(t)
          Ok(_) -> Error("auth_token must be a string")
          Error(_) ->
            Error(
              "Authentication required. Please provide Authorization header.",
            )
        }
      }
      _ ->
        Error("Authentication required. Please provide Authorization header.")
    }

    use token <- result.try(token)

    // Step 2: Get rkey (required) from arguments
    let rkey_result = case schema.get_argument(resolver_ctx, "rkey") {
      option.Some(value.String(r)) -> Ok(r)
      option.Some(_) -> Error("rkey must be a string")
      option.None -> Error("Missing required argument: rkey")
    }

    use rkey <- result.try(rkey_result)

    // Step 3: Verify OAuth token and get AT Protocol session
    use user_info <- result.try(
      atproto_auth.verify_oauth_token(token, ctx.auth_base_url)
      |> result.map_error(fn(err) {
        case err {
          atproto_auth.UnauthorizedToken ->
            "Invalid or expired authentication token"
          atproto_auth.MissingAuthHeader -> "Missing authentication"
          atproto_auth.InvalidAuthHeader -> "Invalid authentication header"
          _ -> "Authentication failed"
        }
      }),
    )

    // Step 4: Ensure actor exists in database
    use is_new_actor <- result.try(actor_validator.ensure_actor_exists(
      ctx.db,
      user_info.did,
      ctx.plc_url,
    ))

    // If new actor, spawn backfill for external collections
    case is_new_actor {
      True -> {
        process.spawn_unlinked(fn() {
          backfill.backfill_external_collections_for_actor(
            ctx.db,
            user_info.did,
            ctx.external_collection_ids,
            ctx.plc_url,
          )
        })
        Nil
      }
      False -> Nil
    }

    use session <- result.try(
      atproto_auth.get_atproto_session(token, ctx.auth_base_url)
      |> result.map_error(fn(_) { "Failed to get AT Protocol session" }),
    )

    // Step 5: Build the record URI to be deleted
    let uri = "at://" <> user_info.did <> "/" <> collection <> "/" <> rkey

    // Step 6: Call deleteRecord via AT Protocol
    let delete_body =
      json.object([
        #("repo", json.string(user_info.did)),
        #("collection", json.string(collection)),
        #("rkey", json.string(rkey)),
      ])
      |> json.to_string

    let pds_url = session.pds_endpoint <> "/xrpc/com.atproto.repo.deleteRecord"

    use response <- result.try(
      dpop.make_dpop_request("POST", pds_url, session, delete_body)
      |> result.map_error(fn(_) { "Failed to delete record on PDS" }),
    )

    // Step 7: Check HTTP status
    use _ <- result.try(case response.status {
      200 | 201 | 204 -> Ok(Nil)
      _ -> {
        // Return actual PDS error
        Error(
          "PDS delete request failed with status "
          <> int.to_string(response.status)
          <> ": "
          <> response.body,
        )
      }
    })

    // Step 8: Delete the record from the database
    use _ <- result.try(
      database.delete_record(ctx.db, uri)
      |> result.map_error(fn(_) { "Failed to delete record from database" }),
    )

    // Step 9: Return the URI of the deleted record
    Ok(value.Object([#("uri", value.String(uri))]))
  }
}

/// Create a resolver for uploadBlob mutation
pub fn upload_blob_resolver_factory(ctx: MutationContext) -> schema.Resolver {
  fn(resolver_ctx: schema.Context) -> Result(value.Value, String) {
    // Step 1: Extract auth token from context data
    let token = case resolver_ctx.data {
      option.Some(value.Object(fields)) -> {
        case list.key_find(fields, "auth_token") {
          Ok(value.String(t)) -> Ok(t)
          Ok(_) -> Error("auth_token must be a string")
          Error(_) ->
            Error(
              "Authentication required. Please provide Authorization header.",
            )
        }
      }
      _ ->
        Error("Authentication required. Please provide Authorization header.")
    }

    use token <- result.try(token)

    // Step 2: Get data and mimeType from arguments
    let data_result = case schema.get_argument(resolver_ctx, "data") {
      option.Some(value.String(d)) -> Ok(d)
      option.Some(_) -> Error("data must be a string")
      option.None -> Error("Missing required argument: data")
    }

    use data_base64 <- result.try(data_result)

    let mime_type_result = case schema.get_argument(resolver_ctx, "mimeType") {
      option.Some(value.String(m)) -> Ok(m)
      option.Some(_) -> Error("mimeType must be a string")
      option.None -> Error("Missing required argument: mimeType")
    }

    use mime_type <- result.try(mime_type_result)

    // Step 3: Verify OAuth token and get AT Protocol session
    use user_info <- result.try(
      atproto_auth.verify_oauth_token(token, ctx.auth_base_url)
      |> result.map_error(fn(err) {
        case err {
          atproto_auth.UnauthorizedToken ->
            "Invalid or expired authentication token"
          atproto_auth.MissingAuthHeader -> "Missing authentication"
          atproto_auth.InvalidAuthHeader -> "Invalid authentication header"
          _ -> "Authentication failed"
        }
      }),
    )

    // Step 4: Ensure actor exists in database
    use is_new_actor <- result.try(actor_validator.ensure_actor_exists(
      ctx.db,
      user_info.did,
      ctx.plc_url,
    ))

    // If new actor, spawn backfill for external collections
    case is_new_actor {
      True -> {
        process.spawn_unlinked(fn() {
          backfill.backfill_external_collections_for_actor(
            ctx.db,
            user_info.did,
            ctx.external_collection_ids,
            ctx.plc_url,
          )
        })
        Nil
      }
      False -> Nil
    }

    use session <- result.try(
      atproto_auth.get_atproto_session(token, ctx.auth_base_url)
      |> result.map_error(fn(_) { "Failed to get AT Protocol session" }),
    )

    // Step 5: Decode base64 data to binary
    use binary_data <- result.try(
      decode_base64(data_base64)
      |> result.map_error(fn(_) { "Failed to decode base64 data" }),
    )

    // Step 6: Upload blob to PDS
    let pds_url = session.pds_endpoint <> "/xrpc/com.atproto.repo.uploadBlob"

    use response <- result.try(
      dpop.make_dpop_request_with_binary(
        "POST",
        pds_url,
        session,
        binary_data,
        mime_type,
      )
      |> result.map_error(fn(_) { "Failed to upload blob to PDS" }),
    )

    // Step 7: Check HTTP status and parse response
    use blob_ref <- result.try(case response.status {
      200 | 201 -> {
        // Parse PDS response: { blob: { $type: "blob", ref: { $link: "..." }, mimeType: "...", size: 123 } }
        let response_decoder = {
          use blob <- decode.field("blob", decode.dynamic)
          decode.success(blob)
        }

        case json.parse(response.body, response_decoder) {
          Ok(blob_dynamic) -> {
            // Extract blob fields from the response
            extract_blob_from_dynamic(blob_dynamic, user_info.did)
          }
          Error(_) -> {
            Error("Failed to parse PDS response. Body: " <> response.body)
          }
        }
      }
      _ -> {
        // Return actual PDS error
        Error(
          "PDS request failed with status "
          <> int.to_string(response.status)
          <> ": "
          <> response.body,
        )
      }
    })

    // Step 8: Return the BlobUploadResponse directly (flat structure)
    Ok(blob_ref)
  }
}

/// Decode base64 string to bit array
fn decode_base64(base64_str: String) -> Result(BitArray, Nil) {
  // Erlang's base64:decode returns BitArray directly, not wrapped in Ok()
  // So we wrap it in Ok to match our function signature
  Ok(do_erlang_base64_decode(base64_str))
}

/// Extract blob fields from dynamic PDS response
fn extract_blob_from_dynamic(
  blob_dynamic: dynamic.Dynamic,
  did: String,
) -> Result(value.Value, String) {
  // Create a decoder for the nested ref.$link field
  let ref_link_decoder = {
    use link <- decode.field("$link", decode.string)
    decode.success(link)
  }

  // Decode all fields including nested ref.$link
  let full_decoder = {
    use mime_type <- decode.field("mimeType", decode.string)
    use size <- decode.field("size", decode.int)
    use ref <- decode.field("ref", ref_link_decoder)
    decode.success(#(ref, mime_type, size))
  }

  use #(ref, mime_type, size) <- result.try(
    decode.run(blob_dynamic, full_decoder)
    |> result.map_error(fn(_) { "Failed to decode blob fields" }),
  )

  Ok(
    value.Object([
      #("ref", value.String(ref)),
      #("mime_type", value.String(mime_type)),
      #("size", value.Int(size)),
      #("did", value.String(did)),
    ]),
  )
}

/// Erlang FFI: base64:decode/1 returns BitArray directly (not Result)
@external(erlang, "base64", "decode")
fn do_erlang_base64_decode(a: String) -> BitArray
