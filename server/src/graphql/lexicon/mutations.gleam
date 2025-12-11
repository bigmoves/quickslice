/// Mutation Resolvers for lexicon GraphQL API
///
/// Implements GraphQL mutation resolvers with AT Protocol integration.
/// These resolvers handle authentication, validation, and database operations.
import actor_validator
import atproto_auth
import backfill
import database/repositories/lexicons
import database/repositories/records
import dpop
import gleam/dict
import gleam/dynamic
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import honk
import honk/errors
import lib/oauth/did_cache
import sqlight
import swell/schema
import swell/value

/// Context for mutation execution
pub type MutationContext {
  MutationContext(
    db: sqlight.Connection,
    did_cache: Subject(did_cache.Message),
    signing_key: option.Option(String),
    atp_client_id: String,
    plc_url: String,
    collection_ids: List(String),
    external_collection_ids: List(String),
  )
}

// ─── Private Auth Helper ───────────────────────────────────────────

/// Authenticated session info returned by auth helper
type AuthenticatedSession {
  AuthenticatedSession(
    user_info: atproto_auth.UserInfo,
    session: atproto_auth.AtprotoSession,
  )
}

/// Extract token, verify auth, ensure actor exists, get ATP session
fn get_authenticated_session(
  resolver_ctx: schema.Context,
  ctx: MutationContext,
) -> Result(AuthenticatedSession, String) {
  // Step 1: Extract auth token from context data
  let token = case resolver_ctx.data {
    option.Some(value.Object(fields)) -> {
      case list.key_find(fields, "auth_token") {
        Ok(value.String(t)) -> Ok(t)
        Ok(_) -> Error("auth_token must be a string")
        Error(_) ->
          Error("Authentication required. Please provide Authorization header.")
      }
    }
    _ -> Error("Authentication required. Please provide Authorization header.")
  }

  use token <- result.try(token)

  // Step 2: Verify OAuth token
  use user_info <- result.try(
    atproto_auth.verify_token(ctx.db, token)
    |> result.map_error(fn(err) {
      case err {
        atproto_auth.UnauthorizedToken -> "Unauthorized"
        atproto_auth.TokenExpired -> "Token expired"
        atproto_auth.MissingAuthHeader -> "Missing authentication"
        atproto_auth.InvalidAuthHeader -> "Invalid authentication header"
        _ -> "Authentication error"
      }
    }),
  )

  // Step 3: Ensure actor exists in database
  use is_new_actor <- result.try(actor_validator.ensure_actor_exists(
    ctx.db,
    user_info.did,
    ctx.plc_url,
  ))

  // If new actor, spawn backfill for all collections
  case is_new_actor {
    True -> {
      process.spawn_unlinked(fn() {
        backfill.backfill_collections_for_actor(
          ctx.db,
          user_info.did,
          ctx.collection_ids,
          ctx.external_collection_ids,
          ctx.plc_url,
        )
      })
      Nil
    }
    False -> Nil
  }

  // Step 4: Get AT Protocol session
  use session <- result.try(
    atproto_auth.get_atp_session(
      ctx.db,
      ctx.did_cache,
      token,
      ctx.signing_key,
      ctx.atp_client_id,
    )
    |> result.map_error(fn(err) {
      case err {
        atproto_auth.SessionNotFound -> "Session not found"
        atproto_auth.SessionNotReady -> "Session not ready"
        atproto_auth.RefreshFailed(msg) -> "Token refresh failed: " <> msg
        atproto_auth.DIDResolutionFailed(msg) ->
          "DID resolution failed: " <> msg
        _ -> "Failed to get ATP session"
      }
    }),
  )

  Ok(AuthenticatedSession(user_info: user_info, session: session))
}

// ─── Private Blob Helpers ──────────────────────────────────────────

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

/// Get blob field paths from a lexicon for a given collection
fn get_blob_paths(
  collection: String,
  lexicons: List(json.Json),
) -> List(List(String)) {
  let lexicon =
    list.find(lexicons, fn(lex) {
      case json.parse(json.to_string(lex), decode.at(["id"], decode.string)) {
        Ok(id) -> id == collection
        Error(_) -> False
      }
    })

  case lexicon {
    Ok(lex) -> {
      let properties_decoder =
        decode.at(
          ["defs", "main", "record", "properties"],
          decode.dict(decode.string, decode.dynamic),
        )
      case json.parse(json.to_string(lex), properties_decoder) {
        Ok(properties) -> extract_blob_paths_from_properties(properties, [])
        Error(_) -> []
      }
    }
    Error(_) -> []
  }
}

/// Recursively extract blob paths from lexicon properties
fn extract_blob_paths_from_properties(
  properties: dict.Dict(String, dynamic.Dynamic),
  current_path: List(String),
) -> List(List(String)) {
  dict.fold(properties, [], fn(acc, field_name, field_def) {
    let field_path = list.append(current_path, [field_name])
    let type_result = decode.run(field_def, decode.at(["type"], decode.string))

    case type_result {
      Ok("blob") -> [field_path, ..acc]
      Ok("object") -> {
        let nested_props_result =
          decode.run(
            field_def,
            decode.at(
              ["properties"],
              decode.dict(decode.string, decode.dynamic),
            ),
          )
        case nested_props_result {
          Ok(nested_props) -> {
            let nested_paths =
              extract_blob_paths_from_properties(nested_props, field_path)
            list.append(nested_paths, acc)
          }
          Error(_) -> acc
        }
      }
      Ok("array") -> {
        let items_type_result =
          decode.run(field_def, decode.at(["items", "type"], decode.string))
        case items_type_result {
          Ok("blob") -> [field_path, ..acc]
          Ok("object") -> {
            let item_props_result =
              decode.run(
                field_def,
                decode.at(
                  ["items", "properties"],
                  decode.dict(decode.string, decode.dynamic),
                ),
              )
            case item_props_result {
              Ok(item_props) -> {
                let nested_paths =
                  extract_blob_paths_from_properties(item_props, field_path)
                list.append(nested_paths, acc)
              }
              Error(_) -> acc
            }
          }
          _ -> acc
        }
      }
      _ -> acc
    }
  })
}

/// Transform blob inputs in a value from GraphQL format to AT Protocol format
fn transform_blob_inputs(
  input: value.Value,
  blob_paths: List(List(String)),
) -> value.Value {
  transform_value_at_paths(input, blob_paths, [])
}

/// Recursively transform values at blob paths
fn transform_value_at_paths(
  val: value.Value,
  blob_paths: List(List(String)),
  current_path: List(String),
) -> value.Value {
  case val {
    value.Object(fields) -> {
      let is_blob_path =
        list.any(blob_paths, fn(path) {
          path == current_path && current_path != []
        })

      case is_blob_path {
        True -> transform_blob_object(fields)
        False -> {
          value.Object(
            list.map(fields, fn(field) {
              let #(key, field_val) = field
              let new_path = list.append(current_path, [key])
              #(key, transform_value_at_paths(field_val, blob_paths, new_path))
            }),
          )
        }
      }
    }
    value.List(items) -> {
      let is_blob_array_path =
        list.any(blob_paths, fn(path) {
          path == current_path && current_path != []
        })

      case is_blob_array_path {
        True -> {
          value.List(
            list.map(items, fn(item) {
              case item {
                value.Object(item_fields) -> transform_blob_object(item_fields)
                _ -> item
              }
            }),
          )
        }
        False -> {
          let paths_through_here =
            list.filter(blob_paths, fn(path) {
              list.length(path) > list.length(current_path)
              && list.take(path, list.length(current_path)) == current_path
            })

          case list.is_empty(paths_through_here) {
            True -> val
            False -> {
              value.List(
                list.map(items, fn(item) {
                  transform_value_at_paths(item, blob_paths, current_path)
                }),
              )
            }
          }
        }
      }
    }
    _ -> val
  }
}

/// Transform a BlobInput object to AT Protocol blob format
fn transform_blob_object(fields: List(#(String, value.Value))) -> value.Value {
  let ref = case list.key_find(fields, "ref") {
    Ok(value.String(r)) -> r
    _ -> ""
  }
  let mime_type = case list.key_find(fields, "mimeType") {
    Ok(value.String(m)) -> m
    _ -> ""
  }
  let size = case list.key_find(fields, "size") {
    Ok(value.Int(s)) -> s
    _ -> 0
  }

  case ref != "" && mime_type != "" {
    True ->
      value.Object([
        #("$type", value.String("blob")),
        #("ref", value.Object([#("$link", value.String(ref))])),
        #("mimeType", value.String(mime_type)),
        #("size", value.Int(size)),
      ])
    False -> value.Object(fields)
  }
}

/// Decode base64 string to bit array
fn decode_base64(base64_str: String) -> Result(BitArray, Nil) {
  Ok(do_erlang_base64_decode(base64_str))
}

/// Extract blob fields from dynamic PDS response
fn extract_blob_from_dynamic(
  blob_dynamic: dynamic.Dynamic,
  did: String,
) -> Result(value.Value, String) {
  let ref_link_decoder = {
    use link <- decode.field("$link", decode.string)
    decode.success(link)
  }

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

// ─── Public Resolver Factories ─────────────────────────────────────

/// Create a resolver factory for create mutations
pub fn create_resolver_factory(
  collection: String,
  ctx: MutationContext,
) -> schema.Resolver {
  fn(resolver_ctx: schema.Context) -> Result(value.Value, String) {
    // Get authenticated session using helper
    use auth <- result.try(get_authenticated_session(resolver_ctx, ctx))

    // Get input and rkey from arguments
    let input_result = case schema.get_argument(resolver_ctx, "input") {
      option.Some(val) -> Ok(val)
      option.None -> Error("Missing required argument: input")
    }

    use input <- result.try(input_result)

    let rkey = case schema.get_argument(resolver_ctx, "rkey") {
      option.Some(value.String(r)) -> option.Some(r)
      _ -> option.None
    }

    // Fetch lexicons for validation and blob path extraction
    use all_lexicon_records <- result.try(
      lexicons.get_all(ctx.db)
      |> result.map_error(fn(_) { "Failed to fetch lexicons" }),
    )

    use all_lex_jsons <- result.try(
      all_lexicon_records
      |> list.try_map(fn(lex) {
        honk.parse_json_string(lex.json)
        |> result.map_error(fn(e) {
          "Failed to parse lexicon JSON: " <> errors.to_string(e)
        })
      }),
    )

    // Transform blob inputs from GraphQL format to AT Protocol format
    let blob_paths = get_blob_paths(collection, all_lex_jsons)
    let transformed_input = transform_blob_inputs(input, blob_paths)
    let record_json_value = graphql_value_to_json_value(transformed_input)
    let record_json_string = json.to_string(record_json_value)

    // Validate against lexicon
    use _ <- result.try(
      honk.validate_record(all_lex_jsons, collection, record_json_value)
      |> result.map_error(fn(err) {
        "Validation failed: " <> errors.to_string(err)
      }),
    )

    // Call createRecord via AT Protocol
    let create_body =
      case rkey {
        option.Some(r) ->
          json.object([
            #("repo", json.string(auth.user_info.did)),
            #("collection", json.string(collection)),
            #("rkey", json.string(r)),
            #("record", record_json_value),
          ])
        option.None ->
          json.object([
            #("repo", json.string(auth.user_info.did)),
            #("collection", json.string(collection)),
            #("record", record_json_value),
          ])
      }
      |> json.to_string

    let pds_url =
      auth.session.pds_endpoint <> "/xrpc/com.atproto.repo.createRecord"

    use response <- result.try(
      dpop.make_dpop_request("POST", pds_url, auth.session, create_body)
      |> result.map_error(fn(_) { "Failed to create record on PDS" }),
    )

    use #(uri, cid) <- result.try(case response.status {
      200 | 201 -> {
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
      _ ->
        Error(
          "PDS request failed with status "
          <> int.to_string(response.status)
          <> ": "
          <> response.body,
        )
    })

    // Index the created record in the database
    use _ <- result.try(
      records.insert(
        ctx.db,
        uri,
        cid,
        auth.user_info.did,
        collection,
        record_json_string,
      )
      |> result.map_error(fn(_) { "Failed to index record in database" }),
    )

    Ok(
      value.Object([
        #("uri", value.String(uri)),
        #("cid", value.String(cid)),
        #("did", value.String(auth.user_info.did)),
        #("collection", value.String(collection)),
        #("indexedAt", value.String("")),
        #("value", input),
      ]),
    )
  }
}

/// Create a resolver factory for update mutations
pub fn update_resolver_factory(
  collection: String,
  ctx: MutationContext,
) -> schema.Resolver {
  fn(resolver_ctx: schema.Context) -> Result(value.Value, String) {
    // Get authenticated session using helper
    use auth <- result.try(get_authenticated_session(resolver_ctx, ctx))

    // Get rkey (required) and input from arguments
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

    // Fetch lexicons for validation and blob path extraction
    use all_lexicon_records <- result.try(
      lexicons.get_all(ctx.db)
      |> result.map_error(fn(_) { "Failed to fetch lexicons" }),
    )

    use all_lex_jsons <- result.try(
      all_lexicon_records
      |> list.try_map(fn(lex) {
        honk.parse_json_string(lex.json)
        |> result.map_error(fn(e) {
          "Failed to parse lexicon JSON: " <> errors.to_string(e)
        })
      }),
    )

    // Transform blob inputs from GraphQL format to AT Protocol format
    let blob_paths = get_blob_paths(collection, all_lex_jsons)
    let transformed_input = transform_blob_inputs(input, blob_paths)
    let record_json_value = graphql_value_to_json_value(transformed_input)
    let record_json_string = json.to_string(record_json_value)

    // Validate against lexicon
    use _ <- result.try(
      honk.validate_record(all_lex_jsons, collection, record_json_value)
      |> result.map_error(fn(err) {
        "Validation failed: " <> errors.to_string(err)
      }),
    )

    // Call putRecord via AT Protocol
    let update_body =
      json.object([
        #("repo", json.string(auth.user_info.did)),
        #("collection", json.string(collection)),
        #("rkey", json.string(rkey)),
        #("record", record_json_value),
      ])
      |> json.to_string

    let pds_url =
      auth.session.pds_endpoint <> "/xrpc/com.atproto.repo.putRecord"

    use response <- result.try(
      dpop.make_dpop_request("POST", pds_url, auth.session, update_body)
      |> result.map_error(fn(_) { "Failed to update record on PDS" }),
    )

    use #(uri, cid) <- result.try(case response.status {
      200 | 201 -> {
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
      _ ->
        Error(
          "PDS request failed with status "
          <> int.to_string(response.status)
          <> ": "
          <> response.body,
        )
    })

    // Update the record in the database
    use _ <- result.try(
      records.update(ctx.db, uri, cid, record_json_string)
      |> result.map_error(fn(_) { "Failed to update record in database" }),
    )

    Ok(
      value.Object([
        #("uri", value.String(uri)),
        #("cid", value.String(cid)),
        #("did", value.String(auth.user_info.did)),
        #("collection", value.String(collection)),
        #("indexedAt", value.String("")),
        #("value", input),
      ]),
    )
  }
}

/// Create a resolver factory for delete mutations
pub fn delete_resolver_factory(
  collection: String,
  ctx: MutationContext,
) -> schema.Resolver {
  fn(resolver_ctx: schema.Context) -> Result(value.Value, String) {
    // Get authenticated session using helper
    use auth <- result.try(get_authenticated_session(resolver_ctx, ctx))

    // Get rkey (required) from arguments
    let rkey_result = case schema.get_argument(resolver_ctx, "rkey") {
      option.Some(value.String(r)) -> Ok(r)
      option.Some(_) -> Error("rkey must be a string")
      option.None -> Error("Missing required argument: rkey")
    }

    use rkey <- result.try(rkey_result)

    // Build the record URI to be deleted
    let uri = "at://" <> auth.user_info.did <> "/" <> collection <> "/" <> rkey

    // Call deleteRecord via AT Protocol
    let delete_body =
      json.object([
        #("repo", json.string(auth.user_info.did)),
        #("collection", json.string(collection)),
        #("rkey", json.string(rkey)),
      ])
      |> json.to_string

    let pds_url =
      auth.session.pds_endpoint <> "/xrpc/com.atproto.repo.deleteRecord"

    use response <- result.try(
      dpop.make_dpop_request("POST", pds_url, auth.session, delete_body)
      |> result.map_error(fn(_) { "Failed to delete record on PDS" }),
    )

    use _ <- result.try(case response.status {
      200 | 201 | 204 -> Ok(Nil)
      _ ->
        Error(
          "PDS delete request failed with status "
          <> int.to_string(response.status)
          <> ": "
          <> response.body,
        )
    })

    // Delete the record from the database
    use _ <- result.try(
      records.delete(ctx.db, uri)
      |> result.map_error(fn(_) { "Failed to delete record from database" }),
    )

    Ok(value.Object([#("uri", value.String(uri))]))
  }
}

/// Create a resolver for uploadBlob mutation
pub fn upload_blob_resolver_factory(ctx: MutationContext) -> schema.Resolver {
  fn(resolver_ctx: schema.Context) -> Result(value.Value, String) {
    // Get authenticated session using helper
    use auth <- result.try(get_authenticated_session(resolver_ctx, ctx))

    // Get data and mimeType from arguments
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

    // Decode base64 data to binary
    use binary_data <- result.try(
      decode_base64(data_base64)
      |> result.map_error(fn(_) { "Failed to decode base64 data" }),
    )

    // Upload blob to PDS
    let pds_url =
      auth.session.pds_endpoint <> "/xrpc/com.atproto.repo.uploadBlob"

    use response <- result.try(
      dpop.make_dpop_request_with_binary(
        "POST",
        pds_url,
        auth.session,
        binary_data,
        mime_type,
      )
      |> result.map_error(fn(_) { "Failed to upload blob to PDS" }),
    )

    use blob_ref <- result.try(case response.status {
      200 | 201 -> {
        let response_decoder = {
          use blob <- decode.field("blob", decode.dynamic)
          decode.success(blob)
        }

        case json.parse(response.body, response_decoder) {
          Ok(blob_dynamic) ->
            extract_blob_from_dynamic(blob_dynamic, auth.user_info.did)
          Error(_) ->
            Error("Failed to parse PDS response. Body: " <> response.body)
        }
      }
      _ ->
        Error(
          "PDS request failed with status "
          <> int.to_string(response.status)
          <> ": "
          <> response.body,
        )
    })

    Ok(blob_ref)
  }
}
