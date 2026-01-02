/// Mutation Resolvers for lexicon GraphQL API
///
/// Implements GraphQL mutation resolvers with AT Protocol integration.
/// These resolvers handle authentication, validation, and database operations.
import actor_validator
import atproto_auth
import backfill
import database/executor.{type Executor}
import database/repositories/label_definitions
import database/repositories/label_preferences
import database/repositories/lexicons
import database/repositories/records
import database/repositories/reports
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
import gleam/string
import honk
import honk/errors
import lexicon_graphql/input/union as union_input
import lib/oauth/did_cache
import pubsub
import swell/schema
import swell/value
import timestamp

/// Context for mutation execution
pub type MutationContext {
  MutationContext(
    db: Executor,
    did_cache: Subject(did_cache.Message),
    signing_key: option.Option(String),
    atp_client_id: String,
    plc_url: String,
    collection_ids: List(String),
    external_collection_ids: List(String),
  )
}

// ─── Private Auth Helpers ───────────────────────────────────────────

/// Authenticated session info returned by auth helper
type AuthenticatedSession {
  AuthenticatedSession(
    user_info: atproto_auth.UserInfo,
    session: atproto_auth.AtprotoSession,
  )
}

/// Lightweight auth that only verifies the token
/// Use this for mutations that don't need ATP session (e.g., label preferences)
fn get_viewer_auth(
  resolver_ctx: schema.Context,
  db: executor.Executor,
) -> Result(atproto_auth.UserInfo, String) {
  // Extract auth token from context data
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

  // Verify OAuth token
  atproto_auth.verify_token(db, token)
  |> result.map_error(fn(err) {
    case err {
      atproto_auth.UnauthorizedToken -> "Unauthorized"
      atproto_auth.TokenExpired -> "Token expired"
      atproto_auth.MissingAuthHeader -> "Missing authentication"
      atproto_auth.InvalidAuthHeader -> "Invalid authentication header"
      _ -> "Authentication error"
    }
  })
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

// ─── Private Union Helpers ────────────────────────────────────────

/// Union field info: path to field and list of possible type refs
type UnionFieldInfo {
  UnionFieldInfo(path: List(String), refs: List(String))
}

/// Get union field info from a lexicon for a given collection
fn get_union_fields(
  collection: String,
  lexicons: List(json.Json),
) -> List(UnionFieldInfo) {
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
        Ok(properties) -> extract_union_fields_from_properties(properties, [])
        Error(_) -> []
      }
    }
    Error(_) -> []
  }
}

/// Recursively extract union fields from lexicon properties
fn extract_union_fields_from_properties(
  properties: dict.Dict(String, dynamic.Dynamic),
  current_path: List(String),
) -> List(UnionFieldInfo) {
  dict.fold(properties, [], fn(acc, field_name, field_def) {
    let field_path = list.append(current_path, [field_name])
    let type_result = decode.run(field_def, decode.at(["type"], decode.string))

    case type_result {
      Ok("union") -> {
        // Extract refs from the union definition
        let refs_result =
          decode.run(field_def, decode.at(["refs"], decode.list(decode.string)))
        case refs_result {
          Ok(refs) -> [UnionFieldInfo(path: field_path, refs: refs), ..acc]
          Error(_) -> acc
        }
      }
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
            let nested_fields =
              extract_union_fields_from_properties(nested_props, field_path)
            list.append(nested_fields, acc)
          }
          Error(_) -> acc
        }
      }
      Ok("array") -> {
        let items_type_result =
          decode.run(field_def, decode.at(["items", "type"], decode.string))
        case items_type_result {
          Ok("union") -> {
            let refs_result =
              decode.run(
                field_def,
                decode.at(["items", "refs"], decode.list(decode.string)),
              )
            case refs_result {
              Ok(refs) -> [UnionFieldInfo(path: field_path, refs: refs), ..acc]
              Error(_) -> acc
            }
          }
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
                let nested_fields =
                  extract_union_fields_from_properties(item_props, field_path)
                list.append(nested_fields, acc)
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

/// Transform union inputs by adding $type based on the discriminator
fn transform_union_inputs(
  input: value.Value,
  union_fields: List(UnionFieldInfo),
) -> value.Value {
  transform_unions_at_paths(input, union_fields, [])
}

/// Recursively transform union values at specified paths
fn transform_unions_at_paths(
  val: value.Value,
  union_fields: List(UnionFieldInfo),
  current_path: List(String),
) -> value.Value {
  case val {
    value.Object(fields) -> {
      // Check if current path matches a union field
      let matching_union =
        list.find(union_fields, fn(uf) { uf.path == current_path })

      case matching_union {
        Ok(union_info) -> transform_union_object(fields, union_info.refs)
        Error(_) -> {
          // Recurse into object fields
          value.Object(
            list.map(fields, fn(field) {
              let #(key, field_val) = field
              let new_path = list.append(current_path, [key])
              #(
                key,
                transform_unions_at_paths(field_val, union_fields, new_path),
              )
            }),
          )
        }
      }
    }
    value.List(items) -> {
      // Check if current path is a union array
      let matching_union =
        list.find(union_fields, fn(uf) { uf.path == current_path })

      case matching_union {
        Ok(union_info) -> {
          // Transform each item in the array
          value.List(
            list.map(items, fn(item) {
              case item {
                value.Object(item_fields) ->
                  transform_union_object(item_fields, union_info.refs)
                _ -> item
              }
            }),
          )
        }
        Error(_) -> {
          // Recurse into list items
          value.List(
            list.map(items, fn(item) {
              transform_unions_at_paths(item, union_fields, current_path)
            }),
          )
        }
      }
    }
    _ -> val
  }
}

/// Transform a union object from GraphQL discriminated format to AT Protocol format
/// GraphQL input: { type: "SELF_LABELS", selfLabels: { values: [...] } }
/// AT Protocol output: { $type: "com.atproto.label.defs#selfLabels", values: [...] }
fn transform_union_object(
  fields: List(#(String, value.Value)),
  refs: List(String),
) -> value.Value {
  // Find the "type" discriminator field
  let type_field = list.key_find(fields, "type")

  case type_field {
    Ok(value.Enum(enum_value)) -> {
      // Convert enum value back to ref
      let matching_ref = find_ref_for_enum_value(enum_value, refs)
      case matching_ref {
        Ok(ref) -> {
          // Find the variant field (same name as the short ref name)
          let short_name = enum_value_to_short_name(enum_value)
          case list.key_find(fields, short_name) {
            Ok(value.Object(variant_fields)) -> {
              // Build AT Protocol format: variant fields + $type
              value.Object([#("$type", value.String(ref)), ..variant_fields])
            }
            _ -> {
              // No variant data, just return $type
              value.Object([#("$type", value.String(ref))])
            }
          }
        }
        Error(_) -> value.Object(fields)
      }
    }
    Ok(value.String(str_value)) -> {
      // Handle string type discriminator (fallback)
      let matching_ref = find_ref_for_enum_value(str_value, refs)
      case matching_ref {
        Ok(ref) -> {
          let short_name = enum_value_to_short_name(str_value)
          case list.key_find(fields, short_name) {
            Ok(value.Object(variant_fields)) -> {
              value.Object([#("$type", value.String(ref)), ..variant_fields])
            }
            _ -> value.Object([#("$type", value.String(ref))])
          }
        }
        Error(_) -> value.Object(fields)
      }
    }
    _ -> value.Object(fields)
  }
}

/// Find the ref that matches an enum value
/// "SELF_LABELS" matches "com.atproto.label.defs#selfLabels"
fn find_ref_for_enum_value(
  enum_value: String,
  refs: List(String),
) -> Result(String, Nil) {
  list.find(refs, fn(ref) { union_input.ref_to_enum_value(ref) == enum_value })
}

/// Convert SCREAMING_SNAKE_CASE to camelCase for field lookup
/// "SELF_LABELS" -> "selfLabels"
fn enum_value_to_short_name(enum_value: String) -> String {
  union_input.screaming_snake_to_camel(enum_value)
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
    let blob_transformed = transform_blob_inputs(input, blob_paths)

    // Transform union inputs from GraphQL discriminated format to AT Protocol format
    let union_fields = get_union_fields(collection, all_lex_jsons)
    let transformed_input =
      transform_union_inputs(blob_transformed, union_fields)

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

    // Publish event for GraphQL subscriptions
    pubsub.publish(pubsub.RecordEvent(
      uri: uri,
      cid: cid,
      did: auth.user_info.did,
      collection: collection,
      value: record_json_string,
      indexed_at: timestamp.current_iso8601(),
      operation: pubsub.Create,
    ))

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
    let blob_transformed = transform_blob_inputs(input, blob_paths)

    // Transform union inputs from GraphQL discriminated format to AT Protocol format
    let union_fields = get_union_fields(collection, all_lex_jsons)
    let transformed_input =
      transform_union_inputs(blob_transformed, union_fields)

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

    // Publish event for GraphQL subscriptions
    pubsub.publish(pubsub.RecordEvent(
      uri: uri,
      cid: cid,
      did: auth.user_info.did,
      collection: collection,
      value: record_json_string,
      indexed_at: timestamp.current_iso8601(),
      operation: pubsub.Update,
    ))

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

    // Publish event for GraphQL subscriptions
    pubsub.publish(pubsub.RecordEvent(
      uri: uri,
      cid: "",
      did: auth.user_info.did,
      collection: collection,
      value: "",
      indexed_at: timestamp.current_iso8601(),
      operation: pubsub.Delete,
    ))

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

/// Create a resolver for createReport mutation
/// Allows authenticated users to submit moderation reports
pub fn create_report_resolver_factory(ctx: MutationContext) -> schema.Resolver {
  fn(resolver_ctx: schema.Context) -> Result(value.Value, String) {
    // Get authenticated session using helper
    use auth <- result.try(get_authenticated_session(resolver_ctx, ctx))

    // Get subjectUri (required) and reasonType (required) from arguments
    let subject_uri_result = case
      schema.get_argument(resolver_ctx, "subjectUri")
    {
      option.Some(value.String(u)) -> Ok(u)
      option.Some(_) -> Error("subjectUri must be a string")
      option.None -> Error("Missing required argument: subjectUri")
    }

    use subject_uri <- result.try(subject_uri_result)

    let reason_type_result = case
      schema.get_argument(resolver_ctx, "reasonType")
    {
      option.Some(value.Enum(r)) -> Ok(string.lowercase(r))
      option.Some(value.String(r)) -> Ok(string.lowercase(r))
      option.Some(_) -> Error("reasonType must be a string")
      option.None -> Error("Missing required argument: reasonType")
    }

    use reason_type <- result.try(reason_type_result)

    // Validate reason_type
    let valid_reasons = [
      "spam",
      "violation",
      "misleading",
      "sexual",
      "rude",
      "other",
    ]
    use _ <- result.try(case list.contains(valid_reasons, reason_type) {
      True -> Ok(Nil)
      False ->
        Error(
          "Invalid reasonType. Must be one of: "
          <> string.join(valid_reasons, ", "),
        )
    })

    // Get optional reason text
    let reason = case schema.get_argument(resolver_ctx, "reason") {
      option.Some(value.String(r)) -> option.Some(r)
      _ -> option.None
    }

    // Insert the report
    use report <- result.try(
      reports.insert(
        ctx.db,
        auth.user_info.did,
        subject_uri,
        reason_type,
        reason,
      )
      |> result.map_error(fn(_) { "Failed to create report" }),
    )

    // Return the created report
    let reason_value = case report.reason {
      option.Some(r) -> value.String(r)
      option.None -> value.Null
    }

    Ok(
      value.Object([
        #("id", value.Int(report.id)),
        #("reporterDid", value.String(report.reporter_did)),
        #("subjectUri", value.String(report.subject_uri)),
        #("reasonType", value.Enum(string.uppercase(report.reason_type))),
        #("reason", reason_value),
        #("status", value.Enum("PENDING")),
        #("createdAt", value.String(report.created_at)),
      ]),
    )
  }
}

// ─── Label Preference Mutation ────────────────────────────────────────────

/// Resolver factory for setLabelPreference mutation
pub fn set_label_preference_resolver_factory(
  ctx: MutationContext,
) -> schema.Resolver {
  fn(resolver_ctx: schema.Context) -> Result(value.Value, String) {
    // Get viewer auth (lightweight - no ATP session needed)
    use user_info <- result.try(get_viewer_auth(resolver_ctx, ctx.db))

    // Get val (required) argument
    let val_result = case schema.get_argument(resolver_ctx, "val") {
      option.Some(value.String(v)) -> Ok(v)
      option.Some(_) -> Error("val must be a string")
      option.None -> Error("Missing required argument: val")
    }

    use val <- result.try(val_result)

    // Get visibility (required) argument
    let visibility_result = case
      schema.get_argument(resolver_ctx, "visibility")
    {
      option.Some(value.Enum(v)) -> Ok(string.lowercase(v))
      option.Some(value.String(v)) -> Ok(string.lowercase(v))
      option.Some(_) -> Error("visibility must be a valid enum value")
      option.None -> Error("Missing required argument: visibility")
    }

    use visibility <- result.try(visibility_result)

    // Validate not a system label (starts with !)
    use _ <- result.try(case string.starts_with(val, "!") {
      True -> Error("Cannot set preference for system labels")
      False -> Ok(Nil)
    })

    // Validate visibility is a valid value
    use _ <- result.try(label_definitions.validate_visibility(visibility))

    // Validate label exists
    use def <- result.try(case label_definitions.get(ctx.db, val) {
      Ok(option.None) -> Error("Unknown label: " <> val)
      Error(_) -> Error("Failed to validate label")
      Ok(option.Some(d)) -> Ok(d)
    })

    // Set the preference
    use _ <- result.try(
      label_preferences.set(ctx.db, user_info.did, val, visibility)
      |> result.map_error(fn(_) { "Failed to set label preference" }),
    )

    // Return the updated preference
    Ok(
      value.Object([
        #("val", value.String(def.val)),
        #("description", value.String(def.description)),
        #("severity", value.Enum(string.uppercase(def.severity))),
        #(
          "defaultVisibility",
          value.Enum(string.uppercase(def.default_visibility)),
        ),
        #("visibility", value.Enum(string.uppercase(visibility))),
      ]),
    )
  }
}
