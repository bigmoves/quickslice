# Lexicon GraphQL Refactor Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Refactor `graphql_gleam.gleam` (754 lines) and `mutation_resolvers.gleam` (1023 lines) into a modular structure under `graphql/lexicon/`, mirroring the successful `graphql/admin/` pattern.

**Architecture:** Split into 4 modules by concern: converters (data transformation), fetchers (database data retrieval), mutations (resolver factories with shared auth helper), and schema (public API entry point). Extract repeated auth code (~40 lines x 4 resolvers) into a single private helper.

**Tech Stack:** Gleam, swell (GraphQL library), lexicon_graphql, sqlight

---

### Task 1: Create converters.gleam

**Files:**
- Create: `server/src/graphql/lexicon/converters.gleam`

**Step 1: Create the directory**

Run: `mkdir -p /Users/chadmiller/code/quickslice/.worktrees/graphql-refactor/server/src/graphql/lexicon`

**Step 2: Create converters.gleam with value conversion functions**

```gleam
/// Value converters for lexicon GraphQL API
///
/// Transform database records and dynamic values to GraphQL value.Value objects
import database/repositories/actors
import database/types
import gleam/dict
import gleam/dynamic
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/result
import sqlight
import swell/value

/// Convert a database Record to a GraphQL value.Value
///
/// Creates an Object with all the record metadata plus the parsed JSON value
pub fn record_to_graphql_value(
  record: types.Record,
  db: sqlight.Connection,
) -> value.Value {
  // Parse the record JSON and convert to GraphQL value
  let value_object = case parse_json_to_value(record.json) {
    Ok(val) -> val
    Error(_) -> value.Object([])
    // Fallback to empty object on parse error
  }

  // Look up actor handle from actor table
  let actor_handle = case actors.get(db, record.did) {
    Ok([actor, ..]) -> value.String(actor.handle)
    _ -> value.Null
  }

  // Create the full record object with metadata and value
  value.Object([
    #("uri", value.String(record.uri)),
    #("cid", value.String(record.cid)),
    #("did", value.String(record.did)),
    #("collection", value.String(record.collection)),
    #("indexedAt", value.String(record.indexed_at)),
    #("actorHandle", actor_handle),
    #("value", value_object),
  ])
}

/// Parse a JSON string and convert it to a GraphQL value.Value
pub fn parse_json_to_value(json_str: String) -> Result(value.Value, String) {
  // Parse JSON string to dynamic value
  case json.parse(json_str, decode.dynamic) {
    Ok(dyn) -> Ok(dynamic_to_value(dyn))
    Error(_) -> Error("Failed to parse JSON")
  }
}

/// Convert a dynamic value to a GraphQL value.Value
pub fn dynamic_to_value(dyn: dynamic.Dynamic) -> value.Value {
  // Try different decoders in order
  case decode.run(dyn, decode.string) {
    Ok(s) -> value.String(s)
    Error(_) ->
      case decode.run(dyn, decode.int) {
        Ok(i) -> value.Int(i)
        Error(_) ->
          case decode.run(dyn, decode.float) {
            Ok(f) -> value.Float(f)
            Error(_) ->
              case decode.run(dyn, decode.bool) {
                Ok(b) -> value.Boolean(b)
                Error(_) ->
                  case decode.run(dyn, decode.list(decode.dynamic)) {
                    Ok(items) -> {
                      let converted_items = list.map(items, dynamic_to_value)
                      value.List(converted_items)
                    }
                    Error(_) ->
                      case
                        decode.run(
                          dyn,
                          decode.dict(decode.string, decode.dynamic),
                        )
                      {
                        Ok(dict) -> {
                          let fields =
                            dict
                            |> dict.to_list
                            |> list.map(fn(entry) {
                              let #(key, val) = entry
                              #(key, dynamic_to_value(val))
                            })
                          value.Object(fields)
                        }
                        Error(_) -> value.Null
                      }
                  }
              }
          }
      }
  }
}

/// Convert a dynamic JSON value to graphql value.Value
pub fn json_dynamic_to_value(dyn: dynamic.Dynamic) -> value.Value {
  // Try different decoders in order
  case decode.run(dyn, decode.string) {
    Ok(s) -> value.String(s)
    Error(_) ->
      case decode.run(dyn, decode.int) {
        Ok(i) -> value.Int(i)
        Error(_) ->
          case decode.run(dyn, decode.float) {
            Ok(f) -> value.Float(f)
            Error(_) ->
              case decode.run(dyn, decode.bool) {
                Ok(b) -> value.Boolean(b)
                Error(_) ->
                  // Try as a list
                  case decode.run(dyn, decode.list(decode.dynamic)) {
                    Ok(items) ->
                      value.List(list.map(items, json_dynamic_to_value))
                    Error(_) ->
                      // Try as an object (dict)
                      case
                        decode.run(
                          dyn,
                          decode.dict(decode.string, decode.dynamic),
                        )
                      {
                        Ok(d) ->
                          value.Object(
                            list.map(dict.to_list(d), fn(pair) {
                              #(pair.0, json_dynamic_to_value(pair.1))
                            }),
                          )
                        Error(_) -> value.Null
                      }
                  }
              }
          }
      }
  }
}

/// Extract a reference URI from a record's JSON
/// This handles both simple string fields (at-uri) and strongRef objects
pub fn extract_reference_uri(
  json_str: String,
  field_name: String,
) -> Result(String, Nil) {
  // Parse the JSON
  case parse_json_to_value(json_str) {
    Ok(value.Object(fields)) -> {
      // Find the field
      case list.key_find(fields, field_name) {
        Ok(value.String(uri)) -> Ok(uri)
        Ok(value.Object(ref_fields)) -> {
          // Handle strongRef: { "uri": "...", "cid": "..." }
          case list.key_find(ref_fields, "uri") {
            Ok(value.String(uri)) -> Ok(uri)
            _ -> Error(Nil)
          }
        }
        _ -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}
```

**Step 3: Run gleam check to verify syntax**

Run: `cd /Users/chadmiller/code/quickslice/.worktrees/graphql-refactor/server && gleam check`
Expected: No errors for converters.gleam

**Step 4: Commit**

```bash
git add src/graphql/lexicon/converters.gleam
git commit -m "refactor: extract lexicon GraphQL converters to separate module"
```

---

### Task 2: Create fetchers.gleam

**Files:**
- Create: `server/src/graphql/lexicon/fetchers.gleam`

**Step 1: Create fetchers.gleam with all database fetcher functions**

```gleam
/// Database fetchers for lexicon GraphQL API
///
/// These functions bridge the database layer to the lexicon_graphql library's
/// expected fetcher signatures for queries, joins, and aggregations.
import database/queries/aggregates
import database/queries/pagination
import database/repositories/actors
import database/repositories/records
import database/types
import gleam/dict
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import graphql/lexicon/converters
import graphql/where_converter
import lexicon_graphql/input/aggregate
import lexicon_graphql/query/dataloader
import lexicon_graphql/schema/database
import sqlight
import swell/value

/// Create a record fetcher for paginated collection queries
pub fn record_fetcher(db: sqlight.Connection) {
  fn(
    collection_nsid: String,
    pagination_params: dataloader.PaginationParams,
  ) -> Result(
    #(
      List(#(value.Value, String)),
      option.Option(String),
      Bool,
      Bool,
      option.Option(Int),
    ),
    String,
  ) {
    // Convert where clause from GraphQL types to SQL types
    let where_clause = case pagination_params.where {
      option.Some(graphql_where) ->
        option.Some(where_converter.convert_where_clause(graphql_where))
      option.None -> option.None
    }

    // Get total count for this collection (with where filter if present)
    let total_count =
      records.get_collection_count_with_where(
        db,
        collection_nsid,
        where_clause,
      )
      |> result.map(option.Some)
      |> result.unwrap(option.None)

    // Fetch records from database for this collection with pagination
    case
      records.get_by_collection_paginated_with_where(
        db,
        collection_nsid,
        pagination_params.first,
        pagination_params.after,
        pagination_params.last,
        pagination_params.before,
        pagination_params.sort_by,
        where_clause,
      )
    {
      Error(_) -> Ok(#([], option.None, False, False, option.None))
      // Return empty result on error
      Ok(#(record_list, next_cursor, has_next_page, has_previous_page)) -> {
        // Convert database records to GraphQL values with cursors
        let graphql_records_with_cursors =
          list.map(record_list, fn(record) {
            let graphql_value = converters.record_to_graphql_value(record, db)
            // Generate cursor for this record
            let record_cursor =
              pagination.generate_cursor_from_record(
                record,
                pagination_params.sort_by,
              )
            #(graphql_value, record_cursor)
          })
        Ok(#(
          graphql_records_with_cursors,
          next_cursor,
          has_next_page,
          has_previous_page,
          total_count,
        ))
      }
    }
  }
}

/// Create a batch fetcher for join operations (forward and reverse)
pub fn batch_fetcher(db: sqlight.Connection) {
  fn(
    uris: List(String),
    collection: String,
    field: option.Option(String),
  ) -> Result(dataloader.BatchResult, String) {
    // Check if this is a forward join (field is None) or reverse join (field is Some)
    case field {
      option.None -> {
        // Determine if we're dealing with DIDs or URIs
        case uris {
          [] -> Ok(dict.new())
          [first, ..] -> {
            case string.starts_with(first, "did:") {
              True -> {
                // DID join: fetch records by DID and collection
                case records.get_by_dids_and_collection(db, uris, collection) {
                  Ok(record_list) -> {
                    // Group records by DID
                    let grouped =
                      list.fold(record_list, dict.new(), fn(acc, record) {
                        let graphql_value =
                          converters.record_to_graphql_value(record, db)
                        let existing =
                          dict.get(acc, record.did) |> result.unwrap([])
                        dict.insert(acc, record.did, [graphql_value, ..existing])
                      })
                    Ok(grouped)
                  }
                  Error(_) -> Error("Failed to fetch records by DIDs")
                }
              }
              False -> {
                // Forward join: fetch records by their URIs
                case records.get_by_uris(db, uris) {
                  Ok(record_list) -> {
                    // Group records by URI
                    let grouped =
                      list.fold(record_list, dict.new(), fn(acc, record) {
                        let graphql_value =
                          converters.record_to_graphql_value(record, db)
                        // For forward joins, return single record per URI
                        dict.insert(acc, record.uri, [graphql_value])
                      })
                    Ok(grouped)
                  }
                  Error(_) -> Error("Failed to fetch records by URIs")
                }
              }
            }
          }
        }
      }
      option.Some(reference_field) -> {
        // Reverse join: fetch records that reference the parent URIs
        case
          records.get_by_reference_field(db, collection, reference_field, uris)
        {
          Ok(record_list) -> {
            // Group records by the parent URI they reference
            // Parse each record's JSON to extract the reference field value
            let grouped =
              list.fold(record_list, dict.new(), fn(acc, record) {
                let graphql_value =
                  converters.record_to_graphql_value(record, db)
                // Extract the reference field from the record JSON to find parent URI
                case
                  converters.extract_reference_uri(record.json, reference_field)
                {
                  Ok(parent_uri) -> {
                    let existing =
                      dict.get(acc, parent_uri) |> result.unwrap([])
                    dict.insert(acc, parent_uri, [graphql_value, ..existing])
                  }
                  Error(_) -> acc
                }
              })
            Ok(grouped)
          }
          Error(_) ->
            Error(
              "Failed to fetch records by reference field: " <> reference_field,
            )
        }
      }
    }
  }
}

/// Create a paginated batch fetcher for join operations with pagination
pub fn paginated_batch_fetcher(db: sqlight.Connection) {
  fn(
    key: String,
    collection: String,
    field: option.Option(String),
    pagination_params: dataloader.PaginationParams,
  ) -> Result(dataloader.PaginatedBatchResult, String) {
    // Convert pagination params to database pagination params
    let db_first = pagination_params.first
    let db_after = pagination_params.after
    let db_last = pagination_params.last
    let db_before = pagination_params.before
    let db_sort_by = pagination_params.sort_by

    // Convert where clause from GraphQL to database format
    let db_where = case pagination_params.where {
      option.Some(where_clause) ->
        option.Some(where_converter.convert_where_clause(where_clause))
      option.None -> option.None
    }

    // Check if this is a DID join (field is None) or reverse join (field is Some)
    case field {
      option.None -> {
        // DID join: key is the DID
        case
          records.get_by_dids_and_collection_paginated(
            db,
            key,
            collection,
            db_first,
            db_after,
            db_last,
            db_before,
            db_sort_by,
            db_where,
          )
        {
          Ok(#(
            record_list,
            _next_cursor,
            has_next_page,
            has_previous_page,
            total_count,
          )) -> {
            // Convert records to GraphQL values with cursors
            let edges =
              list.map(record_list, fn(record) {
                let graphql_value =
                  converters.record_to_graphql_value(record, db)
                let cursor =
                  pagination.generate_cursor_from_record(record, db_sort_by)
                #(graphql_value, cursor)
              })

            Ok(dataloader.PaginatedBatchResult(
              edges: edges,
              has_next_page: has_next_page,
              has_previous_page: has_previous_page,
              total_count: total_count,
            ))
          }
          Error(_) -> Error("Failed to fetch paginated records by DID")
        }
      }
      option.Some(reference_field) -> {
        // Reverse join: key is the parent URI
        case
          records.get_by_reference_field_paginated(
            db,
            collection,
            reference_field,
            key,
            db_first,
            db_after,
            db_last,
            db_before,
            db_sort_by,
            db_where,
          )
        {
          Ok(#(
            record_list,
            _next_cursor,
            has_next_page,
            has_previous_page,
            total_count,
          )) -> {
            // Convert records to GraphQL values with cursors
            let edges =
              list.map(record_list, fn(record) {
                let graphql_value =
                  converters.record_to_graphql_value(record, db)
                let cursor =
                  pagination.generate_cursor_from_record(record, db_sort_by)
                #(graphql_value, cursor)
              })

            Ok(dataloader.PaginatedBatchResult(
              edges: edges,
              has_next_page: has_next_page,
              has_previous_page: has_previous_page,
              total_count: total_count,
            ))
          }
          Error(_) ->
            Error(
              "Failed to fetch paginated records by reference field: "
              <> reference_field,
            )
        }
      }
    }
  }
}

/// Create an aggregate fetcher for GROUP BY queries
pub fn aggregate_fetcher(db: sqlight.Connection) {
  fn(collection_nsid: String, params: database.AggregateParams) {
    // Convert GraphQL where clause to SQL where clause
    let where_clause = case params.where {
      option.Some(graphql_where) ->
        option.Some(where_converter.convert_where_clause(graphql_where))
      option.None -> option.None
    }

    // Convert GroupByFieldInput to types.GroupByField
    let group_by_fields =
      list.map(params.group_by, fn(gb) {
        case gb.interval {
          option.Some(interval) -> {
            let db_interval = case interval {
              aggregate.Hour -> types.Hour
              aggregate.Day -> types.Day
              aggregate.Week -> types.Week
              aggregate.Month -> types.Month
            }
            types.TruncatedField(gb.field, db_interval)
          }
          option.None -> types.SimpleField(gb.field)
        }
      })

    // Call database aggregation function
    aggregates.get_aggregated_records(
      db,
      collection_nsid,
      group_by_fields,
      where_clause,
      params.order_by_desc,
      params.limit,
    )
    |> result.map_error(fn(_) { "Failed to fetch aggregated records" })
  }
}

/// Create a viewer fetcher for authenticated user info
pub fn viewer_fetcher(db: sqlight.Connection) {
  fn(token: String) {
    case atproto_auth.verify_token(db, token) {
      Error(_) -> Error("Invalid or expired token")
      Ok(user_info) -> {
        // Get handle from actors table
        let handle = case actors.get(db, user_info.did) {
          Ok([actor, ..]) -> option.Some(actor.handle)
          _ -> option.None
        }
        Ok(#(user_info.did, handle))
      }
    }
  }
}
```

**Step 2: Add missing import for atproto_auth**

The viewer_fetcher uses `atproto_auth.verify_token`. Add this import at the top:

```gleam
import atproto_auth
```

**Step 3: Run gleam check to verify syntax**

Run: `cd /Users/chadmiller/code/quickslice/.worktrees/graphql-refactor/server && gleam check`
Expected: No errors

**Step 4: Commit**

```bash
git add src/graphql/lexicon/fetchers.gleam
git commit -m "refactor: extract lexicon GraphQL fetchers to separate module"
```

---

### Task 3: Create mutations.gleam

**Files:**
- Create: `server/src/graphql/lexicon/mutations.gleam`

**Step 1: Create mutations.gleam with MutationContext, auth helper, and resolver factories**

This is a large file. Copy from `mutation_resolvers.gleam` with these changes:

1. Add private `AuthenticatedSession` type and `get_authenticated_session` helper
2. Update each resolver factory to use the helper instead of inline auth code
3. Keep all blob transformation helpers as private functions

```gleam
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
    session: atproto_auth.AtpSession,
  )
}

/// Extract token, verify auth, ensure actor exists, get ATP session
/// Consolidates ~40 lines of repeated auth code into one call
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
        atproto_auth.DIDResolutionFailed(msg) -> "DID resolution failed: " <> msg
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

    let pds_url = auth.session.pds_endpoint <> "/xrpc/com.atproto.repo.putRecord"

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
    let uri =
      "at://" <> auth.user_info.did <> "/" <> collection <> "/" <> rkey

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
    let pds_url = auth.session.pds_endpoint <> "/xrpc/com.atproto.repo.uploadBlob"

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
```

**Step 2: Run gleam check to verify syntax**

Run: `cd /Users/chadmiller/code/quickslice/.worktrees/graphql-refactor/server && gleam check`
Expected: No errors

**Step 3: Commit**

```bash
git add src/graphql/lexicon/mutations.gleam
git commit -m "refactor: extract lexicon GraphQL mutations with shared auth helper"
```

---

### Task 4: Create schema.gleam

**Files:**
- Create: `server/src/graphql/lexicon/schema.gleam`

**Step 1: Create schema.gleam with public API functions**

```gleam
/// Lexicon GraphQL schema entry point
///
/// Public API for building and executing the lexicon-driven GraphQL schema.
/// External code should import this module for all lexicon GraphQL operations.
import backfill
import database/repositories/config as config_repo
import database/repositories/lexicons
import gleam/dict
import gleam/dynamic
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import graphql/lexicon/converters
import graphql/lexicon/fetchers
import graphql/lexicon/mutations
import lexicon_graphql
import lexicon_graphql/schema/database
import lib/oauth/did_cache
import sqlight
import swell/executor
import swell/schema
import swell/value

/// Build a GraphQL schema from database lexicons
///
/// This is exposed for WebSocket subscriptions to build the schema once
/// and reuse it for multiple subscription executions.
pub fn build_schema_from_db(
  db: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
  signing_key: option.Option(String),
  atp_client_id: String,
  plc_url: String,
  domain_authority: String,
) -> Result(schema.Schema, String) {
  // Step 1: Fetch lexicons from database
  use lexicon_records <- result.try(
    lexicons.get_all(db)
    |> result.map_error(fn(_) { "Failed to fetch lexicons from database" }),
  )

  // Step 2: Parse lexicon JSON into structured Lexicon types
  let parsed_lexicons =
    lexicon_records
    |> list.filter_map(fn(lex) {
      case lexicon_graphql.parse_lexicon(lex.json) {
        Ok(parsed) -> Ok(parsed)
        Error(_) -> Error(Nil)
      }
    })

  // Check if we got any valid lexicons
  case parsed_lexicons {
    [] -> Error("No valid lexicons found in database")
    _ -> {
      // Step 3: Create fetchers
      let record_fetcher = fetchers.record_fetcher(db)
      let batch_fetcher = fetchers.batch_fetcher(db)
      let paginated_batch_fetcher = fetchers.paginated_batch_fetcher(db)
      let aggregate_fetcher = fetchers.aggregate_fetcher(db)
      let viewer_fetcher = fetchers.viewer_fetcher(db)

      // Step 4: Determine local and external collections for backfill
      let collection_ids =
        parsed_lexicons
        |> list.filter_map(fn(lex) {
          case backfill.nsid_matches_domain_authority(lex.id, domain_authority) {
            True -> Ok(lex.id)
            False -> Error(Nil)
          }
        })

      let external_collection_ids =
        parsed_lexicons
        |> list.filter_map(fn(lex) {
          case backfill.nsid_matches_domain_authority(lex.id, domain_authority) {
            True -> Error(Nil)
            False -> Ok(lex.id)
          }
        })

      // Step 5: Create mutation resolver factories
      let mutation_ctx =
        mutations.MutationContext(
          db: db,
          did_cache: did_cache,
          signing_key: signing_key,
          atp_client_id: atp_client_id,
          plc_url: plc_url,
          collection_ids: collection_ids,
          external_collection_ids: external_collection_ids,
        )

      let create_factory =
        option.Some(fn(collection) {
          mutations.create_resolver_factory(collection, mutation_ctx)
        })

      let update_factory =
        option.Some(fn(collection) {
          mutations.update_resolver_factory(collection, mutation_ctx)
        })

      let delete_factory =
        option.Some(fn(collection) {
          mutations.delete_resolver_factory(collection, mutation_ctx)
        })

      let upload_blob_factory =
        option.Some(fn() {
          mutations.upload_blob_resolver_factory(mutation_ctx)
        })

      // Step 6: Build schema with database-backed resolvers, mutations, and subscriptions
      database.build_schema_with_subscriptions(
        parsed_lexicons,
        record_fetcher,
        option.Some(batch_fetcher),
        option.Some(paginated_batch_fetcher),
        create_factory,
        update_factory,
        delete_factory,
        upload_blob_factory,
        option.Some(aggregate_fetcher),
        option.Some(viewer_fetcher),
      )
    }
  }
}

/// Execute a GraphQL query against lexicons in the database
///
/// This fetches lexicons, builds a schema with database resolvers,
/// executes the query, and returns the result as JSON.
pub fn execute_query_with_db(
  db: sqlight.Connection,
  query_string: String,
  variables_json_str: String,
  auth_token: Result(String, Nil),
  did_cache: Subject(did_cache.Message),
  signing_key: option.Option(String),
  atp_client_id: String,
  plc_url: String,
) -> Result(String, String) {
  // Get domain authority from database
  let domain_authority = case config_repo.get(db, "domain_authority") {
    Ok(authority) -> authority
    Error(_) -> ""
  }

  // Build the schema
  use graphql_schema <- result.try(build_schema_from_db(
    db,
    did_cache,
    signing_key,
    atp_client_id,
    plc_url,
    domain_authority,
  ))

  // Create context with auth token if provided
  let ctx_data = case auth_token {
    Ok(token) -> {
      // Add auth token to context for mutation resolvers
      option.Some(value.Object([#("auth_token", value.String(token))]))
    }
    Error(_) -> option.None
  }

  // Convert json variables to Dict(String, value.Value)
  let variables_dict = json_string_to_variables_dict(variables_json_str)

  let ctx = schema.context_with_variables(ctx_data, variables_dict)

  // Execute the query
  use response <- result.try(executor.execute(query_string, graphql_schema, ctx))

  // Format the response as JSON
  Ok(format_response(response))
}

/// Format an executor.Response as JSON string
/// Per GraphQL spec, only include "errors" field when there are actual errors
pub fn format_response(response: executor.Response) -> String {
  let data_json = value_to_json(response.data)

  case response.errors {
    [] -> "{\"data\": " <> data_json <> "}"
    errors -> {
      let error_strings =
        list.map(errors, fn(err) {
          let message_json = json.string(err.message) |> json.to_string
          let path_json = json.array(err.path, of: json.string) |> json.to_string
          "{\"message\": " <> message_json <> ", \"path\": " <> path_json <> "}"
        })

      let errors_json = "[" <> string.join(error_strings, ",") <> "]"
      "{\"data\": " <> data_json <> ", \"errors\": " <> errors_json <> "}"
    }
  }
}

/// Convert JSON string variables to Dict(String, value.Value)
/// Exported for use by subscription handlers
pub fn json_string_to_variables_dict(
  json_string: String,
) -> dict.Dict(String, value.Value) {
  // First try to extract the "variables" field from the JSON
  let variables_decoder = {
    use vars <- decode.field("variables", decode.dynamic)
    decode.success(vars)
  }

  case json.parse(json_string, variables_decoder) {
    Ok(dyn) -> {
      // Convert dynamic to value.Value
      case converters.json_dynamic_to_value(dyn) {
        value.Object(fields) -> dict.from_list(fields)
        _ -> dict.new()
      }
    }
    Error(_) -> dict.new()
  }
}

/// Re-export parse_json_to_value for WebSocket handler
pub fn parse_json_to_value(json_str: String) -> Result(value.Value, String) {
  converters.parse_json_to_value(json_str)
}

// ─── Private Helpers ───────────────────────────────────────────────

/// Convert a GraphQL value to JSON string
fn value_to_json(val: value.Value) -> String {
  case val {
    value.Null -> "null"
    value.Int(i) -> json.int(i) |> json.to_string
    value.Float(f) -> json.float(f) |> json.to_string
    value.String(s) -> json.string(s) |> json.to_string
    value.Boolean(b) -> json.bool(b) |> json.to_string
    value.Enum(e) -> json.string(e) |> json.to_string
    value.List(items) -> {
      let item_jsons = list.map(items, value_to_json)
      "[" <> string.join(item_jsons, ",") <> "]"
    }
    value.Object(fields) -> {
      let field_jsons =
        list.map(fields, fn(field) {
          let #(key, value) = field
          let key_json = json.string(key) |> json.to_string
          let value_json = value_to_json(value)
          key_json <> ": " <> value_json
        })
      "{" <> string.join(field_jsons, ",") <> "}"
    }
  }
}
```

**Step 2: Run gleam check to verify syntax**

Run: `cd /Users/chadmiller/code/quickslice/.worktrees/graphql-refactor/server && gleam check`
Expected: No errors

**Step 3: Commit**

```bash
git add src/graphql/lexicon/schema.gleam
git commit -m "refactor: add lexicon GraphQL schema entry point"
```

---

### Task 5: Update handler imports

**Files:**
- Modify: `server/src/handlers/graphql.gleam:14`
- Modify: `server/src/handlers/graphql_ws.gleam:13`
- Modify: `server/src/lib/mcp/tools/graphql.gleam:5`

**Step 1: Update handlers/graphql.gleam**

Change line 14 from:
```gleam
import graphql_gleam
```

To:
```gleam
import graphql/lexicon/schema as lexicon_schema
```

Change line 143 from:
```gleam
    graphql_gleam.execute_query_with_db(
```

To:
```gleam
    lexicon_schema.execute_query_with_db(
```

**Step 2: Update handlers/graphql_ws.gleam**

Change line 13 from:
```gleam
import graphql_gleam
```

To:
```gleam
import graphql/lexicon/schema as lexicon_schema
```

Change line 51 (parse_json_to_value call):
```gleam
  let value_object = case graphql_gleam.parse_json_to_value(event.value) {
```

To:
```gleam
  let value_object = case lexicon_schema.parse_json_to_value(event.value) {
```

Change line 94 (format_response call):
```gleam
  Ok(graphql_gleam.format_response(response))
```

To:
```gleam
  Ok(lexicon_schema.format_response(response))
```

Change line 198 (build_schema_from_db call):
```gleam
        graphql_gleam.build_schema_from_db(
```

To:
```gleam
        lexicon_schema.build_schema_from_db(
```

Change line 361 (json_string_to_variables_dict call):
```gleam
                      graphql_gleam.json_string_to_variables_dict(vars_json)
```

To:
```gleam
                      lexicon_schema.json_string_to_variables_dict(vars_json)
```

**Step 3: Update lib/mcp/tools/graphql.gleam**

Change line 5 from:
```gleam
import graphql_gleam
```

To:
```gleam
import graphql/lexicon/schema as lexicon_schema
```

Change line 18 from:
```gleam
  use result_str <- result.try(graphql_gleam.execute_query_with_db(
```

To:
```gleam
  use result_str <- result.try(lexicon_schema.execute_query_with_db(
```

**Step 4: Run gleam check to verify all imports are correct**

Run: `cd /Users/chadmiller/code/quickslice/.worktrees/graphql-refactor/server && gleam check`
Expected: No errors

**Step 5: Commit**

```bash
git add src/handlers/graphql.gleam src/handlers/graphql_ws.gleam src/lib/mcp/tools/graphql.gleam
git commit -m "refactor: update handlers to use new lexicon schema module"
```

---

### Task 6: Delete old files

**Files:**
- Delete: `server/src/graphql_gleam.gleam`
- Delete: `server/src/mutation_resolvers.gleam`

**Step 1: Delete the old files**

```bash
rm /Users/chadmiller/code/quickslice/.worktrees/graphql-refactor/server/src/graphql_gleam.gleam
rm /Users/chadmiller/code/quickslice/.worktrees/graphql-refactor/server/src/mutation_resolvers.gleam
```

**Step 2: Run gleam check to verify no broken imports**

Run: `cd /Users/chadmiller/code/quickslice/.worktrees/graphql-refactor/server && gleam check`
Expected: No errors

**Step 3: Run gleam build to verify full compilation**

Run: `cd /Users/chadmiller/code/quickslice/.worktrees/graphql-refactor/server && gleam build`
Expected: Compiles successfully

**Step 4: Commit**

```bash
git add -A
git commit -m "refactor: remove old graphql_gleam.gleam and mutation_resolvers.gleam"
```

---

### Task 7: Final verification

**Step 1: Run full test suite**

Run: `cd /Users/chadmiller/code/quickslice/.worktrees/graphql-refactor/server && gleam test`
Expected: All tests pass

**Step 2: If tests fail, fix any remaining issues**

Common issues to check:
- Missing imports in test files that referenced `graphql_gleam` directly
- Type mismatches from module reorganization

**Step 3: Final commit (if any fixes needed)**

If all tests pass and the build works, the refactor is complete.

---

## Summary

| Module | Lines (est.) | Responsibility |
|--------|--------------|----------------|
| converters.gleam | ~150 | Record → GraphQL value conversion, JSON parsing |
| fetchers.gleam | ~280 | Database fetcher functions for lexicon_graphql |
| mutations.gleam | ~450 | Mutation resolver factories + auth helper |
| schema.gleam | ~180 | Public API: build_schema, execute_query, format_response |

**Total:** ~1060 lines across 4 files (down from ~1777, saving ~700 lines via auth helper consolidation)

**Files to update:**
- `handlers/graphql.gleam` - import change
- `handlers/graphql_ws.gleam` - import change + 4 function call updates
- `lib/mcp/tools/graphql.gleam` - import change + function call update

**Files to delete:**
- `graphql_gleam.gleam`
- `mutation_resolvers.gleam`
