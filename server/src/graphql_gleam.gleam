/// Pure Gleam GraphQL Implementation
///
/// This module provides GraphQL schema building and query execution
import backfill
import config
import cursor
import database
import gleam/dict
import gleam/dynamic
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import lexicon_graphql/aggregate_input
import lexicon_graphql/dataloader
import lexicon_graphql/db_schema_builder
import lexicon_graphql/lexicon_parser
import mutation_resolvers
import sqlight
import swell/executor
import swell/schema
import swell/value
import where_converter

/// Build a GraphQL schema from database lexicons
///
/// This is exposed for WebSocket subscriptions to build the schema once
/// and reuse it for multiple subscription executions.
pub fn build_schema_from_db(
  db: sqlight.Connection,
  auth_base_url: String,
  plc_url: String,
  domain_authority: String,
) -> Result(schema.Schema, String) {
  // Step 1: Fetch lexicons from database
  use lexicon_records <- result.try(
    database.get_all_lexicons(db)
    |> result.map_error(fn(_) { "Failed to fetch lexicons from database" }),
  )

  // Step 2: Parse lexicon JSON into structured Lexicon types
  let parsed_lexicons =
    lexicon_records
    |> list.filter_map(fn(lex) {
      case lexicon_parser.parse_lexicon(lex.json) {
        Ok(parsed) -> Ok(parsed)
        Error(_) -> Error(Nil)
      }
    })

  // Check if we got any valid lexicons
  case parsed_lexicons {
    [] -> Error("No valid lexicons found in database")
    _ -> {
      // Step 3: Create a record fetcher function that queries the database with pagination
      let record_fetcher = fn(
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
          database.get_collection_count_with_where(
            db,
            collection_nsid,
            where_clause,
          )
          |> result.map(option.Some)
          |> result.unwrap(option.None)

        // Fetch records from database for this collection with pagination
        case
          database.get_records_by_collection_paginated_with_where(
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
          Ok(#(records, next_cursor, has_next_page, has_previous_page)) -> {
            // Convert database records to GraphQL values with cursors
            let graphql_records_with_cursors =
              list.map(records, fn(record) {
                let graphql_value = record_to_graphql_value(record, db)
                // Generate cursor for this record
                let record_cursor =
                  cursor.generate_cursor_from_record(
                    database.record_to_record_like(record),
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

      // Step 3.5: Create a batch fetcher function for join operations
      let batch_fetcher = fn(
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
                    case
                      database.get_records_by_dids_and_collection(
                        db,
                        uris,
                        collection,
                      )
                    {
                      Ok(records) -> {
                        // Group records by DID
                        let grouped =
                          list.fold(records, dict.new(), fn(acc, record) {
                            let graphql_value =
                              record_to_graphql_value(record, db)
                            let existing =
                              dict.get(acc, record.did) |> result.unwrap([])
                            dict.insert(acc, record.did, [
                              graphql_value,
                              ..existing
                            ])
                          })
                        Ok(grouped)
                      }
                      Error(_) -> Error("Failed to fetch records by DIDs")
                    }
                  }
                  False -> {
                    // Forward join: fetch records by their URIs
                    case database.get_records_by_uris(db, uris) {
                      Ok(records) -> {
                        // Group records by URI
                        let grouped =
                          list.fold(records, dict.new(), fn(acc, record) {
                            let graphql_value =
                              record_to_graphql_value(record, db)
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
              database.get_records_by_reference_field(
                db,
                collection,
                reference_field,
                uris,
              )
            {
              Ok(records) -> {
                // Group records by the parent URI they reference
                // Parse each record's JSON to extract the reference field value
                let grouped =
                  list.fold(records, dict.new(), fn(acc, record) {
                    let graphql_value = record_to_graphql_value(record, db)
                    // Extract the reference field from the record JSON to find parent URI
                    case extract_reference_uri(record.json, reference_field) {
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
                  "Failed to fetch records by reference field: "
                  <> reference_field,
                )
            }
          }
        }
      }

      // Step 3.6: Create a paginated batch fetcher function for join operations with pagination
      let paginated_batch_fetcher = fn(
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
              database.get_records_by_dids_and_collection_paginated(
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
                records,
                _next_cursor,
                has_next_page,
                has_previous_page,
                total_count,
              )) -> {
                // Convert records to GraphQL values with cursors
                let edges =
                  list.map(records, fn(record) {
                    let graphql_value = record_to_graphql_value(record, db)
                    let cursor =
                      cursor.generate_cursor_from_record(
                        database.record_to_record_like(record),
                        db_sort_by,
                      )
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
              database.get_records_by_reference_field_paginated(
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
                records,
                _next_cursor,
                has_next_page,
                has_previous_page,
                total_count,
              )) -> {
                // Convert records to GraphQL values with cursors
                let edges =
                  list.map(records, fn(record) {
                    let graphql_value = record_to_graphql_value(record, db)
                    let cursor =
                      cursor.generate_cursor_from_record(
                        database.record_to_record_like(record),
                        db_sort_by,
                      )
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

      // Step 4: Determine local and external collections for backfill
      let collection_ids =
        parsed_lexicons
        |> list.filter_map(fn(lex) {
          case
            backfill.nsid_matches_domain_authority(lex.id, domain_authority)
          {
            True -> Ok(lex.id)
            // Local collection, include
            False -> Error(Nil)
            // External collection, skip
          }
        })

      let external_collection_ids =
        parsed_lexicons
        |> list.filter_map(fn(lex) {
          case
            backfill.nsid_matches_domain_authority(lex.id, domain_authority)
          {
            True -> Error(Nil)
            // Local collection, skip
            False -> Ok(lex.id)
            // External collection, include
          }
        })

      // Step 5: Create mutation resolver factories
      let mutation_ctx =
        mutation_resolvers.MutationContext(
          db: db,
          auth_base_url: auth_base_url,
          plc_url: plc_url,
          collection_ids: collection_ids,
          external_collection_ids: external_collection_ids,
        )

      let create_factory =
        option.Some(fn(collection) {
          mutation_resolvers.create_resolver_factory(collection, mutation_ctx)
        })

      let update_factory =
        option.Some(fn(collection) {
          mutation_resolvers.update_resolver_factory(collection, mutation_ctx)
        })

      let delete_factory =
        option.Some(fn(collection) {
          mutation_resolvers.delete_resolver_factory(collection, mutation_ctx)
        })

      let upload_blob_factory =
        option.Some(fn() {
          mutation_resolvers.upload_blob_resolver_factory(mutation_ctx)
        })

      // Step 5.5: Create an aggregate fetcher function
      let aggregate_fetcher = fn(
        collection_nsid: String,
        params: db_schema_builder.AggregateParams,
      ) {
        // Convert GraphQL where clause to SQL where clause
        let where_clause = case params.where {
          option.Some(graphql_where) ->
            option.Some(where_converter.convert_where_clause(graphql_where))
          option.None -> option.None
        }

        // Convert GroupByFieldInput to database.GroupByField
        let group_by_fields =
          list.map(params.group_by, fn(gb) {
            case gb.interval {
              option.Some(interval) -> {
                let db_interval = case interval {
                  aggregate_input.Hour -> database.Hour
                  aggregate_input.Day -> database.Day
                  aggregate_input.Week -> database.Week
                  aggregate_input.Month -> database.Month
                }
                database.TruncatedField(gb.field, db_interval)
              }
              option.None -> database.SimpleField(gb.field)
            }
          })

        // Call database aggregation function
        database.get_aggregated_records(
          db,
          collection_nsid,
          group_by_fields,
          where_clause,
          params.order_by_desc,
          params.limit,
        )
        |> result.map_error(fn(_) { "Failed to fetch aggregated records" })
      }

      // Step 6: Build schema with database-backed resolvers, mutations, and subscriptions
      db_schema_builder.build_schema_with_subscriptions(
        parsed_lexicons,
        record_fetcher,
        option.Some(batch_fetcher),
        option.Some(paginated_batch_fetcher),
        create_factory,
        update_factory,
        delete_factory,
        upload_blob_factory,
        option.Some(aggregate_fetcher),
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
  auth_base_url: String,
  plc_url: String,
) -> Result(String, String) {
  // Start config cache actor to get domain authority
  let assert Ok(config_subject) = config.start(db)
  let domain_authority = case config.get_domain_authority(config_subject) {
    option.Some(authority) -> authority
    option.None -> ""
  }

  // Build the schema
  use graphql_schema <- result.try(build_schema_from_db(
    db,
    auth_base_url,
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

/// Convert a database Record to a GraphQL value.Value
///
/// Creates an Object with all the record metadata plus the parsed JSON value
pub fn record_to_graphql_value(
  record: database.Record,
  db: sqlight.Connection,
) -> value.Value {
  // Parse the record JSON and convert to GraphQL value
  let value_object = case parse_json_to_value(record.json) {
    Ok(val) -> val
    Error(_) -> value.Object([])
    // Fallback to empty object on parse error
  }

  // Look up actor handle from actor table
  let actor_handle = case database.get_actor(db, record.did) {
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
fn dynamic_to_value(dyn: dynamic.Dynamic) -> value.Value {
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

/// Format an executor.Response as JSON string
pub fn format_response(response: executor.Response) -> String {
  let data_json = value_to_json(response.data)

  let errors_json = case response.errors {
    [] -> "[]"
    errors -> {
      let error_strings =
        list.map(errors, fn(err) {
          let message_json = json.string(err.message) |> json.to_string
          let path_json =
            json.array(err.path, of: json.string) |> json.to_string

          "{\"message\": " <> message_json <> ", \"path\": " <> path_json <> "}"
        })

      "[" <> string.join(error_strings, ",") <> "]"
    }
  }

  "{\"data\": " <> data_json <> ", \"errors\": " <> errors_json <> "}"
}

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
      case json_dynamic_to_value(dyn) {
        value.Object(fields) -> dict.from_list(fields)
        _ -> dict.new()
      }
    }
    Error(_) -> dict.new()
  }
}

/// Convert a dynamic JSON value to graphql value.Value
fn json_dynamic_to_value(dyn: dynamic.Dynamic) -> value.Value {
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
fn extract_reference_uri(
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
