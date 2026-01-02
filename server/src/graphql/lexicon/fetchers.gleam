/// Database fetchers for lexicon GraphQL API
///
/// These functions bridge the database layer to the lexicon_graphql library's
/// expected fetcher signatures for queries, joins, and aggregations.
import atproto_auth
import database/executor.{type Executor}
import database/queries/aggregates
import database/queries/pagination
import database/repositories/actors
import database/repositories/labels
import database/repositories/records
import database/types
import gleam/dict
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import graphql/lexicon/converters
import graphql/where_converter
import lexicon_graphql/input/aggregate
import lexicon_graphql/query/dataloader
import lexicon_graphql/schema/database
import swell/value

/// Filter out records with active takedown labels
fn filter_takedowns(
  db: Executor,
  record_list: List(types.Record),
) -> List(types.Record) {
  let record_uris = list.map(record_list, fn(r) { r.uri })
  let takedown_uris = case labels.get_takedown_uris(db, record_uris) {
    Ok(uris) -> uris
    Error(_) -> []
  }
  list.filter(record_list, fn(record) {
    !list.contains(takedown_uris, record.uri)
  })
}

/// Parse self-labels from a record's JSON if present
/// Self-labels have format: {"$type": "com.atproto.label.defs#selfLabels", "values": [{"val": "..."}]}
fn parse_self_labels_from_json(
  json_str: String,
  uri: String,
) -> List(value.Value) {
  // Decoder for self-labels structure
  let self_labels_decoder = {
    use type_field <- decode.field("$type", decode.string)
    use values <- decode.field(
      "values",
      decode.list({
        use val <- decode.field("val", decode.string)
        decode.success(val)
      }),
    )
    decode.success(#(type_field, values))
  }

  let labels_field_decoder = {
    use labels <- decode.field("labels", self_labels_decoder)
    decode.success(labels)
  }

  case json.parse(json_str, labels_field_decoder) {
    Error(_) -> []
    Ok(#(type_field, vals)) -> {
      case type_field == "com.atproto.label.defs#selfLabels" {
        False -> []
        True -> {
          // Extract DID from URI (at://did:plc:xxx/...)
          let src = case string.split(uri, "/") {
            ["at:", "", did, ..] -> did
            _ -> ""
          }
          list.map(vals, fn(val) {
            value.Object([
              #("val", value.String(val)),
              #("src", value.String(src)),
              #("uri", value.String(uri)),
              #("neg", value.Boolean(False)),
              #("cts", value.Null),
              #("exp", value.Null),
              #("cid", value.Null),
              #("id", value.Null),
            ])
          })
        }
      }
    }
  }
}

/// Create a record fetcher for paginated collection queries
pub fn record_fetcher(db: Executor) {
  fn(collection_nsid: String, pagination_params: dataloader.PaginationParams) -> Result(
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
    // Subtract takedown count for accurate pagination
    let raw_count =
      records.get_collection_count_with_where(db, collection_nsid, where_clause)
      |> result.unwrap(0)
    let takedown_count =
      labels.count_takedowns_for_collection(db, collection_nsid)
      |> result.unwrap(0)
    let total_count = option.Some(int.max(0, raw_count - takedown_count))

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
        // Filter out records with takedown labels
        let filtered_records = filter_takedowns(db, record_list)

        // Convert database records to GraphQL values with cursors
        let graphql_records_with_cursors =
          list.map(filtered_records, fn(record) {
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
pub fn batch_fetcher(db: Executor) {
  fn(uris: List(String), collection: String, field: option.Option(String)) -> Result(
    dataloader.BatchResult,
    String,
  ) {
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
                    // Filter out records with takedown labels
                    let filtered_records = filter_takedowns(db, record_list)

                    // Group records by DID
                    let grouped =
                      list.fold(filtered_records, dict.new(), fn(acc, record) {
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
                    // Filter out records with takedown labels
                    let filtered_records = filter_takedowns(db, record_list)

                    // Group records by URI
                    let grouped =
                      list.fold(filtered_records, dict.new(), fn(acc, record) {
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
            // Filter out records with takedown labels
            let filtered_records = filter_takedowns(db, record_list)

            // Group records by the parent URI they reference
            // Parse each record's JSON to extract the reference field value
            let grouped =
              list.fold(filtered_records, dict.new(), fn(acc, record) {
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
pub fn paginated_batch_fetcher(db: Executor) {
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
            // Filter out records with takedown labels
            let filtered_records = filter_takedowns(db, record_list)

            // Convert records to GraphQL values with cursors
            let edges =
              list.map(filtered_records, fn(record) {
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
            // Filter out records with takedown labels
            let filtered_records = filter_takedowns(db, record_list)

            // Convert records to GraphQL values with cursors
            let edges =
              list.map(filtered_records, fn(record) {
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
pub fn aggregate_fetcher(db: Executor) {
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
pub fn viewer_fetcher(db: Executor) {
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

/// Create a viewer state fetcher for checking viewer relationships
/// This fetches records owned by the viewer that reference parent keys
pub fn viewer_state_fetcher(db: Executor) {
  fn(
    viewer_did: String,
    collection: String,
    reference_field: String,
    parent_keys: List(String),
  ) -> Result(dict.Dict(String, value.Value), String) {
    // Fetch records owned by viewer_did that reference any of the parent_keys
    case
      records.get_viewer_state_records(
        db,
        viewer_did,
        collection,
        reference_field,
        parent_keys,
      )
    {
      Ok(record_list) -> {
        // Create a dict mapping parent_key -> single record (the viewer's record)
        let result =
          list.fold(record_list, dict.new(), fn(acc, record) {
            let graphql_value = converters.record_to_graphql_value(record, db)
            // Extract the reference field value to find which parent this belongs to
            case
              converters.extract_reference_uri(record.json, reference_field)
            {
              Ok(parent_key) -> {
                // Only keep first record per parent (there should only be one)
                case dict.has_key(acc, parent_key) {
                  True -> acc
                  False -> dict.insert(acc, parent_key, graphql_value)
                }
              }
              Error(_) -> acc
            }
          })
        Ok(result)
      }
      Error(_) ->
        Error("Failed to fetch viewer state records for " <> collection)
    }
  }
}

/// Create a notification fetcher for cross-collection DID mention queries
pub fn notification_fetcher(db: Executor) {
  fn(
    did: String,
    collections: option.Option(List(String)),
    first: option.Option(Int),
    after: option.Option(String),
  ) -> Result(
    #(List(#(value.Value, String)), option.Option(String), Bool, Bool),
    String,
  ) {
    use result <- result.try(
      records.get_notifications(db, did, collections, first, after)
      |> result.map_error(fn(e) { "Database error: " <> string.inspect(e) }),
    )

    let #(records_list, end_cursor, has_next, has_prev) = result

    // Convert database records to GraphQL values with cursors
    let converted =
      list.map(records_list, fn(record) {
        let graphql_value = converters.record_to_graphql_value(record, db)
        // Generate cursor for this record (no sort_by for notifications)
        let cursor = pagination.generate_cursor_from_record(record, option.None)
        #(graphql_value, cursor)
      })

    Ok(#(converted, end_cursor, has_next, has_prev))
  }
}

/// Create a labels fetcher for batch loading labels by URI
/// Now accepts tuples of (uri, optional_record_json) to support self-labels
pub fn labels_fetcher(db: Executor) {
  fn(uris_with_json: List(#(String, option.Option(String)))) -> Result(
    dict.Dict(String, List(value.Value)),
    String,
  ) {
    let uris = list.map(uris_with_json, fn(pair) { pair.0 })

    case labels.get_by_uris(db, uris) {
      Ok(label_list) -> {
        // Group moderator labels by URI
        let mod_labels =
          list.fold(label_list, dict.new(), fn(acc, label) {
            let label_value =
              value.Object([
                #("id", value.Int(label.id)),
                #("src", value.String(label.src)),
                #("uri", value.String(label.uri)),
                #("cid", case label.cid {
                  option.Some(c) -> value.String(c)
                  option.None -> value.Null
                }),
                #("val", value.String(label.val)),
                #("neg", value.Boolean(label.neg)),
                #("cts", value.String(label.cts)),
                #("exp", case label.exp {
                  option.Some(e) -> value.String(e)
                  option.None -> value.Null
                }),
              ])
            let existing = dict.get(acc, label.uri) |> result.unwrap([])
            dict.insert(acc, label.uri, [label_value, ..existing])
          })

        // Merge with self-labels from record JSON
        let merged =
          list.fold(uris_with_json, mod_labels, fn(acc, pair) {
            let #(uri, json_opt) = pair
            case json_opt {
              option.None -> acc
              option.Some(json_str) -> {
                let self_labels = parse_self_labels_from_json(json_str, uri)
                case self_labels {
                  [] -> acc
                  _ -> {
                    let existing = dict.get(acc, uri) |> result.unwrap([])
                    dict.insert(acc, uri, list.append(self_labels, existing))
                  }
                }
              }
            }
          })

        Ok(merged)
      }
      Error(_) -> Error("Failed to fetch labels")
    }
  }
}
