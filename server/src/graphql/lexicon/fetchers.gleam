/// Database fetchers for lexicon GraphQL API
///
/// These functions bridge the database layer to the lexicon_graphql library's
/// expected fetcher signatures for queries, joins, and aggregations.
import atproto_auth
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
    let total_count =
      records.get_collection_count_with_where(db, collection_nsid, where_clause)
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
