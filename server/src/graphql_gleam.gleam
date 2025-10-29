/// Pure Gleam GraphQL Implementation
///
/// This module provides GraphQL schema building and query execution using
/// pure Gleam code, replacing the previous Elixir FFI implementation.
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
import graphql/executor
import graphql/schema
import graphql/value
import lexicon_graphql/db_schema_builder
import lexicon_graphql/lexicon_parser
import sqlight

/// Execute a GraphQL query against lexicons in the database
///
/// This fetches lexicons, builds a schema with database resolvers,
/// executes the query, and returns the result as JSON.
pub fn execute_query_with_db(
  db: sqlight.Connection,
  query_string: String,
) -> Result(String, String) {
  // Step 1: Fetch lexicons from database
  use lexicon_records <- result.try(
    database.get_all_lexicons(db)
    |> result.map_error(fn(_) { "Failed to fetch lexicons from database" }),
  )

  // Step 2: Parse lexicon JSON into structured Lexicon types
  let parsed_lexicons =
    lexicon_records
    |> list.filter_map(fn(lex) { lexicon_parser.parse_lexicon(lex.json) })

  // Check if we got any valid lexicons
  case parsed_lexicons {
    [] -> Error("No valid lexicons found in database")
    _ -> {
      // Step 3: Create a record fetcher function that queries the database with pagination
      let record_fetcher = fn(
        collection_nsid: String,
        pagination_params: db_schema_builder.PaginationParams,
      ) -> Result(
        #(List(#(value.Value, String)), option.Option(String), Bool, Bool),
        String,
      ) {
        // Fetch records from database for this collection with pagination
        case
          database.get_records_by_collection_paginated(
            db,
            collection_nsid,
            pagination_params.first,
            pagination_params.after,
            pagination_params.last,
            pagination_params.before,
            pagination_params.sort_by,
          )
        {
          Error(_) -> Ok(#([], option.None, False, False))
          // Return empty result on error
          Ok(#(records, next_cursor, has_next_page, has_previous_page)) -> {
            // Convert database records to GraphQL values with cursors
            let graphql_records_with_cursors =
              list.map(records, fn(record) {
                let graphql_value = record_to_graphql_value(record)
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
            ))
          }
        }
      }

      // Step 4: Build schema with database-backed resolvers
      use graphql_schema <- result.try(
        db_schema_builder.build_schema_with_fetcher(
          parsed_lexicons,
          record_fetcher,
        ),
      )

      // Step 5: Execute the query
      let ctx = schema.context(option.None)
      use response <- result.try(executor.execute(
        query_string,
        graphql_schema,
        ctx,
      ))

      // Step 6: Format the response as JSON
      Ok(format_response(response))
    }
  }
}

/// Convert a database Record to a GraphQL value.Value
///
/// Creates an Object with all the record metadata plus the parsed JSON value
fn record_to_graphql_value(record: database.Record) -> value.Value {
  // Parse the record JSON and convert to GraphQL value
  let value_object = case parse_json_to_value(record.json) {
    Ok(val) -> val
    Error(_) -> value.Object([])
    // Fallback to empty object on parse error
  }

  // Create the full record object with metadata and value
  value.Object([
    #("uri", value.String(record.uri)),
    #("cid", value.String(record.cid)),
    #("did", value.String(record.did)),
    #("collection", value.String(record.collection)),
    #("indexedAt", value.String(record.indexed_at)),
    #("value", value_object),
  ])
}

/// Parse a JSON string and convert it to a GraphQL value.Value
fn parse_json_to_value(json_str: String) -> Result(value.Value, String) {
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
fn format_response(response: executor.Response) -> String {
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
