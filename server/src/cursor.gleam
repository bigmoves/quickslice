/// Cursor-based pagination utilities.
///
/// Cursors encode the position in a result set as base64(field1|field2|...|cid)
/// to enable stable pagination even when new records are inserted.
///
/// The cursor format:
/// - All sort field values are included in the cursor
/// - Values are separated by pipe (|) characters
/// - CID is always the last element as the ultimate tiebreaker
import gleam/bit_array
import gleam/dict
import gleam/dynamic
import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// Decoded cursor components for pagination
pub type DecodedCursor {
  DecodedCursor(
    /// Field values in the order they appear in sortBy
    field_values: List(String),
    /// CID (always the last element)
    cid: String,
  )
}

/// Encodes a string to URL-safe base64 without padding
pub fn encode_base64(input: String) -> String {
  let bytes = bit_array.from_string(input)
  let encoded = bit_array.base64_url_encode(bytes, False)
  encoded
}

/// Decodes a URL-safe base64 string without padding
pub fn decode_base64(input: String) -> Result(String, String) {
  case bit_array.base64_url_decode(input) {
    Ok(bytes) ->
      case bit_array.to_string(bytes) {
        Ok(str) -> Ok(str)
        Error(_) -> Error("Invalid UTF-8 in cursor")
      }
    Error(_) -> Error("Failed to decode base64")
  }
}

/// Record-like type for cursor generation
/// This allows cursor to work with any record type without importing database
pub type RecordLike {
  RecordLike(
    uri: String,
    cid: String,
    did: String,
    collection: String,
    json: String,
    indexed_at: String,
  )
}

/// Extracts a field value from a record.
///
/// Handles both table columns and JSON fields with nested paths.
pub fn extract_field_value(record: RecordLike, field: String) -> String {
  case field {
    "uri" -> record.uri
    "cid" -> record.cid
    "did" -> record.did
    "collection" -> record.collection
    "indexed_at" -> record.indexed_at
    _ -> extract_json_field(record.json, field)
  }
}

/// Extracts a value from a JSON string using a field path
fn extract_json_field(json_str: String, field: String) -> String {
  // Parse the JSON as a dictionary
  let decoder = decode.dict(decode.string, decode.dynamic)
  case json.parse(json_str, decoder) {
    Error(_) -> "NULL"
    Ok(dict) -> {
      // Split field path by dots for nested access
      let path_parts = string.split(field, ".")

      // Navigate through the JSON structure
      extract_from_dict(dict, path_parts)
    }
  }
}

/// Recursively extracts a value from a dict using a path
fn extract_from_dict(
  dict: dict.Dict(String, dynamic.Dynamic),
  path: List(String),
) -> String {
  case path {
    [] -> "NULL"
    [key] -> {
      // Final key - extract and convert to string
      case dict.get(dict, key) {
        Ok(val) -> dynamic_to_string(val)
        Error(_) -> "NULL"
      }
    }
    [key, ..rest] -> {
      // Intermediate key - try to decode as nested dict
      case dict.get(dict, key) {
        Ok(val) -> {
          case decode.run(val, decode.dict(decode.string, decode.dynamic)) {
            Ok(nested_dict) -> extract_from_dict(nested_dict, rest)
            Error(_) -> "NULL"
          }
        }
        Error(_) -> "NULL"
      }
    }
  }
}

/// Converts a dynamic JSON value to a string representation
fn dynamic_to_string(value: dynamic.Dynamic) -> String {
  // Try to decode as string
  case decode.run(value, decode.string) {
    Ok(s) -> s
    Error(_) ->
      // Try as int
      case decode.run(value, decode.int) {
        Ok(i) -> int.to_string(i)
        Error(_) ->
          // Try as float
          case decode.run(value, decode.float) {
            Ok(f) -> float.to_string(f)
            Error(_) ->
              // Try as bool
              case decode.run(value, decode.bool) {
                Ok(b) ->
                  case b {
                    True -> "true"
                    False -> "false"
                  }
                Error(_) -> "NULL"
              }
          }
      }
  }
}

/// Generates a cursor from a record based on the sort configuration.
///
/// Extracts all sort field values from the record and encodes them along with the CID.
/// Format: `base64(field1_value|field2_value|...|cid)`
pub fn generate_cursor_from_record(
  record: RecordLike,
  sort_by: Option(List(#(String, String))),
) -> String {
  let cursor_parts = case sort_by {
    None -> []
    Some(sort_fields) -> {
      list.map(sort_fields, fn(sort_field) {
        let #(field, _direction) = sort_field
        extract_field_value(record, field)
      })
    }
  }

  // Always add CID as the final tiebreaker
  let all_parts = list.append(cursor_parts, [record.cid])

  // Join with pipe and encode
  let cursor_content = string.join(all_parts, "|")
  encode_base64(cursor_content)
}

/// Decodes a base64-encoded cursor back into its components.
///
/// The cursor format is: `base64(field1|field2|...|cid)`
pub fn decode_cursor(
  cursor: String,
  sort_by: Option(List(#(String, String))),
) -> Result(DecodedCursor, String) {
  use decoded_str <- result.try(decode_base64(cursor))

  let parts = string.split(decoded_str, "|")

  // Validate cursor format matches sortBy fields
  let expected_parts = case sort_by {
    None -> 1
    Some(fields) -> list.length(fields) + 1
  }

  case list.length(parts) == expected_parts {
    False ->
      Error(
        "Invalid cursor format: expected "
        <> int.to_string(expected_parts)
        <> " parts, got "
        <> int.to_string(list.length(parts)),
      )
    True -> {
      // Last part is the CID
      case list.reverse(parts) {
        [cid, ..rest_reversed] -> {
          let field_values = list.reverse(rest_reversed)
          Ok(DecodedCursor(field_values: field_values, cid: cid))
        }
        [] -> Error("Cursor has no parts")
      }
    }
  }
}

/// Builds cursor-based WHERE conditions for proper multi-field pagination.
///
/// Creates progressive equality checks for stable multi-field sorting.
/// For each field, we OR together:
/// 1. field1 > cursor_value1
/// 2. field1 = cursor_value1 AND field2 > cursor_value2
/// 3. field1 = cursor_value1 AND field2 = cursor_value2 AND field3 > cursor_value3
///    ... and so on
///    Finally: all fields equal AND cid > cursor_cid
///
/// Returns: #(where_clause_sql, bind_values)
pub fn build_cursor_where_clause(
  decoded_cursor: DecodedCursor,
  sort_by: Option(List(#(String, String))),
  is_before: Bool,
) -> #(String, List(String)) {
  let sort_fields = case sort_by {
    None -> []
    Some(fields) -> fields
  }

  case list.is_empty(sort_fields) {
    True -> #("1=1", [])
    False -> {
      let clauses =
        build_progressive_clauses(
          sort_fields,
          decoded_cursor.field_values,
          decoded_cursor.cid,
          is_before,
        )

      let sql = "(" <> string.join(clauses.0, " OR ") <> ")"
      #(sql, clauses.1)
    }
  }
}

/// Builds progressive equality clauses for cursor pagination
fn build_progressive_clauses(
  sort_fields: List(#(String, String)),
  field_values: List(String),
  cid: String,
  is_before: Bool,
) -> #(List(String), List(String)) {
  let _field_count = list.length(sort_fields)

  // Build clauses for each level
  let #(clauses, params) =
    list.index_map(sort_fields, fn(field, i) {
      // Build equality checks for fields [0..i-1]
      let #(equality_parts, equality_params) = case i {
        0 -> #([], [])
        _ -> {
          list.range(0, i - 1)
          |> list.fold(#([], []), fn(eq_acc, j) {
            let #(eq_parts, eq_params) = eq_acc
            let prior_field =
              list_at(sort_fields, j) |> result.unwrap(#("", ""))
            let value = list_at(field_values, j) |> result.unwrap("")

            let field_ref = build_field_reference(prior_field.0)
            let new_part = field_ref <> " = ?"
            let new_params = list.append(eq_params, [value])

            #(list.append(eq_parts, [new_part]), new_params)
          })
        }
      }

      // Add comparison for current field
      let value = list_at(field_values, i) |> result.unwrap("")

      let comparison_op = get_comparison_operator(field.1, is_before)
      let field_ref = build_field_reference(field.0)

      let comparison_part = field_ref <> " " <> comparison_op <> " ?"
      let all_parts = list.append(equality_parts, [comparison_part])
      let all_params = list.append(equality_params, [value])

      // Combine with AND
      let clause = "(" <> string.join(all_parts, " AND ") <> ")"

      #(clause, all_params)
    })
    |> list.unzip
    |> fn(unzipped) {
      // Flatten the params lists
      let flattened_params = list.flatten(unzipped.1)
      #(unzipped.0, flattened_params)
    }

  // Add final clause: all fields equal AND cid comparison
  let #(final_equality_parts, final_equality_params) =
    list.index_map(sort_fields, fn(field, j) {
      let value = list_at(field_values, j) |> result.unwrap("")
      let field_ref = build_field_reference(field.0)
      #(field_ref <> " = ?", value)
    })
    |> list.unzip

  // CID comparison uses the direction of the last sort field
  let last_field = list.last(sort_fields) |> result.unwrap(#("", "desc"))
  let cid_comparison_op = get_comparison_operator(last_field.1, is_before)

  let final_parts =
    list.append(final_equality_parts, ["cid " <> cid_comparison_op <> " ?"])
  let final_params = list.append(final_equality_params, [cid])

  let final_clause = "(" <> string.join(final_parts, " AND ") <> ")"
  let all_clauses = list.append(clauses, [final_clause])
  let all_params = list.append(params, final_params)

  #(all_clauses, all_params)
}

/// Builds a field reference for SQL queries (handles JSON fields)
fn build_field_reference(field: String) -> String {
  case field {
    "uri" | "cid" | "did" | "collection" | "indexed_at" -> field
    _ -> {
      // JSON field - use json_extract with JSON path
      let json_path = "$." <> string.replace(field, ".", ".")
      "json_extract(json, '" <> json_path <> "')"
    }
  }
}

/// Gets the comparison operator based on sort direction and pagination direction
fn get_comparison_operator(direction: String, is_before: Bool) -> String {
  let is_desc = string.lowercase(direction) == "desc"

  case is_before {
    True ->
      case is_desc {
        True -> ">"
        False -> "<"
      }
    False ->
      case is_desc {
        True -> "<"
        False -> ">"
      }
  }
}

/// Helper to get an element at an index from a list
fn list_at(list: List(a), index: Int) -> Result(a, Nil) {
  list
  |> list.drop(index)
  |> list.first
}
