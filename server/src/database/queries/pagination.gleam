/// Pagination utilities including cursor encoding/decoding and ORDER BY building.
///
/// Cursors encode the position in a result set as base64(field1|field2|...|cid)
/// to enable stable pagination even when new records are inserted.
///
/// The cursor format:
/// - All sort field values are included in the cursor
/// - Values are separated by pipe (|) characters
/// - CID is always the last element as the ultimate tiebreaker
import database/executor.{type Executor}
import database/types.{type Record}
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

// ===== Cursor Types =====

/// Decoded cursor components for pagination
pub type DecodedCursor {
  DecodedCursor(
    /// Field values in the order they appear in sortBy
    field_values: List(String),
    /// CID (always the last element)
    cid: String,
  )
}

// ===== Base64 Encoding/Decoding =====

/// Encodes a string to URL-safe base64 without padding
pub fn encode_base64(input: String) -> String {
  let bytes = bit_array.from_string(input)
  bit_array.base64_url_encode(bytes, False)
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

// ===== Field Value Extraction =====

/// Extracts a field value from a record.
///
/// Handles both table columns and JSON fields with nested paths.
pub fn extract_field_value(record: Record, field: String) -> String {
  case field {
    "uri" -> record.uri
    "cid" -> record.cid
    "did" -> record.did
    "collection" -> record.collection
    "indexed_at" -> record.indexed_at
    "rkey" -> record.rkey
    _ -> extract_json_field(record.json, field)
  }
}

/// Extracts a value from a JSON string using a field path
fn extract_json_field(json_str: String, field: String) -> String {
  let decoder = decode.dict(decode.string, decode.dynamic)
  case json.parse(json_str, decoder) {
    Error(_) -> "NULL"
    Ok(parsed_dict) -> {
      let path_parts = string.split(field, ".")
      extract_from_dict(parsed_dict, path_parts)
    }
  }
}

/// Recursively extracts a value from a dict using a path
fn extract_from_dict(
  d: dict.Dict(String, dynamic.Dynamic),
  path: List(String),
) -> String {
  case path {
    [] -> "NULL"
    [key] -> {
      case dict.get(d, key) {
        Ok(val) -> dynamic_to_string(val)
        Error(_) -> "NULL"
      }
    }
    [key, ..rest] -> {
      case dict.get(d, key) {
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
  case decode.run(value, decode.string) {
    Ok(s) -> s
    Error(_) ->
      case decode.run(value, decode.int) {
        Ok(i) -> int.to_string(i)
        Error(_) ->
          case decode.run(value, decode.float) {
            Ok(f) -> float.to_string(f)
            Error(_) ->
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

// ===== Cursor Generation and Decoding =====

/// Generates a cursor from a record based on the sort configuration.
///
/// Extracts all sort field values from the record and encodes them along with the CID.
/// Format: `base64(field1_value|field2_value|...|cid)`
pub fn generate_cursor_from_record(
  record: Record,
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

  let all_parts = list.append(cursor_parts, [record.cid])
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

// ===== Cursor WHERE Clause Building =====

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
  exec: Executor,
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
          exec,
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
  exec: Executor,
  sort_fields: List(#(String, String)),
  field_values: List(String),
  cid: String,
  is_before: Bool,
) -> #(List(String), List(String)) {
  let #(clauses, params) =
    list.index_map(sort_fields, fn(field, i) {
      let #(equality_parts, equality_params) = case i {
        0 -> #([], [])
        _ -> {
          list.range(0, i - 1)
          |> list.fold(#([], []), fn(eq_acc, j) {
            let #(eq_parts, eq_params) = eq_acc
            let prior_field =
              list_at(sort_fields, j) |> result.unwrap(#("", ""))
            let value = list_at(field_values, j) |> result.unwrap("")

            let field_ref = build_cursor_field_reference(exec, prior_field.0)
            let new_part = field_ref <> " = ?"
            let new_params = list.append(eq_params, [value])

            #(list.append(eq_parts, [new_part]), new_params)
          })
        }
      }

      let value = list_at(field_values, i) |> result.unwrap("")

      let comparison_op = get_comparison_operator(field.1, is_before)
      let field_ref = build_cursor_field_reference(exec, field.0)

      let comparison_part = field_ref <> " " <> comparison_op <> " ?"
      let all_parts = list.append(equality_parts, [comparison_part])
      let all_params = list.append(equality_params, [value])

      let clause = "(" <> string.join(all_parts, " AND ") <> ")"

      #(clause, all_params)
    })
    |> list.unzip
    |> fn(unzipped) {
      let flattened_params = list.flatten(unzipped.1)
      #(unzipped.0, flattened_params)
    }

  let #(final_equality_parts, final_equality_params) =
    list.index_map(sort_fields, fn(field, j) {
      let value = list_at(field_values, j) |> result.unwrap("")
      let field_ref = build_cursor_field_reference(exec, field.0)
      #(field_ref <> " = ?", value)
    })
    |> list.unzip

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

/// Builds a field reference for cursor SQL queries (handles JSON fields)
fn build_cursor_field_reference(exec: Executor, field: String) -> String {
  case field {
    "uri" | "cid" | "did" | "collection" | "indexed_at" | "rkey" -> field
    _ -> executor.json_extract(exec, "json", field)
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
fn list_at(l: List(a), index: Int) -> Result(a, Nil) {
  l
  |> list.drop(index)
  |> list.first
}

// ===== Sort Direction Helpers =====

/// Reverses sort direction for backward pagination
pub fn reverse_sort_direction(direction: String) -> String {
  case string.lowercase(direction) {
    "asc" -> "desc"
    "desc" -> "asc"
    _ -> "asc"
  }
}

/// Reverses all sort fields for backward pagination
pub fn reverse_sort_fields(
  sort_fields: List(#(String, String)),
) -> List(#(String, String)) {
  list.map(sort_fields, fn(field) {
    let #(field_name, direction) = field
    #(field_name, reverse_sort_direction(direction))
  })
}

// ===== ORDER BY Building =====

/// Builds an ORDER BY clause from sort fields
/// use_table_prefix: if True, prefixes table columns with "record." for joins
pub fn build_order_by(
  exec: Executor,
  sort_fields: List(#(String, String)),
  use_table_prefix: Bool,
) -> String {
  let order_parts =
    list.map(sort_fields, fn(field) {
      let #(field_name, direction) = field
      let table_prefix = case use_table_prefix {
        True -> "record."
        False -> ""
      }
      let field_ref = case field_name {
        "uri" | "cid" | "did" | "collection" | "indexed_at" | "rkey" ->
          table_prefix <> field_name
        "createdAt" | "indexedAt" -> {
          let json_field =
            executor.json_extract(exec, table_prefix <> "json", field_name)
          // Validate datetime - syntax differs by dialect
          case executor.dialect(exec) {
            executor.SQLite -> "CASE
            WHEN " <> json_field <> " IS NULL THEN NULL
            WHEN datetime(" <> json_field <> ") IS NULL THEN NULL
            ELSE " <> json_field <> "
           END"
            executor.PostgreSQL ->
              // PostgreSQL: check if value is a valid timestamp format
              "CASE
            WHEN " <> json_field <> " IS NULL THEN NULL
            WHEN " <> json_field <> " ~ '^\\d{4}-\\d{2}-\\d{2}' THEN " <> json_field <> "
            ELSE NULL
           END"
          }
        }
        _ -> executor.json_extract(exec, table_prefix <> "json", field_name)
      }
      let dir = case string.lowercase(direction) {
        "asc" -> "ASC"
        _ -> "DESC"
      }
      field_ref <> " " <> dir <> " NULLS LAST"
    })

  case list.is_empty(order_parts) {
    True -> {
      let prefix = case use_table_prefix {
        True -> "record."
        False -> ""
      }
      prefix <> "indexed_at DESC NULLS LAST"
    }
    False -> string.join(order_parts, ", ")
  }
}
