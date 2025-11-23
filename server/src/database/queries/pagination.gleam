import cursor
import database/types.{type Record}
import gleam/list
import gleam/string

// ===== Pagination Helper Functions =====

/// Converts a Record to a cursor.RecordLike for cursor encoding
pub fn record_to_record_like(record: Record) -> cursor.RecordLike {
  cursor.RecordLike(
    uri: record.uri,
    cid: record.cid,
    did: record.did,
    collection: record.collection,
    json: record.json,
    indexed_at: record.indexed_at,
  )
}

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

/// Builds an ORDER BY clause from sort fields
/// use_table_prefix: if True, prefixes table columns with "record." for joins
pub fn build_order_by(
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
        "uri" | "cid" | "did" | "collection" | "indexed_at" ->
          table_prefix <> field_name
        // For JSON fields, check if they look like dates and handle accordingly
        "createdAt" | "indexedAt" -> {
          // Use CASE to treat invalid dates as NULL for sorting
          let json_field =
            "json_extract(" <> table_prefix <> "json, '$." <> field_name <> "')"
          "CASE
            WHEN " <> json_field <> " IS NULL THEN NULL
            WHEN datetime(" <> json_field <> ") IS NULL THEN NULL
            ELSE " <> json_field <> "
           END"
        }
        _ ->
          "json_extract(" <> table_prefix <> "json, '$." <> field_name <> "')"
      }
      let dir = case string.lowercase(direction) {
        "asc" -> "ASC"
        _ -> "DESC"
      }
      // Always put NULLs last regardless of sort direction
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
