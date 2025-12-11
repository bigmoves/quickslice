# Database Helpers Reorganization Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Reorganize vestigial database helpers (`cursor`, `where_clause`, `where_converter`) from `src/` root into proper layered locations with consolidation.

**Architecture:**
- Merge `cursor.gleam` into `database/queries/pagination.gleam` (both handle pagination concerns)
- Move `where_clause.gleam` to `database/queries/where_clause.gleam` (pure SQL builder)
- Move `where_converter.gleam` to `graphql/where_converter.gleam` (GraphQL→SQL bridge belongs in GraphQL layer)

**Tech Stack:** Gleam, SQLite (sqlight)

---

## Summary of Changes

| Old Location | New Location | Action |
|--------------|--------------|--------|
| `src/cursor.gleam` | `src/database/queries/pagination.gleam` | Merge into existing file |
| `src/where_clause.gleam` | `src/database/queries/where_clause.gleam` | Move |
| `src/where_converter.gleam` | `src/graphql/where_converter.gleam` | Move (new graphql/ folder) |

### Files That Need Import Updates

**For `cursor` → `database/queries/pagination`:**
- `src/graphql_gleam.gleam` (lines 6, 120, 283, 328)
- `src/database/repositories/records.gleam` (lines 1, 553, 556, 630, 711, 714, 788, 961, 964, 1041, 1186, 1189, 1266)
- `test/cursor_test.gleam` → rename to `test/pagination_test.gleam`

**For `where_clause` → `database/queries/where_clause`:**
- `src/where_converter.gleam` (line 10) - will become `graphql/where_converter`
- `src/database/queries/aggregates.gleam` (line 14)
- `src/database/repositories/records.gleam` (line 15)
- `test/where_clause_test.gleam`
- `test/where_sql_builder_test.gleam`
- `test/where_integration_test.gleam`
- `test/where_edge_cases_test.gleam`
- `test/database_aggregation_test.gleam`

**For `where_converter` → `graphql/where_converter`:**
- `src/graphql_gleam.gleam` (line 33)
- `test/graphql_where_integration_test.gleam` (line 15)

---

## Task 1: Move where_clause.gleam to database/queries/

**Files:**
- Move: `src/where_clause.gleam` → `src/database/queries/where_clause.gleam`

**Step 1: Copy the file to new location**

```bash
cp server/src/where_clause.gleam server/src/database/queries/where_clause.gleam
```

**Step 2: Verify file exists at new location**

```bash
ls -la server/src/database/queries/where_clause.gleam
```

Expected: File exists

**Step 3: Delete old file**

```bash
rm server/src/where_clause.gleam
```

**Step 4: Run build to see what breaks**

```bash
cd server && gleam build
```

Expected: FAIL with import errors (this is expected - we'll fix in Task 4)

---

## Task 2: Move where_converter.gleam to graphql/

**Files:**
- Create: `src/graphql/` directory
- Move: `src/where_converter.gleam` → `src/graphql/where_converter.gleam`

**Step 1: Create graphql directory**

```bash
mkdir -p server/src/graphql
```

**Step 2: Copy the file to new location**

```bash
cp server/src/where_converter.gleam server/src/graphql/where_converter.gleam
```

**Step 3: Update internal import in graphql/where_converter.gleam**

Change line 10 from:
```gleam
import where_clause
```

To:
```gleam
import database/queries/where_clause
```

**Step 4: Delete old file**

```bash
rm server/src/where_converter.gleam
```

**Step 5: Run build to see what breaks**

```bash
cd server && gleam build
```

Expected: FAIL with import errors (this is expected - we'll fix in Task 4)

---

## Task 3: Merge cursor.gleam into pagination.gleam

**Files:**
- Merge: `src/cursor.gleam` → `src/database/queries/pagination.gleam`
- Delete: `src/cursor.gleam`

**Step 1: Create the merged pagination.gleam**

Replace `server/src/database/queries/pagination.gleam` with the merged content below. This combines:
- All cursor types and functions
- Existing pagination helpers
- Eliminates `RecordLike` type by using `Record` directly

```gleam
/// Pagination utilities including cursor encoding/decoding and ORDER BY building.
///
/// Cursors encode the position in a result set as base64(field1|field2|...|cid)
/// to enable stable pagination even when new records are inserted.
///
/// The cursor format:
/// - All sort field values are included in the cursor
/// - Values are separated by pipe (|) characters
/// - CID is always the last element as the ultimate tiebreaker
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

            let field_ref = build_cursor_field_reference(prior_field.0)
            let new_part = field_ref <> " = ?"
            let new_params = list.append(eq_params, [value])

            #(list.append(eq_parts, [new_part]), new_params)
          })
        }
      }

      let value = list_at(field_values, i) |> result.unwrap("")

      let comparison_op = get_comparison_operator(field.1, is_before)
      let field_ref = build_cursor_field_reference(field.0)

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
      let field_ref = build_cursor_field_reference(field.0)
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
fn build_cursor_field_reference(field: String) -> String {
  case field {
    "uri" | "cid" | "did" | "collection" | "indexed_at" -> field
    _ -> {
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
        "createdAt" | "indexedAt" -> {
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
```

**Step 2: Delete old cursor.gleam**

```bash
rm server/src/cursor.gleam
```

**Step 3: Run build to see what breaks**

```bash
cd server && gleam build
```

Expected: FAIL with import errors (we'll fix in Task 4)

---

## Task 4: Update All Imports

**Files to modify:**
- `src/graphql_gleam.gleam`
- `src/database/repositories/records.gleam`
- `src/database/queries/aggregates.gleam`
- `test/cursor_test.gleam` (rename to `test/pagination_test.gleam`)
- `test/where_clause_test.gleam`
- `test/where_sql_builder_test.gleam`
- `test/where_integration_test.gleam`
- `test/where_edge_cases_test.gleam`
- `test/database_aggregation_test.gleam`
- `test/graphql_where_integration_test.gleam`

**Step 1: Update graphql_gleam.gleam**

Change line 6 from:
```gleam
import cursor
```
To:
```gleam
import database/queries/pagination
```

Change line 33 from:
```gleam
import where_converter
```
To:
```gleam
import graphql/where_converter
```

Update all usages of `cursor.` to `pagination.`:
- Line 120: `cursor.generate_cursor_from_record` → `pagination.generate_cursor_from_record`
- Line 283: `cursor.generate_cursor_from_record` → `pagination.generate_cursor_from_record`
- Line 328: `cursor.generate_cursor_from_record` → `pagination.generate_cursor_from_record`

Remove the now-unnecessary `record_to_record_like` conversion since `pagination` now works directly with `Record`:
- Line 121: `pagination.record_to_record_like(record)` → `record`
- Similar for lines 284 and 329

**Step 2: Update database/repositories/records.gleam**

Change line 1 from:
```gleam
import cursor
```
To:
```gleam
import database/queries/pagination
```

Change line 15 from:
```gleam
import where_clause
```
To:
```gleam
import database/queries/where_clause
```

Update all `cursor.` references to `pagination.`:
- Line 553: `cursor.decode_cursor` → `pagination.decode_cursor`
- Line 556: `cursor.build_cursor_where_clause` → `pagination.build_cursor_where_clause`
- Line 630: `cursor.generate_cursor_from_record` → `pagination.generate_cursor_from_record`
- And similar for lines 711, 714, 788, 961, 964, 1041, 1186, 1189, 1266

Remove `record_to_record_like` calls - pass `Record` directly.

**Step 3: Update database/queries/aggregates.gleam**

Change line 14 from:
```gleam
import where_clause
```
To:
```gleam
import database/queries/where_clause
```

**Step 4: Rename and update cursor_test.gleam**

```bash
mv server/test/cursor_test.gleam server/test/pagination_test.gleam
```

Update imports in `test/pagination_test.gleam`:

Change line 1 from:
```gleam
import cursor
```
To:
```gleam
import database/queries/pagination
import database/types.{Record}
```

Update all `cursor.` to `pagination.` and `cursor.RecordLike` to `Record`:

```gleam
// Change all occurrences of cursor.RecordLike to Record
// Change all occurrences of cursor. to pagination.
```

**Step 5: Update where_clause_test.gleam**

Change line 7 from:
```gleam
import where_clause
```
To:
```gleam
import database/queries/where_clause
```

**Step 6: Update where_sql_builder_test.gleam**

Change line 8 from:
```gleam
import where_clause
```
To:
```gleam
import database/queries/where_clause
```

**Step 7: Update where_integration_test.gleam**

Change line 12 from:
```gleam
import where_clause
```
To:
```gleam
import database/queries/where_clause
```

**Step 8: Update where_edge_cases_test.gleam**

Change line 13 from:
```gleam
import where_clause
```
To:
```gleam
import database/queries/where_clause
```

**Step 9: Update database_aggregation_test.gleam**

Change line 11 from:
```gleam
import where_clause
```
To:
```gleam
import database/queries/where_clause
```

**Step 10: Update graphql_where_integration_test.gleam**

Change line 15 from:
```gleam
import where_converter
```
To:
```gleam
import graphql/where_converter
```

**Step 11: Run build to verify**

```bash
cd server && gleam build
```

Expected: PASS

**Step 12: Run tests to verify**

```bash
cd server && gleam test
```

Expected: All tests pass

**Step 13: Commit**

```bash
git add -A
git commit -m "refactor: reorganize database helpers into proper layers

- Merge cursor.gleam into database/queries/pagination.gleam
- Move where_clause.gleam to database/queries/where_clause.gleam
- Move where_converter.gleam to graphql/where_converter.gleam
- Update all imports across codebase
- Rename cursor_test.gleam to pagination_test.gleam

This establishes cleaner layering:
- database/queries/ for pure SQL building
- graphql/ for GraphQL-specific adapters"
```

---

## Final Verification

After all tasks complete:

```bash
cd server && gleam build && gleam test
```

Expected: Build succeeds, all tests pass.

### New File Structure

```
server/src/
├── database/
│   └── queries/
│       ├── aggregates.gleam      (updated import)
│       ├── pagination.gleam      (merged: cursor + pagination)
│       └── where_clause.gleam    (moved from src/)
├── graphql/
│   └── where_converter.gleam     (moved from src/, new folder)
└── graphql_gleam.gleam           (updated imports)
```

### Future Opportunity

The new `graphql/` folder sets up for consolidating other GraphQL-related code:
- `graphql_gleam.gleam` could move to `graphql/resolver.gleam` or similar
- Other GraphQL utilities could be grouped here
