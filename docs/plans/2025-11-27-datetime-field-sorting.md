# DateTime Field Sorting Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix datetime fields (like `playedTime`) to sort correctly by passing lexicon field type information to the SQL query builder.

**Architecture:** Thread datetime field names from the lexicon schema through `PaginationParams` to `build_order_by`, which applies SQLite `datetime()` function for proper chronological sorting instead of string comparison.

**Tech Stack:** Gleam, SQLite, GraphQL (swell library)

---

## Task 1: Add datetime_fields to PaginationParams

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/query/dataloader.gleam:27-36`

**Step 1: Update PaginationParams type**

Add `datetime_fields` field to the type definition:

```gleam
pub type PaginationParams {
  PaginationParams(
    first: Option(Int),
    after: Option(String),
    last: Option(Int),
    before: Option(String),
    sort_by: Option(List(#(String, String))),
    where: Option(WhereClause),
    datetime_fields: List(String),
  )
}
```

**Step 2: Run build to check for compile errors**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`
Expected: Compile errors showing all places that construct PaginationParams

**Step 3: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/query/dataloader.gleam
git commit -m "feat: add datetime_fields to PaginationParams type"
```

---

## Task 2: Update build_order_by to accept datetime_fields

**Files:**
- Modify: `server/src/database/queries/pagination.gleam:39-87`

**Step 1: Write the failing test**

Create test in `server/test/database_sorting_test.gleam`:

```gleam
pub fn build_order_by_with_datetime_fields_test() {
  // When a field is in datetime_fields, it should use datetime() SQL
  let result = pagination.build_order_by(
    [#("playedTime", "desc")],
    False,
    ["playedTime"],
  )

  // Should contain datetime() handling
  should.be_true(string.contains(result, "datetime("))
  should.be_true(string.contains(result, "playedTime"))
}

pub fn build_order_by_without_datetime_fields_test() {
  // When a field is NOT in datetime_fields, it should use plain json_extract
  let result = pagination.build_order_by(
    [#("someField", "desc")],
    False,
    [],
  )

  // Should NOT contain datetime() handling
  should.be_false(string.contains(result, "datetime("))
  should.be_true(string.contains(result, "json_extract"))
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: Compile error - build_order_by expects 2 arguments, got 3

**Step 3: Update build_order_by signature and implementation**

```gleam
/// Builds an ORDER BY clause from sort fields
/// use_table_prefix: if True, prefixes table columns with "record." for joins
/// datetime_fields: list of field names that should be treated as datetimes
pub fn build_order_by(
  sort_fields: List(#(String, String)),
  use_table_prefix: Bool,
  datetime_fields: List(String),
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
        _ -> {
          let json_field =
            "json_extract(" <> table_prefix <> "json, '$." <> field_name <> "')"
          case list.contains(datetime_fields, field_name) {
            True ->
              "CASE
            WHEN " <> json_field <> " IS NULL THEN NULL
            WHEN datetime(" <> json_field <> ") IS NULL THEN NULL
            ELSE " <> json_field <> "
           END"
            False -> json_field
          }
        }
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
```

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: Compile errors in records.gleam (callers need updating)

**Step 5: Commit**

```bash
git add server/src/database/queries/pagination.gleam server/test/database_sorting_test.gleam
git commit -m "feat: build_order_by accepts datetime_fields parameter"
```

---

## Task 3: Update records.gleam callers of build_order_by

**Files:**
- Modify: `server/src/database/repositories/records.gleam` (lines ~544, ~679, ~925, ~1161)

**Step 1: Update get_by_collection_paginated function signature**

Find the function around line 500 and add `datetime_fields: List(String)` parameter:

```gleam
pub fn get_by_collection_paginated(
  conn: sqlight.Connection,
  collection: String,
  first: Option(Int),
  after: Option(String),
  last: Option(Int),
  before: Option(String),
  sort_by: Option(List(#(String, String))),
  datetime_fields: List(String),
) -> Result(#(List(Record), Option(String), Bool, Bool), sqlight.Error) {
```

Update the call to `build_order_by`:

```gleam
let order_by_clause = pagination.build_order_by(query_sort_fields, False, datetime_fields)
```

**Step 2: Update get_by_collection_paginated_with_where**

Find around line 648 and add parameter:

```gleam
pub fn get_by_collection_paginated_with_where(
  conn: sqlight.Connection,
  collection: String,
  first: Option(Int),
  after: Option(String),
  last: Option(Int),
  before: Option(String),
  sort_by: Option(List(#(String, String))),
  where: Option(where_clause.WhereClause),
  datetime_fields: List(String),
) -> Result(#(List(Record), Option(String), Bool, Bool), sqlight.Error) {
```

Update the call:

```gleam
let order_by_clause = pagination.build_order_by(query_sort_fields, needs_actor_join, datetime_fields)
```

**Step 3: Update get_by_collection_with_actor_join_paginated**

Find around line 880 and add parameter, update call.

**Step 4: Update query_by_where_clause_with_actor_join_paginated**

Find around line 1100 and add parameter, update call.

**Step 5: Run build to check for remaining errors**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Errors in graphql_gleam.gleam (caller needs updating)

**Step 6: Commit**

```bash
git add server/src/database/repositories/records.gleam
git commit -m "feat: records.gleam functions accept datetime_fields"
```

---

## Task 4: Update graphql_gleam.gleam to pass datetime_fields

**Files:**
- Modify: `server/src/graphql_gleam.gleam` (~line 98-107)

**Step 1: Update record_fetcher to extract datetime_fields from pagination_params**

In the `record_fetcher` function, update the call to `records.get_by_collection_paginated_with_where`:

```gleam
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
    pagination_params.datetime_fields,
  )
```

**Step 2: Run build**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Errors in lexicon_graphql (PaginationParams constructors need datetime_fields)

**Step 3: Commit**

```bash
git add server/src/graphql_gleam.gleam
git commit -m "feat: pass datetime_fields from pagination_params to records"
```

---

## Task 5: Create helper to extract datetime fields from field_type_map

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/schema/database.gleam`

**Step 1: Add helper function after build_field_type_map (around line 1404)**

```gleam
/// Extract list of datetime field names from a field type map
pub fn get_datetime_field_names(
  field_type_map: dict.Dict(String, FieldType),
) -> List(String) {
  field_type_map
  |> dict.to_list
  |> list.filter_map(fn(pair) {
    let #(name, field_type) = pair
    case field_type {
      DateTimeField -> Ok(name)
      _ -> Error(Nil)
    }
  })
}
```

**Step 2: Run build**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`
Expected: PASS (new function, no callers yet)

**Step 3: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/schema/database.gleam
git commit -m "feat: add get_datetime_field_names helper"
```

---

## Task 6: Update extract_pagination_params to include datetime_fields

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/schema/database.gleam` (extract_pagination_params function)

**Step 1: Find extract_pagination_params and update signature**

The function needs to accept the field_type_map and include datetime_fields in the returned PaginationParams:

```gleam
pub fn extract_pagination_params(
  ctx: schema.Context,
  field_type_map: dict.Dict(String, FieldType),
) -> dataloader.PaginationParams {
```

**Step 2: Extract datetime fields and include in return value**

At the end of the function, before returning:

```gleam
let datetime_fields = get_datetime_field_names(field_type_map)

dataloader.PaginationParams(
  first: first,
  after: after,
  last: last,
  before: before,
  sort_by: sort_by,
  where: where,
  datetime_fields: datetime_fields,
)
```

**Step 3: Run build to find callers that need updating**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`
Expected: Errors where extract_pagination_params is called

**Step 4: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/schema/database.gleam
git commit -m "feat: extract_pagination_params accepts field_type_map and returns datetime_fields"
```

---

## Task 7: Update callers of extract_pagination_params

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/schema/database.gleam` (multiple locations)

**Step 1: Find all calls to extract_pagination_params**

Search for `extract_pagination_params(ctx)` and update to `extract_pagination_params(ctx, field_type_map)`.

The field_type_map should be available in the context where these are called (in `build_connection_query_field` and similar functions).

**Step 2: Update each call site**

For each call, ensure the field_type_map is passed. If not available in scope, it needs to be threaded through from the parent function.

**Step 3: Run full build**

Run: `cd /Users/chadmiller/code/quickslice && gleam build`
Expected: PASS

**Step 4: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/schema/database.gleam
git commit -m "feat: pass field_type_map to extract_pagination_params callers"
```

---

## Task 8: Run full test suite and verify fix

**Step 1: Run all tests**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: All tests pass

**Step 2: Manual verification**

Start the server and run the GraphQL query:

```graphql
query {
  fmTealAlphaFeedPlay(sortBy: {field: playedTime, direction: DESC}, first: 10) {
    edges {
      node {
        playedTime
        trackName
      }
    }
  }
}
```

Expected: Results sorted correctly by playedTime in descending order

**Step 3: Final commit**

```bash
git add -A
git commit -m "fix: datetime fields now sort correctly using lexicon schema types"
```

---

## Summary

This plan threads datetime field type information from the lexicon schema through the GraphQL layer to the SQL query builder. The key insight is that `format: "datetime"` in lexicon properties identifies datetime fields, and these need SQLite's `datetime()` function for proper chronological sorting instead of string comparison.
