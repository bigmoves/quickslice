# Admin Connection Pagination Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Convert `labels` and `reports` admin queries from simple lists to Relay-compliant Connection types with opaque cursors, totalCount, and proper PageInfo.

**Architecture:** Add cursor encoding utilities, update repository functions to support cursor-based pagination with count queries, create Connection/Edge types using swell's helpers, and update resolvers to return connection values.

**Tech Stack:** Gleam, swell/connection, base64 encoding for cursors, SQLite/Postgres repositories

---

### Task 1: Create Cursor Encoding Module

**Files:**
- Create: `server/src/graphql/admin/cursor.gleam`
- Test: `server/test/graphql/admin/cursor_test.gleam`

**Step 1: Write the failing test for encode**

```gleam
// server/test/graphql/admin/cursor_test.gleam
import gleeunit/should
import graphql/admin/cursor

pub fn encode_cursor_test() {
  cursor.encode("Label", 42)
  |> should.equal("TGFiZWw6NDI=")
}

pub fn encode_cursor_with_large_id_test() {
  cursor.encode("Report", 12345)
  |> should.equal("UmVwb3J0OjEyMzQ1")
}
```

**Step 2: Run test to verify it fails**

Run: `cd server && gleam test -- --only cursor_test`
Expected: FAIL with module not found

**Step 3: Write minimal implementation for encode**

```gleam
// server/src/graphql/admin/cursor.gleam
/// Cursor encoding/decoding for admin GraphQL connections
///
/// Cursors are opaque base64-encoded strings in format "Type:ID"
/// Example: "Label:42" -> "TGFiZWw6NDI="
import gleam/bit_array
import gleam/int
import gleam/result
import gleam/string

/// Encode a cursor from prefix and ID
/// "Label", 42 -> "TGFiZWw6NDI="
pub fn encode(prefix: String, id: Int) -> String {
  let raw = prefix <> ":" <> int.to_string(id)
  bit_array.from_string(raw)
  |> bit_array.base64_encode(True)
}
```

**Step 4: Run test to verify encode passes**

Run: `cd server && gleam test -- --only cursor_test`
Expected: PASS

**Step 5: Write the failing test for decode**

Add to `cursor_test.gleam`:

```gleam
pub fn decode_cursor_test() {
  cursor.decode("TGFiZWw6NDI=")
  |> should.equal(Ok(#("Label", 42)))
}

pub fn decode_cursor_with_large_id_test() {
  cursor.decode("UmVwb3J0OjEyMzQ1")
  |> should.equal(Ok(#("Report", 12345)))
}

pub fn decode_invalid_cursor_test() {
  cursor.decode("not-valid-base64!!!")
  |> should.be_error()
}

pub fn decode_malformed_cursor_test() {
  // Valid base64 but wrong format (no colon)
  cursor.decode("bm9jb2xvbg==")
  |> should.be_error()
}
```

**Step 6: Run test to verify decode fails**

Run: `cd server && gleam test -- --only cursor_test`
Expected: FAIL with function not defined

**Step 7: Write minimal implementation for decode**

Add to `cursor.gleam`:

```gleam
/// Decode a cursor to prefix and ID
/// "TGFiZWw6NDI=" -> Ok(#("Label", 42))
pub fn decode(cursor: String) -> Result(#(String, Int), Nil) {
  use decoded <- result.try(
    bit_array.base64_decode(cursor)
    |> result.then(fn(bits) {
      bit_array.to_string(bits)
    })
    |> result.nil_error()
  )

  case string.split(decoded, ":") {
    [prefix, id_str] -> {
      case int.parse(id_str) {
        Ok(id) -> Ok(#(prefix, id))
        Error(_) -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}
```

**Step 8: Run test to verify all pass**

Run: `cd server && gleam test -- --only cursor_test`
Expected: All PASS

**Step 9: Commit**

```bash
git add server/src/graphql/admin/cursor.gleam server/test/graphql/admin/cursor_test.gleam
git commit -m "feat(admin): add cursor encoding/decoding for connection pagination"
```

---

### Task 2: Add Connection Types to Admin Types

**Files:**
- Modify: `server/src/graphql/admin/types.gleam`

**Step 1: Read the current types file to understand structure**

Read: `server/src/graphql/admin/types.gleam`

**Step 2: Add imports for swell/connection**

Add to imports section:

```gleam
import swell/connection
```

**Step 3: Add LabelEdge and LabelConnection types**

Add after `label_type`:

```gleam
/// Edge type for Label connection
pub fn label_edge_type() -> schema.Type {
  connection.edge_type("Label", label_type())
}

/// Connection type for paginated Label results
pub fn label_connection_type() -> schema.Type {
  connection.connection_type("Label", label_edge_type())
}
```

**Step 4: Add ReportEdge and ReportConnection types**

Add after `report_type`:

```gleam
/// Edge type for Report connection
pub fn report_edge_type() -> schema.Type {
  connection.edge_type("Report", report_type())
}

/// Connection type for paginated Report results
pub fn report_connection_type() -> schema.Type {
  connection.connection_type("Report", report_edge_type())
}
```

**Step 5: Build to verify types compile**

Run: `cd server && gleam build`
Expected: Build succeeds

**Step 6: Commit**

```bash
git add server/src/graphql/admin/types.gleam
git commit -m "feat(admin): add Label and Report connection types"
```

---

### Task 3: Update Labels Repository for Connection Pagination

**Files:**
- Modify: `server/src/database/repositories/labels.gleam`
- Test: `server/test/database/repositories/labels_test.gleam` (if exists, otherwise add inline verification)

**Step 1: Read the current labels repository**

Read: `server/src/database/repositories/labels.gleam`

**Step 2: Create new function signature for connection pagination**

The existing `get_all` returns `Result(List(Label), Error)`. Add a new function that returns pagination info:

```gleam
/// Result type for paginated label queries
pub type PaginatedLabels {
  PaginatedLabels(
    labels: List(Label),
    has_next_page: Bool,
    total_count: Int,
  )
}

/// Get labels with connection-style pagination
/// Returns labels, whether there's a next page, and total count
pub fn get_paginated(
  conn: Executor,
  uri_filter: Option(String),
  val_filter: Option(String),
  first: Int,
  after_id: Option(Int),
) -> Result(PaginatedLabels, Error) {
  // Fetch first + 1 to detect hasNextPage
  let fetch_limit = first + 1

  // Build base query conditions
  let uri_condition = case uri_filter {
    Some(uri) -> " AND uri = '" <> uri <> "'"
    None -> ""
  }
  let val_condition = case val_filter {
    Some(val) -> " AND val = '" <> val <> "'"
    None -> ""
  }
  let cursor_condition = case after_id {
    Some(id) -> " AND id < " <> int.to_string(id)
    None -> ""
  }

  let where_clause = "WHERE 1=1" <> uri_condition <> val_condition <> cursor_condition

  // Main query
  let query = "SELECT id, src, uri, cid, val, neg, created_at, expires_at, signature
               FROM labels " <> where_clause <> "
               ORDER BY id DESC
               LIMIT " <> int.to_string(fetch_limit)

  // Count query (without cursor, with filters)
  let count_where = "WHERE 1=1" <> uri_condition <> val_condition
  let count_query = "SELECT COUNT(*) FROM labels " <> count_where

  // Execute both queries
  use labels_result <- result.try(execute_query(conn, query))
  use count_result <- result.try(execute_count_query(conn, count_query))

  // Determine if there's a next page
  let has_next = list.length(labels_result) > first
  let labels = case has_next {
    True -> list.take(labels_result, first)
    False -> labels_result
  }

  Ok(PaginatedLabels(
    labels: labels,
    has_next_page: has_next,
    total_count: count_result,
  ))
}
```

Note: The actual implementation will need to match the existing query patterns in this repository. Read the file first to adapt.

**Step 3: Build and verify**

Run: `cd server && gleam build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add server/src/database/repositories/labels.gleam
git commit -m "feat(labels): add get_paginated for connection pagination"
```

---

### Task 4: Update Reports Repository for Connection Pagination

**Files:**
- Modify: `server/src/database/repositories/reports.gleam`

**Step 1: Read the current reports repository**

Read: `server/src/database/repositories/reports.gleam`

**Step 2: Add paginated query function following same pattern as labels**

Add similar `PaginatedReports` type and `get_paginated` function, adapting to the reports table structure and existing query patterns.

**Step 3: Build and verify**

Run: `cd server && gleam build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add server/src/database/repositories/reports.gleam
git commit -m "feat(reports): add get_paginated for connection pagination"
```

---

### Task 5: Update Labels Query Resolver

**Files:**
- Modify: `server/src/graphql/admin/queries.gleam`

**Step 1: Read the current queries file**

Read: `server/src/graphql/admin/queries.gleam`

**Step 2: Add imports**

Add to imports:

```gleam
import graphql/admin/cursor
import swell/connection
```

**Step 3: Update labels field to use connection type and new arguments**

Replace the labels field definition:

```gleam
// labels query (admin only) - Connection type
schema.field_with_args(
  "labels",
  schema.non_null(admin_types.label_connection_type()),
  "Get labels with optional filters (admin only)",
  [
    schema.argument(
      "uri",
      schema.string_type(),
      "Filter by subject URI",
      None,
    ),
    schema.argument(
      "val",
      schema.string_type(),
      "Filter by label value",
      None,
    ),
    schema.argument(
      "first",
      schema.int_type(),
      "Number of items to fetch (default 50)",
      None,
    ),
    schema.argument(
      "after",
      schema.string_type(),
      "Cursor for pagination",
      None,
    ),
  ],
  fn(ctx) {
    case session.get_current_session(req, conn, did_cache) {
      Ok(sess) -> {
        case config_repo.is_admin(conn, sess.did) {
          True -> {
            let uri_filter = case schema.get_argument(ctx, "uri") {
              Some(value.String(u)) -> Some(u)
              _ -> None
            }
            let val_filter = case schema.get_argument(ctx, "val") {
              Some(value.String(v)) -> Some(v)
              _ -> None
            }
            let first = case schema.get_argument(ctx, "first") {
              Some(value.Int(f)) -> f
              _ -> 50
            }
            let after_id = case schema.get_argument(ctx, "after") {
              Some(value.String(c)) -> {
                case cursor.decode(c) {
                  Ok(#("Label", id)) -> Some(id)
                  _ -> None
                }
              }
              _ -> None
            }

            case labels.get_paginated(conn, uri_filter, val_filter, first, after_id) {
              Ok(paginated) -> {
                // Build edges with cursors
                let edges = list.map(paginated.labels, fn(label) {
                  let label_value = converters.label_to_value(label)
                  let cursor_str = cursor.encode("Label", label.id)
                  connection.edge_to_value(connection.Edge(
                    node: label_value,
                    cursor: cursor_str,
                  ))
                })

                // Build page info
                let start_cursor = case list.first(paginated.labels) {
                  Ok(first_label) -> Some(cursor.encode("Label", first_label.id))
                  Error(_) -> None
                }
                let end_cursor = case list.last(paginated.labels) {
                  Ok(last_label) -> Some(cursor.encode("Label", last_label.id))
                  Error(_) -> None
                }

                let page_info = connection.PageInfo(
                  has_next_page: paginated.has_next_page,
                  has_previous_page: option.is_some(after_id),
                  start_cursor: start_cursor,
                  end_cursor: end_cursor,
                )

                let conn_value = connection.Connection(
                  edges: list.map(paginated.labels, fn(label) {
                    connection.Edge(
                      node: converters.label_to_value(label),
                      cursor: cursor.encode("Label", label.id),
                    )
                  }),
                  page_info: page_info,
                  total_count: Some(paginated.total_count),
                )

                Ok(connection.connection_to_value(conn_value))
              }
              Error(_) -> Error("Failed to fetch labels")
            }
          }
          False -> Error("Admin privileges required")
        }
      }
      Error(_) -> Error("Authentication required")
    }
  },
),
```

**Step 4: Build to verify**

Run: `cd server && gleam build`
Expected: Build succeeds

**Step 5: Commit**

```bash
git add server/src/graphql/admin/queries.gleam
git commit -m "feat(admin): update labels query to return Connection type"
```

---

### Task 6: Update Reports Query Resolver

**Files:**
- Modify: `server/src/graphql/admin/queries.gleam`

**Step 1: Update reports field following same pattern as labels**

Replace the reports field with connection-based version using `report_connection_type()`, `first`/`after` arguments, cursor encoding with "Report" prefix, and `reports.get_paginated`.

**Step 2: Build to verify**

Run: `cd server && gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add server/src/graphql/admin/queries.gleam
git commit -m "feat(admin): update reports query to return Connection type"
```

---

### Task 7: Integration Test

**Files:**
- Create: `server/test/graphql/admin/connection_test.gleam`

**Step 1: Write integration test for labels connection**

```gleam
// Test that labels query returns proper connection structure
import gleeunit/should
// ... setup code matching existing test patterns

pub fn labels_connection_structure_test() {
  // Query should return edges, pageInfo, totalCount
  let query = "
    query {
      labels(first: 10) {
        edges {
          node { id val uri }
          cursor
        }
        pageInfo {
          hasNextPage
          hasPreviousPage
          startCursor
          endCursor
        }
        totalCount
      }
    }
  "
  // Execute and verify structure
}

pub fn labels_pagination_test() {
  // Test that after cursor works correctly
}
```

**Step 2: Run tests**

Run: `cd server && gleam test`
Expected: All tests pass

**Step 3: Commit**

```bash
git add server/test/graphql/admin/connection_test.gleam
git commit -m "test(admin): add integration tests for connection pagination"
```

---

### Task 8: Final Verification and Cleanup

**Step 1: Run full test suite**

Run: `cd server && gleam test`
Expected: All tests pass

**Step 2: Manual verification via GraphQL**

Start server and test at `/admin/graphql`:

```graphql
query {
  labels(first: 5) {
    edges {
      node { id val uri createdAt }
      cursor
    }
    pageInfo {
      hasNextPage
      endCursor
    }
    totalCount
  }
}
```

Then paginate:

```graphql
query {
  labels(first: 5, after: "<endCursor from above>") {
    edges {
      node { id val }
      cursor
    }
    pageInfo {
      hasNextPage
      hasPreviousPage
    }
  }
}
```

**Step 3: Commit any final fixes**

```bash
git add -A
git commit -m "chore: final cleanup for admin connection pagination"
```
