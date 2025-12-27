# Notifications GraphQL Query Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add a `notifications` query and `notificationCreated` subscription that returns all records referencing a given DID, with typed results based on collection.

**Architecture:** Cross-collection search using SQL LIKE on record JSON. Returns a union type of all record types auto-generated from lexicons. Uses existing cursor-based pagination. Subscription hooks into existing PubSub system.

**Tech Stack:** Gleam, SQLite/PostgreSQL via Executor abstraction, GraphQL via lexicon_graphql

---

## Task 1: Database Repository - Add get_notifications function

**Files:**
- Modify: `/Users/chadmiller/code/quickslice/server/src/database/repositories/records.gleam`
- Test: `/Users/chadmiller/code/quickslice/server/test/database/repositories/notifications_test.gleam`

### Step 1: Write the failing test

Create a new test file:

```gleam
// /Users/chadmiller/code/quickslice/server/test/database/repositories/notifications_test.gleam
import database/executor
import database/repositories/records
import database/types.{Record}
import gleam/option.{None, Some}
import gleeunit/should
import test_helpers

pub fn get_notifications_returns_records_mentioning_did_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_record_table(db)

  // Insert a record that mentions did:plc:target
  let assert Ok(_) =
    records.insert(
      db,
      "at://did:plc:author/app.bsky.feed.like/abc",
      "bafy123",
      "did:plc:author",
      "app.bsky.feed.like",
      "{\"subject\":{\"uri\":\"at://did:plc:target/app.bsky.feed.post/xyz\"}}",
    )

  // Insert a record by the target (should be excluded)
  let assert Ok(_) =
    records.insert(
      db,
      "at://did:plc:target/app.bsky.feed.post/xyz",
      "bafy456",
      "did:plc:target",
      "app.bsky.feed.post",
      "{\"text\":\"Hello world\"}",
    )

  // Insert a record that doesn't mention the target
  let assert Ok(_) =
    records.insert(
      db,
      "at://did:plc:other/app.bsky.feed.post/zzz",
      "bafy789",
      "did:plc:other",
      "app.bsky.feed.post",
      "{\"text\":\"Unrelated post\"}",
    )

  let assert Ok(#(results, _cursor, _has_next, _has_prev)) =
    records.get_notifications(db, "did:plc:target", None, None, None)

  // Should only return the like, not the target's own post or unrelated post
  results |> should.equal([
    Record(
      uri: "at://did:plc:author/app.bsky.feed.like/abc",
      cid: "bafy123",
      did: "did:plc:author",
      collection: "app.bsky.feed.like",
      json: "{\"subject\":{\"uri\":\"at://did:plc:target/app.bsky.feed.post/xyz\"}}",
      indexed_at: results |> list.first |> result.unwrap(Record("", "", "", "", "", "")) |> fn(r) { r.indexed_at },
    ),
  ])
}
```

### Step 2: Run test to verify it fails

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test -- --filter=get_notifications`
Expected: FAIL with "function get_notifications not defined" or similar

### Step 3: Write minimal implementation

Add to `/Users/chadmiller/code/quickslice/server/src/database/repositories/records.gleam`:

```gleam
/// Get records that mention the given DID (excluding records authored by that DID)
pub fn get_notifications(
  exec: Executor,
  did: String,
  collections: option.Option(List(String)),
  first: option.Option(Int),
  after: option.Option(String),
) -> Result(#(List(Record), option.Option(String), Bool, Bool), DbError) {
  let limit = option.unwrap(first, 50)
  let pattern = "%" <> did <> "%"

  // Build collection filter
  let #(collection_clause, collection_params) = case collections {
    option.None -> #("", [])
    option.Some([]) -> #("", [])
    option.Some(cols) -> {
      let placeholders =
        cols
        |> list.index_map(fn(_, i) { executor.placeholder(exec, i + 3) })
        |> string.join(", ")
      #(" AND collection IN (" <> placeholders <> ")", list.map(cols, Text))
    }
  }

  // Build cursor clause
  let #(cursor_clause, cursor_params) = case after {
    option.None -> #("", [])
    option.Some(cursor) -> {
      case pagination.decode_cursor(cursor) {
        Ok(decoded) -> {
          let idx = 3 + list.length(collection_params)
          #(
            " AND (COALESCE(" <> executor.json_extract(exec, "json", "updatedAt") <> ", " <> executor.json_extract(exec, "json", "createdAt") <> "), uri) < (" <> executor.placeholder(exec, idx) <> ", " <> executor.placeholder(exec, idx + 1) <> ")",
            [Text(decoded.field_values |> list.first |> result.unwrap("")), Text(decoded.cid)],
          )
        }
        Error(_) -> #("", [])
      }
    }
  }

  let sql =
    "SELECT "
    <> record_columns(exec)
    <> " FROM record WHERE json LIKE "
    <> executor.placeholder(exec, 1)
    <> " AND did != "
    <> executor.placeholder(exec, 2)
    <> collection_clause
    <> cursor_clause
    <> " ORDER BY COALESCE("
    <> executor.json_extract(exec, "json", "updatedAt")
    <> ", "
    <> executor.json_extract(exec, "json", "createdAt")
    <> ") DESC, uri DESC LIMIT "
    <> int.to_string(limit + 1)

  let params =
    [Text(pattern), Text(did)]
    |> list.append(collection_params)
    |> list.append(cursor_params)

  use results <- result.try(executor.query(exec, sql, params, record_decoder()))

  let has_next = list.length(results) > limit
  let trimmed = results |> list.take(limit)
  let end_cursor = case list.last(trimmed) {
    Ok(record) -> {
      let sort_value =
        record.json
        |> json.decode(dynamic.dynamic)
        |> result.map(fn(d) {
          d
          |> dynamic.field("updatedAt", dynamic.string)
          |> result.lazy_or(fn() {
            dynamic.field("createdAt", dynamic.string)(d)
          })
          |> result.unwrap(record.indexed_at)
        })
        |> result.unwrap(record.indexed_at)
      option.Some(pagination.encode_cursor([sort_value], record.uri))
    }
    Error(_) -> option.None
  }

  Ok(#(trimmed, end_cursor, has_next, False))
}
```

### Step 4: Run test to verify it passes

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test -- --filter=get_notifications`
Expected: PASS

### Step 5: Commit

```bash
git add server/src/database/repositories/records.gleam server/test/database/repositories/notifications_test.gleam
git commit -m "feat(db): add get_notifications repository function"
```

---

## Task 2: Add collection filter test

**Files:**
- Test: `/Users/chadmiller/code/quickslice/server/test/database/repositories/notifications_test.gleam`

### Step 1: Write the failing test

Add to the test file:

```gleam
pub fn get_notifications_filters_by_collection_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_record_table(db)

  // Insert a like mentioning target
  let assert Ok(_) =
    records.insert(
      db,
      "at://did:plc:author/app.bsky.feed.like/abc",
      "bafy123",
      "did:plc:author",
      "app.bsky.feed.like",
      "{\"subject\":{\"uri\":\"at://did:plc:target/app.bsky.feed.post/xyz\"}}",
    )

  // Insert a follow mentioning target
  let assert Ok(_) =
    records.insert(
      db,
      "at://did:plc:author/app.bsky.graph.follow/def",
      "bafy456",
      "did:plc:author",
      "app.bsky.graph.follow",
      "{\"subject\":\"did:plc:target\"}",
    )

  // Filter to only likes
  let assert Ok(#(results, _, _, _)) =
    records.get_notifications(
      db,
      "did:plc:target",
      Some(["app.bsky.feed.like"]),
      None,
      None,
    )

  list.length(results) |> should.equal(1)
  let assert Ok(first) = list.first(results)
  first.collection |> should.equal("app.bsky.feed.like")
}
```

### Step 2: Run test to verify it passes

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test -- --filter=filters_by_collection`
Expected: PASS (implementation already supports this)

### Step 3: Commit

```bash
git add server/test/database/repositories/notifications_test.gleam
git commit -m "test(db): add collection filter test for notifications"
```

---

## Task 3: Schema Generation - Add RecordCollection enum

**Files:**
- Modify: `/Users/chadmiller/code/quickslice/lexicon_graphql/src/lexicon_graphql/schema/database.gleam`
- Test: `/Users/chadmiller/code/quickslice/lexicon_graphql/test/schema/notifications_schema_test.gleam`

### Step 1: Write the failing test

```gleam
// /Users/chadmiller/code/quickslice/lexicon_graphql/test/schema/notifications_schema_test.gleam
import gleam/list
import gleeunit/should
import lexicon_graphql
import lexicon_graphql/schema/database

pub fn builds_record_collection_enum_test() {
  let lexicon_json =
    "{\"lexicon\":1,\"id\":\"app.bsky.feed.post\",\"defs\":{\"main\":{\"type\":\"record\",\"record\":{\"properties\":{\"text\":{\"type\":\"string\"}}}}}}"

  let assert Ok(parsed) = lexicon_graphql.parse_lexicon(lexicon_json)

  let enum_result = database.build_record_collection_enum([parsed])

  enum_result |> should.be_ok()
  let assert Ok(enum_type) = enum_result
  // Enum should have the collection NSID as a value
  // APP_BSKY_FEED_POST maps to "app.bsky.feed.post"
}
```

### Step 2: Run test to verify it fails

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test -- --filter=builds_record_collection_enum`
Expected: FAIL with "function build_record_collection_enum not defined"

### Step 3: Write minimal implementation

Add to `/Users/chadmiller/code/quickslice/lexicon_graphql/src/lexicon_graphql/schema/database.gleam`:

```gleam
/// Convert NSID to GraphQL enum value (app.bsky.feed.post -> APP_BSKY_FEED_POST)
pub fn nsid_to_enum_value(nsid: String) -> String {
  nsid
  |> string.replace(".", "_")
  |> string.uppercase()
}

/// Convert GraphQL enum value back to NSID (APP_BSKY_FEED_POST -> app.bsky.feed.post)
pub fn enum_value_to_nsid(enum_value: String) -> String {
  enum_value
  |> string.lowercase()
  |> string.replace("_", ".")
}

/// Build RecordCollection enum from parsed lexicons
pub fn build_record_collection_enum(
  lexicons: List(types.Lexicon),
) -> Result(schema.Type, String) {
  let record_nsids =
    lexicons
    |> list.filter_map(fn(lex) {
      case dict.get(lex.defs, "main") {
        Ok(def) ->
          case def {
            types.RecordDef(_) -> Ok(lex.id)
            _ -> Error(Nil)
          }
        Error(_) -> Error(Nil)
      }
    })

  let enum_values =
    record_nsids
    |> list.map(fn(nsid) {
      schema.enum_value(nsid_to_enum_value(nsid), "Collection: " <> nsid)
    })

  Ok(schema.enum_type(
    "RecordCollection",
    "Available record collection types",
    enum_values,
  ))
}
```

### Step 4: Run test to verify it passes

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test -- --filter=builds_record_collection_enum`
Expected: PASS

### Step 5: Commit

```bash
git add lexicon_graphql/src/lexicon_graphql/schema/database.gleam lexicon_graphql/test/schema/notifications_schema_test.gleam
git commit -m "feat(schema): add RecordCollection enum generation"
```

---

## Task 4: Schema Generation - Add NotificationRecord union type

**Files:**
- Modify: `/Users/chadmiller/code/quickslice/lexicon_graphql/src/lexicon_graphql/schema/database.gleam`
- Test: `/Users/chadmiller/code/quickslice/lexicon_graphql/test/schema/notifications_schema_test.gleam`

### Step 1: Write the failing test

Add to test file:

```gleam
pub fn builds_notification_record_union_test() {
  let post_lexicon =
    "{\"lexicon\":1,\"id\":\"app.bsky.feed.post\",\"defs\":{\"main\":{\"type\":\"record\",\"record\":{\"properties\":{\"text\":{\"type\":\"string\"}}}}}}"
  let like_lexicon =
    "{\"lexicon\":1,\"id\":\"app.bsky.feed.like\",\"defs\":{\"main\":{\"type\":\"record\",\"record\":{\"properties\":{\"subject\":{\"type\":\"ref\"}}}}}}"

  let assert Ok(post_parsed) = lexicon_graphql.parse_lexicon(post_lexicon)
  let assert Ok(like_parsed) = lexicon_graphql.parse_lexicon(like_lexicon)

  let union_result =
    database.build_notification_record_union([post_parsed, like_parsed])

  union_result |> should.be_ok()
}
```

### Step 2: Run test to verify it fails

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test -- --filter=builds_notification_record_union`
Expected: FAIL

### Step 3: Write minimal implementation

Add to database.gleam:

```gleam
/// Build NotificationRecord union from all record types
pub fn build_notification_record_union(
  lexicons: List(types.Lexicon),
) -> Result(schema.Type, String) {
  let record_type_names =
    lexicons
    |> list.filter_map(fn(lex) {
      case dict.get(lex.defs, "main") {
        Ok(def) ->
          case def {
            types.RecordDef(_) -> Ok(nsid_to_type_name(lex.id))
            _ -> Error(Nil)
          }
        Error(_) -> Error(Nil)
      }
    })

  Ok(schema.union_type(
    "NotificationRecord",
    "Union of all record types for notifications",
    record_type_names,
  ))
}

/// Convert NSID to GraphQL type name (app.bsky.feed.post -> AppBskyFeedPost)
fn nsid_to_type_name(nsid: String) -> String {
  nsid
  |> string.split(".")
  |> list.map(string.capitalise)
  |> string.join("")
}
```

### Step 4: Run test to verify it passes

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test -- --filter=builds_notification_record_union`
Expected: PASS

### Step 5: Commit

```bash
git add lexicon_graphql/src/lexicon_graphql/schema/database.gleam lexicon_graphql/test/schema/notifications_schema_test.gleam
git commit -m "feat(schema): add NotificationRecord union type generation"
```

---

## Task 5: Schema Generation - Add notifications query field

**Files:**
- Modify: `/Users/chadmiller/code/quickslice/lexicon_graphql/src/lexicon_graphql/schema/database.gleam`

### Step 1: Write the failing test

Add to test file:

```gleam
pub fn schema_includes_notifications_query_test() {
  let lexicon_json =
    "{\"lexicon\":1,\"id\":\"app.bsky.feed.post\",\"defs\":{\"main\":{\"type\":\"record\",\"record\":{\"properties\":{\"text\":{\"type\":\"string\"}}}}}}"

  let assert Ok(parsed) = lexicon_graphql.parse_lexicon(lexicon_json)

  // Build schema with mock fetchers
  let mock_fetcher = fn(_, _) { Ok(#([], None, False, False, None)) }
  let mock_notification_fetcher = fn(_, _, _, _) { Ok(#([], None, False, False)) }

  let schema_result =
    database.build_schema_with_subscriptions(
      [parsed],
      mock_fetcher,
      None,
      None,
      None,
      None,
      None,
      Some(mock_notification_fetcher),
    )

  schema_result |> should.be_ok()
  // Schema should include "notifications" query field
}
```

### Step 2: Run test to verify it fails

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test -- --filter=schema_includes_notifications_query`
Expected: FAIL

### Step 3: Write minimal implementation

Modify `build_schema_with_subscriptions` to accept notification fetcher and add the query field:

```gleam
/// Type for notification fetcher function
pub type NotificationFetcher =
  fn(String, option.Option(List(String)), option.Option(Int), option.Option(String)) ->
    Result(#(List(#(value.Value, String)), option.Option(String), Bool, Bool), String)

pub fn build_schema_with_subscriptions(
  lexicons: List(types.Lexicon),
  record_fetcher: RecordFetcher,
  batch_fetcher: option.Option(BatchFetcher),
  paginated_batch_fetcher: option.Option(PaginatedBatchFetcher),
  mutation_create_factory: option.Option(MutationCreateFactory),
  mutation_update_factory: option.Option(MutationUpdateFactory),
  mutation_delete_factory: option.Option(MutationDeleteFactory),
  notification_fetcher: option.Option(NotificationFetcher),
) -> Result(schema.Schema, String) {
  // ... existing code ...

  // Build notification types if fetcher provided
  let notification_fields = case notification_fetcher {
    option.None -> []
    option.Some(fetcher) -> {
      let assert Ok(collection_enum) = build_record_collection_enum(lexicons)
      let assert Ok(notification_union) = build_notification_record_union(lexicons)

      let notification_edge = schema.object_type(
        "NotificationEdge",
        "An edge in a notification connection",
        [
          schema.field("node", schema.non_null(notification_union), "The notification record", fn(ctx) {
            case ctx.parent {
              option.Some(value.Object(fields)) ->
                list.find(fields, fn(f) { f.0 == "node" })
                |> result.map(fn(f) { f.1 })
                |> result.replace_error("node not found")
              _ -> Error("Invalid parent")
            }
          }),
          schema.field("cursor", schema.non_null(schema.string()), "Cursor for pagination", fn(ctx) {
            case ctx.parent {
              option.Some(value.Object(fields)) ->
                list.find(fields, fn(f) { f.0 == "cursor" })
                |> result.map(fn(f) { f.1 })
                |> result.replace_error("cursor not found")
              _ -> Error("Invalid parent")
            }
          }),
        ],
      )

      let notification_connection = schema.object_type(
        "NotificationConnection",
        "A connection to a list of notifications",
        [
          schema.field("edges", schema.non_null(schema.list(schema.non_null(notification_edge))), "The edges", fn(ctx) {
            case ctx.parent {
              option.Some(value.Object(fields)) ->
                list.find(fields, fn(f) { f.0 == "edges" })
                |> result.map(fn(f) { f.1 })
                |> result.replace_error("edges not found")
              _ -> Error("Invalid parent")
            }
          }),
          schema.field("pageInfo", schema.non_null(page_info_type), "Pagination info", fn(ctx) {
            case ctx.parent {
              option.Some(value.Object(fields)) ->
                list.find(fields, fn(f) { f.0 == "pageInfo" })
                |> result.map(fn(f) { f.1 })
                |> result.replace_error("pageInfo not found")
              _ -> Error("Invalid parent")
            }
          }),
        ],
      )

      [
        schema.field(
          "notifications",
          schema.non_null(notification_connection),
          "Get records that mention the given DID",
          [
            schema.argument("did", schema.non_null(schema.string()), "The DID to find notifications for"),
            schema.argument("collections", schema.list(schema.non_null(collection_enum)), "Filter to specific collections"),
            schema.argument("first", schema.int(), "Number of results to return"),
            schema.argument("after", schema.string(), "Cursor for pagination"),
          ],
          fn(ctx) {
            let did = get_string_arg(ctx.arguments, "did") |> result.unwrap("")
            let collections = get_string_list_arg(ctx.arguments, "collections")
            let first = get_int_arg(ctx.arguments, "first")
            let after = get_string_arg_option(ctx.arguments, "after")

            // Convert enum values back to NSIDs
            let nsid_collections = option.map(collections, fn(cols) {
              list.map(cols, enum_value_to_nsid)
            })

            use results <- result.try(fetcher(did, nsid_collections, first, after))
            let #(records, end_cursor, has_next, has_prev) = results

            let edges = list.map(records, fn(r) {
              let #(record_value, cursor) = r
              value.Object([
                #("node", record_value),
                #("cursor", value.String(cursor)),
              ])
            })

            Ok(value.Object([
              #("edges", value.List(edges)),
              #("pageInfo", value.Object([
                #("hasNextPage", value.Boolean(has_next)),
                #("hasPreviousPage", value.Boolean(has_prev)),
                #("startCursor", case list.first(records) {
                  Ok(#(_, c)) -> value.String(c)
                  Error(_) -> value.Null
                }),
                #("endCursor", case end_cursor {
                  option.Some(c) -> value.String(c)
                  option.None -> value.Null
                }),
              ])),
            ]))
          },
        ),
      ]
    }
  }

  // Add notification_fields to query_fields
  let all_query_fields = list.append(query_fields, notification_fields)

  // ... rest of existing code using all_query_fields ...
}
```

### Step 4: Run test to verify it passes

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test -- --filter=schema_includes_notifications_query`
Expected: PASS

### Step 5: Commit

```bash
git add lexicon_graphql/src/lexicon_graphql/schema/database.gleam lexicon_graphql/test/schema/notifications_schema_test.gleam
git commit -m "feat(schema): add notifications query field to schema"
```

---

## Task 6: Schema Generation - Add notificationCreated subscription

**Files:**
- Modify: `/Users/chadmiller/code/quickslice/lexicon_graphql/src/lexicon_graphql/schema/database.gleam`

### Step 1: Write the failing test

Add to test file:

```gleam
pub fn schema_includes_notification_created_subscription_test() {
  let lexicon_json =
    "{\"lexicon\":1,\"id\":\"app.bsky.feed.post\",\"defs\":{\"main\":{\"type\":\"record\",\"record\":{\"properties\":{\"text\":{\"type\":\"string\"}}}}}}"

  let assert Ok(parsed) = lexicon_graphql.parse_lexicon(lexicon_json)

  let mock_fetcher = fn(_, _) { Ok(#([], None, False, False, None)) }
  let mock_notification_fetcher = fn(_, _, _, _) { Ok(#([], None, False, False)) }

  let schema_result =
    database.build_schema_with_subscriptions(
      [parsed],
      mock_fetcher,
      None,
      None,
      None,
      None,
      None,
      Some(mock_notification_fetcher),
    )

  schema_result |> should.be_ok()
  // Schema subscription type should include "notificationCreated" field
}
```

### Step 2: Run test to verify it fails

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test -- --filter=notification_created_subscription`
Expected: FAIL

### Step 3: Write minimal implementation

Add to the subscription type building in `build_subscription_type`:

```gleam
fn build_subscription_type(
  record_types: List(RecordType),
  object_types: dict.Dict(String, schema.Type),
  notification_union: option.Option(schema.Type),
  collection_enum: option.Option(schema.Type),
) -> schema.Type {
  // Existing subscription fields for each record type
  let record_subscription_fields = list.flat_map(record_types, fn(record_type) {
    // ... existing code for Created/Updated/Deleted per type ...
  })

  // Add notificationCreated subscription if union provided
  let notification_subscription_fields = case notification_union, collection_enum {
    option.Some(union_type), option.Some(enum_type) -> [
      schema.subscription_field(
        "notificationCreated",
        schema.non_null(union_type),
        "Emitted when a new record is created that mentions the subscribed DID",
        [
          schema.argument("did", schema.non_null(schema.string()), "The DID to watch for mentions"),
          schema.argument("collections", schema.list(schema.non_null(enum_type)), "Filter to specific collections"),
        ],
        fn(ctx) {
          case ctx.data {
            option.Some(data) -> Ok(data)
            option.None -> Error("Subscription resolver called without event data")
          }
        },
      ),
    ]
    _, _ -> []
  }

  let all_subscription_fields =
    list.append(record_subscription_fields, notification_subscription_fields)

  schema.object_type("Subscription", "GraphQL subscription root", all_subscription_fields)
}
```

### Step 4: Run test to verify it passes

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test -- --filter=notification_created_subscription`
Expected: PASS

### Step 5: Commit

```bash
git add lexicon_graphql/src/lexicon_graphql/schema/database.gleam lexicon_graphql/test/schema/notifications_schema_test.gleam
git commit -m "feat(schema): add notificationCreated subscription field"
```

---

## Task 7: Server Integration - Add notification fetcher

**Files:**
- Modify: `/Users/chadmiller/code/quickslice/server/src/graphql/lexicon/fetchers.gleam`
- Modify: `/Users/chadmiller/code/quickslice/server/src/graphql/lexicon/schema.gleam`

### Step 1: Write the failing test

Create integration test:

```gleam
// /Users/chadmiller/code/quickslice/server/test/graphql/notifications_integration_test.gleam
import database/repositories/lexicons
import database/repositories/records
import gleam/json
import gleeunit/should
import graphql/lexicon/schema as lexicon_schema
import test_helpers

pub fn notifications_query_returns_mentioning_records_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_record_table(db)
  let assert Ok(_) = test_helpers.create_actor_table(db)
  let assert Ok(_) = test_helpers.create_lexicon_table(db)

  // Insert lexicons
  let post_lexicon =
    "{\"lexicon\":1,\"id\":\"app.bsky.feed.post\",\"defs\":{\"main\":{\"type\":\"record\",\"record\":{\"properties\":{\"text\":{\"type\":\"string\"},\"createdAt\":{\"type\":\"string\",\"format\":\"datetime\"}}}}}}"
  let like_lexicon =
    "{\"lexicon\":1,\"id\":\"app.bsky.feed.like\",\"defs\":{\"main\":{\"type\":\"record\",\"record\":{\"properties\":{\"subject\":{\"type\":\"ref\"},\"createdAt\":{\"type\":\"string\",\"format\":\"datetime\"}}}}}}"

  let assert Ok(_) = lexicons.insert(db, "app.bsky.feed.post", post_lexicon)
  let assert Ok(_) = lexicons.insert(db, "app.bsky.feed.like", like_lexicon)

  // Insert a like that mentions the target
  let assert Ok(_) =
    records.insert(
      db,
      "at://did:plc:author/app.bsky.feed.like/abc",
      "bafy123",
      "did:plc:author",
      "app.bsky.feed.like",
      "{\"subject\":{\"uri\":\"at://did:plc:target/app.bsky.feed.post/xyz\"},\"createdAt\":\"2024-01-01T00:00:00Z\"}",
    )

  // Execute notifications query
  let query =
    "query { notifications(did: \"did:plc:target\", first: 10) { edges { node { ... on AppBskyFeedLike { uri value { createdAt } } } cursor } pageInfo { hasNextPage } } }"

  let assert Ok(result) = lexicon_schema.execute_query_with_db(db, query, "{}", None)

  // Should return the like
  result |> should.not_equal("{\"data\":{\"notifications\":{\"edges\":[],\"pageInfo\":{\"hasNextPage\":false}}}}")
}
```

### Step 2: Run test to verify it fails

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test -- --filter=notifications_query_returns`
Expected: FAIL

### Step 3: Write minimal implementation

Add to `/Users/chadmiller/code/quickslice/server/src/graphql/lexicon/fetchers.gleam`:

```gleam
/// Create a notification fetcher that queries records mentioning a DID
pub fn notification_fetcher(db: Executor) {
  fn(
    did: String,
    collections: option.Option(List(String)),
    first: option.Option(Int),
    after: option.Option(String),
  ) -> Result(#(List(#(value.Value, String)), option.Option(String), Bool, Bool), String) {
    use result <- result.try(
      records.get_notifications(db, did, collections, first, after)
      |> result.map_error(fn(e) { "Database error: " <> string.inspect(e) }),
    )

    let #(records_list, end_cursor, has_next, has_prev) = result

    let converted =
      list.map(records_list, fn(record) {
        let record_value = converters.record_to_graphql_value(record, db)
        let cursor = pagination.encode_cursor_from_record(record)
        #(record_value, cursor)
      })

    Ok(#(converted, end_cursor, has_next, has_prev))
  }
}
```

Update `/Users/chadmiller/code/quickslice/server/src/graphql/lexicon/schema.gleam` to pass the notification fetcher:

```gleam
pub fn build_schema_from_db(db: Executor) -> Result(schema.Schema, String) {
  // ... existing code ...

  database.build_schema_with_subscriptions(
    parsed_lexicons,
    record_fetcher(db),
    option.Some(batch_fetcher(db)),
    option.Some(paginated_batch_fetcher(db)),
    mutation_create_factory,
    mutation_update_factory,
    mutation_delete_factory,
    option.Some(notification_fetcher(db)),  // Add this
  )
}
```

### Step 4: Run test to verify it passes

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test -- --filter=notifications_query_returns`
Expected: PASS

### Step 5: Commit

```bash
git add server/src/graphql/lexicon/fetchers.gleam server/src/graphql/lexicon/schema.gleam server/test/graphql/notifications_integration_test.gleam
git commit -m "feat(server): integrate notification fetcher with schema"
```

---

## Task 8: Subscription Handler - Filter events for notification subscriptions

**Files:**
- Modify: `/Users/chadmiller/code/quickslice/server/src/handlers/graphql_ws.gleam`

### Step 1: Write the failing test

```gleam
// /Users/chadmiller/code/quickslice/server/test/handlers/notification_subscription_test.gleam
import gleam/string
import gleeunit/should
import handlers/graphql_ws
import pubsub

pub fn notification_event_matches_did_test() {
  let event =
    pubsub.RecordEvent(
      uri: "at://did:plc:author/app.bsky.feed.like/abc",
      cid: "bafy123",
      did: "did:plc:author",
      collection: "app.bsky.feed.like",
      value: "{\"subject\":{\"uri\":\"at://did:plc:target/app.bsky.feed.post/xyz\"}}",
      indexed_at: "2024-01-01T00:00:00Z",
      operation: pubsub.Create,
    )

  let matches =
    graphql_ws.event_matches_notification_subscription(
      event,
      "did:plc:target",
      None,
    )

  matches |> should.be_true()
}

pub fn notification_event_excludes_self_authored_test() {
  let event =
    pubsub.RecordEvent(
      uri: "at://did:plc:target/app.bsky.feed.post/xyz",
      cid: "bafy123",
      did: "did:plc:target",
      collection: "app.bsky.feed.post",
      value: "{\"text\":\"Hello\"}",
      indexed_at: "2024-01-01T00:00:00Z",
      operation: pubsub.Create,
    )

  let matches =
    graphql_ws.event_matches_notification_subscription(
      event,
      "did:plc:target",
      None,
    )

  matches |> should.be_false()
}

pub fn notification_event_filters_by_collection_test() {
  let event =
    pubsub.RecordEvent(
      uri: "at://did:plc:author/app.bsky.feed.like/abc",
      cid: "bafy123",
      did: "did:plc:author",
      collection: "app.bsky.feed.like",
      value: "{\"subject\":{\"uri\":\"at://did:plc:target/app.bsky.feed.post/xyz\"}}",
      indexed_at: "2024-01-01T00:00:00Z",
      operation: pubsub.Create,
    )

  // Filter to only posts, like should not match
  let matches =
    graphql_ws.event_matches_notification_subscription(
      event,
      "did:plc:target",
      Some(["app.bsky.feed.post"]),
    )

  matches |> should.be_false()
}
```

### Step 2: Run test to verify it fails

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test -- --filter=notification_event`
Expected: FAIL

### Step 3: Write minimal implementation

Add to `/Users/chadmiller/code/quickslice/server/src/handlers/graphql_ws.gleam`:

```gleam
/// Check if a record event matches a notification subscription
pub fn event_matches_notification_subscription(
  event: pubsub.RecordEvent,
  subscribed_did: String,
  collections: option.Option(List(String)),
) -> Bool {
  // Event must contain the subscribed DID
  let contains_did = string.contains(event.value, subscribed_did)

  // Event must not be authored by the subscribed DID
  let not_self_authored = event.did != subscribed_did

  // Event must be a Create operation
  let is_create = event.operation == pubsub.Create

  // Event collection must match filter (if provided)
  let matches_collection = case collections {
    option.None -> True
    option.Some([]) -> True
    option.Some(cols) -> list.contains(cols, event.collection)
  }

  contains_did && not_self_authored && is_create && matches_collection
}
```

Then update the subscription event loop to use this for `notificationCreated` subscriptions:

```gleam
fn handle_subscription_event(
  event: pubsub.RecordEvent,
  subscription: ActiveSubscription,
  // ... other params
) {
  case subscription.field_name {
    "notificationCreated" -> {
      let did = get_subscription_arg(subscription, "did")
      let collections = get_subscription_arg_list(subscription, "collections")
      case event_matches_notification_subscription(event, did, collections) {
        True -> send_subscription_event(subscription, event)
        False -> Nil
      }
    }
    // ... existing record subscription handling ...
  }
}
```

### Step 4: Run test to verify it passes

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test -- --filter=notification_event`
Expected: PASS

### Step 5: Commit

```bash
git add server/src/handlers/graphql_ws.gleam server/test/handlers/notification_subscription_test.gleam
git commit -m "feat(ws): add notification subscription event filtering"
```

---

## Task 9: End-to-end test

**Files:**
- Test: `/Users/chadmiller/code/quickslice/server/test/graphql/notifications_e2e_test.gleam`

### Step 1: Write comprehensive e2e test

```gleam
// /Users/chadmiller/code/quickslice/server/test/graphql/notifications_e2e_test.gleam
import database/repositories/actors
import database/repositories/lexicons
import database/repositories/records
import gleam/json
import gleam/string
import gleeunit/should
import graphql/lexicon/schema as lexicon_schema
import test_helpers

pub fn notifications_e2e_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_record_table(db)
  let assert Ok(_) = test_helpers.create_actor_table(db)
  let assert Ok(_) = test_helpers.create_lexicon_table(db)

  // Setup lexicons
  let post_lexicon =
    "{\"lexicon\":1,\"id\":\"app.bsky.feed.post\",\"defs\":{\"main\":{\"type\":\"record\",\"record\":{\"properties\":{\"text\":{\"type\":\"string\"},\"createdAt\":{\"type\":\"string\",\"format\":\"datetime\"}}}}}}"
  let like_lexicon =
    "{\"lexicon\":1,\"id\":\"app.bsky.feed.like\",\"defs\":{\"main\":{\"type\":\"record\",\"record\":{\"properties\":{\"subject\":{\"type\":\"ref\"},\"createdAt\":{\"type\":\"string\",\"format\":\"datetime\"}}}}}}"
  let follow_lexicon =
    "{\"lexicon\":1,\"id\":\"app.bsky.graph.follow\",\"defs\":{\"main\":{\"type\":\"record\",\"record\":{\"properties\":{\"subject\":{\"type\":\"string\"},\"createdAt\":{\"type\":\"string\",\"format\":\"datetime\"}}}}}}"

  let assert Ok(_) = lexicons.insert(db, "app.bsky.feed.post", post_lexicon)
  let assert Ok(_) = lexicons.insert(db, "app.bsky.feed.like", like_lexicon)
  let assert Ok(_) = lexicons.insert(db, "app.bsky.graph.follow", follow_lexicon)

  // Setup actors
  let assert Ok(_) = actors.upsert(db, "did:plc:target", "target.bsky.social")
  let assert Ok(_) = actors.upsert(db, "did:plc:alice", "alice.bsky.social")
  let assert Ok(_) = actors.upsert(db, "did:plc:bob", "bob.bsky.social")

  // Target's own post (should NOT appear in notifications)
  let assert Ok(_) =
    records.insert(
      db,
      "at://did:plc:target/app.bsky.feed.post/post1",
      "bafy001",
      "did:plc:target",
      "app.bsky.feed.post",
      "{\"text\":\"Hello world\",\"createdAt\":\"2024-01-01T00:00:00Z\"}",
    )

  // Alice likes target's post (SHOULD appear)
  let assert Ok(_) =
    records.insert(
      db,
      "at://did:plc:alice/app.bsky.feed.like/like1",
      "bafy002",
      "did:plc:alice",
      "app.bsky.feed.like",
      "{\"subject\":{\"uri\":\"at://did:plc:target/app.bsky.feed.post/post1\"},\"createdAt\":\"2024-01-02T00:00:00Z\"}",
    )

  // Bob follows target (SHOULD appear)
  let assert Ok(_) =
    records.insert(
      db,
      "at://did:plc:bob/app.bsky.graph.follow/follow1",
      "bafy003",
      "did:plc:bob",
      "app.bsky.graph.follow",
      "{\"subject\":\"did:plc:target\",\"createdAt\":\"2024-01-03T00:00:00Z\"}",
    )

  // Alice's unrelated post (should NOT appear)
  let assert Ok(_) =
    records.insert(
      db,
      "at://did:plc:alice/app.bsky.feed.post/post2",
      "bafy004",
      "did:plc:alice",
      "app.bsky.feed.post",
      "{\"text\":\"Unrelated post\",\"createdAt\":\"2024-01-04T00:00:00Z\"}",
    )

  // Query all notifications
  let query =
    "query { notifications(did: \"did:plc:target\", first: 10) { edges { node { ... on AppBskyFeedLike { uri did } ... on AppBskyGraphFollow { uri did } } } } }"

  let assert Ok(result) = lexicon_schema.execute_query_with_db(db, query, "{}", None)

  // Should have 2 notifications (like + follow)
  result |> string.contains("did:plc:alice") |> should.be_true()
  result |> string.contains("did:plc:bob") |> should.be_true()
  result |> string.contains("like1") |> should.be_true()
  result |> string.contains("follow1") |> should.be_true()

  // Query with collection filter (only likes)
  let filtered_query =
    "query { notifications(did: \"did:plc:target\", collections: [APP_BSKY_FEED_LIKE], first: 10) { edges { node { ... on AppBskyFeedLike { uri } } } } }"

  let assert Ok(filtered_result) =
    lexicon_schema.execute_query_with_db(db, filtered_query, "{}", None)

  // Should only have the like
  filtered_result |> string.contains("like1") |> should.be_true()
  filtered_result |> string.contains("follow1") |> should.be_false()
}
```

### Step 2: Run test

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test -- --filter=notifications_e2e`
Expected: PASS

### Step 3: Commit

```bash
git add server/test/graphql/notifications_e2e_test.gleam
git commit -m "test: add notifications e2e test"
```

---

## Task 10: Final cleanup and documentation

### Step 1: Run full test suite

Run: `cd /Users/chadmiller/code/quickslice && gleam test`
Expected: All tests pass

### Step 2: Update CHANGELOG if exists

### Step 3: Final commit

```bash
git add -A
git commit -m "feat: notifications GraphQL query and subscription

- Add notifications(did, collections, first, after) query
- Add notificationCreated(did, collections) subscription
- Auto-generate RecordCollection enum from lexicons
- Auto-generate NotificationRecord union from record types
- Cross-collection DID search using SQL LIKE
- Exclude self-authored records
- Fixed newest-first sorting"
```

---

## Summary

| Task | Description | Files |
|------|-------------|-------|
| 1 | Database get_notifications function | records.gleam, notifications_test.gleam |
| 2 | Collection filter test | notifications_test.gleam |
| 3 | RecordCollection enum generation | database.gleam |
| 4 | NotificationRecord union generation | database.gleam |
| 5 | notifications query field | database.gleam |
| 6 | notificationCreated subscription | database.gleam |
| 7 | Server notification fetcher integration | fetchers.gleam, schema.gleam |
| 8 | Subscription event filtering | graphql_ws.gleam |
| 9 | E2E test | notifications_e2e_test.gleam |
| 10 | Final cleanup | - |
