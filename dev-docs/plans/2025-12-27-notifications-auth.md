# Notifications Query Authentication Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove `viewerDid` argument from notifications query and use authenticated user's DID from auth token instead.

**Architecture:** The notifications query currently requires a `viewerDid` argument passed by the client. We'll change it to use the same pattern as viewer state fields: read `viewer_did` from context variables (injected by server from auth token). Unauthenticated requests will return an error.

**Tech Stack:** Gleam, lexicon_graphql schema, GraphQL

---

### Task 1: Update Schema - Remove viewerDid Argument

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/schema/database.gleam:2296-2302`

**Step 1: Remove viewerDid from build_notification_query_args**

Find and remove the `viewerDid` argument from `build_notification_query_args`:

```gleam
// BEFORE (lines 2296-2302):
  let base_args = [
    schema.argument(
      "viewerDid",
      schema.non_null(schema.string_type()),
      "DID of the viewer to get notifications for",
      option.None,
    ),
    schema.argument(

// AFTER:
  let base_args = [
    schema.argument(
```

Remove the entire `schema.argument("viewerDid", ...)` block (lines 2297-2302).

**Step 2: Verify removal compiles**

Run: `cd /Users/chadmiller/code/quickslice && gleam build`
Expected: Compilation succeeds (resolver still has the old code but we'll fix that next)

---

### Task 2: Update Schema - Use get_viewer_did_from_context

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/schema/database.gleam:2205-2272`

**Step 1: Update resolver to use get_viewer_did_from_context**

Replace the resolver's argument extraction with context variable lookup:

```gleam
// BEFORE (lines 2205-2272):
          fn(ctx: schema.Context) {
            // Get viewer DID from context (viewer field should be resolved first)
            case schema.get_argument(ctx, "viewerDid") {
              option.Some(value.String(viewer_did)) -> {
                // ... rest of resolver ...
              }
              _ -> Error("notifications query requires viewerDid argument")
            }
          },

// AFTER:
          fn(ctx: schema.Context) {
            // Get viewer DID from auth token (injected by server into variables)
            case get_viewer_did_from_context(ctx) {
              Ok(viewer_did) -> {
                // ... rest of resolver (unchanged) ...
              }
              Error(Nil) -> Error("notifications query requires authentication")
            }
          },
```

The inner resolver logic (lines 2209-2269) remains exactly the same.

**Step 2: Verify schema compiles**

Run: `cd /Users/chadmiller/code/quickslice && gleam build`
Expected: Compilation succeeds

**Step 3: Commit schema changes**

```bash
git add lexicon_graphql/src/lexicon_graphql/schema/database.gleam
git commit -m "feat(notifications): use authenticated user DID instead of viewerDid argument

Remove viewerDid argument from notifications query. Now uses viewer_did
from auth token context, matching the pattern used by viewer state fields.
Unauthenticated requests return 'notifications query requires authentication'."
```

---

### Task 3: Update Tests - Add OAuth Setup

**Files:**
- Modify: `server/test/graphql/notifications_e2e_test.gleam`

**Step 1: Add test_helpers import for OAuth setup**

The file already imports `test_helpers`. Verify it's present at line 17.

**Step 2: Update notifications_returns_mentioning_records_test**

Add OAuth table setup and test token after existing table creation (after line 172):

```gleam
  // After line 172: let assert Ok(_) = test_helpers.create_actor_table(exec)
  // Add:
  let assert Ok(_) = test_helpers.create_oauth_tables(exec)
  let assert Ok(_) =
    test_helpers.insert_test_token(exec, "test-notification-token", "did:plc:target")
```

Update the query string (line 235) to remove viewerDid:

```gleam
// BEFORE:
      notifications(viewerDid: \"did:plc:target\", first: 10) {

// AFTER:
      notifications(first: 10) {
```

Update the execute_query_with_db call to pass auth token (line 258-267):

```gleam
// BEFORE:
    lexicon_schema.execute_query_with_db(
      exec,
      query,
      "{}",
      Error(Nil),
      cache,

// AFTER:
    lexicon_schema.execute_query_with_db(
      exec,
      query,
      "{}",
      Ok("test-notification-token"),
      cache,
```

**Step 3: Update notifications_filters_by_collection_test**

Add OAuth setup after line 294:

```gleam
  let assert Ok(_) = test_helpers.create_oauth_tables(exec)
  let assert Ok(_) =
    test_helpers.insert_test_token(exec, "test-notification-token", "did:plc:target")
```

Update query string (line 335):

```gleam
// BEFORE:
      notifications(viewerDid: \"did:plc:target\", collections: [APP_BSKY_FEED_LIKE], first: 10) {

// AFTER:
      notifications(collections: [APP_BSKY_FEED_LIKE], first: 10) {
```

Update execute_query_with_db call (line 355):

```gleam
      Error(Nil),
// change to:
      Ok("test-notification-token"),
```

**Step 4: Update notifications_excludes_self_authored_test**

Add OAuth setup after line 383:

```gleam
  let assert Ok(_) = test_helpers.create_oauth_tables(exec)
  let assert Ok(_) =
    test_helpers.insert_test_token(exec, "test-notification-token", "did:plc:target")
```

Update query string (line 406):

```gleam
// BEFORE:
      notifications(viewerDid: \"did:plc:target\", first: 10) {

// AFTER:
      notifications(first: 10) {
```

Update execute_query_with_db call (line 423):

```gleam
      Error(Nil),
// change to:
      Ok("test-notification-token"),
```

---

### Task 4: Run Tests and Verify

**Step 1: Run notification tests**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test -- --filter notifications`
Expected: All 3 notification tests pass

**Step 2: Run all tests to check for regressions**

Run: `cd /Users/chadmiller/code/quickslice && gleam test`
Expected: All tests pass

**Step 3: Commit test changes**

```bash
git add server/test/graphql/notifications_e2e_test.gleam
git commit -m "test(notifications): update tests to use OAuth authentication

Tests now use test tokens instead of viewerDid argument, matching
the new authentication-based approach for the notifications query."
```

---

### Task 5: Final Verification

**Step 1: Run full test suite**

Run: `cd /Users/chadmiller/code/quickslice && gleam test`
Expected: All tests pass

**Step 2: Verify schema introspection (optional)**

The notifications query should no longer show viewerDid as a required argument in the schema.

---

## Summary of Changes

| File | Change |
|------|--------|
| `lexicon_graphql/src/lexicon_graphql/schema/database.gleam` | Remove `viewerDid` argument, use `get_viewer_did_from_context()` |
| `server/test/graphql/notifications_e2e_test.gleam` | Add OAuth setup, remove `viewerDid` from queries, pass auth token |

**Error behavior:**
- Unauthenticated requests: `"notifications query requires authentication"`
