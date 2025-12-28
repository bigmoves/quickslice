# Viewer DID from Auth Token Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Extract the viewer's DID from the authenticated user's token instead of requiring clients to pass it as a GraphQL variable.

**Architecture:** Currently, viewer state fields require clients to pass `viewer_did` as a GraphQL variable, which is redundant (the DID is already in the auth token) and insecure (clients could spoof it). We'll modify the query execution to verify the auth token, extract the DID, and inject it into context.data where resolvers can access it.

**Tech Stack:** Gleam, swell GraphQL library, atproto_auth module

---

### Task 1: Update Schema to Extract Viewer DID from Auth Token

**Files:**
- Modify: `server/src/graphql/lexicon/schema.gleam:1-30` (imports)
- Modify: `server/src/graphql/lexicon/schema.gleam:176-188` (execute_query_with_db context creation)

**Step 1: Add atproto_auth import**

In `server/src/graphql/lexicon/schema.gleam`, add the import after line 4:

```gleam
import atproto_auth
```

**Step 2: Modify context creation to extract viewer DID**

Replace the context creation block (lines 176-188) with:

```gleam
  // Create context with viewer DID if authenticated
  let ctx_data = case auth_token {
    Ok(token) -> {
      // Verify token and extract user DID
      case atproto_auth.verify_token(db, token) {
        Ok(user_info) -> {
          // Add both auth_token (for mutations) and viewer_did (for viewer fields)
          option.Some(value.Object([
            #("auth_token", value.String(token)),
            #("viewer_did", value.String(user_info.did)),
          ]))
        }
        Error(_) -> {
          // Token invalid/expired - still allow query but without viewer context
          option.None
        }
      }
    }
    Error(_) -> option.None
  }

  // Convert json variables to Dict(String, value.Value)
  let variables_dict = json_string_to_variables_dict(variables_json_str)

  let ctx = schema.context_with_variables(ctx_data, variables_dict)
```

**Step 3: Build and verify no compile errors**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles successfully

**Step 4: Commit**

```bash
git add server/src/graphql/lexicon/schema.gleam
git commit -m "feat(schema): extract viewer_did from auth token into context"
```

---

### Task 2: Update Viewer State Resolver to Read from Context Data

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/schema/database.gleam:1112-1119` (get_viewer_did_from_context function)

**Step 1: Update get_viewer_did_from_context to read from ctx.data**

Replace the `get_viewer_did_from_context` function (around lines 1112-1119) with:

```gleam
/// Extract viewer DID from context data
/// The viewer DID is set by the server after verifying the auth token
fn get_viewer_did_from_context(ctx: schema.Context) -> Result(String, Nil) {
  case ctx.data {
    option.Some(value.Object(fields)) -> {
      case list.key_find(fields, "viewer_did") {
        Ok(value.String(did)) -> Ok(did)
        _ -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}
```

**Step 2: Build and verify no compile errors**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`
Expected: Compiles successfully

**Step 3: Build server to verify integration**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles successfully

**Step 4: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/schema/database.gleam
git commit -m "feat(viewer-state): read viewer_did from context data instead of variables"
```

---

### Task 3: Add Test Helper for Mock Auth Tokens

**Files:**
- Modify: `server/test/test_helpers.gleam`

Insert a test token into `oauth_access_tokens` table that maps to the test viewer DID.

**Step 1: Check oauth_access_tokens table structure**

Review `server/src/database/repositories/oauth_access_tokens.gleam` to understand the required fields.

**Step 2: Add helper to create oauth_access_tokens table and insert test token**

In `server/test/test_helpers.gleam`, add:

```gleam
/// Create oauth_access_tokens table for testing
pub fn create_oauth_access_tokens_table(exec: Executor) -> Result(Nil, sqlight.Error) {
  let sql = "
    CREATE TABLE IF NOT EXISTS oauth_access_tokens (
      token TEXT PRIMARY KEY,
      user_id TEXT NOT NULL,
      client_id TEXT NOT NULL,
      scope TEXT NOT NULL,
      expires_at INTEGER NOT NULL,
      created_at INTEGER NOT NULL,
      revoked INTEGER NOT NULL DEFAULT 0
    )
  "
  case sqlight.query(sql, exec, [], decode.dynamic) {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(e)
  }
}

/// Insert a test token that maps to a DID
pub fn insert_test_token(
  exec: Executor,
  token: String,
  did: String,
) -> Result(Nil, sqlight.Error) {
  let far_future = 9999999999  // Won't expire
  let sql = "INSERT INTO oauth_access_tokens (token, user_id, client_id, scope, expires_at, created_at, revoked) VALUES (?, ?, 'test-client', 'atproto', ?, ?, 0)"

  case sqlight.query(sql, exec, [sqlight.text(token), sqlight.text(did), sqlight.int(far_future), sqlight.int(0)], decode.dynamic) {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(e)
  }
}
```

**Step 3: Build and verify**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles successfully

**Step 4: Commit**

```bash
git add server/test/test_helpers.gleam
git commit -m "test: add helper to create mock auth tokens for testing"
```

---

### Task 4: Update Integration Tests to Use Mock Auth Token

**Files:**
- Modify: `server/test/viewer_state_integration_test.gleam`

**Step 1: Update test setup to create token table and insert test token**

Add to each test's setup:

```gleam
// Setup auth token for viewer
let assert Ok(_) = test_helpers.create_oauth_access_tokens_table(exec)
let assert Ok(_) = test_helpers.insert_test_token(exec, "test-viewer-token", "did:plc:viewer")
```

**Step 2: Update requests to use Authorization header instead of variables**

Change from:
```gleam
let query =
  json.object([
    #("query", json.string("{ ... }")),
    #("variables", json.object([#("viewer_did", json.string("did:plc:viewer"))])),
  ])
  |> json.to_string

let request =
  simulate.request(http.Post, "/graphql")
  |> simulate.string_body(query)
  |> simulate.header("content-type", "application/json")
```

To:
```gleam
let query =
  json.object([
    #("query", json.string("{ ... }")),
  ])
  |> json.to_string

let request =
  simulate.request(http.Post, "/graphql")
  |> simulate.string_body(query)
  |> simulate.header("content-type", "application/json")
  |> simulate.header("authorization", "Bearer test-viewer-token")
```

**Step 3: Run viewer state tests**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test -- --filter viewer_state`
Expected: All viewer_state tests pass

**Step 4: Run all tests**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: All tests pass

**Step 5: Commit**

```bash
git add server/test/viewer_state_integration_test.gleam
git commit -m "test(viewer-state): use mock auth tokens instead of variables"
```

---

### Task 5: Update WebSocket Handler for Subscriptions

**Files:**
- Check: `server/src/handlers/graphql_ws.gleam` - verify it passes auth context correctly

**Step 1: Review WebSocket handler**

Read `server/src/handlers/graphql_ws.gleam` to verify:
1. Auth token is extracted from WebSocket connection
2. Context is created similarly to HTTP handler

**Step 2: Update if needed**

If the WebSocket handler creates context differently, update it to also verify token and inject viewer_did.

**Step 3: Commit if changes made**

```bash
git add server/src/handlers/graphql_ws.gleam
git commit -m "feat(ws): extract viewer_did from auth for subscription context"
```

---

### Task 5: Final Verification

**Step 1: Run all server tests**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: All tests pass

**Step 2: Run lexicon_graphql tests**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`
Expected: All tests pass

**Step 3: Manual verification (optional)**

Start the server and test with a real authenticated request:
- Query with auth token should auto-populate viewer fields
- Query without auth token should return null for viewer fields

---

## Summary of Changes

| File | Change |
|------|--------|
| `server/src/graphql/lexicon/schema.gleam` | Import atproto_auth, verify token, inject viewer_did into context.data |
| `lexicon_graphql/src/lexicon_graphql/schema/database.gleam` | Read viewer_did from context.data with variable fallback |
| `server/src/handlers/graphql_ws.gleam` | (If needed) Same auth extraction for WebSocket |

## API Behavior After Change

**Before:** Clients must pass `viewer_did` variable:
```json
{
  "query": "{ gallery { viewerFavorite { uri } } }",
  "variables": { "viewer_did": "did:plc:abc123" }
}
```

**After:** Viewer DID extracted automatically from auth token:
```json
{
  "query": "{ gallery { viewerFavorite { uri } } }"
}
```
(With `Authorization: Bearer <token>` header)
