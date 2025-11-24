# Handler Reorganization Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Reorganize all HTTP/WebSocket handlers into a dedicated `handlers/` directory with consistent naming (no `_handler` suffix) to improve code organization and discoverability.

**Architecture:** Extract inline route handlers from `server.gleam` into separate modules, move existing `*_handler.gleam` files to `handlers/` directory with renamed modules (dropping `_handler` suffix), and update all imports. Keep `oauth/` as a separate directory for OAuth-related functionality.

**Tech Stack:** Gleam, Wisp web framework, Mist HTTP server

---

### Task 1: Create handlers directory and health handler

**Files:**
- Create: `/Users/chadmiller/code/quickslice/server/src/handlers/health.gleam`

**Step 1: Create handlers directory**

Run: `mkdir -p /Users/chadmiller/code/quickslice/server/src/handlers`
Expected: Directory created successfully

**Step 2: Create health.gleam handler**

Create `/Users/chadmiller/code/quickslice/server/src/handlers/health.gleam` with:

```gleam
/// Health check endpoint handler
///
/// Handles /health endpoint with database connectivity verification
import database/repositories/lexicons
import sqlight
import wisp

/// Handle health check request
/// Returns 200 if database is accessible, 503 if not
pub fn handle(db: sqlight.Connection) -> wisp.Response {
  // Try a simple database query to verify connectivity
  case lexicons.get_count(db) {
    Ok(_) -> {
      // Database is accessible
      wisp.response(200)
      |> wisp.set_header("content-type", "application/json")
      |> wisp.set_body(wisp.Text("{\"status\": \"healthy\"}"))
    }
    Error(_) -> {
      // Database is not accessible
      wisp.response(503)
      |> wisp.set_header("content-type", "application/json")
      |> wisp.set_body(wisp.Text(
        "{\"status\": \"unhealthy\", \"message\": \"Database connection failed\"}",
      ))
    }
  }
}
```

**Step 3: Build to verify syntax**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds with no errors

**Step 4: Commit**

```bash
git add server/src/handlers/health.gleam
git commit -m "feat: create health handler module

Extract health check endpoint from server.gleam into dedicated handler

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

### Task 2: Create backfill handler

**Files:**
- Create: `/Users/chadmiller/code/quickslice/server/src/handlers/backfill.gleam`

**Step 1: Create backfill.gleam handler**

Create `/Users/chadmiller/code/quickslice/server/src/handlers/backfill.gleam` with:

```gleam
/// Backfill endpoint handler
///
/// Handles /backfill POST endpoint for triggering collection backfills
import backfill
import config
import database/repositories/lexicons
import gleam/erlang/process
import gleam/http as gleam_http
import gleam/int
import gleam/list
import gleam/option
import sqlight
import wisp

/// Handle backfill request
/// Only accepts POST method to trigger backfill
pub fn handle(
  req: wisp.Request,
  db: sqlight.Connection,
  config_subject: process.Subject(config.Message),
) -> wisp.Response {
  case req.method {
    gleam_http.Post -> {
      // Get domain authority from config
      let domain_authority = case config.get_domain_authority(config_subject) {
        option.Some(authority) -> authority
        option.None -> ""
      }

      // Get all record-type lexicons
      case lexicons.get_record_types(db) {
        Ok(lexicons) -> {
          case lexicons {
            [] -> {
              wisp.response(200)
              |> wisp.set_header("content-type", "application/json")
              |> wisp.set_body(wisp.Text(
                "{\"status\": \"no_lexicons\", \"message\": \"No record-type lexicons found\"}",
              ))
            }
            _ -> {
              // Separate lexicons by domain authority
              let #(collections, external_collections) =
                lexicons
                |> list.partition(fn(lex) {
                  backfill.nsid_matches_domain_authority(
                    lex.id,
                    domain_authority,
                  )
                })

              let collection_ids = list.map(collections, fn(lex) { lex.id })
              let external_collection_ids =
                list.map(external_collections, fn(lex) { lex.id })

              // Run backfill in background process
              let backfill_config = backfill.default_config()
              process.spawn_unlinked(fn() {
                backfill.backfill_collections(
                  [],
                  collection_ids,
                  external_collection_ids,
                  backfill_config,
                  db,
                )
              })

              wisp.response(200)
              |> wisp.set_header("content-type", "application/json")
              |> wisp.set_body(wisp.Text(
                "{\"status\": \"started\", \"collections\": "
                <> int.to_string(list.length(collection_ids))
                <> ", \"external_collections\": "
                <> int.to_string(list.length(external_collection_ids))
                <> "}",
              ))
            }
          }
        }
        Error(_) -> {
          wisp.response(500)
          |> wisp.set_header("content-type", "application/json")
          |> wisp.set_body(wisp.Text(
            "{\"error\": \"database_error\", \"message\": \"Failed to fetch lexicons\"}",
          ))
        }
      }
    }
    _ -> {
      wisp.response(405)
      |> wisp.set_header("content-type", "application/json")
      |> wisp.set_body(wisp.Text(
        "{\"error\": \"method_not_allowed\", \"message\": \"Use POST to trigger backfill\"}",
      ))
    }
  }
}
```

**Step 2: Build to verify syntax**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds with no errors

**Step 3: Commit**

```bash
git add server/src/handlers/backfill.gleam
git commit -m "feat: create backfill handler module

Extract backfill endpoint from server.gleam into dedicated handler

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

### Task 3: Create index handler

**Files:**
- Create: `/Users/chadmiller/code/quickslice/server/src/handlers/index.gleam`

**Step 1: Create index.gleam handler**

Create `/Users/chadmiller/code/quickslice/server/src/handlers/index.gleam` with:

```gleam
/// Index/SPA handler
///
/// Serves the client SPA's index.html for root and fallback routes
import simplifile
import wisp

/// Handle index route and SPA fallback
/// Serves the bundled client application's index.html
pub fn handle() -> wisp.Response {
  // Serve the client SPA's index.html (bundled by lustre dev tools)
  let assert Ok(priv_dir) = wisp.priv_directory("server")
  let index_path = priv_dir <> "/static/index.html"

  case simplifile.read(index_path) {
    Ok(contents) -> wisp.html_response(contents, 200)
    Error(_) ->
      wisp.html_response(
        "<h1>Error</h1><p>Client application not found. Run 'gleam run -m lustre/dev build' in the client directory.</p>",
        500,
      )
  }
}
```

**Step 2: Build to verify syntax**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds with no errors

**Step 3: Commit**

```bash
git add server/src/handlers/index.gleam
git commit -m "feat: create index handler module

Extract SPA index route from server.gleam into dedicated handler

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

### Task 4: Move graphql_handler to handlers/graphql

**Files:**
- Move: `/Users/chadmiller/code/quickslice/server/src/graphql_handler.gleam` → `/Users/chadmiller/code/quickslice/server/src/handlers/graphql.gleam`

**Step 1: Move and rename the file**

Run: `cd /Users/chadmiller/code/quickslice/server && mv src/graphql_handler.gleam src/handlers/graphql.gleam`
Expected: File moved successfully

**Step 2: Update module name in graphql.gleam**

The file already has appropriate naming - function is `handle_graphql_request` which is fine. No changes needed to the file contents.

**Step 3: Build to verify**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add server/src/handlers/graphql.gleam
git add server/src/graphql_handler.gleam
git commit -m "refactor: move graphql_handler to handlers/graphql

Move to organized handlers directory with consistent naming

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

### Task 5: Move client_graphql_handler to handlers/client_graphql

**Files:**
- Move: `/Users/chadmiller/code/quickslice/server/src/client_graphql_handler.gleam` → `/Users/chadmiller/code/quickslice/server/src/handlers/client_graphql.gleam`

**Step 1: Move and rename the file**

Run: `cd /Users/chadmiller/code/quickslice/server && mv src/client_graphql_handler.gleam src/handlers/client_graphql.gleam`
Expected: File moved successfully

**Step 2: Build to verify**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add server/src/handlers/client_graphql.gleam
git add server/src/client_graphql_handler.gleam
git commit -m "refactor: move client_graphql_handler to handlers/client_graphql

Move to organized handlers directory with consistent naming

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

### Task 6: Move graphql_ws_handler to handlers/graphql_ws

**Files:**
- Move: `/Users/chadmiller/code/quickslice/server/src/graphql_ws_handler.gleam` → `/Users/chadmiller/code/quickslice/server/src/handlers/graphql_ws.gleam`

**Step 1: Move and rename the file**

Run: `cd /Users/chadmiller/code/quickslice/server && mv src/graphql_ws_handler.gleam src/handlers/graphql_ws.gleam`
Expected: File moved successfully

**Step 2: Build to verify**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add server/src/handlers/graphql_ws.gleam
git add server/src/graphql_ws_handler.gleam
git commit -m "refactor: move graphql_ws_handler to handlers/graphql_ws

Move to organized handlers directory with consistent naming

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

### Task 7: Move graphiql_handler to handlers/graphiql

**Files:**
- Move: `/Users/chadmiller/code/quickslice/server/src/graphiql_handler.gleam` → `/Users/chadmiller/code/quickslice/server/src/handlers/graphiql.gleam`

**Step 1: Move and rename the file**

Run: `cd /Users/chadmiller/code/quickslice/server && mv src/graphiql_handler.gleam src/handlers/graphiql.gleam`
Expected: File moved successfully

**Step 2: Build to verify**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add server/src/handlers/graphiql.gleam
git add server/src/graphiql_handler.gleam
git commit -m "refactor: move graphiql_handler to handlers/graphiql

Move to organized handlers directory with consistent naming

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

### Task 8: Move upload_handler to handlers/upload

**Files:**
- Move: `/Users/chadmiller/code/quickslice/server/src/upload_handler.gleam` → `/Users/chadmiller/code/quickslice/server/src/handlers/upload.gleam`

**Step 1: Move and rename the file**

Run: `cd /Users/chadmiller/code/quickslice/server && mv src/upload_handler.gleam src/handlers/upload.gleam`
Expected: File moved successfully

**Step 2: Build to verify**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add server/src/handlers/upload.gleam
git add server/src/upload_handler.gleam
git commit -m "refactor: move upload_handler to handlers/upload

Move to organized handlers directory with consistent naming

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

### Task 9: Move settings_handler to handlers/settings

**Files:**
- Move: `/Users/chadmiller/code/quickslice/server/src/settings_handler.gleam` → `/Users/chadmiller/code/quickslice/server/src/handlers/settings.gleam`

**Step 1: Move and rename the file**

Run: `cd /Users/chadmiller/code/quickslice/server && mv src/settings_handler.gleam src/handlers/settings.gleam`
Expected: File moved successfully

**Step 2: Build to verify**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add server/src/handlers/settings.gleam
git add server/src/settings_handler.gleam
git commit -m "refactor: move settings_handler to handlers/settings

Move to organized handlers directory with consistent naming

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

### Task 10: Update server.gleam imports and routing

**Files:**
- Modify: `/Users/chadmiller/code/quickslice/server/src/server.gleam`

**Step 1: Update imports (lines 5-31)**

Replace the handler imports:

```gleam
import client_graphql_handler
import graphiql_handler
import graphql_handler
import graphql_ws_handler
import upload_handler
```

With:

```gleam
import handlers/backfill as backfill_handler
import handlers/client_graphql as client_graphql_handler
import handlers/graphiql as graphiql_handler
import handlers/graphql as graphql_handler
import handlers/graphql_ws as graphql_ws_handler
import handlers/health as health_handler
import handlers/index as index_handler
import handlers/upload as upload_handler
```

**Step 2: Update routing in handle_request (lines 549-578)**

Replace:

```gleam
  case segments {
    [] -> index_route(req, ctx)
    ["health"] -> handle_health_check(ctx)
    ["oauth", "authorize"] ->
      handlers.handle_oauth_authorize(req, ctx.db, ctx.oauth_config)
    ["oauth", "callback"] ->
      handlers.handle_oauth_callback(req, ctx.db, ctx.oauth_config)
    ["logout"] -> handlers.handle_logout(req, ctx.db)
    ["backfill"] -> handle_backfill_request(req, ctx)
    ["admin", "graphql"] ->
      client_graphql_handler.handle_client_graphql_request(
        req,
        ctx.db,
        ctx.admin_dids,
        ctx.jetstream_consumer,
      )
    ["graphql"] ->
      graphql_handler.handle_graphql_request(
        req,
        ctx.db,
        ctx.auth_base_url,
        ctx.plc_url,
      )
    ["graphiql"] ->
      graphiql_handler.handle_graphiql_request(req, ctx.db, ctx.oauth_config)
    ["upload"] ->
      upload_handler.handle_upload_request(req, ctx.db, ctx.oauth_config)
    // Fallback: serve SPA index.html for client-side routing
    _ -> index_route(req, ctx)
  }
```

With:

```gleam
  case segments {
    [] -> index_handler.handle()
    ["health"] -> health_handler.handle(ctx.db)
    ["oauth", "authorize"] ->
      handlers.handle_oauth_authorize(req, ctx.db, ctx.oauth_config)
    ["oauth", "callback"] ->
      handlers.handle_oauth_callback(req, ctx.db, ctx.oauth_config)
    ["logout"] -> handlers.handle_logout(req, ctx.db)
    ["backfill"] -> backfill_handler.handle(req, ctx.db, ctx.config)
    ["admin", "graphql"] ->
      client_graphql_handler.handle_client_graphql_request(
        req,
        ctx.db,
        ctx.admin_dids,
        ctx.jetstream_consumer,
      )
    ["graphql"] ->
      graphql_handler.handle_graphql_request(
        req,
        ctx.db,
        ctx.auth_base_url,
        ctx.plc_url,
      )
    ["graphiql"] ->
      graphiql_handler.handle_graphiql_request(req, ctx.db, ctx.oauth_config)
    ["upload"] ->
      upload_handler.handle_upload_request(req, ctx.db, ctx.oauth_config)
    // Fallback: serve SPA index.html for client-side routing
    _ -> index_handler.handle()
  }
```

**Step 3: Remove inline handler functions (lines 581-693)**

Delete the following functions entirely:
- `handle_backfill_request` (lines 581-657)
- `handle_health_check` (lines 659-677)
- `index_route` (lines 679-693)

**Step 4: Update WebSocket handler reference (line 512)**

Replace:

```gleam
                graphql_ws_handler.handle_websocket(
```

With:

```gleam
                graphql_ws_handler.handle_websocket(
```

(No change needed - the function name stays the same)

**Step 5: Build to verify all changes**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds with no errors

**Step 6: Commit**

```bash
git add server/src/server.gleam
git commit -m "refactor: update server.gleam to use handlers/ directory

- Update imports to use handlers/* modules
- Route all endpoints to dedicated handler modules
- Remove inline handler functions (health, backfill, index)
- Simplify server.gleam to thin routing layer

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

### Task 11: Search for other files importing moved handlers

**Files:**
- Search and potentially modify other files importing handlers

**Step 1: Search for imports of old handler modules**

Run: `cd /Users/chadmiller/code/quickslice/server && grep -r "import.*_handler" src/ --include="*.gleam" | grep -v "src/handlers/"`
Expected: Shows any files that still import the old handler modules (likely jetstream_consumer.gleam based on earlier grep)

**Step 2: Check jetstream_consumer.gleam**

Run: `grep "import.*_handler" /Users/chadmiller/code/quickslice/server/src/jetstream_consumer.gleam`
Expected: Shows which handler it imports

**Step 3: Update jetstream_consumer.gleam if needed**

If it imports `event_handler`, check if that needs updating or if it's a different module entirely. Make necessary updates.

**Step 4: Build to verify**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds with no errors

**Step 5: Commit if changes were needed**

```bash
git add server/src/jetstream_consumer.gleam
git commit -m "refactor: update imports in jetstream_consumer

Update to use new handler module locations

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

### Task 12: Final verification and testing

**Files:**
- Test: All endpoints

**Step 1: Run full build**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Clean build with no errors or warnings

**Step 2: Run tests if they exist**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: All tests pass

**Step 3: Start server and verify endpoints**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam run`
Expected: Server starts successfully

**Step 4: Test health endpoint**

Run: `curl http://localhost:8000/health`
Expected: `{"status": "healthy"}` response

**Step 5: Verify file structure**

Run: `ls -la /Users/chadmiller/code/quickslice/server/src/handlers/`
Expected: Shows all handler files:
- backfill.gleam
- client_graphql.gleam
- graphiql.gleam
- graphql.gleam
- graphql_ws.gleam
- health.gleam
- index.gleam
- settings.gleam
- upload.gleam

**Step 6: Commit final verification**

```bash
git commit --allow-empty -m "verify: handler reorganization complete

All handlers moved to handlers/ directory with consistent naming.
Server builds and runs successfully.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

## Summary

After completion, the codebase will have:

1. **New structure:**
   ```
   server/src/
   ├── handlers/
   │   ├── backfill.gleam
   │   ├── client_graphql.gleam
   │   ├── graphiql.gleam
   │   ├── graphql.gleam
   │   ├── graphql_ws.gleam
   │   ├── health.gleam
   │   ├── index.gleam
   │   ├── settings.gleam
   │   └── upload.gleam
   ├── oauth/
   │   └── handlers.gleam (unchanged)
   └── server.gleam (simplified routing)
   ```

2. **Benefits:**
   - All HTTP handlers in one discoverable location
   - Consistent naming without `_handler` suffix
   - `server.gleam` reduced by ~150 lines
   - Clear separation of concerns
   - Easier to find and modify endpoints

3. **DRY, YAGNI, TDD principles:**
   - No premature abstraction - simple file moves
   - Frequent commits for easy rollback
   - Build verification at each step

---

**Plan complete! Ready for execution using superpowers:subagent-driven-development**
