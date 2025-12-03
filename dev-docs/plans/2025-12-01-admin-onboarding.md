# Admin Onboarding Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace ADMIN_DIDS env var with database-stored admin list, with onboarding flow for first admin setup.

**Architecture:** On first boot (no admins configured), redirect all routes to `/onboarding`. User enters handle, completes OAuth, becomes first admin. Admin list stored in config table. Settings page allows admin management.

**Tech Stack:** Gleam, Lustre, SQLite, wisp HTTP framework

---

## Task 1: Add Admin DID Repository Functions

**Files:**
- Modify: `server/src/database/repositories/config.gleam`

**Step 1: Write the failing test**

No existing test file for config repository. Skip test for repository layer - will be covered by integration.

**Step 2: Add get_admin_dids function**

Add after line 83 in `server/src/database/repositories/config.gleam`:

```gleam
import gleam/list
import gleam/string

/// Get admin DIDs from config
pub fn get_admin_dids(conn: sqlight.Connection) -> List(String) {
  case get(conn, "admin_dids") {
    Ok(value) -> {
      value
      |> string.split(",")
      |> list.map(string.trim)
      |> list.filter(fn(did) { !string.is_empty(did) })
    }
    Error(_) -> []
  }
}

/// Add an admin DID to the list
pub fn add_admin_did(
  conn: sqlight.Connection,
  did: String,
) -> Result(Nil, sqlight.Error) {
  let current = get_admin_dids(conn)
  case list.contains(current, did) {
    True -> Ok(Nil)  // Already exists, idempotent
    False -> {
      let new_list = list.append(current, [did])
      let value = string.join(new_list, ",")
      set(conn, "admin_dids", value)
    }
  }
}

pub type RemoveAdminError {
  LastAdminError
  NotFoundError
  DatabaseError(sqlight.Error)
}

/// Remove an admin DID from the list
/// Returns error if trying to remove the last admin
pub fn remove_admin_did(
  conn: sqlight.Connection,
  did: String,
) -> Result(List(String), RemoveAdminError) {
  let current = get_admin_dids(conn)
  case list.contains(current, did) {
    False -> Error(NotFoundError)
    True -> {
      let new_list = list.filter(current, fn(d) { d != did })
      case new_list {
        [] -> Error(LastAdminError)
        _ -> {
          let value = string.join(new_list, ",")
          case set(conn, "admin_dids", value) {
            Ok(_) -> Ok(new_list)
            Error(err) -> Error(DatabaseError(err))
          }
        }
      }
    }
  }
}

/// Check if a DID is an admin
pub fn is_admin(conn: sqlight.Connection, did: String) -> Bool {
  let admins = get_admin_dids(conn)
  list.contains(admins, did)
}

/// Check if any admins are configured
pub fn has_admins(conn: sqlight.Connection) -> Bool {
  case get_admin_dids(conn) {
    [] -> False
    _ -> True
  }
}
```

**Step 3: Add imports at top of file**

Add to existing imports:
```gleam
import gleam/list
import gleam/string
```

**Step 4: Build and verify**

Run: `cd server && gleam build`
Expected: Build succeeds

**Step 5: Commit**

```bash
git add server/src/database/repositories/config.gleam
git commit -m "feat: add admin DID repository functions"
```

---

## Task 2: Remove ADMIN_DIDS from Server Context

**Files:**
- Modify: `server/src/server.gleam`

**Step 1: Remove admin_dids from Context type**

In `server/src/server.gleam`, find the Context type definition (around line 50-60) and remove `admin_dids: List(String)`:

Before:
```gleam
pub type Context {
  Context(
    db: sqlight.Connection,
    external_base_url: String,
    plc_url: String,
    admin_dids: List(String),
    backfill_state: Subject(backfill_state.Message),
    config: Subject(config.Message),
    jetstream_consumer: Option(Subject(jetstream_consumer.ManagerMessage)),
    did_cache: Subject(did_cache.Message),
    oauth_signing_key: Option(String),
    oauth_supported_scopes: List(String),
    oauth_loopback_mode: Bool,
  )
}
```

After:
```gleam
pub type Context {
  Context(
    db: sqlight.Connection,
    external_base_url: String,
    plc_url: String,
    backfill_state: Subject(backfill_state.Message),
    config: Subject(config.Message),
    jetstream_consumer: Option(Subject(jetstream_consumer.ManagerMessage)),
    did_cache: Subject(did_cache.Message),
    oauth_signing_key: Option(String),
    oauth_supported_scopes: List(String),
    oauth_loopback_mode: Bool,
  )
}
```

**Step 2: Remove ADMIN_DIDS env var parsing**

Find and remove lines 347-356 (the ADMIN_DIDS parsing block):

Remove:
```gleam
  // Parse ADMIN_DIDS from environment variable (comma-separated list)
  let admin_dids = case envoy.get("ADMIN_DIDS") {
    Ok(dids_str) -> {
      dids_str
      |> string.split(",")
      |> list.map(string.trim)
      |> list.filter(fn(did) { !string.is_empty(did) })
    }
    Error(_) -> []
  }
```

**Step 3: Remove admin_dids from Context construction**

Find the Context construction (around line 438-451) and remove `admin_dids: admin_dids,`:

Before:
```gleam
  let ctx =
    Context(
      db: db,
      external_base_url: external_base_url,
      plc_url: plc_url,
      admin_dids: admin_dids,
      backfill_state: backfill_state_subject,
      ...
    )
```

After:
```gleam
  let ctx =
    Context(
      db: db,
      external_base_url: external_base_url,
      plc_url: plc_url,
      backfill_state: backfill_state_subject,
      ...
    )
```

**Step 4: Build to find all places that now fail**

Run: `cd server && gleam build`
Expected: Build fails with errors showing where admin_dids is used

**Step 5: Commit partial progress**

```bash
git add server/src/server.gleam
git commit -m "refactor: remove admin_dids from server Context"
```

---

## Task 3: Update Settings Handler to Use Config Repository

**Files:**
- Modify: `server/src/handlers/settings.gleam`

**Step 1: Update Context type to remove admin_dids**

In `server/src/handlers/settings.gleam`, update the Context type:

Before:
```gleam
pub type Context {
  Context(
    db: sqlight.Connection,
    admin_dids: List(String),
    config: process.Subject(config.Message),
    jetstream_consumer: option.Option(
      process.Subject(jetstream_consumer.ManagerMessage),
    ),
    did_cache: process.Subject(did_cache.Message),
  )
}
```

After:
```gleam
pub type Context {
  Context(
    db: sqlight.Connection,
    config: process.Subject(config.Message),
    jetstream_consumer: option.Option(
      process.Subject(jetstream_consumer.ManagerMessage),
    ),
    did_cache: process.Subject(did_cache.Message),
  )
}
```

**Step 2: Update admin check to use config repository**

Change the handle function (around line 35-57):

Before:
```gleam
pub fn handle(req: wisp.Request, ctx: Context) -> wisp.Response {
  // Get current user from session
  let #(current_user, user_is_admin) = case
    session.get_current_user(req, ctx.db, ctx.did_cache)
  {
    Ok(#(did, handle, _access_token)) -> {
      let admin = is_admin(did, ctx.admin_dids)
      #(option.Some(#(did, handle)), admin)
    }
    Error(_) -> #(option.None, False)
  }
  ...
}
```

After:
```gleam
pub fn handle(req: wisp.Request, ctx: Context) -> wisp.Response {
  // Get current user from session
  let #(current_user, user_is_admin) = case
    session.get_current_user(req, ctx.db, ctx.did_cache)
  {
    Ok(#(did, handle, _access_token)) -> {
      let admin = config_repo.is_admin(ctx.db, did)
      #(option.Some(#(did, handle)), admin)
    }
    Error(_) -> #(option.None, False)
  }
  ...
}
```

**Step 3: Remove the local is_admin function**

Remove lines 158-160:
```gleam
fn is_admin(did: String, admin_dids: List(String)) -> Bool {
  list.contains(admin_dids, did)
}
```

**Step 4: Build and verify**

Run: `cd server && gleam build`
Expected: Build succeeds (or shows remaining errors to fix)

**Step 5: Commit**

```bash
git add server/src/handlers/settings.gleam
git commit -m "refactor: settings handler uses config repo for admin check"
```

---

## Task 4: Update Client GraphQL Schema Admin Checks

**Files:**
- Modify: `server/src/client_schema.gleam`

**Step 1: Add config repository import**

Add import at top:
```gleam
import database/repositories/config as config_repo
```

**Step 2: Update is_admin helper function**

Find the is_admin function (around lines 35-36) and update:

Before:
```gleam
fn is_admin(did: String, admin_dids: List(String)) -> Bool {
  list.contains(admin_dids, did)
}
```

After:
```gleam
fn is_admin(conn: sqlight.Connection, did: String) -> Bool {
  config_repo.is_admin(conn, did)
}
```

**Step 3: Update all is_admin calls**

Search for all uses of `is_admin(` and update them. Each call that was `is_admin(sess.did, admin_dids)` becomes `is_admin(conn, sess.did)`.

Key locations to update:
- Line 708: `let user_is_admin = is_admin(conn, sess.did)`
- Line 783: `case is_admin(conn, sess.did) {`
- Line 1012: `case is_admin(conn, sess.did) {`
- Line 1058: `case is_admin(conn, sess.did) {`
- Line 1258: `case is_admin(conn, sess.did) {`
- Line 1423: `case is_admin(conn, sess.did) {`
- Line 1544: `case is_admin(conn, sess.did) {`

**Step 4: Remove admin_dids parameter from functions**

Update function signatures that take admin_dids parameter:
- `query_type` function (around line 695)
- `mutation_type` function (around line 885)
- `schema` function (around line 1575)

Remove `admin_dids: List(String)` parameter from each.

**Step 5: Build and verify**

Run: `cd server && gleam build`
Expected: Build succeeds

**Step 6: Commit**

```bash
git add server/src/client_schema.gleam
git commit -m "refactor: client schema uses config repo for admin checks"
```

---

## Task 5: Update Client GraphQL Handler

**Files:**
- Modify: `server/src/handlers/client_graphql.gleam`

**Step 1: Remove admin_dids from handler parameters**

Update the Context type and all function signatures to remove `admin_dids: List(String)`.

**Step 2: Update schema calls**

Remove admin_dids from calls to `client_schema.schema(...)`.

**Step 3: Build and verify**

Run: `cd server && gleam build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add server/src/handlers/client_graphql.gleam
git commit -m "refactor: client graphql handler removes admin_dids parameter"
```

---

## Task 6: Update Server Router to Pass Correct Context

**Files:**
- Modify: `server/src/server.gleam`

**Step 1: Update handler calls to remove admin_dids**

Find all places where handlers are called with admin_dids and remove that parameter:
- Settings handler call (around line 580)
- Client GraphQL handler calls

**Step 2: Build and verify**

Run: `cd server && gleam build`
Expected: Build succeeds with no errors

**Step 3: Commit**

```bash
git add server/src/server.gleam
git commit -m "refactor: server router removes admin_dids from handler calls"
```

---

## Task 7: Create Onboarding Handler

**Files:**
- Create: `server/src/handlers/onboarding.gleam`

**Step 1: Create the onboarding handler file**

```gleam
/// Onboarding handler
/// GET /onboarding - Shows first-run setup page
import database/repositories/config as config_repo
import gleam/string_tree
import sqlight
import wisp

pub type Context {
  Context(db: sqlight.Connection, external_base_url: String)
}

/// Handle GET /onboarding
pub fn handle(req: wisp.Request, ctx: Context) -> wisp.Response {
  // If admins already exist, redirect to home
  case config_repo.has_admins(ctx.db) {
    True -> wisp.redirect("/")
    False -> render_onboarding_page(ctx.external_base_url)
  }
}

fn render_onboarding_page(external_base_url: String) -> wisp.Response {
  let html =
    string_tree.from_string(
      "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
  <title>Welcome to Quickslice</title>
  <script src=\"https://cdn.tailwindcss.com\"></script>
</head>
<body class=\"bg-zinc-900 min-h-screen flex items-center justify-center\">
  <div class=\"bg-zinc-800 rounded-lg p-8 max-w-md w-full mx-4 border border-zinc-700\">
    <h1 class=\"text-2xl font-semibold text-zinc-200 mb-2 text-center\">Welcome to Quickslice</h1>
    <p class=\"text-zinc-400 text-sm mb-6 text-center\">Enter your Bluesky handle to become the first administrator.</p>
    <form method=\"POST\" action=\"/admin/oauth/authorize\" class=\"space-y-4\">
      <input type=\"hidden\" name=\"onboarding\" value=\"true\" />
      <div>
        <label for=\"login_hint\" class=\"block text-sm text-zinc-400 mb-2\">Bluesky Handle</label>
        <input
          type=\"text\"
          id=\"login_hint\"
          name=\"login_hint\"
          placeholder=\"handle.bsky.social\"
          required
          class=\"w-full px-4 py-2 bg-zinc-900 border border-zinc-700 rounded text-zinc-300 placeholder-zinc-600 focus:outline-none focus:border-zinc-500\"
        />
      </div>
      <button
        type=\"submit\"
        class=\"w-full px-4 py-2 bg-blue-600 hover:bg-blue-500 text-white rounded transition-colors font-medium\"
      >
        Complete Setup
      </button>
    </form>
    <p class=\"text-zinc-500 text-xs mt-4 text-center\">
      You'll be redirected to Bluesky to authenticate.
    </p>
  </div>
</body>
</html>",
    )

  wisp.response(200)
  |> wisp.set_header("content-type", "text/html")
  |> wisp.set_body(wisp.Text(string_tree.to_string(html)))
}
```

**Step 2: Build and verify**

Run: `cd server && gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add server/src/handlers/onboarding.gleam
git commit -m "feat: add onboarding page handler"
```

---

## Task 8: Add Onboarding Route and Hard Block Middleware

**Files:**
- Modify: `server/src/server.gleam`

**Step 1: Add import for onboarding handler**

Add to imports:
```gleam
import handlers/onboarding as onboarding_handler
```

**Step 2: Add onboarding middleware**

Create a function to check onboarding state and wrap routes:

```gleam
import database/repositories/config as config_repo

/// Check if onboarding is required and redirect if needed
fn require_onboarding_complete(
  req: wisp.Request,
  ctx: Context,
  next: fn() -> wisp.Response,
) -> wisp.Response {
  // Skip check for these paths
  let path = wisp.path_segments(req)
  let skip_paths = [
    ["onboarding"],
    ["admin", "oauth", "authorize"],
    ["admin", "oauth", "callback"],
    ["static"],
    ["favicon.ico"],
  ]

  // Check if path starts with any skip path
  let should_skip = list.any(skip_paths, fn(skip) {
    list.take(path, list.length(skip)) == skip
  })

  case should_skip {
    True -> next()
    False -> {
      case config_repo.has_admins(ctx.db) {
        True -> next()
        False -> wisp.redirect("/onboarding")
      }
    }
  }
}
```

**Step 3: Add GET /onboarding route**

In the request handler, add the route:

```gleam
["onboarding"] -> {
  case req.method {
    http.Get -> onboarding_handler.handle(req, onboarding_handler.Context(
      db: ctx.db,
      external_base_url: ctx.external_base_url,
    ))
    _ -> wisp.method_not_allowed([http.Get])
  }
}
```

**Step 4: Wrap routes with onboarding middleware**

Apply `require_onboarding_complete` to the main route handler.

**Step 5: Build and verify**

Run: `cd server && gleam build`
Expected: Build succeeds

**Step 6: Commit**

```bash
git add server/src/server.gleam
git commit -m "feat: add onboarding route and hard block middleware"
```

---

## Task 9: Update OAuth Callback to Store First Admin

**Files:**
- Modify: `server/src/handlers/admin_oauth_callback.gleam`

**Step 1: Add config repository import**

Add import:
```gleam
import database/repositories/config as config_repo
```

**Step 2: Update process_callback to detect onboarding mode**

After the successful session creation (around line 120, after `Ok(_) -> {`), before redirecting:

```gleam
// Check if this is onboarding (no admins yet)
// If so, add this user as the first admin
case config_repo.has_admins(conn) {
  False -> {
    let _ = config_repo.add_admin_did(conn, did)
    wisp.log_info("[onboarding] First admin registered: " <> did)
  }
  True -> Nil
}
```

**Step 3: Build and verify**

Run: `cd server && gleam build`
Expected: Build succeeds

**Step 4: Test manually**

1. Delete the database to start fresh
2. Start the server
3. Visit http://localhost:8080/
4. Should redirect to /onboarding
5. Enter a handle, complete OAuth
6. Should redirect to home as logged-in admin

**Step 5: Commit**

```bash
git add server/src/handlers/admin_oauth_callback.gleam
git commit -m "feat: OAuth callback stores first admin during onboarding"
```

---

## Task 10: Add Admin Management GraphQL Mutations

**Files:**
- Modify: `server/src/client_schema.gleam`

**Step 1: Add admins query field**

In the `query_type` function, add a field to return the admin list (only for admins):

```gleam
field.new("admins")
|> field.type_(graphql.list(graphql.non_null(graphql.string())))
|> field.resolve(fn(_parent, _args, ctx) {
  case ctx.session {
    None -> graphql.resolve_value(graphql.null())
    Some(sess) -> {
      case is_admin(conn, sess.did) {
        False -> graphql.resolve_value(graphql.null())
        True -> {
          let admins = config_repo.get_admin_dids(conn)
          graphql.resolve_value(graphql.list_value(
            list.map(admins, graphql.string_value)
          ))
        }
      }
    }
  }
})
```

**Step 2: Add addAdmin mutation**

In the `mutation_type` function:

```gleam
field.new("addAdmin")
|> field.type_(graphql.list(graphql.non_null(graphql.string())))
|> field.arg(arg.new("did", graphql.non_null(graphql.string())))
|> field.resolve(fn(_parent, args, ctx) {
  case ctx.session {
    None -> graphql.resolve_error("Authentication required")
    Some(sess) -> {
      case is_admin(conn, sess.did) {
        False -> graphql.resolve_error("Admin access required")
        True -> {
          let did = case dict.get(args, "did") {
            Ok(graphql.StringValue(d)) -> d
            _ -> ""
          }
          // Validate DID format
          case string.starts_with(did, "did:") {
            False -> graphql.resolve_error("Invalid DID format")
            True -> {
              case config_repo.add_admin_did(conn, did) {
                Ok(_) -> {
                  let admins = config_repo.get_admin_dids(conn)
                  graphql.resolve_value(graphql.list_value(
                    list.map(admins, graphql.string_value)
                  ))
                }
                Error(_) -> graphql.resolve_error("Failed to add admin")
              }
            }
          }
        }
      }
    }
  }
})
```

**Step 3: Add removeAdmin mutation**

```gleam
field.new("removeAdmin")
|> field.type_(graphql.list(graphql.non_null(graphql.string())))
|> field.arg(arg.new("did", graphql.non_null(graphql.string())))
|> field.resolve(fn(_parent, args, ctx) {
  case ctx.session {
    None -> graphql.resolve_error("Authentication required")
    Some(sess) -> {
      case is_admin(conn, sess.did) {
        False -> graphql.resolve_error("Admin access required")
        True -> {
          let did = case dict.get(args, "did") {
            Ok(graphql.StringValue(d)) -> d
            _ -> ""
          }
          case config_repo.remove_admin_did(conn, did) {
            Ok(admins) -> {
              graphql.resolve_value(graphql.list_value(
                list.map(admins, graphql.string_value)
              ))
            }
            Error(config_repo.LastAdminError) ->
              graphql.resolve_error("Cannot remove the last admin")
            Error(config_repo.NotFoundError) ->
              graphql.resolve_error("DID not found in admin list")
            Error(config_repo.DatabaseError(_)) ->
              graphql.resolve_error("Database error")
          }
        }
      }
    }
  }
})
```

**Step 4: Build and verify**

Run: `cd server && gleam build`
Expected: Build succeeds

**Step 5: Commit**

```bash
git add server/src/client_schema.gleam
git commit -m "feat: add admins query and addAdmin/removeAdmin mutations"
```

---

## Task 11: Add Admin Management UI to Settings Page (Client)

**Files:**
- Modify: `client/src/pages/settings.gleam`

**Step 1: Add admin management messages**

Add to the `Msg` type:

```gleam
// Admin management messages
UpdateNewAdminDid(String)
SubmitAddAdmin
ConfirmRemoveAdmin(String)
CancelRemoveAdmin
SubmitRemoveAdmin
```

**Step 2: Add admin management state to Model**

Add to Model:

```gleam
new_admin_did: String,
remove_confirm_admin_did: Option(String),
admin_alert: Option(#(String, String)),
```

**Step 3: Update init() to initialize new fields**

Add to init:

```gleam
new_admin_did: "",
remove_confirm_admin_did: None,
admin_alert: None,
```

**Step 4: Add administrators section**

Create new function `administrators_section`:

```gleam
fn administrators_section(cache: Cache, model: Model) -> Element(Msg) {
  // Query admins from cache (will need new GraphQL query)
  html.div([attribute.class("bg-zinc-800/50 rounded p-6")], [
    html.h2([attribute.class("text-xl font-semibold text-zinc-300 mb-4")], [
      element.text("Administrators"),
    ]),
    // Admin alert
    case model.admin_alert {
      Some(#(kind, message)) -> {
        let alert_kind = case kind {
          "success" -> alert.Success
          "error" -> alert.Error
          _ -> alert.Info
        }
        alert.alert(alert_kind, message)
      }
      None -> element.none()
    },
    // Admin list (loaded from currentSession.admins or separate query)
    html.div([attribute.class("space-y-2 mb-4")], [
      // TODO: Map over admins list and render each with remove button
    ]),
    // Add admin form
    html.div([attribute.class("flex gap-2")], [
      html.input([
        attribute.type_("text"),
        attribute.class(
          "font-mono px-3 py-2 text-sm text-zinc-300 bg-zinc-900 border border-zinc-800 rounded flex-1",
        ),
        attribute.placeholder("did:plc:..."),
        attribute.value(model.new_admin_did),
        event.on_input(UpdateNewAdminDid),
      ]),
      button.button(
        disabled: string.is_empty(model.new_admin_did),
        on_click: SubmitAddAdmin,
        text: "Add",
      ),
    ]),
  ])
}
```

**Step 5: Add section to view**

In the view function, add `administrators_section(cache, model)` to the list of sections.

**Step 6: Build client**

Run: `cd client && gleam build`
Expected: Build succeeds

**Step 7: Commit**

```bash
git add client/src/pages/settings.gleam
git commit -m "feat: add administrators section to settings page"
```

---

## Task 12: Add GetAdmins GraphQL Query to Client

**Files:**
- Create: `client/src/generated/queries/get_admins.gleam`
- Modify: `client/src/quickslice_client.gleam`

**Step 1: Create GetAdmins query module**

```gleam
/// GetAdmins query - fetches the list of admin DIDs
import gleam/dynamic/decode
import gleam/list
import gleam/result

pub type GetAdminsResponse {
  GetAdminsResponse(admins: List(String))
}

pub fn parse_get_admins_response(
  json: decode.Dynamic,
) -> Result(GetAdminsResponse, List(decode.DecodeError)) {
  let decoder = {
    use admins <- decode.field(
      "admins",
      decode.optional(decode.list(decode.string)),
    )
    decode.success(GetAdminsResponse(
      admins: admins |> result.unwrap([]),
    ))
  }
  decode.run(json, decode.field("data", decoder))
}
```

**Step 2: Add admin mutations**

Create mutation handlers in the main client for `addAdmin` and `removeAdmin`.

**Step 3: Build client**

Run: `cd client && gleam build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add client/src/generated/queries/get_admins.gleam
git add client/src/quickslice_client.gleam
git commit -m "feat: add GetAdmins query and admin mutations to client"
```

---

## Task 13: Remove ADMIN_DIDS from .env.example

**Files:**
- Modify: `server/.env.example`

**Step 1: Remove ADMIN_DIDS line**

Remove the ADMIN_DIDS line and comment from .env.example.

**Step 2: Commit**

```bash
git add server/.env.example
git commit -m "chore: remove ADMIN_DIDS from .env.example"
```

---

## Task 14: Final Integration Test

**Step 1: Clean database and test full flow**

```bash
cd server
rm -f quickslice.db
gleam run
```

**Step 2: Test onboarding flow**

1. Visit http://localhost:8080/ - should redirect to /onboarding
2. Enter handle, complete OAuth
3. Should be logged in as admin, redirected to home

**Step 3: Test admin management**

1. Go to Settings page
2. Add a new admin DID
3. Try to remove yourself (should fail if only admin)
4. Verify new admin DID appears in list

**Step 4: Test non-admin access**

1. Log out
2. Log in as different (non-admin) user
3. Verify they cannot access settings
4. Verify they are not treated as admin

**Step 5: Commit final state**

```bash
git add -A
git commit -m "feat: complete admin onboarding implementation"
```

---

## Summary of Files Changed

**Modified:**
- `server/src/database/repositories/config.gleam` - Admin DID functions
- `server/src/server.gleam` - Remove admin_dids from Context, add onboarding middleware
- `server/src/handlers/settings.gleam` - Use config repo for admin check
- `server/src/handlers/admin_oauth_callback.gleam` - Store first admin during onboarding
- `server/src/handlers/client_graphql.gleam` - Remove admin_dids parameter
- `server/src/client_schema.gleam` - Admin queries/mutations, use config repo
- `client/src/pages/settings.gleam` - Add administrators section
- `client/src/quickslice_client.gleam` - Admin management handlers
- `server/.env.example` - Remove ADMIN_DIDS

**Created:**
- `server/src/handlers/onboarding.gleam` - Onboarding page handler
- `client/src/generated/queries/get_admins.gleam` - GetAdmins query
