# Remove Legacy OAuth Auto-Registration Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove OAuth auto-registration code, env vars, config storage, and UI now that OAuth flow is working.

**Architecture:** Delete OAuth credential helpers from config repo, remove oauthClientId from GraphQL schema, remove OAuth UI section, update reset logic, clean up docs.

**Tech Stack:** Gleam, Wisp, Swell (GraphQL), Lustre (client), SQLite

---

### Task 1: Remove OAuth Functions from Config Repository

**Files:**
- Modify: `server/src/database/repositories/config.gleam:86-112`

**Step 1: Remove get_oauth_credentials function**

Delete lines 86-102:
```gleam
/// Get OAuth client credentials from config table
/// Returns a tuple of (client_id, client_secret, redirect_uri) if all values exist
pub fn get_oauth_credentials(
  conn: sqlight.Connection,
) -> Result(Option(#(String, String, String)), sqlight.Error) {
  case get(conn, "oauth_client_id"), get(conn, "oauth_client_secret") {
    Ok(client_id), Ok(client_secret) -> {
      let redirect_uri = case get(conn, "oauth_redirect_uri") {
        Ok(uri) -> uri
        Error(_) -> ""
      }
      Ok(Some(#(client_id, client_secret, redirect_uri)))
    }
    Error(_), _ -> Ok(None)
    _, Error(_) -> Ok(None)
  }
}
```

**Step 2: Remove delete_oauth_credentials function**

Delete lines 104-112:
```gleam
/// Delete OAuth credentials from config table
pub fn delete_oauth_credentials(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  use _ <- result.try(delete(conn, "oauth_client_id"))
  use _ <- result.try(delete(conn, "oauth_client_secret"))
  use _ <- result.try(delete(conn, "oauth_redirect_uri"))
  Ok(Nil)
}
```

**Step 3: Build to verify**

Run: `cd server && gleam build`
Expected: Compilation errors (expected - other files still reference these functions)

**Step 4: Commit**

```bash
git add server/src/database/repositories/config.gleam
git commit -m "refactor: remove OAuth credential functions from config repo"
```

---

### Task 2: Update Settings Handler Reset Logic

**Files:**
- Modify: `server/src/handlers/settings.gleam:364-405`

**Step 1: Remove OAuth deletion from reset**

Change the reset logic from:
```gleam
    Ok("RESET") -> {
      // Delete all data
      let domain_result = config_repo.delete_domain_authority(ctx.db)
      let lexicons_result = lexicons.delete_all(ctx.db)
      let records_result = records.delete_all(ctx.db)
      let actors_result = actors.delete_all(ctx.db)
      let oauth_result = config_repo.delete_oauth_credentials(ctx.db)

      case
        domain_result,
        lexicons_result,
        records_result,
        actors_result,
        oauth_result
      {
        Ok(_), Ok(_), Ok(_), Ok(_), Ok(_) -> {
          logging.log(
            logging.Info,
            "[settings] OAuth credentials deleted, re-registration will occur on next interaction",
          )
```

To:
```gleam
    Ok("RESET") -> {
      // Delete all data
      let domain_result = config_repo.delete_domain_authority(ctx.db)
      let lexicons_result = lexicons.delete_all(ctx.db)
      let records_result = records.delete_all(ctx.db)
      let actors_result = actors.delete_all(ctx.db)

      case domain_result, lexicons_result, records_result, actors_result {
        Ok(_), Ok(_), Ok(_), Ok(_) -> {
```

**Step 2: Update error pattern match**

Change:
```gleam
        _, _, _, _, _ -> {
```

To:
```gleam
        _, _, _, _ -> {
```

**Step 3: Build to verify**

Run: `cd server && gleam build`
Expected: Compilation errors (client_schema still references oauth functions)

**Step 4: Commit**

```bash
git add server/src/handlers/settings.gleam
git commit -m "refactor: remove OAuth deletion from reset logic"
```

---

### Task 3: Update GraphQL Schema - Remove oauthClientId

**Files:**
- Modify: `server/src/client_schema.gleam:308-353, 517-531, 583-601, 738-745`

**Step 1: Remove oauthClientId field from settings_type**

In `settings_type()` function (around line 308), remove lines 336-351:
```gleam
    schema.field(
      "oauthClientId",
      schema.string_type(),
      "OAuth client ID if registered",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "oauthClientId") {
              Ok(client_id) -> Ok(client_id)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    ),
```

**Step 2: Simplify settings_to_value function**

Change from:
```gleam
fn settings_to_value(
  domain_authority: String,
  oauth_client_id: Option(String),
) -> value.Value {
  let oauth_value = case oauth_client_id {
    Some(id) -> value.String(id)
    None -> value.Null
  }

  value.Object([
    #("id", value.String("Settings:singleton")),
    #("domainAuthority", value.String(domain_authority)),
    #("oauthClientId", oauth_value),
  ])
}
```

To:
```gleam
fn settings_to_value(domain_authority: String) -> value.Value {
  value.Object([
    #("id", value.String("Settings:singleton")),
    #("domainAuthority", value.String(domain_authority)),
  ])
}
```

**Step 3: Update settings query resolver**

Change from:
```gleam
    // settings query
    schema.field(
      "settings",
      schema.non_null(settings_type()),
      "Get system settings",
      fn(_ctx) {
        let domain_authority = case config_repo.get(conn, "domain_authority") {
          Ok(authority) -> authority
          Error(_) -> ""
        }

        let oauth_client_id = case config_repo.get_oauth_credentials(conn) {
          Ok(Some(#(client_id, _secret, _uri))) -> Some(client_id)
          _ -> None
        }

        Ok(settings_to_value(domain_authority, oauth_client_id))
      },
    ),
```

To:
```gleam
    // settings query
    schema.field(
      "settings",
      schema.non_null(settings_type()),
      "Get system settings",
      fn(_ctx) {
        let domain_authority = case config_repo.get(conn, "domain_authority") {
          Ok(authority) -> authority
          Error(_) -> ""
        }

        Ok(settings_to_value(domain_authority))
      },
    ),
```

**Step 4: Update updateDomainAuthority mutation**

Change from:
```gleam
                // Fetch OAuth client ID to return complete Settings
                let oauth_client_id = case
                  config_repo.get_oauth_credentials(conn)
                {
                  Ok(Some(#(client_id, _secret, _uri))) -> Some(client_id)
                  _ -> None
                }
                Ok(settings_to_value(authority, oauth_client_id))
```

To:
```gleam
                Ok(settings_to_value(authority))
```

**Step 5: Build server**

Run: `cd server && gleam build`
Expected: PASS

**Step 6: Commit**

```bash
git add server/src/client_schema.gleam
git commit -m "refactor: remove oauthClientId from GraphQL schema"
```

---

### Task 4: Update Client Generated Query Types

**Files:**
- Modify: `client/src/generated/queries/get_settings.gleam`

**Step 1: Remove oauth_client_id from Settings type**

Change from:
```gleam
pub type Settings {
  Settings(
    id: String,
    domain_authority: String,
    oauth_client_id: Option(String),
  )
}
```

To:
```gleam
pub type Settings {
  Settings(id: String, domain_authority: String)
}
```

**Step 2: Remove from decoder**

Change from:
```gleam
pub fn settings_decoder() -> decode.Decoder(Settings) {
  use id <- decode.field("id", decode.string)
  use domain_authority <- decode.field("domainAuthority", decode.string)
  use oauth_client_id <- decode.field(
    "oauthClientId",
    decode.optional(decode.string),
  )
  decode.success(Settings(
    id: id,
    domain_authority: domain_authority,
    oauth_client_id: oauth_client_id,
  ))
}
```

To:
```gleam
pub fn settings_decoder() -> decode.Decoder(Settings) {
  use id <- decode.field("id", decode.string)
  use domain_authority <- decode.field("domainAuthority", decode.string)
  decode.success(Settings(id: id, domain_authority: domain_authority))
}
```

**Step 3: Remove from settings_to_json**

Change from:
```gleam
pub fn settings_to_json(input: Settings) -> json.Json {
  json.object([
    #("id", json.string(input.id)),
    #("domainAuthority", json.string(input.domain_authority)),
    #("oauthClientId", json.nullable(input.oauth_client_id, json.string)),
  ])
}
```

To:
```gleam
pub fn settings_to_json(input: Settings) -> json.Json {
  json.object([
    #("id", json.string(input.id)),
    #("domainAuthority", json.string(input.domain_authority)),
  ])
}
```

**Step 4: Update GraphQL query string**

Change from:
```gleam
pub fn get_settings(client: squall.Client) -> Result(Request(String), String) {
  squall.prepare_request(
    client,
    "query GetSettings {\n  settings {\n    id\n    domainAuthority\n    oauthClientId\n  }\n}",
    json.object([]),
  )
}
```

To:
```gleam
pub fn get_settings(client: squall.Client) -> Result(Request(String), String) {
  squall.prepare_request(
    client,
    "query GetSettings {\n  settings {\n    id\n    domainAuthority\n  }\n}",
    json.object([]),
  )
}
```

**Step 5: Build client**

Run: `cd client && gleam build`
Expected: Compilation errors (settings.gleam still references oauth_client_id)

**Step 6: Commit**

```bash
git add client/src/generated/queries/get_settings.gleam
git commit -m "refactor: remove oauth_client_id from generated query types"
```

---

### Task 5: Remove OAuth Section from Settings UI

**Files:**
- Modify: `client/src/pages/settings.gleam:1-360`

**Step 1: Remove oauth_section call from view**

Change line 160 from:
```gleam
            html.div([attribute.class("space-y-6")], [
              domain_authority_section(data.settings, model, is_saving),
              lexicons_section(model),
              oauth_section(data.settings),
              danger_zone_section(model),
            ])
```

To:
```gleam
            html.div([attribute.class("space-y-6")], [
              domain_authority_section(data.settings, model, is_saving),
              lexicons_section(model),
              danger_zone_section(model),
            ])
```

**Step 2: Delete oauth_section function**

Remove entire function (lines 227-271):
```gleam
fn oauth_section(settings: get_settings.Settings) -> Element(Msg) {
  html.div([attribute.class("bg-zinc-800/50 rounded p-6")], [
    // ... entire function body
  ])
}
```

**Step 3: Remove OAuth from danger zone bullet list**

Change lines 321-327 from:
```gleam
    html.ul([attribute.class("text-sm text-zinc-400 mb-4 ml-4 list-disc")], [
      html.li([], [element.text("Domain authority configuration")]),
      html.li([], [element.text("OAuth client credentials")]),
      html.li([], [element.text("All lexicon definitions")]),
      html.li([], [element.text("All indexed records")]),
      html.li([], [element.text("All actors")]),
    ]),
```

To:
```gleam
    html.ul([attribute.class("text-sm text-zinc-400 mb-4 ml-4 list-disc")], [
      html.li([], [element.text("Domain authority configuration")]),
      html.li([], [element.text("All lexicon definitions")]),
      html.li([], [element.text("All indexed records")]),
      html.li([], [element.text("All actors")]),
    ]),
```

**Step 4: Update docstring**

Remove `oauthClientId` from the GraphQL query examples in the module docstring (lines 1-35).

**Step 5: Build client**

Run: `cd client && gleam build`
Expected: PASS

**Step 6: Commit**

```bash
git add client/src/pages/settings.gleam
git commit -m "refactor: remove OAuth settings section from UI"
```

---

### Task 6: Clean Up Environment Variables

**Files:**
- Modify: `server/.env.example`

**Step 1: Remove OAuth auto-registration section**

Remove lines 9-23:
```
# OAuth Auto-Registration (recommended)
# When enabled, the server will automatically register as an OAuth client with your AIP server
# on startup if no credentials exist. Credentials are stored in the database config table.
ENABLE_OAUTH_AUTO_REGISTER=true

# OAuth Client Credentials (manual configuration - optional)
# If you set these environment variables, they will override auto-registered credentials.
# You can leave these commented out if using auto-registration.
# OAUTH_CLIENT_ID=your_client_id
# OAUTH_CLIENT_SECRET=your_client_secret

# External Base URL - used to construct the OAuth redirect URI
# The redirect URI will be: ${EXTERNAL_BASE_URL}/oauth/callback
# Defaults to http://localhost:8000 if not set
# EXTERNAL_BASE_URL=http://localhost:8000
```

Also remove line 28:
```
# Note: OAuth uses AIP_BASE_URL as the authorization server URL
```

**Step 2: Commit**

```bash
git add server/.env.example
git commit -m "refactor: remove OAuth auto-registration env vars"
```

---

### Task 7: Update Deployment Documentation

**Files:**
- Modify: `docs/deployment.md`

**Step 1: Remove OAuth env vars from table**

Remove lines 14-17 from the environment variables table:
```
| `ENABLE_OAUTH_AUTO_REGISTER` | Optional | `false` | Enable automatic OAuth client registration with AIP on server boot |
| `OAUTH_CLIENT_ID` | Optional | - | OAuth client ID (auto-registered if `ENABLE_OAUTH_AUTO_REGISTER=true`) |
| `OAUTH_CLIENT_SECRET` | Optional | - | OAuth client secret (auto-registered if `ENABLE_OAUTH_AUTO_REGISTER=true`) |
| `EXTERNAL_BASE_URL` | Optional | `http://localhost:8000` | Base URL of your application (used for OAuth redirect: `${EXTERNAL_BASE_URL}/oauth/callback`) |
```

**Step 2: Remove OAuth Configuration section**

Remove lines 30-59 (entire "### OAuth Configuration" section including Option A and Option B).

**Step 3: Remove OAuth from Fly.io secrets**

Remove lines 119-130:
```bash
# OAuth configuration (choose one approach):

# Option A: Auto-registration (recommended)
# Automatically registers OAuth client with AIP on startup
fly secrets set ENABLE_OAUTH_AUTO_REGISTER=true
fly secrets set AIP_BASE_URL=https://your-aip-server.com

# Option B: Manual configuration
# Use pre-existing OAuth client credentials
fly secrets set OAUTH_CLIENT_ID=your_client_id
fly secrets set OAUTH_CLIENT_SECRET=your_client_secret
```

**Step 4: Remove OAuth from Railway instructions**

Remove lines 166-175:
```
# OAuth - Option A: Auto-registration (recommended)
ENABLE_OAUTH_AUTO_REGISTER=true
AIP_BASE_URL=https://your-aip-server.com
EXTERNAL_BASE_URL=https://your-app.up.railway.app

# OAuth - Option B: Manual configuration
# OAUTH_CLIENT_ID=your_client_id
# OAUTH_CLIENT_SECRET=your_client_secret
# EXTERNAL_BASE_URL=https://your-app.up.railway.app
```

**Step 5: Remove OAuth from Docker Compose**

Remove lines 235-241:
```yaml
      # OAuth auto-registration (recommended)
      - ENABLE_OAUTH_AUTO_REGISTER=${ENABLE_OAUTH_AUTO_REGISTER:-false}
      - AIP_BASE_URL=${AIP_BASE_URL}
      # Or use manual OAuth configuration
      # - OAUTH_CLIENT_ID=${OAUTH_CLIENT_ID}
      # - OAUTH_CLIENT_SECRET=${OAUTH_CLIENT_SECRET}
```

Remove lines 258-265:
```bash
# OAuth auto-registration (recommended)
ENABLE_OAUTH_AUTO_REGISTER=true
AIP_BASE_URL=https://your-aip-server.com

# Or use manual OAuth configuration
# OAUTH_CLIENT_ID=your_client_id
# OAUTH_CLIENT_SECRET=your_client_secret
```

**Step 6: Remove OAuth security note**

Remove line 341:
```
5. **Keep OAuth secrets private** - Use environment variables for client secrets
```

**Step 7: Commit**

```bash
git add docs/deployment.md
git commit -m "docs: remove OAuth auto-registration instructions"
```

---

### Task 8: Final Verification

**Step 1: Build server**

Run: `cd server && gleam build`
Expected: PASS

**Step 2: Build client**

Run: `cd client && gleam build`
Expected: PASS

**Step 3: Run server (manual test)**

Run: `cd server && gleam run`
Expected: Server starts without errors

**Step 4: Test settings page**

Navigate to `/settings` in browser
Expected:
- No OAuth section visible
- Domain authority section works
- Lexicons section works
- Danger zone reset works (without OAuth mention)

**Step 5: Final commit (if any cleanup needed)**

```bash
git add -A
git commit -m "chore: OAuth auto-registration cleanup complete"
```
