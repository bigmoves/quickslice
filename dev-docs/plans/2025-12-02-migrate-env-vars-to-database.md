# Migrate Environment Variables to Database Configuration

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove config cache actor and migrate four environment variables (`RELAY_URL`, `PLC_DIRECTORY_URL`, `JETSTREAM_URL`, `OAUTH_SUPPORTED_SCOPES`) to database-backed configuration accessible via GraphQL API and UI.

**Architecture:** Direct database queries via `config_repo.get()` replace the config cache actor pattern. Settings are stored in the `config` table with hardcoded defaults. Changes take effect immediately without server restart (except Jetstream consumer auto-restarts when URL changes).

**Tech Stack:** Gleam, SQLite (via sqlight), GraphQL (via swell), Lustre (frontend)

---

## Task 1: Add Database Repository Functions

**Files:**
- Modify: `server/src/database/repositories/config.gleam:172` (add after existing functions)

**Step 1: Add relay_url getter function**

```gleam
// Add after line 172

// ===== External Services Configuration =====

/// Get relay URL from config, with hardcoded default
pub fn get_relay_url(conn: sqlight.Connection) -> String {
  case get(conn, "relay_url") {
    Ok(url) -> url
    Error(_) -> "https://relay1.us-west.bsky.network"
  }
}
```

**Step 2: Add plc_directory_url getter function**

```gleam
/// Get PLC directory URL from config, with hardcoded default
pub fn get_plc_directory_url(conn: sqlight.Connection) -> String {
  case get(conn, "plc_directory_url") {
    Ok(url) -> url
    Error(_) -> "https://plc.directory"
  }
}
```

**Step 3: Add jetstream_url getter function**

```gleam
/// Get Jetstream URL from config, with hardcoded default
pub fn get_jetstream_url(conn: sqlight.Connection) -> String {
  case get(conn, "jetstream_url") {
    Ok(url) -> url
    Error(_) -> "wss://jetstream2.us-west.bsky.network/subscribe"
  }
}
```

**Step 4: Add oauth_supported_scopes getter functions**

```gleam
/// Get OAuth supported scopes from config, with hardcoded default
/// Returns space-separated string
pub fn get_oauth_supported_scopes(conn: sqlight.Connection) -> String {
  case get(conn, "oauth_supported_scopes") {
    Ok(scopes) -> scopes
    Error(_) -> "atproto transition:generic"
  }
}

/// Parse OAuth supported scopes into List(String)
pub fn get_oauth_supported_scopes_list(conn: sqlight.Connection) -> List(String) {
  let scopes_str = get_oauth_supported_scopes(conn)
  scopes_str
  |> string.split(" ")
  |> list.map(string.trim)
  |> list.filter(fn(s) { !string.is_empty(s) })
}
```

**Step 5: Add setter functions**

```gleam
/// Set relay URL
pub fn set_relay_url(
  conn: sqlight.Connection,
  url: String,
) -> Result(Nil, sqlight.Error) {
  set(conn, "relay_url", url)
}

/// Set PLC directory URL
pub fn set_plc_directory_url(
  conn: sqlight.Connection,
  url: String,
) -> Result(Nil, sqlight.Error) {
  set(conn, "plc_directory_url", url)
}

/// Set Jetstream URL
pub fn set_jetstream_url(
  conn: sqlight.Connection,
  url: String,
) -> Result(Nil, sqlight.Error) {
  set(conn, "jetstream_url", url)
}

/// Set OAuth supported scopes (space-separated string)
pub fn set_oauth_supported_scopes(
  conn: sqlight.Connection,
  scopes: String,
) -> Result(Nil, sqlight.Error) {
  set(conn, "oauth_supported_scopes", scopes)
}
```

**Step 6: Build to verify compilation**

```bash
cd server
gleam build
```

Expected: Success with no errors

**Step 7: Commit**

```bash
git add server/src/database/repositories/config.gleam
git commit -m "feat(config): add repository functions for external services config"
```

---

## Task 2: Update server.gleam - Remove Config Actor and Context Fields

**Files:**
- Modify: `server/src/server.gleam:5` (remove import)
- Modify: `server/src/server.gleam:52-67` (Context type)
- Modify: `server/src/server.gleam:138-151` (run_backfill_command)
- Modify: `server/src/server.gleam:346-398` (remove env var reading)
- Modify: `server/src/server.gleam:426-438` (Context construction)
- Modify: `server/src/server.gleam:467` (WebSocket handler)

**Step 1: Add config_repo import**

Add after other imports:
```gleam
import database/repositories/config as config_repo
```

**Step 2: Remove config import**

Delete line 5:
```gleam
import config  // DELETE THIS LINE
```

**Step 3: Update Context type**

In the Context type (lines 52-67), remove these three fields:
```gleam
pub type Context {
  Context(
    db: sqlight.Connection,
    external_base_url: String,
    // REMOVE: plc_url: String,
    backfill_state: process.Subject(backfill_state.Message),
    // REMOVE: config: process.Subject(config.Message),
    jetstream_consumer: option.Option(
      process.Subject(jetstream_consumer.ManagerMessage),
    ),
    did_cache: process.Subject(did_cache.Message),
    oauth_signing_key: option.Option(String),
    // REMOVE: oauth_supported_scopes: List(String),
    oauth_loopback_mode: Bool,
  )
}
```

**Step 4: Update run_backfill_command function**

Replace lines 138-151:
```gleam
// BEFORE:
// Start config cache actor
let assert Ok(config_subject) = config.start(db)

// Get domain authority from config
let domain_authority = case config.get_domain_authority(config_subject) {
  option.Some(authority) -> authority
  option.None -> {
    logging.log(
      logging.Warning,
      "No domain_authority configured. All collections will be treated as external.",
    )
    ""
  }
}

// AFTER:
// Get domain authority from database
let domain_authority = case config_repo.get(db, "domain_authority") {
  Ok(authority) -> authority
  Error(_) -> {
    logging.log(
      logging.Warning,
      "No domain_authority configured. All collections will be treated as external.",
    )
    ""
  }
}
```

**Step 5: Delete environment variable reading code**

Delete lines 346-350 (PLC_DIRECTORY_URL):
```gleam
// DELETE:
let plc_url = case envoy.get("PLC_DIRECTORY_URL") {
  Ok(url) -> url
  Error(_) -> "https://plc.directory"
}
```

Delete lines 372-398 (OAUTH_SUPPORTED_SCOPES):
```gleam
// DELETE all OAUTH_SUPPORTED_SCOPES env var reading and validation code
```

Delete lines 418-420 (config.start()):
```gleam
// DELETE:
let assert Ok(config_subject) = config.start(db)
logging.log(logging.Info, "[server] Config cache actor initialized")
```

**Step 6: Update Context construction**

In the Context constructor (lines 426-438), remove three fields:
```gleam
let ctx =
  Context(
    db: db,
    external_base_url: external_base_url,
    // REMOVE: plc_url: plc_url,
    backfill_state: backfill_state_subject,
    // REMOVE: config: config_subject,
    jetstream_consumer: jetstream_subject,
    did_cache: did_cache_subject,
    oauth_signing_key: oauth_signing_key,
    // REMOVE: oauth_supported_scopes: oauth_supported_scopes,
    oauth_loopback_mode: oauth_loopback_mode,
  )
```

**Step 7: Update WebSocket handler domain_authority usage**

Replace line 467:
```gleam
// BEFORE:
let domain_authority = case config.get_domain_authority(ctx.config) {
  option.Some(authority) -> authority
  option.None -> ""
}

// AFTER:
let domain_authority = case config_repo.get(ctx.db, "domain_authority") {
  Ok(authority) -> authority
  Error(_) -> ""
}
```

**Step 8: Update build_loopback_client_id calls (lines 526-531, 546-551, 622-627, 643-648)**

Find all usages of `ctx.oauth_supported_scopes` and replace with:
```gleam
// BEFORE:
string.join(ctx.oauth_supported_scopes, " ")

// AFTER:
config_repo.get_oauth_supported_scopes(ctx.db)
```

**Step 9: Update handler calls passing oauth_supported_scopes**

Find lines 569, 541-542, and update:
```gleam
// BEFORE:
ctx.oauth_supported_scopes,

// AFTER:
config_repo.get_oauth_supported_scopes_list(ctx.db),
```

**Step 10: Update oauth metadata handler call (line 600-602)**

```gleam
// BEFORE:
oauth_metadata_handler.handle(
  ctx.external_base_url,
  ctx.oauth_supported_scopes,
)

// AFTER:
oauth_metadata_handler.handle(
  ctx.external_base_url,
  config_repo.get_oauth_supported_scopes_list(ctx.db),
)
```

**Step 11: Update oauth_client_metadata_handler call (line 605-615)**

```gleam
// BEFORE:
string.join(ctx.oauth_supported_scopes, " "),

// AFTER:
config_repo.get_oauth_supported_scopes(ctx.db),
```

**Step 12: Update MCP handler context (line 594)**

```gleam
// BEFORE:
supported_scopes: ctx.oauth_supported_scopes,

// AFTER:
supported_scopes: config_repo.get_oauth_supported_scopes_list(ctx.db),
```

**Step 13: Build to verify compilation**

```bash
cd server
gleam build
```

Expected: Success (may have errors in other files we'll fix next)

**Step 14: Commit**

```bash
git add server/src/server.gleam
git commit -m "refactor(server): remove config actor, update Context type"
```

---

## Task 3: Delete Config Cache Actor File

**Files:**
- Delete: `server/src/config.gleam`

**Step 1: Delete the file**

```bash
rm server/src/config.gleam
```

**Step 2: Build to verify**

```bash
cd server
gleam build
```

Expected: Build errors in files still importing config (we'll fix these next)

**Step 3: Commit**

```bash
git add server/src/config.gleam
git commit -m "refactor: remove config cache actor"
```

---

## Task 4: Update backfill.gleam

**Files:**
- Modify: `server/src/backfill.gleam:63-66` (PLC URL)
- Modify: `server/src/backfill.gleam:1189-1192` (Relay URL)

**Step 1: Add import**

Add at top with other imports:
```gleam
import database/repositories/config as config_repo
```

**Step 2: Update PLC URL usage (line 63-66)**

```gleam
// BEFORE:
let plc_url = case envoy.get("PLC_DIRECTORY_URL") {
  Ok(url) -> url
  Error(_) -> "https://plc.directory"
}

// AFTER:
let plc_url = config_repo.get_plc_directory_url(db)
```

**Step 3: Update Relay URL usage (line 1189-1192)**

```gleam
// BEFORE:
let relay_url = case envoy.get("RELAY_URL") {
  Ok(url) -> url
  Error(_) -> "https://relay1.us-west.bsky.network"
}

// AFTER:
let relay_url = config_repo.get_relay_url(db)
```

**Step 4: Build to verify**

```bash
cd server
gleam build
```

Expected: Success

**Step 5: Commit**

```bash
git add server/src/backfill.gleam
git commit -m "refactor(backfill): read URLs from database config"
```

---

## Task 5: Update jetstream_consumer.gleam

**Files:**
- Modify: `server/src/jetstream_consumer.gleam:2` (remove import)
- Modify: `server/src/jetstream_consumer.gleam:563-566` (PLC URL)
- Modify: `server/src/jetstream_consumer.gleam:633-636` (Jetstream URL)

**Step 1: Remove config import**

Delete line 2:
```gleam
import config  // DELETE
```

**Step 2: Add config_repo import**

Add with other imports:
```gleam
import database/repositories/config as config_repo
```

**Step 3: Update PLC URL usage (line 563-566)**

```gleam
// BEFORE:
let plc_url = case envoy.get("PLC_DIRECTORY_URL") {
  Ok(url) -> url
  Error(_) -> "https://plc.directory"
}

// AFTER:
let plc_url = config_repo.get_plc_directory_url(db)
```

**Step 4: Update Jetstream URL usage (line 633-636)**

```gleam
// BEFORE:
let jetstream_url = case envoy.get("JETSTREAM_URL") {
  Ok(url) -> url
  Error(_) -> "wss://jetstream2.us-west.bsky.network/subscribe"
}

// AFTER:
let jetstream_url = config_repo.get_jetstream_url(db)
```

**Step 5: Remove config.start() call (line 569)**

Delete:
```gleam
let assert Ok(config_subject) = config.start(db)
```

**Step 6: Update domain_authority usage (line 572-576)**

```gleam
// BEFORE:
let domain_authority = case config.get_domain_authority(config_subject) {
  option.Some(authority) -> authority
  option.None -> ""
}

// AFTER:
let domain_authority = case config_repo.get(db, "domain_authority") {
  Ok(authority) -> authority
  Error(_) -> ""
}
```

**Step 7: Build to verify**

```bash
cd server
gleam build
```

Expected: Success

**Step 8: Commit**

```bash
git add server/src/jetstream_consumer.gleam
git commit -m "refactor(jetstream): read config from database"
```

---

## Task 6: Update handlers/settings.gleam

**Files:**
- Modify: `server/src/handlers/settings.gleam:2` (remove import)
- Modify: `server/src/handlers/settings.gleam:23-32` (Context type)
- Modify: `server/src/handlers/settings.gleam:96-102` (set domain authority)
- Modify: `server/src/handlers/settings.gleam:367` (remove reload)

**Step 1: Remove config import**

Delete line 2:
```gleam
import config  // DELETE
```

**Step 2: Update Context type**

Remove config field from Context (lines 23-32):
```gleam
pub type Context {
  Context(
    db: sqlight.Connection,
    // REMOVE: config: process.Subject(config.Message),
    jetstream_consumer: option.Option(
      process.Subject(jetstream_consumer.ManagerMessage),
    ),
    did_cache: process.Subject(did_cache.Message),
  )
}
```

**Step 3: Update set domain authority call (lines 96-102)**

```gleam
// BEFORE:
case
  config.set_domain_authority(
    ctx.config,
    ctx.db,
    domain_authority,
  )
{
  Ok(_) -> { /* ... */ }
  Error(_) -> { /* ... */ }
}

// AFTER:
case config_repo.set(ctx.db, "domain_authority", domain_authority) {
  Ok(_) -> { /* ... */ }
  Error(_) -> { /* ... */ }
}
```

**Step 4: Remove config reload call (line 367)**

Delete:
```gleam
let _ = config.reload(ctx.config, ctx.db)
```

**Step 5: Build to verify**

```bash
cd server
gleam build
```

Expected: Success

**Step 6: Commit**

```bash
git add server/src/handlers/settings.gleam
git commit -m "refactor(settings): remove config actor usage"
```

---

## Task 7: Update client_schema.gleam - Settings Type

**Files:**
- Modify: `server/src/client_schema.gleam:359-404` (settings_type function)
- Modify: `server/src/client_schema.gleam:686-695` (settings_to_value helper)
- Modify: `server/src/client_schema.gleam:1292-1295` (PLC URL in backfill mutation)

**Step 1: Update PLC URL in backfill mutation (line 1292-1295)**

```gleam
// BEFORE:
let plc_url = case envoy.get("PLC_DIRECTORY_URL") {
  Ok(url) -> url
  Error(_) -> "https://plc.directory"
}

// AFTER:
let plc_url = config_repo.get_plc_directory_url(conn)
```

**Step 2: Add relayUrl field to settings_type (after line 403)**

```gleam
schema.field(
  "relayUrl",
  schema.non_null(schema.string_type()),
  "AT Protocol relay URL for backfill operations",
  fn(ctx) {
    case ctx.data {
      Some(value.Object(fields)) -> {
        case list.key_find(fields, "relayUrl") {
          Ok(url) -> Ok(url)
          Error(_) -> Ok(value.Null)
        }
      }
      _ -> Ok(value.Null)
    }
  },
),
```

**Step 3: Add plcDirectoryUrl field**

```gleam
schema.field(
  "plcDirectoryUrl",
  schema.non_null(schema.string_type()),
  "PLC directory URL for DID resolution",
  fn(ctx) {
    case ctx.data {
      Some(value.Object(fields)) -> {
        case list.key_find(fields, "plcDirectoryUrl") {
          Ok(url) -> Ok(url)
          Error(_) -> Ok(value.Null)
        }
      }
      _ -> Ok(value.Null)
    }
  },
),
```

**Step 4: Add jetstreamUrl field**

```gleam
schema.field(
  "jetstreamUrl",
  schema.non_null(schema.string_type()),
  "Jetstream WebSocket endpoint for real-time indexing",
  fn(ctx) {
    case ctx.data {
      Some(value.Object(fields)) -> {
        case list.key_find(fields, "jetstreamUrl") {
          Ok(url) -> Ok(url)
          Error(_) -> Ok(value.Null)
        }
      }
      _ -> Ok(value.Null)
    }
  },
),
```

**Step 5: Add oauthSupportedScopes field**

```gleam
schema.field(
  "oauthSupportedScopes",
  schema.non_null(schema.string_type()),
  "Space-separated OAuth scopes supported by this server",
  fn(ctx) {
    case ctx.data {
      Some(value.Object(fields)) -> {
        case list.key_find(fields, "oauthSupportedScopes") {
          Ok(scopes) -> Ok(scopes)
          Error(_) -> Ok(value.Null)
        }
      }
      _ -> Ok(value.Null)
    }
  },
),
```

**Step 6: Update settings_to_value function signature (lines 686-695)**

```gleam
// BEFORE:
fn settings_to_value(
  domain_authority: String,
  admin_dids: List(String),
) -> value.Value {
  value.Object([
    #("id", value.String("Settings:singleton")),
    #("domainAuthority", value.String(domain_authority)),
    #("adminDids", value.List(list.map(admin_dids, value.String))),
  ])
}

// AFTER:
fn settings_to_value(
  domain_authority: String,
  admin_dids: List(String),
  relay_url: String,
  plc_directory_url: String,
  jetstream_url: String,
  oauth_supported_scopes: String,
) -> value.Value {
  value.Object([
    #("id", value.String("Settings:singleton")),
    #("domainAuthority", value.String(domain_authority)),
    #("adminDids", value.List(list.map(admin_dids, value.String))),
    #("relayUrl", value.String(relay_url)),
    #("plcDirectoryUrl", value.String(plc_directory_url)),
    #("jetstreamUrl", value.String(jetstream_url)),
    #("oauthSupportedScopes", value.String(oauth_supported_scopes)),
  ])
}
```

**Step 7: Build to verify**

```bash
cd server
gleam build
```

Expected: Build errors in settings query/mutation (we'll fix next)

**Step 8: Commit**

```bash
git add server/src/client_schema.gleam
git commit -m "feat(graphql): add four external service fields to Settings type"
```

---

## Task 8: Update Settings Query Resolver

**Files:**
- Modify: `server/src/client_schema.gleam:770-784` (settings query)

**Step 1: Update settings query resolver**

Replace the query resolver (lines 770-784):
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
    let admin_dids = config_repo.get_admin_dids(conn)
    let relay_url = config_repo.get_relay_url(conn)
    let plc_directory_url = config_repo.get_plc_directory_url(conn)
    let jetstream_url = config_repo.get_jetstream_url(conn)
    let oauth_supported_scopes = config_repo.get_oauth_supported_scopes(conn)

    Ok(settings_to_value(
      domain_authority,
      admin_dids,
      relay_url,
      plc_directory_url,
      jetstream_url,
      oauth_supported_scopes,
    ))
  },
),
```

**Step 2: Build to verify**

```bash
cd server
gleam build
```

Expected: Success

**Step 3: Commit**

```bash
git add server/src/client_schema.gleam
git commit -m "feat(graphql): return external service URLs in settings query"
```

---

## Task 9: Update Settings Mutation - Add Arguments

**Files:**
- Modify: `server/src/client_schema.gleam:929-1051` (updateSettings mutation)

**Step 1: Add relayUrl argument (after adminDids argument)**

Find the arguments section and add:
```gleam
schema.argument(
  "relayUrl",
  schema.string_type(),
  "New relay URL (optional)",
  None,
),
```

**Step 2: Add plcDirectoryUrl argument**

```gleam
schema.argument(
  "plcDirectoryUrl",
  schema.string_type(),
  "New PLC directory URL (optional)",
  None,
),
```

**Step 3: Add jetstreamUrl argument**

```gleam
schema.argument(
  "jetstreamUrl",
  schema.string_type(),
  "New Jetstream URL (optional)",
  None,
),
```

**Step 4: Add oauthSupportedScopes argument**

```gleam
schema.argument(
  "oauthSupportedScopes",
  schema.string_type(),
  "New OAuth supported scopes space-separated (optional)",
  None,
),
```

**Step 5: Build to verify arguments added**

```bash
cd server
gleam build
```

Expected: Success

**Step 6: Commit**

```bash
git add server/src/client_schema.gleam
git commit -m "feat(graphql): add external service arguments to updateSettings"
```

---

## Task 10: Update Settings Mutation - Add Update Logic

**Files:**
- Modify: `server/src/client_schema.gleam:929-1051` (updateSettings mutation resolver)

**Step 1: Add relay URL update logic**

After the admin DIDs update logic (around line 1028), add:
```gleam
// Update relay URL if provided
case schema.get_argument(ctx, "relayUrl") {
  Some(value.String(url)) if url != "" -> {
    case config_repo.set_relay_url(conn, url) {
      Ok(_) -> Nil
      Error(_) -> {
        logging.log(logging.Error, "[updateSettings] Failed to update relay URL")
      }
    }
  }
  _ -> Nil
}
```

**Step 2: Add PLC directory URL update logic**

```gleam
// Update PLC directory URL if provided
case schema.get_argument(ctx, "plcDirectoryUrl") {
  Some(value.String(url)) if url != "" -> {
    case config_repo.set_plc_directory_url(conn, url) {
      Ok(_) -> Nil
      Error(_) -> {
        logging.log(logging.Error, "[updateSettings] Failed to update PLC directory URL")
      }
    }
  }
  _ -> Nil
}
```

**Step 3: Add Jetstream URL update logic with restart**

```gleam
// Update Jetstream URL if provided and restart consumer
let jetstream_url_changed = case schema.get_argument(ctx, "jetstreamUrl") {
  Some(value.String(url)) if url != "" -> {
    case config_repo.set_jetstream_url(conn, url) {
      Ok(_) -> True
      Error(_) -> {
        logging.log(logging.Error, "[updateSettings] Failed to update Jetstream URL")
        False
      }
    }
  }
  _ -> False
}

// If Jetstream URL changed, restart consumer
case jetstream_url_changed {
  True -> {
    case jetstream_subject {
      Some(consumer) -> {
        logging.log(
          logging.Info,
          "[updateSettings] Restarting Jetstream consumer due to URL change",
        )
        case jetstream_consumer.restart(consumer) {
          Ok(_) -> logging.log(logging.Info, "[updateSettings] Jetstream consumer restarted")
          Error(err) -> logging.log(logging.Error, "[updateSettings] Failed to restart Jetstream: " <> err)
        }
      }
      None -> Nil
    }
  }
  False -> Nil
}
```

**Step 4: Add OAuth scopes update logic with validation**

```gleam
// Update OAuth supported scopes if provided (with validation)
case schema.get_argument(ctx, "oauthSupportedScopes") {
  Some(value.String(scopes)) if scopes != "" -> {
    case scope_validator.validate_scope_format(scopes) {
      Ok(_) -> {
        case config_repo.set_oauth_supported_scopes(conn, scopes) {
          Ok(_) -> Nil
          Error(_) -> {
            logging.log(logging.Error, "[updateSettings] Failed to update OAuth scopes")
          }
        }
      }
      Error(e) -> {
        logging.log(
          logging.Error,
          "[updateSettings] Invalid OAuth scopes format: " <> error.error_description(e),
        )
      }
    }
  }
  _ -> Nil
}
```

**Step 5: Update final settings return**

Find the final return statement (around line 1034-1042) and update:
```gleam
// Return updated settings
let final_authority = case config_repo.get(conn, "domain_authority") {
  Ok(a) -> a
  Error(_) -> ""
}
let final_admin_dids = config_repo.get_admin_dids(conn)
let final_relay_url = config_repo.get_relay_url(conn)
let final_plc_directory_url = config_repo.get_plc_directory_url(conn)
let final_jetstream_url = config_repo.get_jetstream_url(conn)
let final_oauth_scopes = config_repo.get_oauth_supported_scopes(conn)

Ok(settings_to_value(
  final_authority,
  final_admin_dids,
  final_relay_url,
  final_plc_directory_url,
  final_jetstream_url,
  final_oauth_scopes,
))
```

**Step 6: Build to verify**

```bash
cd server
gleam build
```

Expected: Success

**Step 7: Commit**

```bash
git add server/src/client_schema.gleam
git commit -m "feat(graphql): implement external service updates in mutation"
```

---

## Task 11: Regenerate Frontend GraphQL Queries

**Prerequisites:**
- Server must be running with updated GraphQL schema
- Backend tasks 1-10 must be complete

**Step 1: Start the server**

```bash
cd server
gleam run
```

Expected: Server starts on http://localhost:8080

**Step 2: Regenerate GraphQL queries from schema**

In a new terminal:
```bash
cd client
gleam run -m squall unstable-cache http://localhost:8080/admin/graphql
```

Expected: Squall generates updated query types in `client/src/generated/queries/`

**Step 3: Verify generated Settings type**

Check `client/src/generated/queries/get_settings.gleam`:

```gleam
pub type Settings {
  Settings(
    id: String,
    domain_authority: String,
    admin_dids: List(String),
    relay_url: String,
    plc_directory_url: String,
    jetstream_url: String,
    oauth_supported_scopes: String,
  )
}
```

**Step 2: Update GraphQL query string**

```gleam
pub fn get_settings(client: squall.Client) -> Result(Request(String), String) {
  squall.prepare_request(
    client,
    "query GetSettings {
      settings {
        id
        domainAuthority
        adminDids
        relayUrl
        plcDirectoryUrl
        jetstreamUrl
        oauthSupportedScopes
      }
    }",
    json.object([]),
  )
}
```

**Step 3: Update parser function**

Add fields to decoder:
```gleam
use relay_url <- decode.field("relayUrl", decode.string)
use plc_directory_url <- decode.field("plcDirectoryUrl", decode.string)
use jetstream_url <- decode.field("jetstreamUrl", decode.string)
use oauth_supported_scopes <- decode.field("oauthSupportedScopes", decode.string)
```

And constructor:
```gleam
decode.success(Settings(
  id: id,
  domain_authority: domain_authority,
  admin_dids: admin_dids,
  relay_url: relay_url,
  plc_directory_url: plc_directory_url,
  jetstream_url: jetstream_url,
  oauth_supported_scopes: oauth_supported_scopes,
))
```

**Step 4: Verify update_settings mutation was also regenerated**

Check `client/src/generated/queries/update_settings.gleam` includes new parameters.

**Step 5: Build to verify**

```bash
cd client
gleam build
```

Expected: Success

**Step 6: Commit**

```bash
git add client/src/generated/queries/
git commit -m "feat(client): regenerate GraphQL queries with external services"
```

---

## Task 12: Update Frontend - Settings Page Model

**Files:**
- Modify: `client/src/pages/settings.gleam:98-128` (Msg type)
- Modify: `client/src/pages/settings.gleam:130-154` (Model type)
- Modify: `client/src/pages/settings.gleam:180-202` (init function)

**Step 1: Add messages to Msg type (after line 128)**

```gleam
// External services configuration messages
UpdateRelayUrlInput(String)
SubmitRelayUrl
UpdatePlcDirectoryUrlInput(String)
SubmitPlcDirectoryUrl
UpdateJetstreamUrlInput(String)
SubmitJetstreamUrl
UpdateOAuthSupportedScopesInput(String)
SubmitOAuthSupportedScopes
```

**Step 2: Add fields to Model type (after line 154)**

```gleam
// External services configuration state
relay_url_input: String,
plc_directory_url_input: String,
jetstream_url_input: String,
oauth_supported_scopes_input: String,
services_alert: Option(#(String, String)),
```

**Step 3: Update init function (after line 202)**

```gleam
relay_url_input: "",
plc_directory_url_input: "",
jetstream_url_input: "",
oauth_supported_scopes_input: "",
services_alert: None,
```

**Step 4: Add helper functions after clear_admin_alert**

```gleam
pub fn set_services_alert(model: Model, kind: String, message: String) -> Model {
  Model(..model, services_alert: Some(#(kind, message)))
}

pub fn clear_services_alert(model: Model) -> Model {
  Model(..model, services_alert: None)
}
```

**Step 5: Build to verify**

```bash
cd client
gleam build
```

Expected: Success

**Step 6: Commit**

```bash
git add client/src/pages/settings.gleam
git commit -m "feat(settings): add model fields for external services UI"
```

---

## Task 13: Update Frontend - External Services View Section

**Files:**
- Modify: `client/src/pages/settings.gleam` (add new function after admin_management_section)

**Step 1: Add external_services_section function**

Add after admin_management_section function (around line 967):
```gleam
fn external_services_section(
  settings: get_settings.Settings,
  model: Model,
  is_saving: Bool,
) -> Element(Msg) {
  html.div(
    [attribute.class("bg-zinc-800/50 rounded-lg p-6 border border-zinc-700")],
    [
      html.h2([attribute.class("text-lg font-medium text-zinc-300 mb-4")], [
        element.text("External Services Configuration"),
      ]),
      html.p([attribute.class("text-sm text-zinc-500 mb-6")], [
        element.text(
          "Configure URLs for external AT Protocol services. Changes take effect immediately.",
        ),
      ]),
      // Alert message for this section
      case model.services_alert {
        Some(#(kind, message)) -> {
          let alert_kind = case kind {
            "success" -> alert.Success
            "error" -> alert.Error
            _ -> alert.Info
          }
          html.div([attribute.class("mb-4")], [alert.alert(alert_kind, message)])
        }
        None -> element.none()
      },
      // Relay URL
      html.div([attribute.class("mb-6")], [
        html.label(
          [attribute.class("block text-sm font-medium text-zinc-400 mb-2")],
          [element.text("Relay URL")],
        ),
        html.p([attribute.class("text-xs text-zinc-500 mb-2")], [
          element.text("AT Protocol relay for backfill operations"),
        ]),
        html.div([attribute.class("flex gap-2")], [
          html.input([
            attribute.type_("text"),
            attribute.class(
              "flex-1 px-3 py-2 bg-zinc-900 border border-zinc-700 rounded text-zinc-300 text-sm font-mono",
            ),
            attribute.placeholder(settings.relay_url),
            attribute.value(model.relay_url_input),
            event.on_input(UpdateRelayUrlInput),
          ]),
          button.button(
            disabled: is_saving || model.relay_url_input == "",
            on_click: SubmitRelayUrl,
            text: "Update",
          ),
        ]),
      ]),
      // PLC Directory URL
      html.div([attribute.class("mb-6")], [
        html.label(
          [attribute.class("block text-sm font-medium text-zinc-400 mb-2")],
          [element.text("PLC Directory URL")],
        ),
        html.p([attribute.class("text-xs text-zinc-500 mb-2")], [
          element.text("PLC directory for DID resolution"),
        ]),
        html.div([attribute.class("flex gap-2")], [
          html.input([
            attribute.type_("text"),
            attribute.class(
              "flex-1 px-3 py-2 bg-zinc-900 border border-zinc-700 rounded text-zinc-300 text-sm font-mono",
            ),
            attribute.placeholder(settings.plc_directory_url),
            attribute.value(model.plc_directory_url_input),
            event.on_input(UpdatePlcDirectoryUrlInput),
          ]),
          button.button(
            disabled: is_saving || model.plc_directory_url_input == "",
            on_click: SubmitPlcDirectoryUrl,
            text: "Update",
          ),
        ]),
      ]),
      // Jetstream URL
      html.div([attribute.class("mb-6")], [
        html.label(
          [attribute.class("block text-sm font-medium text-zinc-400 mb-2")],
          [element.text("Jetstream URL")],
        ),
        html.p([attribute.class("text-xs text-zinc-500 mb-2")], [
          element.text("WebSocket endpoint (consumer restarts automatically)"),
        ]),
        html.div([attribute.class("flex gap-2")], [
          html.input([
            attribute.type_("text"),
            attribute.class(
              "flex-1 px-3 py-2 bg-zinc-900 border border-zinc-700 rounded text-zinc-300 text-sm font-mono",
            ),
            attribute.placeholder(settings.jetstream_url),
            attribute.value(model.jetstream_url_input),
            event.on_input(UpdateJetstreamUrlInput),
          ]),
          button.button(
            disabled: is_saving || model.jetstream_url_input == "",
            on_click: SubmitJetstreamUrl,
            text: "Update",
          ),
        ]),
      ]),
      // OAuth Supported Scopes
      html.div([attribute.class("mb-0")], [
        html.label(
          [attribute.class("block text-sm font-medium text-zinc-400 mb-2")],
          [element.text("OAuth Supported Scopes")],
        ),
        html.p([attribute.class("text-xs text-zinc-500 mb-2")], [
          element.text("Space-separated OAuth scopes (e.g., 'atproto transition:generic')"),
        ]),
        html.div([attribute.class("flex gap-2")], [
          html.input([
            attribute.type_("text"),
            attribute.class(
              "flex-1 px-3 py-2 bg-zinc-900 border border-zinc-700 rounded text-zinc-300 text-sm font-mono",
            ),
            attribute.placeholder(settings.oauth_supported_scopes),
            attribute.value(model.oauth_supported_scopes_input),
            event.on_input(UpdateOAuthSupportedScopesInput),
          ]),
          button.button(
            disabled: is_saving || model.oauth_supported_scopes_input == "",
            on_click: SubmitOAuthSupportedScopes,
            text: "Update",
          ),
        ]),
      ]),
    ],
  )
}
```

**Step 2: Update view function to include section**

Find the sections rendering (around line 280-286) and add:
```gleam
html.div([attribute.class("space-y-6")], [
  domain_authority_section(data.settings, model, is_saving),
  external_services_section(data.settings, model, is_saving),  // <- ADD THIS
  lexicons_section(model),
  oauth_clients_section(cache, model),
  admin_management_section(data.settings, model),
  danger_zone_section(model),
])
```

**Step 3: Build to verify**

```bash
cd client
gleam build
```

Expected: Success

**Step 4: Commit**

```bash
git add client/src/pages/settings.gleam
git commit -m "feat(settings): add external services configuration UI section"
```

---

## Task 14: Update Frontend - Message Handlers

**Files:**
- Modify: `client/src/pages/settings.gleam` (update function)

**Step 1: Add UpdateRelayUrlInput handler**

In the update function, add:
```gleam
UpdateRelayUrlInput(value) -> {
  #(Model(..model, relay_url_input: value), effect.none())
}
```

**Step 2: Add SubmitRelayUrl handler**

```gleam
SubmitRelayUrl -> {
  let variables =
    json.object([#("relayUrl", json.string(model.relay_url_input))])

  #(
    clear_services_alert(model) |> fn(m) { Model(..m, relay_url_input: "") },
    update_settings.execute(variables, fn(result) {
      case result {
        Ok(_) ->
          set_services_alert(model, "success", "Relay URL updated successfully")
        Error(err) ->
          set_services_alert(model, "error", "Failed: " <> err)
      }
    }),
  )
}
```

**Step 3: Add UpdatePlcDirectoryUrlInput handler**

```gleam
UpdatePlcDirectoryUrlInput(value) -> {
  #(Model(..model, plc_directory_url_input: value), effect.none())
}
```

**Step 4: Add SubmitPlcDirectoryUrl handler**

```gleam
SubmitPlcDirectoryUrl -> {
  let variables =
    json.object([#("plcDirectoryUrl", json.string(model.plc_directory_url_input))])

  #(
    clear_services_alert(model) |> fn(m) { Model(..m, plc_directory_url_input: "") },
    update_settings.execute(variables, fn(result) {
      case result {
        Ok(_) ->
          set_services_alert(model, "success", "PLC Directory URL updated successfully")
        Error(err) ->
          set_services_alert(model, "error", "Failed: " <> err)
      }
    }),
  )
}
```

**Step 5: Add UpdateJetstreamUrlInput handler**

```gleam
UpdateJetstreamUrlInput(value) -> {
  #(Model(..model, jetstream_url_input: value), effect.none())
}
```

**Step 6: Add SubmitJetstreamUrl handler**

```gleam
SubmitJetstreamUrl -> {
  let variables =
    json.object([#("jetstreamUrl", json.string(model.jetstream_url_input))])

  #(
    clear_services_alert(model) |> fn(m) { Model(..m, jetstream_url_input: "") },
    update_settings.execute(variables, fn(result) {
      case result {
        Ok(_) ->
          set_services_alert(
            model,
            "success",
            "Jetstream URL updated (consumer restarting)",
          )
        Error(err) ->
          set_services_alert(model, "error", "Failed: " <> err)
      }
    }),
  )
}
```

**Step 7: Add UpdateOAuthSupportedScopesInput handler**

```gleam
UpdateOAuthSupportedScopesInput(value) -> {
  #(Model(..model, oauth_supported_scopes_input: value), effect.none())
}
```

**Step 8: Add SubmitOAuthSupportedScopes handler**

```gleam
SubmitOAuthSupportedScopes -> {
  let variables =
    json.object([
      #("oauthSupportedScopes", json.string(model.oauth_supported_scopes_input)),
    ])

  #(
    clear_services_alert(model)
    |> fn(m) { Model(..m, oauth_supported_scopes_input: "") },
    update_settings.execute(variables, fn(result) {
      case result {
        Ok(_) ->
          set_services_alert(
            model,
            "success",
            "OAuth scopes updated successfully",
          )
        Error(err) ->
          set_services_alert(model, "error", "Failed: " <> err)
      }
    }),
  )
}
```

**Step 9: Build to verify**

```bash
cd client
gleam build
```

Expected: Success

**Step 10: Commit**

```bash
git add client/src/pages/settings.gleam
git commit -m "feat(settings): add message handlers for external services"
```

---

## Task 15: Update Environment Documentation

**Files:**
- Modify: `server/.env.example`

**Step 1: Remove environment variable lines**

Delete these lines:
```bash
# Line 34:
JETSTREAM_URL=wss://jetstream2.us-east.bsky.network/subscribe

# Lines 44-45:
RELAY_URL=https://relay1.us-west.bsky.network
PLC_DIRECTORY_URL=https://plc.directory

# Line 22 (if uncommented):
# OAUTH_SUPPORTED_SCOPES=atproto transition:generic
```

**Step 2: Add migration note**

Add at the end of the file:
```bash
# External Services Configuration (Migrated to Database)
# These settings are now configured via the Settings UI (/settings)
# Defaults:
#   - Relay URL: https://relay1.us-west.bsky.network
#   - PLC Directory URL: https://plc.directory
#   - Jetstream URL: wss://jetstream2.us-east.bsky.network/subscribe
#   - OAuth Supported Scopes: atproto transition:generic
```

**Step 3: Commit**

```bash
git add server/.env.example
git commit -m "docs: document external services migration to database"
```

---

## Task 16: End-to-End Testing

**Step 1: Start fresh server**

```bash
cd server
gleam run
```

Expected: Server starts successfully with no config cache actor

**Step 2: Open settings page**

Navigate to: http://localhost:8080/settings

Expected: New "External Services Configuration" section appears

**Step 3: Verify defaults in placeholders**

Expected placeholders:
- Relay URL: `https://relay1.us-west.bsky.network`
- PLC Directory URL: `https://plc.directory`
- Jetstream URL: `wss://jetstream2.us-east.bsky.network/subscribe`
- OAuth Scopes: `atproto transition:generic`

**Step 4: Update relay URL**

Enter: `https://relay.test.com`
Click "Update"

Expected: Success message "Relay URL updated successfully"

**Step 5: Verify GraphQL query returns new value**

Query in GraphiQL:
```graphql
query {
  settings {
    relayUrl
  }
}
```

Expected: Returns `"https://relay.test.com"`

**Step 6: Test Jetstream URL update and restart**

Enter: `wss://jetstream.test.com/subscribe`
Click "Update"

Expected:
- Success message "Jetstream URL updated (consumer restarting)"
- Server logs show: `[updateSettings] Restarting Jetstream consumer due to URL change`

**Step 7: Test OAuth scope validation**

Enter invalid scopes: `invalid scope format`
Click "Update"

Expected: Error message with validation failure

**Step 8: Test valid OAuth scopes**

Enter: `atproto transition:generic repo:test`
Click "Update"

Expected: Success message "OAuth scopes updated successfully"

**Step 9: Verify no config.gleam references**

```bash
cd server
grep -r "import config" src/ --include="*.gleam" | grep -v "config_repo"
```

Expected: No results (or only false positives like comments)

**Step 10: Verify Context has no removed fields**

```bash
grep -n "plc_url\|oauth_supported_scopes" server/src/server.gleam
```

Expected: No matches in Context type

---

## Testing Complete

All tasks implemented and tested. The config cache actor has been removed, environment variables migrated to database, and the Settings UI now provides full control over external service configuration.

**Final verification checklist:**
- ✅ Config cache actor deleted
- ✅ Four settings stored in database with defaults
- ✅ GraphQL API extended with four fields
- ✅ Settings UI has new section
- ✅ Jetstream consumer restarts on URL change
- ✅ OAuth scope validation works
- ✅ Changes take effect immediately
- ✅ Environment variables removed from .env.example
