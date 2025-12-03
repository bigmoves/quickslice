# Initialize External Services Config Defaults on Startup

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Ensure external services configuration defaults (Relay URL, PLC Directory URL, Jetstream URL, OAuth Scopes) are written to the database on server startup and visible in the Settings UI, plus restart Jetstream consumer when PLC URL changes.

**Architecture:** Add a database initialization function that checks if each config key exists and sets defaults if missing (never overwrites). Call this during server startup after database schema migration. Modify the `updateSettings` mutation to restart the Jetstream consumer when PLC URL changes, similar to the existing Jetstream URL change handling.

**Tech Stack:** Gleam, SQLite (sqlight), GraphQL (swell)

---

## Task 1: Add Config Initialization Function

**Files:**
- Modify: `server/src/database/repositories/config.gleam:240` (after setter functions)

**Step 1: Add initialization function with relay URL**

Add this function after line 240 (after `set_oauth_supported_scopes`):

```gleam
/// Initialize external services config with defaults if not already set
/// Should be called once during server startup
pub fn initialize_external_services_defaults(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  // Only set if the key doesn't exist (don't overwrite user settings)

  // Relay URL
  case get(conn, "relay_url") {
    Error(_) -> {
      let _ = set(conn, "relay_url", "https://relay1.us-west.bsky.network")
      Nil
    }
    Ok(_) -> Nil
  }

  Ok(Nil)
}
```

**Step 2: Add PLC Directory URL initialization**

Extend the function to include PLC URL (inside the function, before `Ok(Nil)`):

```gleam
  // PLC Directory URL
  case get(conn, "plc_directory_url") {
    Error(_) -> {
      let _ = set(conn, "plc_directory_url", "https://plc.directory")
      Nil
    }
    Ok(_) -> Nil
  }
```

**Step 3: Add Jetstream URL initialization**

Continue adding to the function:

```gleam
  // Jetstream URL
  case get(conn, "jetstream_url") {
    Error(_) -> {
      let _ = set(conn, "jetstream_url", "wss://jetstream2.us-west.bsky.network/subscribe")
      Nil
    }
    Ok(_) -> Nil
  }
```

**Step 4: Add OAuth Supported Scopes initialization**

Complete the function:

```gleam
  // OAuth Supported Scopes
  case get(conn, "oauth_supported_scopes") {
    Error(_) -> {
      let _ = set(conn, "oauth_supported_scopes", "atproto transition:generic")
      Nil
    }
    Ok(_) -> Nil
  }
```

**Step 5: Build to verify compilation**

```bash
cd server
gleam build
```

Expected: Success with no errors

**Step 6: Commit**

```bash
git add server/src/database/repositories/config.gleam
git commit -m "feat(config): add initialization function for external services defaults"
```

---

## Task 2: Call Initialization on Server Startup

**Files:**
- Modify: `server/src/server.gleam` (find database initialization in main function)

**Step 1: Locate database initialization**

Search for where the database is initialized in the `main()` function. Look for:
```gleam
let db = database.initialize(db_path)
```

Or similar database initialization code.

**Step 2: Add config initialization call**

Right after the database initialization (after schema migration completes), add:

```gleam
// Initialize external services config defaults
let _ = config_repo.initialize_external_services_defaults(db)
```

**Step 3: Build to verify**

```bash
cd server
gleam build
```

Expected: Success

**Step 4: Test with fresh database**

Delete the database and start the server to verify initialization:

```bash
rm server/quickslice.db
cd server
gleam run
```

Expected: Server starts successfully with no errors about missing config

**Step 5: Verify defaults in database**

While server is running, check the settings via GraphQL at http://localhost:8080/admin/graphql:

```graphql
query {
  settings {
    relayUrl
    plcDirectoryUrl
    jetstreamUrl
    oauthSupportedScopes
  }
}
```

Expected response:
```json
{
  "data": {
    "settings": {
      "relayUrl": "https://relay1.us-west.bsky.network",
      "plcDirectoryUrl": "https://plc.directory",
      "jetstreamUrl": "wss://jetstream2.us-west.bsky.network/subscribe",
      "oauthSupportedScopes": "atproto transition:generic"
    }
  }
}
```

**Step 6: Stop server and commit**

```bash
# Stop the server (Ctrl+C)
git add server/src/server.gleam
git commit -m "feat(server): initialize external services config on startup"
```

---

## Task 3: Restart Jetstream When PLC URL Changes

**Files:**
- Modify: `server/src/client_schema.gleam:1177-1203` (updateSettings mutation - PLC URL section)

**Step 1: Change return type from Ok(Nil) to Ok(True)**

Find line 1192 in the PLC URL update section and change:

```gleam
// BEFORE:
Ok(_) -> Ok(Nil)

// AFTER:
Ok(_) -> Ok(True)
```

**Step 2: Change default return from Ok(Nil) to Ok(False)**

Find line 1201 (the default case) and change:

```gleam
// BEFORE:
_ -> Ok(Nil)

// AFTER:
_ -> Ok(False)
```

**Step 3: Update pattern match variable name**

Find line 1204 and change from `plc_url_result` to track boolean:

```gleam
// BEFORE:
case plc_url_result {
  Error(err) -> Error(err)
  Ok(_) -> {

// AFTER:
case plc_url_result {
  Error(err) -> Error(err)
  Ok(plc_changed) -> {
```

**Step 4: Add Jetstream restart logic**

Right after the `Ok(plc_changed) -> {` line, before continuing to the next setting, add:

```gleam
    // Restart Jetstream if PLC URL changed
    case plc_changed {
      True -> {
        case jetstream_subject {
          Some(consumer) -> {
            logging.log(
              logging.Info,
              "[updateSettings] Restarting Jetstream consumer due to PLC URL change",
            )
            case jetstream_consumer.restart(consumer) {
              Ok(_) ->
                logging.log(
                  logging.Info,
                  "[updateSettings] Jetstream consumer restarted",
                )
              Error(err) ->
                logging.log(
                  logging.Error,
                  "[updateSettings] Failed to restart Jetstream: " <> err,
                )
            }
          }
          None -> Nil
        }
      }
      False -> Nil
    }
```

**Step 5: Build to verify**

```bash
cd server
gleam build
```

Expected: Success with no errors

**Step 6: Test PLC URL update with restart**

Start the server:
```bash
cd server
gleam run
```

Navigate to http://localhost:8080/settings and update the PLC Directory URL to a different value (e.g., `https://test-plc.example.com`).

Expected in server logs:
```
[updateSettings] Restarting Jetstream consumer due to PLC URL change
[updateSettings] Jetstream consumer restarted
[jetstream] Starting Jetstream consumer...
```

**Step 7: Commit**

```bash
git add server/src/client_schema.gleam
git commit -m "feat(graphql): restart Jetstream consumer when PLC URL changes"
```

---

## Task 4: Remove Unused Import

**Files:**
- Modify: `server/src/client_schema.gleam:15`

**Step 1: Remove the unused envoy import**

Find line 15 which has:
```gleam
import envoy
```

Delete this entire line.

**Step 2: Build to verify warning is gone**

```bash
cd server
gleam build
```

Expected: Success with NO warning about unused `import envoy`

**Step 3: Commit**

```bash
git add server/src/client_schema.gleam
git commit -m "refactor(client_schema): remove unused envoy import"
```

---

## Task 5: End-to-End Testing

**Step 1: Test fresh database initialization**

```bash
cd server
rm quickslice.db
gleam run
```

Expected:
- Server starts successfully
- No errors about missing config values

**Step 2: Verify Settings UI shows defaults**

Navigate to http://localhost:8080/settings

Expected all four fields show their default values:
- Relay URL: `https://relay1.us-west.bsky.network`
- PLC Directory URL: `https://plc.directory`
- Jetstream URL: `wss://jetstream2.us-west.bsky.network/subscribe`
- OAuth Supported Scopes: `atproto transition:generic`

**Step 3: Test custom value persistence**

1. Change PLC URL to `https://custom-plc.example.com` and save
2. Stop the server (Ctrl+C)
3. Start the server again: `gleam run`
4. Navigate to http://localhost:8080/settings

Expected: Custom PLC URL `https://custom-plc.example.com` is retained (not overwritten with default)

**Step 4: Test PLC URL change restarts consumer**

1. Change PLC URL to a different value
2. Check server logs

Expected logs:
```
[updateSettings] Restarting Jetstream consumer due to PLC URL change
[updateSettings] Jetstream consumer restarted
[jetstream] Starting Jetstream consumer...
```

**Step 5: Verify DID resolution uses new PLC URL**

If Jetstream events are coming in, verify that DID resolution attempts use the updated PLC URL (check logs for PLC directory requests).

---

## Testing Complete

All tasks implemented. The external services configuration now:
- ✅ Initializes defaults in database on fresh startup
- ✅ Shows actual values in Settings UI (not placeholders)
- ✅ Restarts Jetstream consumer when PLC URL changes
- ✅ Never overwrites user-configured values
- ✅ No unused imports

## Files Modified

1. `server/src/database/repositories/config.gleam` - Added `initialize_external_services_defaults()`
2. `server/src/server.gleam` - Call initialization on startup
3. `server/src/client_schema.gleam` - Restart Jetstream on PLC URL change, remove unused import
