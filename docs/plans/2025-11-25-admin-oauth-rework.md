# Admin OAuth Rework Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Rework admin OAuth to use internal `lib/oauth/` abstractions, bypassing external auth service registration and the OAuth server's client registration layer.

**Architecture:** Admin users authenticate via ATProtocol directly to their PDS. The flow uses the same bridge/DID resolver infrastructure as the OAuth server but skips the client registration layer. A minimal `admin_session` table links browser cookies to existing `oauth_atp_session` records.

**Tech Stack:** Gleam, Wisp, SQLite (sqlight), ATProtocol OAuth (DPoP, PKCE)

---

## Task 1: Add admin_session table schema

**Files:**
- Modify: `server/src/database/schema/tables.gleam`

**Step 1: Read the existing tables file**

```bash
# Understand existing table structure
```

**Step 2: Add admin_session table definition**

Add after line ~456 (after `oauth_atp_request` table):

```gleam
/// Admin browser session linking to ATP session
pub const admin_session_sql = "
CREATE TABLE IF NOT EXISTS admin_session (
  session_id TEXT PRIMARY KEY,
  atp_session_id TEXT NOT NULL,
  created_at INTEGER NOT NULL DEFAULT (unixepoch())
);
CREATE INDEX IF NOT EXISTS idx_admin_session_atp_session_id ON admin_session(atp_session_id);
"
```

**Step 3: Add to init function**

Find the `init` function and add:

```gleam
use _ <- result.try(sqlight.exec(admin_session_sql, conn))
```

**Step 4: Run build to verify**

```bash
cd /Users/chadmiller/code/quickslice/server && gleam build
```
Expected: Build succeeds

**Step 5: Commit**

```bash
git add server/src/database/schema/tables.gleam
git commit -m "feat: add admin_session table schema"
```

---

## Task 2: Create admin_session repository

**Files:**
- Create: `server/src/database/repositories/admin_session.gleam`

**Step 1: Create repository file**

```gleam
/// Admin session repository operations
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import sqlight

/// Admin session record
pub type AdminSession {
  AdminSession(
    session_id: String,
    atp_session_id: String,
    created_at: Int,
  )
}

/// Create a new admin session
pub fn insert(
  conn: sqlight.Connection,
  session_id: String,
  atp_session_id: String,
) -> Result(Nil, sqlight.Error) {
  let sql = "
    INSERT INTO admin_session (session_id, atp_session_id)
    VALUES (?, ?)
  "

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(session_id), sqlight.text(atp_session_id)],
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Get admin session by session_id (cookie ID)
pub fn get(
  conn: sqlight.Connection,
  session_id: String,
) -> Result(Option(AdminSession), sqlight.Error) {
  let sql = "
    SELECT session_id, atp_session_id, created_at
    FROM admin_session
    WHERE session_id = ?
  "

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(session_id)],
    expecting: decoder(),
  ))

  case list.first(rows) {
    Ok(session) -> Ok(Some(session))
    Error(_) -> Ok(None)
  }
}

/// Delete admin session (logout)
pub fn delete(
  conn: sqlight.Connection,
  session_id: String,
) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM admin_session WHERE session_id = ?"

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(session_id)],
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Decode admin session from database row
fn decoder() -> decode.Decoder(AdminSession) {
  use session_id <- decode.field(0, decode.string)
  use atp_session_id <- decode.field(1, decode.string)
  use created_at <- decode.field(2, decode.int)

  decode.success(AdminSession(
    session_id: session_id,
    atp_session_id: atp_session_id,
    created_at: created_at,
  ))
}
```

**Step 2: Run build to verify**

```bash
cd /Users/chadmiller/code/quickslice/server && gleam build
```
Expected: Build succeeds

**Step 3: Commit**

```bash
git add server/src/database/repositories/admin_session.gleam
git commit -m "feat: add admin_session repository"
```

---

## Task 3: Create admin OAuth authorize handler

**Files:**
- Create: `server/src/handlers/admin_oauth_authorize.gleam`

**Step 1: Create the handler file**

```gleam
/// Admin OAuth authorize handler
/// POST /admin/oauth/authorize - Initiates ATProtocol OAuth for admin login
import database/repositories/oauth_atp_requests
import database/repositories/oauth_atp_sessions
import database/types.{OAuthAtpRequest, OAuthAtpSession}
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/http
import gleam/http/request as http_request
import gleam/httpc
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/uri
import lib/oauth/atproto/did_resolver
import lib/oauth/did_cache
import lib/oauth/dpop/keygen
import lib/oauth/pkce
import lib/oauth/token_generator
import sqlight
import wisp

/// Handle POST /admin/oauth/authorize
pub fn handle(
  req: wisp.Request,
  conn: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
  redirect_uri: String,
  client_id: String,
  signing_key: Option(String),
) -> wisp.Response {
  case req.method {
    http.Post -> {
      use formdata <- wisp.require_form(req)

      // Get login_hint from form
      let login_hint = case list.key_find(formdata.values, "login_hint") {
        Ok(hint) -> hint
        Error(_) -> ""
      }

      case login_hint == "" {
        True -> error_response(400, "login_hint is required")
        False -> process_authorize(conn, did_cache, login_hint, redirect_uri, client_id, signing_key)
      }
    }
    _ -> wisp.method_not_allowed([http.Post])
  }
}

fn process_authorize(
  conn: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
  login_hint: String,
  redirect_uri: String,
  client_id: String,
  signing_key: Option(String),
) -> wisp.Response {
  // Resolve handle to DID if needed
  let did_result = case string.starts_with(login_hint, "did:") {
    True -> Ok(login_hint)
    False -> did_resolver.resolve_handle_to_did(login_hint)
  }

  case did_result {
    Error(_) -> error_response(400, "Failed to resolve handle to DID")
    Ok(did) -> {
      // Generate session_id
      let session_id = token_generator.generate_session_id()
      let now = token_generator.current_timestamp()
      let expires_at = token_generator.expiration_timestamp(600)

      // Generate ATP OAuth state
      let atp_oauth_state = token_generator.generate_state()

      // Generate DPoP key pair
      let dpop_key = keygen.generate_dpop_jwk()

      // Generate signing key JKT
      let signing_key_jkt = case signing_key {
        Some(key) -> token_generator.compute_jkt(key)
        None -> token_generator.generate_state()
      }

      // Generate PKCE for ATP OAuth
      let pkce_verifier = pkce.generate_code_verifier()
      let code_challenge = pkce.generate_code_challenge(pkce_verifier)

      // Store OAuth request for callback
      let oauth_req = OAuthAtpRequest(
        oauth_state: atp_oauth_state,
        authorization_server: "https://unknown-pds.example.com",
        nonce: token_generator.generate_state(),
        pkce_verifier: pkce_verifier,
        signing_public_key: signing_key_jkt,
        dpop_private_key: dpop_key,
        created_at: now,
        expires_at: expires_at,
      )

      case oauth_atp_requests.insert(conn, oauth_req) {
        Error(_) -> error_response(500, "Failed to store OAuth request")
        Ok(_) -> {
          // Create ATP session
          let atp_session = OAuthAtpSession(
            session_id: session_id,
            iteration: 0,
            did: Some(did),
            session_created_at: now,
            atp_oauth_state: atp_oauth_state,
            signing_key_jkt: signing_key_jkt,
            dpop_key: dpop_key,
            access_token: None,
            refresh_token: None,
            access_token_created_at: None,
            access_token_expires_at: None,
            access_token_scopes: None,
            session_exchanged_at: None,
            exchange_error: None,
          )

          case oauth_atp_sessions.insert(conn, atp_session) {
            Error(_) -> error_response(500, "Failed to store ATP session")
            Ok(_) -> {
              // Resolve DID to get PDS endpoint
              case did_resolver.resolve_did_with_cache(did_cache, did, True) {
                Error(_) -> error_response(400, "Failed to resolve DID")
                Ok(did_doc) -> {
                  case did_resolver.get_pds_endpoint(did_doc) {
                    None -> error_response(400, "No PDS endpoint in DID document")
                    Some(pds_endpoint) -> {
                      // Get authorization server metadata
                      case fetch_auth_server_metadata(pds_endpoint) {
                        Error(err) -> error_response(500, "Failed to get auth server: " <> err)
                        Ok(auth_endpoint) -> {
                          // Build authorization URL
                          let auth_url =
                            auth_endpoint
                            <> "?client_id=" <> uri.percent_encode(client_id)
                            <> "&redirect_uri=" <> uri.percent_encode(redirect_uri)
                            <> "&response_type=code"
                            <> "&code_challenge=" <> uri.percent_encode(code_challenge)
                            <> "&code_challenge_method=S256"
                            <> "&state=" <> uri.percent_encode(atp_oauth_state)
                            <> "&scope=" <> uri.percent_encode("atproto transition:generic")
                            <> "&login_hint=" <> uri.percent_encode(did)

                          wisp.redirect(auth_url)
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Fetch authorization server metadata from PDS
fn fetch_auth_server_metadata(pds_endpoint: String) -> Result(String, String) {
  let pr_url = pds_endpoint <> "/.well-known/oauth-protected-resource"

  case http_request.to(pr_url) {
    Error(_) -> Error("Invalid URL")
    Ok(pr_req) -> {
      case httpc.send(pr_req) {
        Error(_) -> Error("Request failed")
        Ok(pr_resp) -> {
          case pr_resp.status {
            200 -> {
              let decoder = decode.at(["authorization_servers"], decode.list(decode.string))
              case json.parse(pr_resp.body, decoder) {
                Ok([first, ..]) -> {
                  // Get authorization endpoint from auth server metadata
                  let as_url = first <> "/.well-known/oauth-authorization-server"
                  case http_request.to(as_url) {
                    Error(_) -> Error("Invalid auth server URL")
                    Ok(as_req) -> {
                      case httpc.send(as_req) {
                        Error(_) -> Error("Auth server request failed")
                        Ok(as_resp) -> {
                          case as_resp.status {
                            200 -> {
                              let endpoint_decoder = decode.at(["authorization_endpoint"], decode.string)
                              case json.parse(as_resp.body, endpoint_decoder) {
                                Ok(endpoint) -> Ok(endpoint)
                                Error(_) -> Error("No authorization_endpoint in metadata")
                              }
                            }
                            _ -> Error("Auth server metadata request failed")
                          }
                        }
                      }
                    }
                  }
                }
                _ -> Error("No authorization servers found")
              }
            }
            _ -> Error("Protected resource metadata request failed")
          }
        }
      }
    }
  }
}

fn error_response(status: Int, message: String) -> wisp.Response {
  wisp.log_error("Admin OAuth error: " <> message)
  wisp.response(status)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text("{\"error\": \"" <> message <> "\"}"))
}
```

**Step 2: Run build to verify**

```bash
cd /Users/chadmiller/code/quickslice/server && gleam build
```
Expected: Build succeeds

**Step 3: Commit**

```bash
git add server/src/handlers/admin_oauth_authorize.gleam
git commit -m "feat: add admin OAuth authorize handler"
```

---

## Task 4: Create admin OAuth callback handler

**Files:**
- Create: `server/src/handlers/admin_oauth_callback.gleam`

**Step 1: Create the handler file**

```gleam
/// Admin OAuth callback handler
/// GET /admin/oauth/callback - Handles ATP OAuth callback for admin login
import database/repositories/admin_session
import database/repositories/oauth_atp_requests
import database/repositories/oauth_atp_sessions
import gleam/erlang/process.{type Subject}
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import lib/oauth/atproto/bridge
import lib/oauth/did_cache
import lib/oauth/token_generator
import oauth/session
import sqlight
import wisp

/// Handle GET /admin/oauth/callback
pub fn handle(
  req: wisp.Request,
  conn: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
  redirect_uri: String,
  client_id: String,
  signing_key: Option(String),
) -> wisp.Response {
  // Parse query parameters
  let query = wisp.get_query(req)

  let code_result = list.key_find(query, "code")
  let state_result = list.key_find(query, "state")

  case code_result, state_result {
    Error(_), _ -> error_response(400, "Missing 'code' parameter")
    _, Error(_) -> error_response(400, "Missing 'state' parameter")
    Ok(code), Ok(state) -> {
      process_callback(req, conn, did_cache, code, state, redirect_uri, client_id, signing_key)
    }
  }
}

fn process_callback(
  req: wisp.Request,
  conn: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
  code: String,
  state: String,
  redirect_uri: String,
  client_id: String,
  signing_key: Option(String),
) -> wisp.Response {
  // Retrieve ATP session by state
  case oauth_atp_sessions.get_by_state(conn, state) {
    Error(err) -> error_response(500, "Database error: " <> string.inspect(err))
    Ok(None) -> error_response(400, "Invalid or expired state parameter")
    Ok(Some(atp_session)) -> {
      // Retrieve ATP request to get PKCE verifier
      case oauth_atp_requests.get(conn, state) {
        Error(err) -> error_response(500, "Database error: " <> string.inspect(err))
        Ok(None) -> error_response(400, "OAuth request not found - PKCE verifier missing")
        Ok(Some(atp_request)) -> {
          let code_verifier = atp_request.pkce_verifier

          // Call bridge to exchange code for tokens
          case bridge.handle_callback(
            conn,
            did_cache,
            atp_session,
            code,
            code_verifier,
            redirect_uri,
            client_id,
            state,
            signing_key,
          ) {
            Error(bridge_err) -> {
              error_response(500, "Token exchange failed: " <> bridge_error_to_string(bridge_err))
            }
            Ok(updated_session) -> {
              // Clean up one-time-use oauth request
              let _ = oauth_atp_requests.delete(conn, state)

              // Generate admin session ID (for cookie)
              let admin_session_id = token_generator.generate_session_id()

              // Create admin session linking to ATP session
              case admin_session.insert(conn, admin_session_id, updated_session.session_id) {
                Error(err) -> error_response(500, "Failed to create admin session: " <> string.inspect(err))
                Ok(_) -> {
                  // Set session cookie and redirect to home
                  wisp.redirect("/")
                  |> session.set_session_cookie(req, admin_session_id)
                }
              }
            }
          }
        }
      }
    }
  }
}

fn error_response(status: Int, message: String) -> wisp.Response {
  wisp.log_error("Admin OAuth callback error: " <> message)
  let json_body = json.object([
    #("error", json.string("server_error")),
    #("error_description", json.string(message)),
  ])

  wisp.response(status)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text(json.to_string(json_body)))
}

fn bridge_error_to_string(err: bridge.BridgeError) -> String {
  case err {
    bridge.DIDResolutionError(_) -> "DID resolution failed"
    bridge.PDSNotFound(msg) -> "PDS not found: " <> msg
    bridge.TokenExchangeError(msg) -> "Token exchange failed: " <> msg
    bridge.HTTPError(msg) -> "HTTP error: " <> msg
    bridge.InvalidResponse(msg) -> "Invalid response: " <> msg
    bridge.StorageError(msg) -> "Storage error: " <> msg
    bridge.MetadataFetchError(msg) -> "Metadata fetch failed: " <> msg
    bridge.PARError(msg) -> "PAR error: " <> msg
  }
}
```

**Step 2: Run build to verify**

```bash
cd /Users/chadmiller/code/quickslice/server && gleam build
```
Expected: Build succeeds

**Step 3: Commit**

```bash
git add server/src/handlers/admin_oauth_callback.gleam
git commit -m "feat: add admin OAuth callback handler"
```

---

## Task 5: Update server routes

**Files:**
- Modify: `server/src/server.gleam`

**Step 1: Add imports for new handlers**

Add near line ~20 with other handler imports:

```gleam
import handlers/admin_oauth_authorize as admin_oauth_authorize_handler
import handlers/admin_oauth_callback as admin_oauth_callback_handler
```

**Step 2: Add new routes**

Add after line ~595 (after the old `_oauth` routes, or replace them):

```gleam
    ["admin", "oauth", "authorize"] ->
      admin_oauth_authorize_handler.handle(
        req,
        ctx.db,
        ctx.did_cache,
        ctx.oauth_config.redirect_uri,
        ctx.oauth_config.client_id,
        ctx.oauth_signing_key,
      )
    ["admin", "oauth", "callback"] ->
      admin_oauth_callback_handler.handle(
        req,
        ctx.db,
        ctx.did_cache,
        ctx.oauth_config.redirect_uri,
        ctx.oauth_config.client_id,
        ctx.oauth_signing_key,
      )
```

**Step 3: Update redirect_uri config**

The `redirect_uri` passed to the handler needs to point to the new callback endpoint. Check how `ctx.oauth_config.redirect_uri` is set and ensure it can be configured separately for admin OAuth, OR update the handler to construct the redirect_uri dynamically based on the request host.

**Step 4: Run build to verify**

```bash
cd /Users/chadmiller/code/quickslice/server && gleam build
```
Expected: Build succeeds

**Step 5: Commit**

```bash
git add server/src/server.gleam
git commit -m "feat: add admin OAuth routes"
```

---

## Task 6: Update client login form

**Files:**
- Modify: `client/src/components/layout.gleam`

**Step 1: Update form action URL**

Change line 44 from:
```gleam
attribute.action("/oauth/authorize"),
```

To:
```gleam
attribute.action("/admin/oauth/authorize"),
```

**Step 2: Rebuild client**

```bash
cd /Users/chadmiller/code/quickslice/client && gleam build
```
Expected: Build succeeds

**Step 3: Commit**

```bash
git add client/src/components/layout.gleam
git commit -m "feat: update login form to use admin OAuth endpoint"
```

---

## Task 7: Update session.gleam to use admin_session repository

**Files:**
- Modify: `server/src/oauth/session.gleam`

**Step 1: Update get_current_session to use admin_session**

Modify `get_current_session` function to:
1. Get session_id from cookie
2. Look up admin_session by session_id
3. Get atp_session_id from admin_session
4. Look up oauth_atp_session by atp_session_id

```gleam
/// Get the current user session with ATP session data
pub fn get_current_session_v2(
  req: Request,
  db: Connection,
) -> Result(#(String, String, String), Nil) {
  // Get admin session ID from cookie
  use session_id <- result.try(get_session_id(req))

  // Look up admin session
  use admin_sess <- result.try(
    admin_session.get(db, session_id)
    |> result.replace_error(Nil)
    |> result.then(fn(opt) {
      case opt {
        Some(s) -> Ok(s)
        None -> Error(Nil)
      }
    })
  )

  // Look up ATP session
  use atp_sess <- result.try(
    oauth_atp_sessions.get_latest(db, admin_sess.atp_session_id)
    |> result.replace_error(Nil)
    |> result.then(fn(opt) {
      case opt {
        Some(s) -> Ok(s)
        None -> Error(Nil)
      }
    })
  )

  // Return DID, handle (from DID for now), and access_token
  case atp_sess.did, atp_sess.access_token {
    Some(did), Some(token) -> Ok(#(did, did, token))
    _, _ -> Error(Nil)
  }
}
```

**Step 2: Add imports**

Add at top of file:
```gleam
import database/repositories/admin_session
import database/repositories/oauth_atp_sessions
```

**Step 3: Run build to verify**

```bash
cd /Users/chadmiller/code/quickslice/server && gleam build
```
Expected: Build succeeds

**Step 4: Commit**

```bash
git add server/src/oauth/session.gleam
git commit -m "feat: update session module to use admin_session repository"
```

---

## Task 8: Update logout handler

**Files:**
- Modify: `server/src/oauth/handlers.gleam` (or create new handler)

**Step 1: Update handle_logout to delete admin_session**

The logout handler needs to delete from `admin_session` table instead of `oauth_sessions`.

```gleam
/// Handle POST /logout - Clear session and redirect
pub fn handle_logout(req: Request, db: sqlight.Connection) -> Response {
  // Get session ID and delete admin session
  case session.get_session_id(req) {
    Ok(session_id) -> {
      let _ = admin_session.delete(db, session_id)
      wisp.log_info("Admin user logged out")
    }
    Error(_) -> Nil
  }

  // Clear cookie and redirect
  wisp.redirect("/")
  |> session.clear_session_cookie(req)
}
```

**Step 2: Run build to verify**

```bash
cd /Users/chadmiller/code/quickslice/server && gleam build
```
Expected: Build succeeds

**Step 3: Commit**

```bash
git add server/src/oauth/handlers.gleam
git commit -m "feat: update logout to use admin_session"
```

---

## Task 9: Test the flow end-to-end

**Step 1: Run the server**

```bash
cd /Users/chadmiller/code/quickslice/server && gleam run
```

**Step 2: Test login flow**

1. Navigate to the client interface
2. Enter a Bluesky handle in the login form
3. Submit and verify redirect to ATP authorization
4. Complete authorization at PDS
5. Verify redirect back and session cookie set
6. Verify logged-in state displays correctly

**Step 3: Test logout**

1. Click logout
2. Verify session cleared
3. Verify redirected to home with login form visible

---

## Task 10: Clean up old files (after verification)

**Files:**
- Remove old routes from `server/src/server.gleam` (lines 591-594: `_oauth` routes)
- Consider removing `server/src/oauth/registration.gleam` if no longer needed
- Consider removing `server/src/oauth/pkce.gleam` (use `lib/oauth/pkce.gleam` instead)

**Step 1: Remove old _oauth routes**

Delete from server.gleam:
```gleam
["_oauth", "authorize"] ->
  handlers.handle_oauth_authorize(req, ctx.db, ctx.oauth_config)
["_oauth", "callback"] ->
  handlers.handle_oauth_callback(req, ctx.db, ctx.oauth_config)
```

**Step 2: Run build to verify**

```bash
cd /Users/chadmiller/code/quickslice/server && gleam build
```
Expected: Build succeeds

**Step 3: Commit**

```bash
git add -A
git commit -m "chore: remove legacy _oauth routes"
```

---

## Critical Files Reference

Files to read before starting implementation:
- `server/src/handlers/oauth/authorize.gleam` - Reference for ATP auth initiation pattern
- `server/src/handlers/oauth/atp_callback.gleam` - Reference for callback handling pattern
- `server/src/lib/oauth/atproto/bridge.gleam` - Bridge API for token exchange
- `server/src/database/repositories/oauth_atp_sessions.gleam` - Repository pattern reference
- `server/src/oauth/session.gleam` - Cookie management functions to preserve
- `server/src/database/types.gleam:310-342` - OAuthAtpSession and OAuthAtpRequest types
