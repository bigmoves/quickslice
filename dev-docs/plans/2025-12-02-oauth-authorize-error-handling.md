# OAuth Authorize Error Handling Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace JSON error responses in OAuth authorize endpoints with proper redirects per OAuth spec (RFC 6749 Section 4.1.2.1).

**Architecture:**
- Errors **before** `redirect_uri` is validated → JSON error (can't redirect to unknown/invalid URI)
- Errors **after** `redirect_uri` is validated → redirect with `error` and `error_description` params
- Admin flow redirects to `/` or `/onboarding` based on admin existence
- Third-party flow redirects to client's validated `redirect_uri`

**Tech Stack:** Gleam (server), wisp HTTP framework

---

## Task 1: Fix Admin OAuth Authorize (`/admin/oauth/authorize`)

**Files:**
- Modify: `server/src/handlers/admin_oauth_authorize.gleam`

**Step 1: Add config_repo import**

Add after line 3:
```gleam
import database/repositories/config as config_repo
```

**Step 2: Replace error_response with error_redirect**

Replace the `error_response` function (lines 247-252) with:

```gleam
fn error_redirect(
  conn: sqlight.Connection,
  error: String,
  description: String,
) -> wisp.Response {
  wisp.log_error("Admin OAuth error: " <> description)

  let redirect_path = case config_repo.has_admins(conn) {
    True -> "/"
    False -> "/onboarding"
  }

  let redirect_url =
    redirect_path
    <> "?error="
    <> uri.percent_encode(error)
    <> "&error_description="
    <> uri.percent_encode(description)

  wisp.redirect(redirect_url)
}
```

**Step 3: Update all error_response calls**

| Location | Old | New |
|----------|-----|-----|
| Line 45 | `error_response(400, "login_hint is required")` | `error_redirect(conn, "invalid_request", "Please enter a handle to login")` |
| Line 78 | `error_response(400, "Failed to resolve handle to DID")` | `error_redirect(conn, "invalid_request", "Could not find that handle")` |
| Line 115 | `error_response(500, "Failed to store OAuth request")` | `error_redirect(conn, "server_error", "Failed to start login")` |
| Line 137 | `error_response(500, "Failed to store ATP session")` | `error_redirect(conn, "server_error", "Failed to start login")` |
| Line 141 | `error_response(400, "Failed to resolve DID")` | `error_redirect(conn, "invalid_request", "Could not resolve account")` |
| Lines 144-145 | `error_response(400, "No PDS endpoint in DID document")` | `error_redirect(conn, "invalid_request", "Account has no PDS configured")` |
| Lines 149-153 | `error_response(500, "Failed to get auth server: " <> err)` | `error_redirect(conn, "server_error", "Could not connect to login server")` |

**Step 4: Build and verify**

Run: `cd server && gleam build`
Expected: Build succeeds

**Step 5: Commit**

```bash
git add server/src/handlers/admin_oauth_authorize.gleam
git commit -m "feat: redirect with error params from admin oauth authorize"
```

---

## Task 2: Fix Third-Party OAuth Authorize (`/oauth/authorize`)

**Files:**
- Modify: `server/src/handlers/oauth/authorize.gleam`

**Step 1: Change handle function to use RedirectWithError after validation**

The handler already has `RedirectWithError` type but doesn't use it. The challenge is that errors in `handle_authorize` lose the `redirect_uri` context.

Replace the `handle` function (lines 44-87) with:

```gleam
/// Handle GET /oauth/authorize
pub fn handle(
  req: wisp.Request,
  conn: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
  redirect_uri: String,
  client_id: String,
  signing_key: Option(String),
) -> wisp.Response {
  case req.method {
    http.Get | http.Post -> {
      case req.query {
        Some(query) -> {
          handle_authorize_with_error_redirect(
            query,
            conn,
            did_cache,
            redirect_uri,
            client_id,
            signing_key,
          )
        }
        None -> {
          json_error_response("Missing query parameters")
        }
      }
    }
    _ -> wisp.method_not_allowed([http.Get, http.Post])
  }
}

fn json_error_response(message: String) -> wisp.Response {
  wisp.log_error("Authorization error: " <> message)
  wisp.response(400)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text("{\"error\": \"" <> message <> "\"}"))
}
```

**Step 2: Add new handler that tracks redirect context**

Add after the `handle` function:

```gleam
/// Handle authorization with proper error redirects after validation
fn handle_authorize_with_error_redirect(
  query: String,
  conn: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
  server_redirect_uri: String,
  server_client_id: String,
  signing_key: Option(String),
) -> wisp.Response {
  // Parse query parameters
  let params = case uri.parse_query(query) {
    Ok(p) -> p
    Error(_) -> {
      return json_error_response("Failed to parse query string")
    }
  }

  // Check for PAR request_uri
  case get_param(params, "request_uri") {
    Some(_) -> {
      json_error_response("PAR flow not yet implemented")
    }
    None -> {
      // Parse minimal request to get redirect_uri and state
      let client_redirect_uri = get_param(params, "redirect_uri")
      let state = get_param(params, "state")
      let client_id_param = get_param(params, "client_id")

      // Before we have validated redirect_uri, use JSON errors
      case client_redirect_uri, client_id_param {
        None, _ -> json_error_response("redirect_uri is required")
        _, None -> json_error_response("client_id is required")
        Some(ruri), Some(cid) -> {
          // Get and validate client
          case oauth_clients.get(conn, cid) {
            Error(_) -> json_error_response("Failed to retrieve client")
            Ok(None) -> json_error_response("Client not found")
            Ok(Some(client)) -> {
              // Validate redirect_uri matches client
              case validator.validate_redirect_uri(ruri) {
                Error(e) -> json_error_response(error.error_description(e))
                Ok(_) -> {
                  case validator.validate_redirect_uri_match(ruri, client.redirect_uris, client.require_redirect_exact) {
                    Error(e) -> json_error_response(error.error_description(e))
                    Ok(_) -> {
                      // redirect_uri is now validated - use redirects for subsequent errors
                      case handle_standard_flow(params, conn, did_cache, server_redirect_uri, server_client_id, signing_key) {
                        Ok(response) -> build_redirect_response(response)
                        Error(err) -> {
                          build_redirect_response(RedirectWithError(
                            redirect_uri: ruri,
                            error: "server_error",
                            error_description: err,
                            state: state,
                          ))
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
```

**Step 3: Build and verify**

Run: `cd server && gleam build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add server/src/handlers/oauth/authorize.gleam
git commit -m "feat: redirect with error params from third-party oauth authorize"
```

---

## Task 3: Manual Testing

**Step 1: Test admin invalid handle**

1. Start server: `cd server && gleam run`
2. Go to `http://localhost:8080/onboarding`
3. Enter invalid handle `notreal.invalid`
4. Click login
5. Verify redirect to `/onboarding?error=invalid_request&error_description=...`
6. Verify red alert shows "Login failed: Could not find that handle"

**Step 2: Test third-party invalid handle**

1. Create a test OAuth client with redirect_uri `http://localhost:3000/callback`
2. Navigate to `/oauth/authorize?client_id=...&redirect_uri=http://localhost:3000/callback&response_type=code&login_hint=invalid.handle`
3. Verify redirect to `http://localhost:3000/callback?error=server_error&error_description=Failed%20to%20resolve%20handle`

**Step 3: Verify normal flows still work**

1. Complete admin login with valid handle
2. Complete third-party OAuth flow with valid handle

---

## Summary

| Task | File | Change |
|------|------|--------|
| 1 | `admin_oauth_authorize.gleam` | Replace `error_response` with redirect to `/` or `/onboarding` |
| 2 | `oauth/authorize.gleam` | Use `RedirectWithError` after `redirect_uri` validated |
| 3 | Manual testing | Verify all error and happy paths |
