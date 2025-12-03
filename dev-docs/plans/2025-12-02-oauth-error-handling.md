# OAuth Error Handling Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Display friendly UI messages when users deny OAuth authorization instead of showing raw JSON errors.

**Architecture:** Check for OAuth `error` query param before `code` param in callback handlers. Forward errors to appropriate redirect (admin UI or third-party client's redirect_uri). Client parses error from URL and displays alert.

**Tech Stack:** Gleam (server + Lustre client), wisp HTTP framework, modem routing

---

## Task 1: Admin OAuth Callback Error Handling

**Files:**
- Modify: `server/src/handlers/admin_oauth_callback.gleam:33-55`

**Step 1: Add imports for URI encoding and result handling**

At the top of the file, add:

```gleam
import gleam/result
import gleam/uri
```

**Step 2: Run the build to verify imports work**

Run: `cd server && gleam build`
Expected: Build succeeds

**Step 3: Modify handle function to check for error param first**

Replace the `handle` function body (lines 33-55) with:

```gleam
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

  // Check for OAuth error FIRST (user denied, etc.)
  case list.key_find(query, "error") {
    Ok(error) -> {
      let error_description =
        list.key_find(query, "error_description")
        |> result.unwrap("")

      // Redirect to / or /onboarding based on admin existence
      let redirect_path = case config_repo.has_admins(conn) {
        True -> "/"
        False -> "/onboarding"
      }

      let redirect_url =
        redirect_path
        <> "?error="
        <> uri.percent_encode(error)
        <> "&error_description="
        <> uri.percent_encode(error_description)

      wisp.redirect(redirect_url)
    }
    Error(_) -> {
      // Normal flow: check for code and state
      let code_result = list.key_find(query, "code")
      let state_result = list.key_find(query, "state")

      case code_result, state_result {
        Error(_), _ -> error_response(400, "Missing 'code' parameter")
        _, Error(_) -> error_response(400, "Missing 'state' parameter")
        Ok(code), Ok(state) -> {
          process_callback(
            req,
            conn,
            did_cache,
            code,
            state,
            redirect_uri,
            client_id,
            signing_key,
          )
        }
      }
    }
  }
}
```

**Step 4: Run the build to verify changes compile**

Run: `cd server && gleam build`
Expected: Build succeeds

**Step 5: Commit**

```bash
git add server/src/handlers/admin_oauth_callback.gleam
git commit -m "feat: handle OAuth errors in admin callback with redirect"
```

---

## Task 2: Third-Party OAuth Callback Error Handling

**Files:**
- Modify: `server/src/handlers/oauth/atp_callback.gleam`

**Step 1: Add imports for URI encoding and result handling**

At the top of the file, ensure these imports exist:

```gleam
import gleam/result
import gleam/uri
```

**Step 2: Run the build to verify imports work**

Run: `cd server && gleam build`
Expected: Build succeeds

**Step 3: Read current atp_callback.gleam to understand structure**

Review file to find the `handle` function and understand the flow.

**Step 4: Modify handle function to check for error param first**

At the start of the `handle` function, before checking for `code`, add error handling:

```gleam
pub fn handle(
  req: wisp.Request,
  conn: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
  redirect_uri: String,
  client_id: String,
  signing_key: Option(String),
) -> wisp.Response {
  let query = wisp.get_query(req)

  // Check for OAuth error FIRST (user denied, etc.)
  case list.key_find(query, "error") {
    Ok(error) -> {
      handle_oauth_error(conn, query, error)
    }
    Error(_) -> {
      // Normal flow continues...
      let code_result = list.key_find(query, "code")
      let state_result = list.key_find(query, "state")
      // ... rest of existing logic
    }
  }
}

fn handle_oauth_error(
  conn: sqlight.Connection,
  query: List(#(String, String)),
  error: String,
) -> wisp.Response {
  let error_description =
    list.key_find(query, "error_description")
    |> result.unwrap("")
  let state =
    list.key_find(query, "state")
    |> result.unwrap("")

  // Look up client's redirect_uri via: state -> atp_session -> session_id -> auth_request
  case oauth_atp_sessions.get_by_state(conn, state) {
    Ok(Some(atp_session)) -> {
      case oauth_auth_requests.get(conn, atp_session.session_id) {
        Ok(Some(auth_request)) -> {
          // Build redirect URL with error params
          let separator = case string.contains(auth_request.redirect_uri, "?") {
            True -> "&"
            False -> "?"
          }

          let redirect_url =
            auth_request.redirect_uri
            <> separator
            <> "error="
            <> uri.percent_encode(error)
            <> "&error_description="
            <> uri.percent_encode(error_description)
            <> case auth_request.state {
              Some(client_state) ->
                "&state=" <> uri.percent_encode(client_state)
              None -> ""
            }

          wisp.redirect(redirect_url)
        }
        _ -> error_response(400, "missing_parameter", "OAuth session not found")
      }
    }
    _ -> error_response(400, "missing_parameter", "Invalid state parameter")
  }
}
```

**Step 5: Run the build to verify changes compile**

Run: `cd server && gleam build`
Expected: Build succeeds

**Step 6: Commit**

```bash
git add server/src/handlers/oauth/atp_callback.gleam
git commit -m "feat: handle OAuth errors in third-party callback with redirect"
```

---

## Task 3: Client-Side OAuth Error Display

**Files:**
- Modify: `client/src/quickslice_client.gleam`

**Step 1: Add oauth_error field to Model type**

Find the `Model` type definition and add `oauth_error` field:

```gleam
pub type Model {
  Model(
    cache: squall_cache.Cache,
    registry: registry.Registry,
    route: Route,
    time_range: get_activity_buckets.TimeRange,
    settings_page_model: settings.Model,
    backfill_page_model: backfill.Model,
    backfill_status: backfill_polling.BackfillStatus,
    auth_state: AuthState,
    mobile_menu_open: Bool,
    login_autocomplete: actor_autocomplete.Model,
    oauth_error: option.Option(String),
  )
}
```

**Step 2: Add helper function to parse OAuth error from URI**

Add this function before `init`:

```gleam
fn parse_oauth_error(uri: uri.Uri) -> option.Option(String) {
  case uri.query {
    option.Some(query_string) -> {
      let params = uri.parse_query(query_string) |> result.unwrap([])
      case list.key_find(params, "error") {
        Ok("access_denied") -> option.Some("Login was cancelled")
        Ok(error) -> {
          let description =
            list.key_find(params, "error_description")
            |> result.unwrap(error)
            |> uri.percent_decode
            |> result.unwrap(error)
          option.Some("Login failed: " <> description)
        }
        Error(_) -> option.None
      }
    }
    option.None -> option.None
  }
}
```

**Step 3: Update init to parse OAuth error and initialize field**

In the `init` function, parse the OAuth error from initial URI and add to Model:

```gleam
fn init(_flags) -> #(Model, Effect(Msg)) {
  // ... existing code ...

  // Parse OAuth error from URL if present
  let oauth_error = case modem.initial_uri() {
    Ok(uri) -> parse_oauth_error(uri)
    Error(_) -> option.None
  }

  // ... existing code ...

  #(
    Model(
      cache: initial_cache,
      registry: reg,
      route: initial_route,
      time_range: ONEDAY,
      settings_page_model: settings.init(),
      backfill_page_model: backfill.init(),
      backfill_status: backfill_polling.Idle,
      auth_state: NotAuthenticated,
      mobile_menu_open: False,
      login_autocomplete: actor_autocomplete.init(),
      oauth_error: oauth_error,
    ),
    combined_effects,
  )
}
```

**Step 4: Add DismissOAuthError message**

Add to the `Msg` type:

```gleam
pub type Msg {
  // ... existing messages ...
  DismissOAuthError
}
```

**Step 5: Handle DismissOAuthError in update**

Add case in `update` function:

```gleam
DismissOAuthError -> {
  #(Model(..model, oauth_error: option.None), effect.none())
}
```

**Step 6: Import alert component**

Add import at top of file:

```gleam
import components/alert
```

**Step 7: Display OAuth error in view**

In the `view` function, add error display after the header:

```gleam
fn view(model: Model) -> Element(Msg) {
  // ... existing code ...

  html.div(
    [attribute.class("max-w-4xl mx-auto px-4 py-6 sm:px-6 sm:py-12")],
    [
      // Header (existing)
      case model.route {
        Onboarding -> element.none()
        _ -> layout.header(...)
      },
      // OAuth error alert (NEW)
      case model.oauth_error {
        option.Some(error_msg) -> alert.alert(alert.Error, error_msg)
        option.None -> element.none()
      },
      // Route content (existing)
      case model.route {
        Home -> view_home(model)
        // ...
      },
    ],
  )
}
```

**Step 8: Run the build to verify changes compile**

Run: `cd client && gleam build`
Expected: Build succeeds

**Step 9: Commit**

```bash
git add client/src/quickslice_client.gleam
git commit -m "feat: display OAuth error messages in client UI"
```

---

## Task 4: Manual Testing

**Step 1: Test admin OAuth error flow**

1. Start the server: `cd server && gleam run`
2. Navigate to `http://localhost:8080/onboarding`
3. Enter a handle and click login
4. On PDS authorization page, click "Deny"
5. Verify redirect to `/onboarding?error=access_denied&error_description=...`
6. Verify UI shows red alert: "Login was cancelled"

**Step 2: Test with existing admin**

1. Register as admin first (complete normal login)
2. Log out (clear cookies)
3. Try to login from `/` and deny
4. Verify redirect to `/?error=access_denied&...`
5. Verify UI shows error alert

**Step 3: Verify normal login still works**

1. Complete a normal login flow (accept authorization)
2. Verify login succeeds and redirects properly

---

## Summary

| Task | File | Change |
|------|------|--------|
| 1 | `server/src/handlers/admin_oauth_callback.gleam` | Check error param first, redirect to `/` or `/onboarding` |
| 2 | `server/src/handlers/oauth/atp_callback.gleam` | Check error param first, forward to client's redirect_uri |
| 3 | `client/src/quickslice_client.gleam` | Parse error from URL, display alert |
| 4 | Manual testing | Verify error and happy path flows |
