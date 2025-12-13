# Add `sub` Claim to OAuth Token Response

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Return the user's DID as a `sub` claim in OAuth token responses so client SDKs can identify the authenticated user.

**Architecture:** Modify the `token_response` function to accept an optional `sub` parameter and include it in the JSON response. Update both call sites (authorization code grant and refresh token grant) to pass the user's DID.

**Tech Stack:** Gleam, wisp HTTP framework

---

## Task 1: Add Test for `sub` Claim in Token Response

**Files:**
- Modify: `server/test/oauth/token_test.gleam`

**Step 1: Write the failing test**

Add a new test at the end of the file that verifies the `sub` claim is present in a successful token response. This requires setting up a complete authorization code flow.

```gleam
pub fn token_response_includes_sub_claim_test() {
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_all_tables(exec)

  // Create a public client
  let test_client =
    types.OAuthClient(
      client_id: "test-client",
      client_secret: None,
      client_name: "Test Client",
      redirect_uris: ["https://example.com/callback"],
      grant_types: [types.AuthorizationCode, types.RefreshToken],
      response_types: [types.Code],
      scope: Some("atproto"),
      token_endpoint_auth_method: types.AuthNone,
      client_type: types.Public,
      created_at: 0,
      updated_at: 0,
      metadata: "{}",
      access_token_expiration: 3600,
      refresh_token_expiration: 2_592_000,
      require_redirect_exact: True,
      registration_access_token: None,
      jwks: None,
    )
  let assert Ok(_) = oauth_clients.insert(exec, test_client)

  // Create an authorization code with a DID as user_id
  let test_code =
    types.OAuthAuthorizationCode(
      code: "test-auth-code",
      client_id: "test-client",
      user_id: "did:plc:testuser123",
      session_id: Some("test-session"),
      session_iteration: Some(0),
      redirect_uri: "https://example.com/callback",
      scope: Some("atproto"),
      code_challenge: Some("E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"),
      code_challenge_method: Some("S256"),
      created_at: 0,
      expires_at: test_helpers.future_timestamp(600),
      used: False,
      dpop_jkt: None,
    )
  let assert Ok(_) = oauth_authorization_codes.insert(exec, test_code)

  // Exchange code for token
  let req =
    simulate.request(http.Post, "/oauth/token")
    |> simulate.header("content-type", "application/x-www-form-urlencoded")
    |> simulate.string_body(
      "grant_type=authorization_code&client_id=test-client&code=test-auth-code&redirect_uri=https://example.com/callback&code_verifier=dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk",
    )

  let response = token.handle(req, exec, "http://localhost:8080")

  response.status |> should.equal(200)

  case response.body {
    wisp.Text(response_body) -> {
      // Verify sub claim is present with the user's DID
      response_body |> string.contains("\"sub\"") |> should.be_true
      response_body |> string.contains("did:plc:testuser123") |> should.be_true
    }
    _ -> should.fail()
  }
}
```

**Step 2: Add required import**

Add `oauth_authorization_codes` to the imports at the top of the test file:

```gleam
import database/repositories/oauth_authorization_codes
```

**Step 3: Run test to verify it fails**

Run: `cd server && gleam test`

Expected: Test fails because `sub` is not in the response.

**Step 4: Commit**

```bash
git add server/test/oauth/token_test.gleam
git commit -m "test: add failing test for sub claim in token response"
```

---

## Task 2: Add `sub` Parameter to `token_response` Function

**Files:**
- Modify: `server/src/handlers/oauth/token.gleam:757-785`

**Step 1: Update function signature**

Change the `token_response` function to accept a `sub` parameter:

```gleam
fn token_response(
  access_token: String,
  token_type: String,
  expires_in: Int,
  refresh_token: Option(String),
  scope: Option(String),
  sub: Option(String),
) -> wisp.Response {
  let base_fields = [
    #("access_token", json.string(access_token)),
    #("token_type", json.string(token_type)),
    #("expires_in", json.int(expires_in)),
  ]

  let with_refresh = case refresh_token {
    Some(rt) -> list.append(base_fields, [#("refresh_token", json.string(rt))])
    None -> base_fields
  }

  let with_scope = case scope {
    Some(s) -> list.append(with_refresh, [#("scope", json.string(s))])
    None -> with_refresh
  }

  let with_sub = case sub {
    Some(s) -> list.append(with_scope, [#("sub", json.string(s))])
    None -> with_scope
  }

  wisp.response(200)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_header("cache-control", "no-store")
  |> wisp.set_header("pragma", "no-cache")
  |> wisp.set_body(wisp.Text(json.to_string(json.object(with_sub))))
}
```

**Step 2: Verify build fails**

Run: `cd server && gleam build`

Expected: Build fails because call sites don't pass the new `sub` argument.

---

## Task 3: Update Authorization Code Grant Call Site

**Files:**
- Modify: `server/src/handlers/oauth/token.gleam:370-376`

**Step 1: Pass `code.user_id` as `sub`**

Update the `token_response` call in the authorization code grant handler:

```gleam
                                      token_response(
                                        access_token_value,
                                        token_type_str,
                                        client.access_token_expiration,
                                        Some(refresh_token_value),
                                        code.scope,
                                        Some(code.user_id),
                                      )
```

**Step 2: Verify build still fails**

Run: `cd server && gleam build`

Expected: Build still fails because refresh token grant call site not updated yet.

---

## Task 4: Update Refresh Token Grant Call Site

**Files:**
- Modify: `server/src/handlers/oauth/token.gleam:583-589`

**Step 1: Pass `old_refresh_token.user_id` as `sub`**

Update the `token_response` call in the refresh token grant handler:

```gleam
                                            token_response(
                                              new_access_token_value,
                                              token_type_str,
                                              client.access_token_expiration,
                                              Some(new_refresh_token_value),
                                              scope,
                                              Some(old_refresh_token.user_id),
                                            )
```

**Step 2: Build and run tests**

Run: `cd server && gleam build && gleam test`

Expected: Build succeeds, all tests pass including the new `sub` claim test.

**Step 3: Commit**

```bash
git add server/src/handlers/oauth/token.gleam
git commit -m "feat: add sub claim to OAuth token response

The token endpoint now returns the user's DID as a 'sub' claim
in the token response. This is required by AT Protocol OAuth
clients to identify the authenticated user.

Fixes: missing sub claim causing client SDK getUser() to return null"
```

---

## Task 5: Add Test for `sub` Claim in Refresh Token Response

**Files:**
- Modify: `server/test/oauth/token_test.gleam`

**Step 1: Write the test**

Add a test to verify `sub` is also present when refreshing tokens:

```gleam
pub fn refresh_token_response_includes_sub_claim_test() {
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_all_tables(exec)

  // Create a public client
  let test_client =
    types.OAuthClient(
      client_id: "test-client",
      client_secret: None,
      client_name: "Test Client",
      redirect_uris: ["https://example.com/callback"],
      grant_types: [types.AuthorizationCode, types.RefreshToken],
      response_types: [types.Code],
      scope: Some("atproto"),
      token_endpoint_auth_method: types.AuthNone,
      client_type: types.Public,
      created_at: 0,
      updated_at: 0,
      metadata: "{}",
      access_token_expiration: 3600,
      refresh_token_expiration: 2_592_000,
      require_redirect_exact: True,
      registration_access_token: None,
      jwks: None,
    )
  let assert Ok(_) = oauth_clients.insert(exec, test_client)

  // Create a refresh token with a DID as user_id
  let test_refresh_token =
    types.OAuthRefreshToken(
      token: "test-refresh-token",
      access_token: "old-access-token",
      client_id: "test-client",
      user_id: "did:plc:refreshuser456",
      session_id: Some("test-session"),
      session_iteration: Some(0),
      scope: Some("atproto"),
      created_at: 0,
      expires_at: None,
      revoked: False,
    )
  let assert Ok(_) = oauth_refresh_tokens.insert(exec, test_refresh_token)

  // Refresh the token
  let req =
    simulate.request(http.Post, "/oauth/token")
    |> simulate.header("content-type", "application/x-www-form-urlencoded")
    |> simulate.string_body(
      "grant_type=refresh_token&client_id=test-client&refresh_token=test-refresh-token",
    )

  let response = token.handle(req, exec, "http://localhost:8080")

  response.status |> should.equal(200)

  case response.body {
    wisp.Text(response_body) -> {
      // Verify sub claim is present with the user's DID
      response_body |> string.contains("\"sub\"") |> should.be_true
      response_body |> string.contains("did:plc:refreshuser456") |> should.be_true
    }
    _ -> should.fail()
  }
}
```

**Step 2: Run tests**

Run: `cd server && gleam test`

Expected: All tests pass.

**Step 3: Commit**

```bash
git add server/test/oauth/token_test.gleam
git commit -m "test: verify sub claim in refresh token response"
```

---

## Summary

This fix adds a `sub` claim containing the user's DID to OAuth token responses. The change is minimal:

1. Add `sub: Option(String)` parameter to `token_response` function
2. Include `sub` in JSON response when present
3. Pass `code.user_id` (authorization code grant) or `old_refresh_token.user_id` (refresh token grant) to the function

The user ID is already the DID (set in `atp_callback.gleam` from `session.did`), so no additional lookups are needed.
