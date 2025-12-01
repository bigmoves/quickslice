# Client Secret Validation Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Enforce client_secret validation at the token endpoint for confidential clients.

**Architecture:** Add a `validate_client_authentication` function that checks the client's `token_endpoint_auth_method` and verifies credentials accordingly. Called early in both `handle_authorization_code` and `handle_refresh_token` flows.

**Tech Stack:** Gleam, wisp, existing OAuth types and test patterns.

---

## Task 1: Add Tests for Client Secret Validation

**Files:**
- Modify: `server/test/oauth/token_test.gleam`

**Step 1: Add test for confidential client without secret**

```gleam
pub fn token_confidential_client_missing_secret_returns_401_test() {
  let assert Ok(conn) = sqlight.open(":memory:")
  let assert Ok(_) = tables.create_oauth_client_table(conn)
  let assert Ok(_) = tables.create_oauth_authorization_code_table(conn)

  // Create a confidential client that requires client_secret_post
  let test_client =
    types.OAuthClient(
      client_id: "confidential-client",
      client_secret: Some("super-secret"),
      client_name: "Confidential Client",
      redirect_uris: ["https://example.com/callback"],
      grant_types: [types.AuthorizationCode],
      response_types: [types.Code],
      scope: Some("atproto"),
      token_endpoint_auth_method: types.ClientSecretPost,
      client_type: types.Confidential,
      created_at: 0,
      updated_at: 0,
      metadata: "{}",
      access_token_expiration: 3600,
      refresh_token_expiration: 2_592_000,
      require_redirect_exact: True,
      registration_access_token: None,
      jwks: None,
    )
  let assert Ok(_) = oauth_clients.insert(conn, test_client)

  // Token request without client_secret
  let req =
    simulate.request(http.Post, "/oauth/token")
    |> simulate.header("content-type", "application/x-www-form-urlencoded")
    |> simulate.string_body(
      "grant_type=authorization_code&client_id=confidential-client&code=test-code&redirect_uri=https://example.com/callback&code_verifier=test",
    )

  let response = token.handle(req, conn)

  // Should return 401 unauthorized
  response.status |> should.equal(401)

  case response.body {
    wisp.Text(response_body) -> {
      response_body |> string.contains("invalid_client") |> should.be_true
    }
    _ -> should.fail()
  }
}
```

**Step 2: Add test for confidential client with wrong secret**

```gleam
pub fn token_confidential_client_wrong_secret_returns_401_test() {
  let assert Ok(conn) = sqlight.open(":memory:")
  let assert Ok(_) = tables.create_oauth_client_table(conn)

  let test_client =
    types.OAuthClient(
      client_id: "confidential-client",
      client_secret: Some("super-secret"),
      client_name: "Confidential Client",
      redirect_uris: ["https://example.com/callback"],
      grant_types: [types.AuthorizationCode],
      response_types: [types.Code],
      scope: Some("atproto"),
      token_endpoint_auth_method: types.ClientSecretPost,
      client_type: types.Confidential,
      created_at: 0,
      updated_at: 0,
      metadata: "{}",
      access_token_expiration: 3600,
      refresh_token_expiration: 2_592_000,
      require_redirect_exact: True,
      registration_access_token: None,
      jwks: None,
    )
  let assert Ok(_) = oauth_clients.insert(conn, test_client)

  // Token request with wrong client_secret
  let req =
    simulate.request(http.Post, "/oauth/token")
    |> simulate.header("content-type", "application/x-www-form-urlencoded")
    |> simulate.string_body(
      "grant_type=authorization_code&client_id=confidential-client&client_secret=wrong-secret&code=test-code&redirect_uri=https://example.com/callback",
    )

  let response = token.handle(req, conn)

  response.status |> should.equal(401)

  case response.body {
    wisp.Text(response_body) -> {
      response_body |> string.contains("invalid_client") |> should.be_true
    }
    _ -> should.fail()
  }
}
```

**Step 3: Add test for public client (no secret required)**

```gleam
pub fn token_public_client_no_secret_required_test() {
  let assert Ok(conn) = sqlight.open(":memory:")
  let assert Ok(_) = tables.create_oauth_client_table(conn)

  // Create a public client with auth_method = none
  let test_client =
    types.OAuthClient(
      client_id: "public-client",
      client_secret: None,
      client_name: "Public Client",
      redirect_uris: ["https://example.com/callback"],
      grant_types: [types.AuthorizationCode],
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
  let assert Ok(_) = oauth_clients.insert(conn, test_client)

  // Token request without client_secret - should not fail on auth
  // (will fail later due to missing code, but that's fine for this test)
  let req =
    simulate.request(http.Post, "/oauth/token")
    |> simulate.header("content-type", "application/x-www-form-urlencoded")
    |> simulate.string_body(
      "grant_type=authorization_code&client_id=public-client&code=test-code&redirect_uri=https://example.com/callback&code_verifier=test",
    )

  let response = token.handle(req, conn)

  // Should NOT be 401 (auth passed, will fail on invalid code instead)
  response.status |> should.not_equal(401)
}
```

**Step 4: Run tests to verify they fail**

Run: `gleam test -- --only token`
Expected: New tests FAIL (validation not implemented yet)

**Step 5: Commit test file**

```bash
git add server/test/oauth/token_test.gleam
git commit -m "test(oauth): add client secret validation tests"
```

---

## Task 2: Implement Client Authentication Validation

**Files:**
- Modify: `server/src/handlers/oauth/token.gleam`

**Step 1: Add validate_client_authentication function**

Add after the existing imports and before `handle`:

```gleam
/// Validate client authentication based on token_endpoint_auth_method
fn validate_client_authentication(
  client: types.OAuthClient,
  params: List(#(String, String)),
) -> Result(Nil, wisp.Response) {
  case client.token_endpoint_auth_method {
    types.AuthNone -> {
      // Public clients don't need authentication
      Ok(Nil)
    }
    types.ClientSecretPost -> {
      // Client secret must be in POST body
      case get_param(params, "client_secret") {
        None ->
          Error(error_response(
            401,
            "invalid_client",
            "client_secret is required for confidential clients",
          ))
        Some(provided_secret) -> {
          case client.client_secret {
            None ->
              Error(error_response(
                500,
                "server_error",
                "Client has no secret configured",
              ))
            Some(stored_secret) -> {
              case provided_secret == stored_secret {
                True -> Ok(Nil)
                False ->
                  Error(error_response(
                    401,
                    "invalid_client",
                    "Invalid client credentials",
                  ))
              }
            }
          }
        }
      }
    }
    types.ClientSecretBasic -> {
      // TODO: Implement Basic auth header parsing
      // For now, fall back to checking POST body
      case get_param(params, "client_secret") {
        None ->
          Error(error_response(
            401,
            "invalid_client",
            "client_secret is required (Basic auth not yet supported)",
          ))
        Some(provided_secret) -> {
          case client.client_secret {
            None ->
              Error(error_response(
                500,
                "server_error",
                "Client has no secret configured",
              ))
            Some(stored_secret) -> {
              case provided_secret == stored_secret {
                True -> Ok(Nil)
                False ->
                  Error(error_response(
                    401,
                    "invalid_client",
                    "Invalid client credentials",
                  ))
              }
            }
          }
        }
      }
    }
    types.PrivateKeyJwt -> {
      // TODO: Implement JWT client authentication
      Error(error_response(
        501,
        "unsupported_auth_method",
        "private_key_jwt authentication not yet implemented",
      ))
    }
  }
}
```

**Step 2: Add validation call in handle_authorization_code**

Find this section (around line 82):
```gleam
        Ok(Some(client)) -> {
          // Get authorization code
```

Change to:
```gleam
        Ok(Some(client)) -> {
          // Validate client authentication
          case validate_client_authentication(client, params) {
            Error(err) -> err
            Ok(_) -> {
              // Get authorization code
```

And add closing braces at the end of the function (before the final `}` that closes the `Ok(Some(client))` case).

**Step 3: Add validation call in handle_refresh_token**

Find this section (around line 235):
```gleam
              Ok(Some(client)) -> {
                // Get refresh token
```

Change to:
```gleam
              Ok(Some(client)) -> {
                // Validate client authentication
                case validate_client_authentication(client, params) {
                  Error(err) -> err
                  Ok(_) -> {
                    // Get refresh token
```

And add closing braces at the end.

**Step 4: Import types module**

Ensure `types` is available - update the import if needed:
```gleam
import database/types
```

**Step 5: Run tests**

Run: `gleam test -- --only token`
Expected: All tests PASS

**Step 6: Commit implementation**

```bash
git add server/src/handlers/oauth/token.gleam
git commit -m "feat(oauth): enforce client_secret for confidential clients"
```

---

## Task 3: Manual Verification

**Step 1: Build and run server**

Run: `gleam run`

**Step 2: Test with confidential client without secret**

```bash
curl -X POST http://localhost:8080/oauth/token \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "grant_type=authorization_code&client_id=YOUR_CONFIDENTIAL_CLIENT&code=test&redirect_uri=http://127.0.0.1:3000/"
```

Expected: `401` with `invalid_client` error

**Step 3: Test with public client**

Create a public client in the UI, then test token exchange works without secret (will fail on invalid code, but not on auth).

**Step 4: Final commit if any fixes needed**

```bash
git add -A
git commit -m "fix(oauth): address issues found in manual testing"
```

---

## Summary

| Task | Description | Files |
|------|-------------|-------|
| 1 | Add tests for client secret validation | `server/test/oauth/token_test.gleam` |
| 2 | Implement validate_client_authentication | `server/src/handlers/oauth/token.gleam` |
| 3 | Manual verification | N/A |

**Total commits:** 2-3 focused commits following conventional commit format.
