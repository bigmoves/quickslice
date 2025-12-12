import database/repositories/oauth_clients
import database/repositories/oauth_refresh_tokens
import database/types
import gleam/http
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import handlers/oauth/token
import test_helpers
import wisp
import wisp/simulate

pub fn token_missing_grant_type_returns_400_test() {
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_all_tables(exec)

  let req =
    simulate.request(http.Post, "/oauth/token")
    |> simulate.header("content-type", "application/x-www-form-urlencoded")
    |> simulate.string_body("client_id=test")

  let response = token.handle(req, exec, "http://localhost:8080")
  response.status |> should.equal(400)
}

pub fn token_unsupported_grant_type_returns_400_test() {
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_all_tables(exec)

  let req =
    simulate.request(http.Post, "/oauth/token")
    |> simulate.header("content-type", "application/x-www-form-urlencoded")
    |> simulate.string_body("grant_type=password&client_id=test")

  let response = token.handle(req, exec, "http://localhost:8080")
  response.status |> should.equal(400)
}

pub fn token_refresh_invalid_scope_returns_400_test() {
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_all_tables(exec)
  let assert Ok(_) = test_helpers.create_all_tables(exec)

  // Create a test client
  let test_client =
    types.OAuthClient(
      client_id: "test-client",
      client_secret: Some("secret"),
      client_name: "Test Client",
      redirect_uris: ["https://example.com/callback"],
      grant_types: [types.AuthorizationCode, types.RefreshToken],
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
  let assert Ok(_) = oauth_clients.insert(exec, test_client)

  // Create a test refresh token
  let test_refresh_token =
    types.OAuthRefreshToken(
      token: "test-refresh-token",
      access_token: "test-access-token",
      client_id: "test-client",
      user_id: "test-user",
      session_id: Some("test-session"),
      session_iteration: Some(0),
      scope: Some("atproto"),
      created_at: 0,
      expires_at: None,
      revoked: False,
    )
  let assert Ok(_) = oauth_refresh_tokens.insert(exec, test_refresh_token)

  // Refresh token request with invalid scope
  let req =
    simulate.request(http.Post, "/oauth/token")
    |> simulate.header("content-type", "application/x-www-form-urlencoded")
    |> simulate.string_body(
      "grant_type=refresh_token&client_id=test-client&refresh_token=test-refresh-token&scope=invalid:::",
    )

  let response = token.handle(req, exec, "http://localhost:8080")

  // Should return 400 with invalid_scope error
  response.status |> should.equal(400)

  case response.body {
    wisp.Text(response_body) -> {
      response_body |> string.contains("invalid_scope") |> should.be_true
    }
    _ -> should.fail()
  }
}

pub fn token_confidential_client_missing_secret_returns_401_test() {
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_all_tables(exec)
  let assert Ok(_) = test_helpers.create_all_tables(exec)

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
  let assert Ok(_) = oauth_clients.insert(exec, test_client)

  // Token request without client_secret
  let req =
    simulate.request(http.Post, "/oauth/token")
    |> simulate.header("content-type", "application/x-www-form-urlencoded")
    |> simulate.string_body(
      "grant_type=authorization_code&client_id=confidential-client&code=test-code&redirect_uri=https://example.com/callback&code_verifier=test",
    )

  let response = token.handle(req, exec, "http://localhost:8080")

  // Should return 401 unauthorized
  response.status |> should.equal(401)

  case response.body {
    wisp.Text(response_body) -> {
      response_body |> string.contains("invalid_client") |> should.be_true
    }
    _ -> should.fail()
  }
}

pub fn token_confidential_client_wrong_secret_returns_401_test() {
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_all_tables(exec)

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
  let assert Ok(_) = oauth_clients.insert(exec, test_client)

  // Token request with wrong client_secret
  let req =
    simulate.request(http.Post, "/oauth/token")
    |> simulate.header("content-type", "application/x-www-form-urlencoded")
    |> simulate.string_body(
      "grant_type=authorization_code&client_id=confidential-client&client_secret=wrong-secret&code=test-code&redirect_uri=https://example.com/callback",
    )

  let response = token.handle(req, exec, "http://localhost:8080")

  response.status |> should.equal(401)

  case response.body {
    wisp.Text(response_body) -> {
      response_body |> string.contains("invalid_client") |> should.be_true
    }
    _ -> should.fail()
  }
}

pub fn token_public_client_no_secret_required_test() {
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_all_tables(exec)

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
  let assert Ok(_) = oauth_clients.insert(exec, test_client)

  // Token request without client_secret - should not fail on auth
  // (will fail later due to missing code, but that's fine for this test)
  let req =
    simulate.request(http.Post, "/oauth/token")
    |> simulate.header("content-type", "application/x-www-form-urlencoded")
    |> simulate.string_body(
      "grant_type=authorization_code&client_id=public-client&code=test-code&redirect_uri=https://example.com/callback&code_verifier=test",
    )

  let response = token.handle(req, exec, "http://localhost:8080")

  // Should NOT be 401 (auth passed, will fail on invalid code instead)
  response.status |> should.not_equal(401)
}
