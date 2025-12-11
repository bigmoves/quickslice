import database/repositories/oauth_clients
import database/types
import gleam/http
import gleam/option.{None, Some}
import gleeunit/should
import handlers/oauth/authorize
import lib/oauth/did_cache
import test_helpers
import wisp/simulate

pub fn authorize_missing_query_returns_400_test() {
  // Start the DID cache actor for testing
  let assert Ok(cache) = did_cache.start()

  // Open an in-memory database for testing
  let assert Ok(exec) = test_helpers.create_test_db()

  let req = simulate.request(http.Get, "/oauth/authorize")
  let response =
    authorize.handle(
      req,
      exec,
      cache,
      "http://localhost:8080/oauth/callback",
      "test-client-id",
      option.None,
    )

  response.status |> should.equal(400)
}

pub fn authorize_invalid_scope_redirects_with_error_test() {
  let assert Ok(cache) = did_cache.start()
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_all_tables(exec)

  // Create a test client using the repository
  let test_client =
    types.OAuthClient(
      client_id: "test-client",
      client_secret: Some("secret"),
      client_name: "Test Client",
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

  // Authorization request with invalid scope - query is parsed from path
  // Include login_hint so we pass the login_hint check and get to scope validation
  let req =
    simulate.request(
      http.Get,
      "/oauth/authorize?response_type=code&client_id=test-client&redirect_uri=https://example.com/callback&scope=invalid:::&login_hint=did:plc:test123",
    )

  let response =
    authorize.handle(
      req,
      exec,
      cache,
      "http://localhost:8080/oauth/callback",
      "test-client-id",
      None,
    )

  // Per OAuth 2.0 spec (RFC 6749 Section 4.1.2.1), once redirect_uri is validated,
  // errors should be redirected to the client, not returned as 400
  response.status |> should.equal(303)
}
