import database/repositories/oauth_clients
import database/schema/tables
import database/types
import gleam/http
import gleam/option.{None, Some}
import gleeunit/should
import handlers/oauth/authorize
import lib/oauth/did_cache
import sqlight
import wisp/simulate

pub fn authorize_missing_query_returns_400_test() {
  // Start the DID cache actor for testing
  let assert Ok(cache) = did_cache.start()

  // Open an in-memory database for testing
  let assert Ok(conn) = sqlight.open(":memory:")

  let req = simulate.request(http.Get, "/oauth/authorize")
  let response =
    authorize.handle(
      req,
      conn,
      cache,
      "http://localhost:8080/oauth/callback",
      "test-client-id",
      option.None,
    )

  response.status |> should.equal(400)
}

pub fn authorize_invalid_scope_returns_400_test() {
  let assert Ok(cache) = did_cache.start()
  let assert Ok(conn) = sqlight.open(":memory:")
  let assert Ok(_) = tables.create_oauth_client_table(conn)

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
  let assert Ok(_) = oauth_clients.insert(conn, test_client)

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
      conn,
      cache,
      "http://localhost:8080/oauth/callback",
      "test-client-id",
      None,
    )

  // Should return 400 due to invalid scope format
  response.status |> should.equal(400)
}
