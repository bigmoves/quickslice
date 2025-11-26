import gleam/http
import gleam/option
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
