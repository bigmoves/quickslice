import gleam/http
import gleam/option.{None}
import gleeunit/should
import handlers/oauth/atp_callback
import lib/oauth/did_cache
import test_helpers
import wisp/simulate

pub fn atp_callback_missing_code_returns_400_test() {
  let assert Ok(cache) = did_cache.start()
  let assert Ok(exec) = test_helpers.create_test_db()

  let req = simulate.request(http.Get, "/oauth/atp/callback?state=test-state")
  let response =
    atp_callback.handle(
      req,
      exec,
      cache,
      "http://localhost:8080/oauth/atp/callback",
      "test-client-id",
      None,
    )

  response.status |> should.equal(400)
}

pub fn atp_callback_missing_state_returns_400_test() {
  let assert Ok(cache) = did_cache.start()
  let assert Ok(exec) = test_helpers.create_test_db()

  let req = simulate.request(http.Get, "/oauth/atp/callback?code=test-code")
  let response =
    atp_callback.handle(
      req,
      exec,
      cache,
      "http://localhost:8080/oauth/atp/callback",
      "test-client-id",
      None,
    )

  response.status |> should.equal(400)
}
