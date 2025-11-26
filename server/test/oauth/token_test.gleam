import gleam/http
import gleeunit/should
import handlers/oauth/token
import sqlight
import wisp/simulate

pub fn token_missing_grant_type_returns_400_test() {
  let assert Ok(conn) = sqlight.open(":memory:")

  let req =
    simulate.request(http.Post, "/oauth/token")
    |> simulate.header("content-type", "application/x-www-form-urlencoded")
    |> simulate.string_body("client_id=test")

  let response = token.handle(req, conn)
  response.status |> should.equal(400)
}

pub fn token_unsupported_grant_type_returns_400_test() {
  let assert Ok(conn) = sqlight.open(":memory:")

  let req =
    simulate.request(http.Post, "/oauth/token")
    |> simulate.header("content-type", "application/x-www-form-urlencoded")
    |> simulate.string_body("grant_type=password&client_id=test")

  let response = token.handle(req, conn)
  response.status |> should.equal(400)
}
