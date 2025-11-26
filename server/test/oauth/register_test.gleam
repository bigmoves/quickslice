import database/schema/tables
import gleam/http
import gleam/json
import gleam/string
import gleeunit/should
import handlers/oauth/register
import sqlight
import wisp
import wisp/simulate

pub fn register_valid_client_test() {
  let assert Ok(conn) = sqlight.open(":memory:")
  let assert Ok(_) = tables.create_oauth_client_table(conn)

  let body =
    json.object([
      #("client_name", json.string("Test Client")),
      #(
        "redirect_uris",
        json.array([json.string("https://example.com/callback")], fn(x) { x }),
      ),
    ])
    |> json.to_string

  let req =
    simulate.request(http.Post, "/oauth/register")
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/json")

  let response = register.handle(req, conn)

  response.status |> should.equal(201)

  case response.body {
    wisp.Text(response_body) -> {
      response_body |> string.contains("client_id") |> should.be_true
      response_body |> string.contains("Test Client") |> should.be_true
    }
    _ -> should.fail()
  }
}

pub fn register_missing_redirect_uris_test() {
  let assert Ok(conn) = sqlight.open(":memory:")

  let body =
    json.object([
      #("client_name", json.string("Test Client")),
      #("redirect_uris", json.array([], fn(x) { x })),
    ])
    |> json.to_string

  let req =
    simulate.request(http.Post, "/oauth/register")
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/json")

  let response = register.handle(req, conn)

  response.status |> should.equal(400)
}

pub fn register_http_non_localhost_redirect_uri_rejected_test() {
  let assert Ok(conn) = sqlight.open(":memory:")
  let assert Ok(_) = tables.create_oauth_client_table(conn)

  // http:// on a non-localhost domain should be rejected
  let body =
    json.object([
      #("client_name", json.string("Test Client")),
      #(
        "redirect_uris",
        json.array([json.string("http://example.com/callback")], fn(x) { x }),
      ),
    ])
    |> json.to_string

  let req =
    simulate.request(http.Post, "/oauth/register")
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/json")

  let response = register.handle(req, conn)

  // Should return 400 Bad Request because HTTP is only allowed for localhost
  response.status |> should.equal(400)

  case response.body {
    wisp.Text(response_body) -> {
      response_body |> string.contains("redirect") |> should.be_true
    }
    _ -> should.fail()
  }
}

pub fn register_http_localhost_redirect_uri_allowed_test() {
  let assert Ok(conn) = sqlight.open(":memory:")
  let assert Ok(_) = tables.create_oauth_client_table(conn)

  // http://localhost should be allowed
  let body =
    json.object([
      #("client_name", json.string("Test Client")),
      #(
        "redirect_uris",
        json.array([json.string("http://localhost:3000/callback")], fn(x) { x }),
      ),
    ])
    |> json.to_string

  let req =
    simulate.request(http.Post, "/oauth/register")
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/json")

  let response = register.handle(req, conn)

  // Should return 201 Created - localhost http is allowed
  response.status |> should.equal(201)
}
