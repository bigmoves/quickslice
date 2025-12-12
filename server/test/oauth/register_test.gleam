import gleam/http
import gleam/json
import gleam/string
import gleeunit/should
import handlers/oauth/register
import test_helpers
import wisp
import wisp/simulate

pub fn register_valid_client_test() {
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_all_tables(exec)

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

  let response = register.handle(req, exec)

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
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_all_tables(exec)

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

  let response = register.handle(req, exec)

  response.status |> should.equal(400)
}

pub fn register_http_non_localhost_redirect_uri_rejected_test() {
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_all_tables(exec)

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

  let response = register.handle(req, exec)

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
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_all_tables(exec)

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

  let response = register.handle(req, exec)

  // Should return 201 Created - localhost http is allowed
  response.status |> should.equal(201)
}

pub fn register_valid_scope_test() {
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_all_tables(exec)

  let body =
    json.object([
      #("client_name", json.string("Test Client")),
      #(
        "redirect_uris",
        json.array([json.string("https://example.com/callback")], fn(x) { x }),
      ),
      #("scope", json.string("atproto repo:* account:email")),
    ])
    |> json.to_string

  let req =
    simulate.request(http.Post, "/oauth/register")
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/json")

  let response = register.handle(req, exec)

  response.status |> should.equal(201)

  case response.body {
    wisp.Text(response_body) -> {
      response_body |> string.contains("client_id") |> should.be_true
      response_body
      |> string.contains("atproto repo:* account:email")
      |> should.be_true
    }
    _ -> should.fail()
  }
}

pub fn register_invalid_scope_test() {
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_all_tables(exec)

  let body =
    json.object([
      #("client_name", json.string("Test Client")),
      #(
        "redirect_uris",
        json.array([json.string("https://example.com/callback")], fn(x) { x }),
      ),
      #("scope", json.string("atproto invalid:::")),
    ])
    |> json.to_string

  let req =
    simulate.request(http.Post, "/oauth/register")
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/json")

  let response = register.handle(req, exec)

  // Should return 400 with invalid_scope error
  response.status |> should.equal(400)

  case response.body {
    wisp.Text(response_body) -> {
      response_body |> string.contains("invalid_scope") |> should.be_true
    }
    _ -> should.fail()
  }
}
