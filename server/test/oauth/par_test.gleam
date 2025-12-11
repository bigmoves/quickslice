import database/executor.{type Executor}
import database/sqlite/connection as db_connection
import gleam/http
import gleam/string
import gleeunit/should
import handlers/oauth/par
import wisp
import wisp/simulate

fn setup_test_db() -> Executor {
  let assert Ok(exec) = db_connection.connect("sqlite::memory:")

  // Create oauth_client table
  let assert Ok(_) =
    executor.exec(
      exec,
      "CREATE TABLE oauth_client (
        client_id TEXT PRIMARY KEY,
        client_secret TEXT,
        client_name TEXT NOT NULL,
        redirect_uris TEXT NOT NULL,
        grant_types TEXT NOT NULL,
        response_types TEXT NOT NULL,
        scope TEXT,
        token_endpoint_auth_method TEXT NOT NULL,
        client_type TEXT NOT NULL,
        created_at INTEGER NOT NULL,
        updated_at INTEGER NOT NULL,
        metadata TEXT NOT NULL,
        access_token_expiration INTEGER NOT NULL,
        refresh_token_expiration INTEGER NOT NULL,
        require_redirect_exact INTEGER NOT NULL,
        registration_access_token TEXT,
        jwks TEXT
      )",
      [],
    )

  // Create oauth_par_request table
  let assert Ok(_) =
    executor.exec(
      exec,
      "CREATE TABLE oauth_par_request (
        request_uri TEXT PRIMARY KEY,
        authorization_request TEXT NOT NULL,
        client_id TEXT NOT NULL,
        created_at INTEGER NOT NULL,
        expires_at INTEGER NOT NULL,
        subject TEXT,
        metadata TEXT NOT NULL
      )",
      [],
    )

  // Insert a test client
  let assert Ok(_) =
    executor.exec(
      exec,
      "INSERT INTO oauth_client (client_id, client_name, redirect_uris, grant_types, response_types, token_endpoint_auth_method, client_type, created_at, updated_at, metadata, access_token_expiration, refresh_token_expiration, require_redirect_exact) VALUES ('test_client', 'Test', '[\"https://example.com/callback\"]', '[\"authorization_code\"]', '[\"code\"]', 'client_secret_post', 'confidential', 0, 0, '{}', 3600, 86400, 1)",
      [],
    )

  exec
}

pub fn par_valid_request_test() {
  let exec = setup_test_db()

  let body =
    "client_id=test_client&response_type=code&redirect_uri=https://example.com/callback"

  let req =
    simulate.request(http.Post, "/oauth/par")
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/x-www-form-urlencoded")

  let response = par.handle(req, exec)

  // Should return 201 Created
  response.status |> should.equal(201)

  // Body should contain request_uri
  case response.body {
    wisp.Text(response_body) -> {
      response_body |> string.contains("request_uri") |> should.be_true
      response_body |> string.contains("expires_in") |> should.be_true
    }
    _ -> should.fail()
  }
}

pub fn par_missing_client_id_test() {
  let exec = setup_test_db()

  let body = "response_type=code&redirect_uri=https://example.com/callback"

  let req =
    simulate.request(http.Post, "/oauth/par")
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/x-www-form-urlencoded")

  let response = par.handle(req, exec)

  // Should return 400 Bad Request
  response.status |> should.equal(400)
}
