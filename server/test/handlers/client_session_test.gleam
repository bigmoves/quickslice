/// Tests for client session HTTP handlers
import database/executor
import database/repositories/client_session as client_session_repo
import database/sqlite/connection as db_connection
import gleam/erlang/process
import gleam/http
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import handlers/client_session
import lib/oauth/did_cache
import wisp/simulate

fn setup_test_db() {
  let assert Ok(exec) = db_connection.connect("sqlite::memory:")

  // Create required tables
  let assert Ok(_) =
    executor.exec(
      exec,
      "CREATE TABLE config (
        key TEXT PRIMARY KEY,
        value TEXT NOT NULL,
        updated_at INTEGER
      )",
      [],
    )

  let assert Ok(_) =
    executor.exec(
      exec,
      "CREATE TABLE client_session (
        session_id TEXT PRIMARY KEY,
        client_id TEXT NOT NULL,
        user_did TEXT,
        atp_session_id TEXT,
        dpop_jkt TEXT NOT NULL,
        created_at INTEGER NOT NULL,
        last_activity_at INTEGER NOT NULL
      )",
      [],
    )

  let assert Ok(_) =
    executor.exec(
      exec,
      "CREATE TABLE oauth_access_token (
        token TEXT PRIMARY KEY,
        token_type TEXT NOT NULL,
        client_id TEXT NOT NULL,
        user_id TEXT,
        session_id TEXT,
        session_iteration INTEGER,
        scope TEXT,
        created_at INTEGER NOT NULL,
        expires_at INTEGER NOT NULL,
        revoked INTEGER NOT NULL DEFAULT 0,
        dpop_jkt TEXT
      )",
      [],
    )

  exec
}

fn setup_did_cache() -> process.Subject(did_cache.Message) {
  let assert Ok(cache) = did_cache.start()
  cache
}

// ===== POST /api/client/session Tests =====

pub fn create_session_returns_200_test() {
  let exec = setup_test_db()
  let did_cache = setup_did_cache()

  let body = "{\"clientId\": \"test-client\", \"dpopJkt\": \"jkt-123\"}"
  let req =
    simulate.request(http.Post, "/api/client/session")
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/json")

  let response = client_session.handle(req, exec, did_cache)

  response.status |> should.equal(200)
}

pub fn create_session_sets_cookie_test() {
  let exec = setup_test_db()
  let did_cache = setup_did_cache()

  let body = "{\"clientId\": \"test-client\", \"dpopJkt\": \"jkt-123\"}"
  let req =
    simulate.request(http.Post, "/api/client/session")
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/json")

  let response = client_session.handle(req, exec, did_cache)

  // Check for set-cookie header
  let has_cookie =
    response.headers
    |> has_header_starting_with("set-cookie", "quickslice_client_session=")

  has_cookie |> should.be_true()
}

pub fn create_session_with_user_did_test() {
  let exec = setup_test_db()
  let did_cache = setup_did_cache()

  let body =
    "{\"clientId\": \"test-client\", \"dpopJkt\": \"jkt-123\", \"userDid\": \"did:plc:test\", \"atpSessionId\": \"atp-123\"}"
  let req =
    simulate.request(http.Post, "/api/client/session")
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/json")

  let response = client_session.handle(req, exec, did_cache)

  response.status |> should.equal(200)
}

pub fn create_session_missing_client_id_returns_400_test() {
  let exec = setup_test_db()
  let did_cache = setup_did_cache()

  let body = "{\"dpopJkt\": \"jkt-123\"}"
  let req =
    simulate.request(http.Post, "/api/client/session")
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/json")

  let response = client_session.handle(req, exec, did_cache)

  response.status |> should.equal(400)
}

pub fn create_session_missing_dpop_jkt_returns_400_test() {
  let exec = setup_test_db()
  let did_cache = setup_did_cache()

  let body = "{\"clientId\": \"test-client\"}"
  let req =
    simulate.request(http.Post, "/api/client/session")
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/json")

  let response = client_session.handle(req, exec, did_cache)

  response.status |> should.equal(400)
}

pub fn create_session_invalid_json_returns_400_test() {
  let exec = setup_test_db()
  let did_cache = setup_did_cache()

  let body = "not json"
  let req =
    simulate.request(http.Post, "/api/client/session")
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/json")

  let response = client_session.handle(req, exec, did_cache)

  response.status |> should.equal(400)
}

// ===== GET /api/client/session Tests =====

pub fn get_session_without_cookie_returns_unauthenticated_test() {
  let exec = setup_test_db()
  let did_cache = setup_did_cache()

  let req = simulate.request(http.Get, "/api/client/session")

  let response = client_session.handle(req, exec, did_cache)

  response.status |> should.equal(200)
  // Response should indicate not authenticated
}

pub fn get_session_with_invalid_cookie_returns_unauthenticated_test() {
  let exec = setup_test_db()
  let did_cache = setup_did_cache()

  let req =
    simulate.request(http.Get, "/api/client/session")
    |> simulate.header("cookie", "quickslice_client_session=invalid")

  let response = client_session.handle(req, exec, did_cache)

  response.status |> should.equal(200)
  // Response should indicate not authenticated (invalid/expired cookie)
}

// ===== DELETE /api/client/session Tests =====

pub fn delete_session_returns_200_test() {
  let exec = setup_test_db()
  let did_cache = setup_did_cache()

  let req = simulate.request(http.Delete, "/api/client/session")

  let response = client_session.handle(req, exec, did_cache)

  response.status |> should.equal(200)
}

pub fn delete_session_clears_cookie_test() {
  let exec = setup_test_db()
  let did_cache = setup_did_cache()

  // First create a session
  let assert Ok(_) =
    client_session_repo.insert(
      exec,
      "test-session",
      "test-client",
      Some("did:plc:test"),
      None,
      "jkt",
    )

  let req = simulate.request(http.Delete, "/api/client/session")

  let response = client_session.handle(req, exec, did_cache)

  // Check for set-cookie header that clears the cookie
  let has_cookie =
    response.headers
    |> has_header_starting_with("set-cookie", "quickslice_client_session=")

  has_cookie |> should.be_true()
}

// ===== Method Not Allowed Tests =====

pub fn put_returns_405_test() {
  let exec = setup_test_db()
  let did_cache = setup_did_cache()

  let req = simulate.request(http.Put, "/api/client/session")

  let response = client_session.handle(req, exec, did_cache)

  response.status |> should.equal(405)
}

pub fn patch_returns_405_test() {
  let exec = setup_test_db()
  let did_cache = setup_did_cache()

  let req = simulate.request(http.Patch, "/api/client/session")

  let response = client_session.handle(req, exec, did_cache)

  response.status |> should.equal(405)
}

// ===== Helper Functions =====

fn has_header_starting_with(
  headers: List(#(String, String)),
  name: String,
  value_prefix: String,
) -> Bool {
  case headers {
    [] -> False
    [#(n, v), ..rest] ->
      case string.lowercase(n) == string.lowercase(name) {
        True ->
          case string.starts_with(v, value_prefix) {
            True -> True
            False -> has_header_starting_with(rest, name, value_prefix)
          }
        False -> has_header_starting_with(rest, name, value_prefix)
      }
  }
}
