/// Tests for client session management module
import database/executor
import database/repositories/client_session as client_session_repo
import database/repositories/config as config_repo
import database/sqlite/connection as db_connection
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import lib/client_session

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

// ===== Session ID Generation Tests =====

pub fn generate_session_id_returns_non_empty_string_test() {
  let session_id = client_session.generate_session_id()
  { string.length(session_id) > 0 } |> should.be_true()
}

pub fn generate_session_id_is_url_safe_test() {
  let session_id = client_session.generate_session_id()

  // Should not contain characters that need URL encoding
  let contains_unsafe =
    string.contains(session_id, "+")
    || string.contains(session_id, "/")
    || string.contains(session_id, "=")

  contains_unsafe |> should.be_false()
}

pub fn generate_session_id_is_unique_test() {
  let id1 = client_session.generate_session_id()
  let id2 = client_session.generate_session_id()
  let id3 = client_session.generate_session_id()

  { id1 != id2 } |> should.be_true()
  { id2 != id3 } |> should.be_true()
  { id1 != id3 } |> should.be_true()
}

pub fn generate_session_id_has_sufficient_length_test() {
  let session_id = client_session.generate_session_id()

  // 32 bytes base64url encoded should be ~43 characters
  { string.length(session_id) >= 40 } |> should.be_true()
}

// ===== Create Session Tests =====

pub fn create_session_returns_session_id_test() {
  let exec = setup_test_db()

  let result =
    client_session.create_session(exec, "client-123", None, None, "jkt-abc")

  result |> should.be_ok()

  let assert Ok(session_id) = result
  { string.length(session_id) > 0 } |> should.be_true()
}

pub fn create_session_stores_in_database_test() {
  let exec = setup_test_db()

  let assert Ok(session_id) =
    client_session.create_session(
      exec,
      "client-456",
      Some("did:plc:user"),
      Some("atp-123"),
      "jkt-xyz",
    )

  // Verify it's in the database
  let assert Ok(Some(session)) = client_session_repo.get(exec, session_id)
  session.client_id |> should.equal("client-456")
  session.user_did |> should.equal(Some("did:plc:user"))
  session.atp_session_id |> should.equal(Some("atp-123"))
  session.dpop_jkt |> should.equal("jkt-xyz")
}

// ===== Verify DPoP JKT Tests =====

// Note: These tests would require setting up wisp request context which is complex.
// The verify_dpop_jkt function requires extracting session from signed cookie.
// For now, we test the underlying repository operations which are covered in
// the repository tests.

// ===== Cookie Config Integration Tests =====

pub fn cookie_secure_auto_mode_test() {
  let exec = setup_test_db()

  // Default should be Auto
  let mode = config_repo.get_cookie_secure(exec)
  mode |> should.equal(config_repo.Auto)
}

pub fn cookie_same_site_strict_mode_test() {
  let exec = setup_test_db()

  // Default should be Strict
  let mode = config_repo.get_cookie_same_site(exec)
  mode |> should.equal(config_repo.Strict)
}

pub fn set_cookie_config_test() {
  let exec = setup_test_db()

  // Set cookie config
  let assert Ok(_) = config_repo.set_cookie_same_site(exec, config_repo.Lax)
  let assert Ok(_) = config_repo.set_cookie_secure(exec, config_repo.Always)
  let assert Ok(_) = config_repo.set_cookie_domain(exec, ".example.com")

  // Verify
  config_repo.get_cookie_same_site(exec) |> should.equal(config_repo.Lax)
  config_repo.get_cookie_secure(exec) |> should.equal(config_repo.Always)

  let assert Ok(domain) = config_repo.get_cookie_domain(exec)
  domain |> should.equal(".example.com")
}
