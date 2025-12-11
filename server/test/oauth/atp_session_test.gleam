import database/executor.{type Executor}
import database/sqlite/connection as db_connection
import gleam/http
import gleeunit/should
import handlers/oauth/atp_session
import wisp/simulate

fn setup_tables(exec: Executor) {
  // Create oauth_access_token table
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
  Nil
}

pub fn atp_session_missing_auth_returns_401_test() {
  let assert Ok(exec) = db_connection.connect("sqlite::memory:")
  setup_tables(exec)

  let req = simulate.request(http.Get, "/api/atp/sessions/test-session")
  let response = atp_session.handle(req, exec, "test-session")

  response.status |> should.equal(401)
}

pub fn atp_session_invalid_token_returns_401_test() {
  let assert Ok(exec) = db_connection.connect("sqlite::memory:")
  setup_tables(exec)

  let req =
    simulate.request(http.Get, "/api/atp/sessions/test-session")
    |> simulate.header("authorization", "Bearer invalid-token")

  let response = atp_session.handle(req, exec, "test-session")

  response.status |> should.equal(401)
}
