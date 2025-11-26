import gleam/http
import gleeunit/should
import handlers/oauth/atp_session
import sqlight
import wisp/simulate

fn setup_tables(conn: sqlight.Connection) {
  // Create oauth_access_token table
  let assert Ok(_) =
    sqlight.exec(
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
      conn,
    )
  Nil
}

pub fn atp_session_missing_auth_returns_401_test() {
  let assert Ok(conn) = sqlight.open(":memory:")
  setup_tables(conn)

  let req = simulate.request(http.Get, "/api/atp/sessions/test-session")
  let response = atp_session.handle(req, conn, "test-session")

  response.status |> should.equal(401)
}

pub fn atp_session_invalid_token_returns_401_test() {
  let assert Ok(conn) = sqlight.open(":memory:")
  setup_tables(conn)

  let req =
    simulate.request(http.Get, "/api/atp/sessions/test-session")
    |> simulate.header("authorization", "Bearer invalid-token")

  let response = atp_session.handle(req, conn, "test-session")

  response.status |> should.equal(401)
}
