/// Tests for client session repository operations
import database/executor
import database/repositories/client_session
import database/sqlite/connection as db_connection
import gleam/option.{None, Some}
import gleeunit/should

fn setup_test_db() {
  let assert Ok(exec) = db_connection.connect("sqlite::memory:")

  // Create client_session table
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

  exec
}

pub fn insert_creates_session_test() {
  let exec = setup_test_db()

  let result =
    client_session.insert(
      exec,
      "session-123",
      "client-abc",
      None,
      None,
      "jkt-xyz",
    )

  result |> should.be_ok()

  let assert Ok(session) = result
  session.session_id |> should.equal("session-123")
  session.client_id |> should.equal("client-abc")
  session.user_did |> should.equal(None)
  session.atp_session_id |> should.equal(None)
  session.dpop_jkt |> should.equal("jkt-xyz")
}

pub fn insert_with_user_did_test() {
  let exec = setup_test_db()

  let result =
    client_session.insert(
      exec,
      "session-456",
      "client-def",
      Some("did:plc:test123"),
      Some("atp-session-789"),
      "jkt-abc",
    )

  result |> should.be_ok()

  let assert Ok(session) = result
  session.user_did |> should.equal(Some("did:plc:test123"))
  session.atp_session_id |> should.equal(Some("atp-session-789"))
}

pub fn get_returns_existing_session_test() {
  let exec = setup_test_db()

  // Insert a session
  let assert Ok(_) =
    client_session.insert(
      exec,
      "session-get-test",
      "client-123",
      Some("did:plc:user"),
      None,
      "jkt-123",
    )

  // Get the session
  let result = client_session.get(exec, "session-get-test")

  result |> should.be_ok()
  let assert Ok(Some(session)) = result
  session.session_id |> should.equal("session-get-test")
  session.client_id |> should.equal("client-123")
  session.user_did |> should.equal(Some("did:plc:user"))
}

pub fn get_returns_none_for_missing_session_test() {
  let exec = setup_test_db()

  let result = client_session.get(exec, "nonexistent-session")

  result |> should.be_ok()
  let assert Ok(None) = result
}

pub fn touch_updates_last_activity_test() {
  let exec = setup_test_db()

  // Insert a session
  let assert Ok(original) =
    client_session.insert(
      exec,
      "session-touch-test",
      "client-touch",
      None,
      None,
      "jkt-touch",
    )

  // Touch the session
  let result = client_session.touch(exec, "session-touch-test")
  result |> should.be_ok()

  // Get the session and verify last_activity_at changed
  let assert Ok(Some(updated)) = client_session.get(exec, "session-touch-test")
  // last_activity_at should be >= original (timestamps are in seconds)
  { updated.last_activity_at >= original.last_activity_at } |> should.be_true()
}

pub fn update_auth_sets_user_and_atp_session_test() {
  let exec = setup_test_db()

  // Insert a session without auth
  let assert Ok(_) =
    client_session.insert(
      exec,
      "session-auth-test",
      "client-auth",
      None,
      None,
      "jkt-auth",
    )

  // Update with auth info
  let result =
    client_session.update_auth(
      exec,
      "session-auth-test",
      "did:plc:authenticated",
      "atp-session-id-123",
    )
  result |> should.be_ok()

  // Verify the update
  let assert Ok(Some(session)) = client_session.get(exec, "session-auth-test")
  session.user_did |> should.equal(Some("did:plc:authenticated"))
  session.atp_session_id |> should.equal(Some("atp-session-id-123"))
}

pub fn delete_removes_session_test() {
  let exec = setup_test_db()

  // Insert a session
  let assert Ok(_) =
    client_session.insert(
      exec,
      "session-delete-test",
      "client-delete",
      None,
      None,
      "jkt-delete",
    )

  // Delete the session
  let result = client_session.delete(exec, "session-delete-test")
  result |> should.be_ok()

  // Verify it's gone
  let assert Ok(None) = client_session.get(exec, "session-delete-test")
}

pub fn delete_expired_removes_old_sessions_test() {
  let exec = setup_test_db()

  // Insert a session manually with old timestamp (1 second old)
  let assert Ok(_) =
    executor.exec(
      exec,
      "INSERT INTO client_session (session_id, client_id, dpop_jkt, created_at, last_activity_at)
       VALUES ('old-session', 'client', 'jkt', 1, 1)",
      [],
    )

  // Insert a fresh session
  let assert Ok(_) =
    client_session.insert(exec, "fresh-session", "client", None, None, "jkt")

  // Delete sessions older than 1000 seconds (effectively current time - 1000)
  // The old session with last_activity_at=1 should be deleted
  let result = client_session.delete_expired(exec, 1000)
  result |> should.be_ok()

  // Old session should be gone
  let assert Ok(None) = client_session.get(exec, "old-session")

  // Fresh session should still exist
  let assert Ok(Some(_)) = client_session.get(exec, "fresh-session")
}
