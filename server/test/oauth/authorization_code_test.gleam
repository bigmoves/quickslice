import database/repositories/oauth_authorization_code
import database/schema/tables
import database/types.{OAuthAuthorizationCode, S256}
import gleam/option.{None, Some}
import gleeunit/should
import sqlight

pub fn insert_and_get_authorization_code_test() {
  let assert Ok(conn) = sqlight.open(":memory:")

  // Create table using schema function
  let assert Ok(_) = tables.create_oauth_authorization_code_table(conn)

  let code =
    OAuthAuthorizationCode(
      code: "test-code-123",
      client_id: "client-1",
      user_id: "did:plc:test",
      session_id: Some("session-1"),
      session_iteration: Some(1),
      redirect_uri: "http://localhost/callback",
      scope: Some("atproto"),
      code_challenge: Some("challenge"),
      code_challenge_method: Some(S256),
      nonce: Some("nonce-1"),
      created_at: 1000,
      expires_at: 2000,
      used: False,
    )

  let assert Ok(Nil) = oauth_authorization_code.insert(conn, code)
  let assert Ok(Some(retrieved)) =
    oauth_authorization_code.get(conn, "test-code-123")

  retrieved.code |> should.equal("test-code-123")
  retrieved.client_id |> should.equal("client-1")
  retrieved.used |> should.equal(False)
}

pub fn mark_code_used_test() {
  let assert Ok(conn) = sqlight.open(":memory:")

  let assert Ok(_) = tables.create_oauth_authorization_code_table(conn)

  let code =
    OAuthAuthorizationCode(
      code: "test-code-456",
      client_id: "client-1",
      user_id: "did:plc:test",
      session_id: None,
      session_iteration: None,
      redirect_uri: "http://localhost/callback",
      scope: None,
      code_challenge: None,
      code_challenge_method: None,
      nonce: None,
      created_at: 1000,
      expires_at: 2000,
      used: False,
    )

  let assert Ok(Nil) = oauth_authorization_code.insert(conn, code)
  let assert Ok(Nil) = oauth_authorization_code.mark_used(conn, "test-code-456")
  let assert Ok(Some(retrieved)) =
    oauth_authorization_code.get(conn, "test-code-456")

  retrieved.used |> should.equal(True)
}

pub fn get_nonexistent_code_returns_none_test() {
  let assert Ok(conn) = sqlight.open(":memory:")

  let assert Ok(_) = tables.create_oauth_authorization_code_table(conn)

  let assert Ok(None) = oauth_authorization_code.get(conn, "nonexistent")
}
