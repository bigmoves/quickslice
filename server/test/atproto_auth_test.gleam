import atproto_auth
import database/executor.{type Executor, Int, Text}
import database/repositories/oauth_access_tokens
import database/repositories/oauth_atp_sessions
import database/types.{Bearer, OAuthAccessToken, OAuthAtpSession}
import gleam/option.{None, Some}
import gleeunit/should
import lib/oauth/did_cache
import test_helpers

fn setup_db() -> Executor {
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_oauth_tables(exec)
  // Create the test client that access tokens reference
  let assert Ok(_) =
    executor.exec(
      exec,
      "INSERT INTO oauth_client (client_id, client_name, redirect_uris, grant_types, response_types, token_endpoint_auth_method, client_type, created_at, updated_at)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
      [
        Text("test-client"),
        Text("Test Client"),
        Text("[\"http://localhost\"]"),
        Text("[\"authorization_code\"]"),
        Text("[\"code\"]"),
        Text("none"),
        Text("public"),
        Int(1000),
        Int(1000),
      ],
    )
  exec
}

pub fn verify_token_valid_returns_user_info_test() {
  let exec = setup_db()

  // Insert a valid token
  let token =
    OAuthAccessToken(
      token: "valid-token",
      token_type: Bearer,
      client_id: "test-client",
      user_id: Some("did:plc:testuser123"),
      session_id: Some("session-123"),
      session_iteration: Some(1),
      scope: Some("atproto"),
      created_at: 1000,
      expires_at: 9_999_999_999,
      revoked: False,
      dpop_jkt: None,
    )
  let assert Ok(_) = oauth_access_tokens.insert(exec, token)

  let result = atproto_auth.verify_token(exec, "valid-token")

  result |> should.be_ok
  let assert Ok(user_info) = result
  user_info.did |> should.equal("did:plc:testuser123")
}

pub fn verify_token_not_found_returns_error_test() {
  let exec = setup_db()

  let result = atproto_auth.verify_token(exec, "nonexistent-token")

  result |> should.be_error
  let assert Error(err) = result
  err |> should.equal(atproto_auth.UnauthorizedToken)
}

pub fn verify_token_revoked_returns_error_test() {
  let exec = setup_db()

  let token =
    OAuthAccessToken(
      token: "revoked-token",
      token_type: Bearer,
      client_id: "test-client",
      user_id: Some("did:plc:testuser123"),
      session_id: Some("session-123"),
      session_iteration: Some(1),
      scope: Some("atproto"),
      created_at: 1000,
      expires_at: 9_999_999_999,
      revoked: True,
      dpop_jkt: None,
    )
  let assert Ok(_) = oauth_access_tokens.insert(exec, token)

  let result = atproto_auth.verify_token(exec, "revoked-token")

  result |> should.be_error
  let assert Error(err) = result
  err |> should.equal(atproto_auth.UnauthorizedToken)
}

pub fn verify_token_expired_returns_error_test() {
  let exec = setup_db()

  let token =
    OAuthAccessToken(
      token: "expired-token",
      token_type: Bearer,
      client_id: "test-client",
      user_id: Some("did:plc:testuser123"),
      session_id: Some("session-123"),
      session_iteration: Some(1),
      scope: Some("atproto"),
      created_at: 1000,
      expires_at: 1,
      // Already expired
      revoked: False,
      dpop_jkt: None,
    )
  let assert Ok(_) = oauth_access_tokens.insert(exec, token)

  let result = atproto_auth.verify_token(exec, "expired-token")

  result |> should.be_error
  let assert Error(err) = result
  err |> should.equal(atproto_auth.TokenExpired)
}

pub fn verify_token_no_user_id_returns_error_test() {
  let exec = setup_db()

  let token =
    OAuthAccessToken(
      token: "no-user-token",
      token_type: Bearer,
      client_id: "test-client",
      user_id: None,
      session_id: Some("session-123"),
      session_iteration: Some(1),
      scope: Some("atproto"),
      created_at: 1000,
      expires_at: 9_999_999_999,
      revoked: False,
      dpop_jkt: None,
    )
  let assert Ok(_) = oauth_access_tokens.insert(exec, token)

  let result = atproto_auth.verify_token(exec, "no-user-token")

  result |> should.be_error
  let assert Error(err) = result
  err |> should.equal(atproto_auth.UnauthorizedToken)
}

// ===== get_atp_session tests =====

pub fn get_atp_session_no_session_id_returns_error_test() {
  let exec = setup_db()

  // Token without session_id
  let token =
    OAuthAccessToken(
      token: "no-session-token",
      token_type: Bearer,
      client_id: "test-client",
      user_id: Some("did:plc:testuser123"),
      session_id: None,
      session_iteration: None,
      scope: Some("atproto"),
      created_at: 1000,
      expires_at: 9_999_999_999,
      revoked: False,
      dpop_jkt: None,
    )
  let assert Ok(_) = oauth_access_tokens.insert(exec, token)

  let assert Ok(cache) = did_cache.start()

  let result =
    atproto_auth.get_atp_session(exec, cache, "no-session-token", None, "")

  result |> should.be_error
  let assert Error(err) = result
  err |> should.equal(atproto_auth.SessionNotFound)
}

pub fn get_atp_session_not_exchanged_returns_error_test() {
  let exec = setup_db()

  // Token with session_id
  let token =
    OAuthAccessToken(
      token: "has-session-token",
      token_type: Bearer,
      client_id: "test-client",
      user_id: Some("did:plc:testuser123"),
      session_id: Some("session-123"),
      session_iteration: Some(1),
      scope: Some("atproto"),
      created_at: 1000,
      expires_at: 9_999_999_999,
      revoked: False,
      dpop_jkt: None,
    )
  let assert Ok(_) = oauth_access_tokens.insert(exec, token)

  // ATP session that hasn't been exchanged yet (no access_token, no session_exchanged_at)
  let atp_session =
    OAuthAtpSession(
      session_id: "session-123",
      iteration: 1,
      did: Some("did:plc:testuser123"),
      session_created_at: 1000,
      atp_oauth_state: "state-123",
      signing_key_jkt: "jkt-123",
      dpop_key: "{\"kty\":\"EC\"}",
      access_token: None,
      refresh_token: None,
      access_token_created_at: None,
      access_token_expires_at: None,
      access_token_scopes: None,
      session_exchanged_at: None,
      exchange_error: None,
    )
  let assert Ok(_) = oauth_atp_sessions.insert(exec, atp_session)

  let assert Ok(cache) = did_cache.start()

  let result =
    atproto_auth.get_atp_session(exec, cache, "has-session-token", None, "")

  result |> should.be_error
  let assert Error(err) = result
  err |> should.equal(atproto_auth.SessionNotReady)
}

pub fn get_atp_session_with_exchange_error_returns_error_test() {
  let exec = setup_db()

  let token =
    OAuthAccessToken(
      token: "error-session-token",
      token_type: Bearer,
      client_id: "test-client",
      user_id: Some("did:plc:testuser123"),
      session_id: Some("session-456"),
      session_iteration: Some(1),
      scope: Some("atproto"),
      created_at: 1000,
      expires_at: 9_999_999_999,
      revoked: False,
      dpop_jkt: None,
    )
  let assert Ok(_) = oauth_access_tokens.insert(exec, token)

  // ATP session with exchange error
  let atp_session =
    OAuthAtpSession(
      session_id: "session-456",
      iteration: 1,
      did: Some("did:plc:testuser123"),
      session_created_at: 1000,
      atp_oauth_state: "state-456",
      signing_key_jkt: "jkt-456",
      dpop_key: "{\"kty\":\"EC\"}",
      access_token: Some("token"),
      refresh_token: Some("refresh"),
      access_token_created_at: Some(1000),
      access_token_expires_at: Some(9_999_999_999),
      access_token_scopes: Some("atproto"),
      session_exchanged_at: Some(1000),
      exchange_error: Some("exchange failed"),
    )
  let assert Ok(_) = oauth_atp_sessions.insert(exec, atp_session)

  let assert Ok(cache) = did_cache.start()

  let result =
    atproto_auth.get_atp_session(exec, cache, "error-session-token", None, "")

  result |> should.be_error
  let assert Error(err) = result
  err |> should.equal(atproto_auth.SessionNotReady)
}
