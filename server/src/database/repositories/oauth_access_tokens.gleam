/// OAuth access token repository operations
import database/executor.{type DbError, type Executor, Int as DbInt, Text}
import database/types.{type OAuthAccessToken, OAuthAccessToken}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}

/// Insert a new access token
pub fn insert(exec: Executor, token: OAuthAccessToken) -> Result(Nil, DbError) {
  let p1 = executor.placeholder(exec, 1)
  let p2 = executor.placeholder(exec, 2)
  let p3 = executor.placeholder(exec, 3)
  let p4 = executor.placeholder(exec, 4)
  let p5 = executor.placeholder(exec, 5)
  let p6 = executor.placeholder(exec, 6)
  let p7 = executor.placeholder(exec, 7)
  let p8 = executor.placeholder(exec, 8)
  let p9 = executor.placeholder(exec, 9)
  let p10 = executor.placeholder(exec, 10)
  let p11 = executor.placeholder(exec, 11)

  let sql = "INSERT INTO oauth_access_token (
      token, token_type, client_id, user_id, session_id,
      session_iteration, scope, created_at, expires_at,
      revoked, dpop_jkt
    ) VALUES (" <> p1 <> ", " <> p2 <> ", " <> p3 <> ", " <> p4 <> ", " <> p5 <> ", " <> p6 <> ", " <> p7 <> ", " <> p8 <> ", " <> p9 <> ", " <> p10 <> ", " <> p11 <> ")"

  let params = [
    Text(token.token),
    Text(types.token_type_to_string(token.token_type)),
    Text(token.client_id),
    executor.nullable_text(token.user_id),
    executor.nullable_text(token.session_id),
    executor.nullable_int(token.session_iteration),
    executor.nullable_text(token.scope),
    DbInt(token.created_at),
    DbInt(token.expires_at),
    executor.bool_value(token.revoked),
    executor.nullable_text(token.dpop_jkt),
  ]

  executor.exec(exec, sql, params)
}

/// Get an access token by token value
pub fn get(
  exec: Executor,
  token_value: String,
) -> Result(Option(OAuthAccessToken), DbError) {
  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "SELECT token, token_type, client_id, user_id, session_id,
              session_iteration, scope, created_at, expires_at,
              revoked, dpop_jkt
       FROM oauth_access_token WHERE token = " <> executor.placeholder(exec, 1)
    executor.PostgreSQL ->
      "SELECT token, token_type, client_id, user_id, session_id,
              session_iteration, scope, created_at, expires_at,
              revoked::int, dpop_jkt
       FROM oauth_access_token WHERE token = " <> executor.placeholder(exec, 1)
  }

  case executor.query(exec, sql, [Text(token_value)], decoder()) {
    Ok(rows) ->
      case list.first(rows) {
        Ok(token) -> Ok(Some(token))
        Error(_) -> Ok(None)
      }
    Error(err) -> Error(err)
  }
}

/// Get an access token by DPoP JKT (thumbprint)
pub fn get_by_jkt(
  exec: Executor,
  jkt: String,
) -> Result(Option(OAuthAccessToken), DbError) {
  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "SELECT token, token_type, client_id, user_id, session_id,
              session_iteration, scope, created_at, expires_at,
              revoked, dpop_jkt
       FROM oauth_access_token
       WHERE dpop_jkt = " <> executor.placeholder(exec, 1) <> " AND revoked = 0"
    executor.PostgreSQL ->
      "SELECT token, token_type, client_id, user_id, session_id,
              session_iteration, scope, created_at, expires_at,
              revoked::int, dpop_jkt
       FROM oauth_access_token
       WHERE dpop_jkt = " <> executor.placeholder(exec, 1) <> " AND revoked = FALSE"
  }

  case executor.query(exec, sql, [Text(jkt)], decoder()) {
    Ok(rows) ->
      case list.first(rows) {
        Ok(token) -> Ok(Some(token))
        Error(_) -> Ok(None)
      }
    Error(err) -> Error(err)
  }
}

/// Get the latest non-revoked access token by session_id (atp_session_id)
pub fn get_by_session_id(
  exec: Executor,
  session_id: String,
) -> Result(Option(OAuthAccessToken), DbError) {
  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "SELECT token, token_type, client_id, user_id, session_id,
              session_iteration, scope, created_at, expires_at,
              revoked, dpop_jkt
       FROM oauth_access_token
       WHERE session_id = " <> executor.placeholder(exec, 1) <> " AND revoked = 0
       ORDER BY created_at DESC
       LIMIT 1"
    executor.PostgreSQL ->
      "SELECT token, token_type, client_id, user_id, session_id,
              session_iteration, scope, created_at, expires_at,
              revoked::int, dpop_jkt
       FROM oauth_access_token
       WHERE session_id = " <> executor.placeholder(exec, 1) <> " AND revoked = FALSE
       ORDER BY created_at DESC
       LIMIT 1"
  }

  case executor.query(exec, sql, [Text(session_id)], decoder()) {
    Ok(rows) ->
      case list.first(rows) {
        Ok(token) -> Ok(Some(token))
        Error(_) -> Ok(None)
      }
    Error(err) -> Error(err)
  }
}

/// Update session iteration for an access token (after ATP token refresh)
pub fn update_session_iteration(
  exec: Executor,
  token_value: String,
  new_iteration: Int,
) -> Result(Nil, DbError) {
  let sql =
    "UPDATE oauth_access_token SET session_iteration = "
    <> executor.placeholder(exec, 1)
    <> " WHERE token = "
    <> executor.placeholder(exec, 2)

  executor.exec(exec, sql, [DbInt(new_iteration), Text(token_value)])
}

/// Revoke an access token
pub fn revoke(exec: Executor, token_value: String) -> Result(Nil, DbError) {
  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "UPDATE oauth_access_token SET revoked = 1 WHERE token = "
      <> executor.placeholder(exec, 1)
    executor.PostgreSQL ->
      "UPDATE oauth_access_token SET revoked = TRUE WHERE token = "
      <> executor.placeholder(exec, 1)
  }

  executor.exec(exec, sql, [Text(token_value)])
}

/// Delete expired access tokens
pub fn delete_expired(exec: Executor, now: Int) -> Result(Nil, DbError) {
  let sql =
    "DELETE FROM oauth_access_token WHERE expires_at <= "
    <> executor.placeholder(exec, 1)

  executor.exec(exec, sql, [DbInt(now)])
}

/// Decode access token from database row
fn decoder() -> decode.Decoder(OAuthAccessToken) {
  use token <- decode.field(0, decode.string)
  use token_type_str <- decode.field(1, decode.string)
  use client_id <- decode.field(2, decode.string)
  use user_id <- decode.field(3, decode.optional(decode.string))
  use session_id <- decode.field(4, decode.optional(decode.string))
  use session_iteration <- decode.field(5, decode.optional(decode.int))
  use scope <- decode.field(6, decode.optional(decode.string))
  use created_at <- decode.field(7, decode.int)
  use expires_at <- decode.field(8, decode.int)
  use revoked <- decode.field(9, decode.int)
  use dpop_jkt <- decode.field(10, decode.optional(decode.string))

  let token_type =
    types.token_type_from_string(token_type_str)
    |> option.unwrap(types.Bearer)

  decode.success(OAuthAccessToken(
    token: token,
    token_type: token_type,
    client_id: client_id,
    user_id: user_id,
    session_id: session_id,
    session_iteration: session_iteration,
    scope: scope,
    created_at: created_at,
    expires_at: expires_at,
    revoked: revoked == 1,
    dpop_jkt: dpop_jkt,
  ))
}
