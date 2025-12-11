/// OAuth refresh token repository operations
import database/executor.{type DbError, type Executor, Int as DbInt, Text}
import database/types.{type OAuthRefreshToken, OAuthRefreshToken}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}

/// Insert a new refresh token
pub fn insert(exec: Executor, token: OAuthRefreshToken) -> Result(Nil, DbError) {
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

  let sql = "INSERT INTO oauth_refresh_token (
      token, access_token, client_id, user_id, session_id, session_iteration,
      scope, created_at, expires_at, revoked
    ) VALUES (" <> p1 <> ", " <> p2 <> ", " <> p3 <> ", " <> p4 <> ", " <> p5 <> ", " <> p6 <> ", " <> p7 <> ", " <> p8 <> ", " <> p9 <> ", " <> p10 <> ")"

  let params = [
    Text(token.token),
    Text(token.access_token),
    Text(token.client_id),
    Text(token.user_id),
    executor.nullable_text(token.session_id),
    executor.nullable_int(token.session_iteration),
    executor.nullable_text(token.scope),
    DbInt(token.created_at),
    executor.nullable_int(token.expires_at),
    executor.bool_value(token.revoked),
  ]

  executor.exec(exec, sql, params)
}

/// Get a refresh token by token value
pub fn get(
  exec: Executor,
  token_value: String,
) -> Result(Option(OAuthRefreshToken), DbError) {
  let sql =
    "SELECT token, access_token, client_id, user_id, session_id, session_iteration,
            scope, created_at, expires_at, revoked
     FROM oauth_refresh_token WHERE token = "
    <> executor.placeholder(exec, 1)

  case executor.query(exec, sql, [Text(token_value)], decoder()) {
    Ok(rows) ->
      case list.first(rows) {
        Ok(token) -> Ok(Some(token))
        Error(_) -> Ok(None)
      }
    Error(err) -> Error(err)
  }
}

/// Get the latest non-revoked refresh token by session_id (atp_session_id)
pub fn get_by_session_id(
  exec: Executor,
  session_id: String,
) -> Result(Option(OAuthRefreshToken), DbError) {
  let sql =
    "SELECT token, access_token, client_id, user_id, session_id, session_iteration,
            scope, created_at, expires_at, revoked
     FROM oauth_refresh_token
     WHERE session_id = "
    <> executor.placeholder(exec, 1)
    <> " AND revoked = 0
     ORDER BY created_at DESC
     LIMIT 1"

  case executor.query(exec, sql, [Text(session_id)], decoder()) {
    Ok(rows) ->
      case list.first(rows) {
        Ok(token) -> Ok(Some(token))
        Error(_) -> Ok(None)
      }
    Error(err) -> Error(err)
  }
}

/// Revoke a refresh token
pub fn revoke(exec: Executor, token_value: String) -> Result(Nil, DbError) {
  let sql =
    "UPDATE oauth_refresh_token SET revoked = 1 WHERE token = "
    <> executor.placeholder(exec, 1)

  executor.exec(exec, sql, [Text(token_value)])
}

/// Delete expired refresh tokens
pub fn delete_expired(exec: Executor, now: Int) -> Result(Nil, DbError) {
  let sql =
    "DELETE FROM oauth_refresh_token WHERE expires_at IS NOT NULL AND expires_at <= "
    <> executor.placeholder(exec, 1)

  executor.exec(exec, sql, [DbInt(now)])
}

/// Decode refresh token from database row
fn decoder() -> decode.Decoder(OAuthRefreshToken) {
  use token <- decode.field(0, decode.string)
  use access_token <- decode.field(1, decode.string)
  use client_id <- decode.field(2, decode.string)
  use user_id <- decode.field(3, decode.string)
  use session_id <- decode.field(4, decode.optional(decode.string))
  use session_iteration <- decode.field(5, decode.optional(decode.int))
  use scope <- decode.field(6, decode.optional(decode.string))
  use created_at <- decode.field(7, decode.int)
  use expires_at <- decode.field(8, decode.optional(decode.int))
  use revoked <- decode.field(9, decode.int)

  decode.success(OAuthRefreshToken(
    token: token,
    access_token: access_token,
    client_id: client_id,
    user_id: user_id,
    session_id: session_id,
    session_iteration: session_iteration,
    scope: scope,
    created_at: created_at,
    expires_at: expires_at,
    revoked: revoked == 1,
  ))
}
