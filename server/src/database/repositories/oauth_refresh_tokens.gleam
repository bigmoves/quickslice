/// OAuth refresh token repository operations
import database/types.{type OAuthRefreshToken, OAuthRefreshToken}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import sqlight

/// Insert a new refresh token
pub fn insert(
  conn: sqlight.Connection,
  token: OAuthRefreshToken,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO oauth_refresh_token (
      token, access_token, client_id, user_id, session_id, session_iteration,
      scope, created_at, expires_at, revoked
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  "

  let params = [
    sqlight.text(token.token),
    sqlight.text(token.access_token),
    sqlight.text(token.client_id),
    sqlight.text(token.user_id),
    sqlight.nullable(sqlight.text, token.session_id),
    sqlight.nullable(sqlight.int, token.session_iteration),
    sqlight.nullable(sqlight.text, token.scope),
    sqlight.int(token.created_at),
    sqlight.nullable(sqlight.int, token.expires_at),
    sqlight.bool(token.revoked),
  ]

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: params,
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Get a refresh token by token value
pub fn get(
  conn: sqlight.Connection,
  token_value: String,
) -> Result(Option(OAuthRefreshToken), sqlight.Error) {
  let sql =
    "SELECT token, access_token, client_id, user_id, session_id, session_iteration,
            scope, created_at, expires_at, revoked
     FROM oauth_refresh_token WHERE token = ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(token_value)],
    expecting: decoder(),
  ))

  case list.first(rows) {
    Ok(token) -> Ok(Some(token))
    Error(_) -> Ok(None)
  }
}

/// Get the latest non-revoked refresh token by session_id (atp_session_id)
pub fn get_by_session_id(
  conn: sqlight.Connection,
  session_id: String,
) -> Result(Option(OAuthRefreshToken), sqlight.Error) {
  let sql =
    "SELECT token, access_token, client_id, user_id, session_id, session_iteration,
            scope, created_at, expires_at, revoked
     FROM oauth_refresh_token
     WHERE session_id = ? AND revoked = 0
     ORDER BY created_at DESC
     LIMIT 1"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(session_id)],
    expecting: decoder(),
  ))

  case list.first(rows) {
    Ok(token) -> Ok(Some(token))
    Error(_) -> Ok(None)
  }
}

/// Revoke a refresh token
pub fn revoke(
  conn: sqlight.Connection,
  token_value: String,
) -> Result(Nil, sqlight.Error) {
  let sql = "UPDATE oauth_refresh_token SET revoked = 1 WHERE token = ?"

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(token_value)],
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Delete expired refresh tokens
pub fn delete_expired(
  conn: sqlight.Connection,
  now: Int,
) -> Result(Int, sqlight.Error) {
  let sql =
    "DELETE FROM oauth_refresh_token WHERE expires_at IS NOT NULL AND expires_at <= ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.int(now)],
    expecting: decode.dynamic,
  ))
  Ok(list.length(rows))
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
