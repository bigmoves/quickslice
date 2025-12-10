/// OAuth access token repository operations
import database/types.{type OAuthAccessToken, OAuthAccessToken}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import sqlight

/// Insert a new access token
pub fn insert(
  conn: sqlight.Connection,
  token: OAuthAccessToken,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO oauth_access_token (
      token, token_type, client_id, user_id, session_id,
      session_iteration, scope, created_at, expires_at,
      revoked, dpop_jkt
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  "

  let params = [
    sqlight.text(token.token),
    sqlight.text(types.token_type_to_string(token.token_type)),
    sqlight.text(token.client_id),
    sqlight.nullable(sqlight.text, token.user_id),
    sqlight.nullable(sqlight.text, token.session_id),
    sqlight.nullable(sqlight.int, token.session_iteration),
    sqlight.nullable(sqlight.text, token.scope),
    sqlight.int(token.created_at),
    sqlight.int(token.expires_at),
    sqlight.bool(token.revoked),
    sqlight.nullable(sqlight.text, token.dpop_jkt),
  ]

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: params,
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Get an access token by token value
pub fn get(
  conn: sqlight.Connection,
  token_value: String,
) -> Result(Option(OAuthAccessToken), sqlight.Error) {
  let sql =
    "SELECT token, token_type, client_id, user_id, session_id,
            session_iteration, scope, created_at, expires_at,
            revoked, dpop_jkt
     FROM oauth_access_token WHERE token = ?"

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

/// Get an access token by DPoP JKT (thumbprint)
pub fn get_by_jkt(
  conn: sqlight.Connection,
  jkt: String,
) -> Result(Option(OAuthAccessToken), sqlight.Error) {
  let sql =
    "SELECT token, token_type, client_id, user_id, session_id,
            session_iteration, scope, created_at, expires_at,
            revoked, dpop_jkt
     FROM oauth_access_token
     WHERE dpop_jkt = ? AND revoked = 0"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(jkt)],
    expecting: decoder(),
  ))

  case list.first(rows) {
    Ok(token) -> Ok(Some(token))
    Error(_) -> Ok(None)
  }
}

/// Get the latest non-revoked access token by session_id (atp_session_id)
pub fn get_by_session_id(
  conn: sqlight.Connection,
  session_id: String,
) -> Result(Option(OAuthAccessToken), sqlight.Error) {
  let sql =
    "SELECT token, token_type, client_id, user_id, session_id,
            session_iteration, scope, created_at, expires_at,
            revoked, dpop_jkt
     FROM oauth_access_token
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

/// Update session iteration for an access token (after ATP token refresh)
pub fn update_session_iteration(
  conn: sqlight.Connection,
  token_value: String,
  new_iteration: Int,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "UPDATE oauth_access_token SET session_iteration = ? WHERE token = ?"

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.int(new_iteration), sqlight.text(token_value)],
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Revoke an access token
pub fn revoke(
  conn: sqlight.Connection,
  token_value: String,
) -> Result(Nil, sqlight.Error) {
  let sql = "UPDATE oauth_access_token SET revoked = 1 WHERE token = ?"

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(token_value)],
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Delete expired access tokens
pub fn delete_expired(
  conn: sqlight.Connection,
  now: Int,
) -> Result(Int, sqlight.Error) {
  let sql = "DELETE FROM oauth_access_token WHERE expires_at <= ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.int(now)],
    expecting: decode.dynamic,
  ))
  Ok(list.length(rows))
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
