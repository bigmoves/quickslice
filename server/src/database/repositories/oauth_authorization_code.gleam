/// OAuth authorization code repository operations
import database/types.{
  type OAuthAuthorizationCode, OAuthAuthorizationCode, Plain, S256,
}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import sqlight

/// Insert a new authorization code
pub fn insert(
  conn: sqlight.Connection,
  code: OAuthAuthorizationCode,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO oauth_authorization_code (
      code, client_id, user_id, session_id, session_iteration, redirect_uri, scope,
      code_challenge, code_challenge_method, nonce, created_at, expires_at, used
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  "

  let params = [
    sqlight.text(code.code),
    sqlight.text(code.client_id),
    sqlight.text(code.user_id),
    sqlight.nullable(sqlight.text, code.session_id),
    sqlight.nullable(sqlight.int, code.session_iteration),
    sqlight.text(code.redirect_uri),
    sqlight.nullable(sqlight.text, code.scope),
    sqlight.nullable(sqlight.text, code.code_challenge),
    sqlight.nullable(
      sqlight.text,
      option.map(
        code.code_challenge_method,
        types.code_challenge_method_to_string,
      ),
    ),
    sqlight.nullable(sqlight.text, code.nonce),
    sqlight.int(code.created_at),
    sqlight.int(code.expires_at),
    sqlight.bool(code.used),
  ]

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: params,
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Get an authorization code by code value
pub fn get(
  conn: sqlight.Connection,
  code_value: String,
) -> Result(Option(OAuthAuthorizationCode), sqlight.Error) {
  let sql =
    "SELECT code, client_id, user_id, session_id, session_iteration, redirect_uri, scope,
            code_challenge, code_challenge_method, nonce, created_at, expires_at, used
     FROM oauth_authorization_code WHERE code = ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(code_value)],
    expecting: decoder(),
  ))

  case list.first(rows) {
    Ok(code) -> Ok(Some(code))
    Error(_) -> Ok(None)
  }
}

/// Mark an authorization code as used
pub fn mark_used(
  conn: sqlight.Connection,
  code_value: String,
) -> Result(Nil, sqlight.Error) {
  let sql = "UPDATE oauth_authorization_code SET used = 1 WHERE code = ?"

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(code_value)],
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Delete an authorization code
pub fn delete(
  conn: sqlight.Connection,
  code_value: String,
) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM oauth_authorization_code WHERE code = ?"

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(code_value)],
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Delete expired authorization codes
pub fn delete_expired(
  conn: sqlight.Connection,
  now: Int,
) -> Result(Int, sqlight.Error) {
  let sql = "DELETE FROM oauth_authorization_code WHERE expires_at <= ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.int(now)],
    expecting: decode.dynamic,
  ))
  Ok(list.length(rows))
}

/// Decode authorization code from database row
fn decoder() -> decode.Decoder(OAuthAuthorizationCode) {
  use code <- decode.field(0, decode.string)
  use client_id <- decode.field(1, decode.string)
  use user_id <- decode.field(2, decode.string)
  use session_id <- decode.field(3, decode.optional(decode.string))
  use session_iteration <- decode.field(4, decode.optional(decode.int))
  use redirect_uri <- decode.field(5, decode.string)
  use scope <- decode.field(6, decode.optional(decode.string))
  use code_challenge <- decode.field(7, decode.optional(decode.string))
  use code_challenge_method_str <- decode.field(
    8,
    decode.optional(decode.string),
  )
  use nonce <- decode.field(9, decode.optional(decode.string))
  use created_at <- decode.field(10, decode.int)
  use expires_at <- decode.field(11, decode.int)
  use used <- decode.field(12, decode.int)

  let code_challenge_method = case code_challenge_method_str {
    Some("S256") -> Some(S256)
    Some("plain") -> Some(Plain)
    _ -> None
  }

  decode.success(OAuthAuthorizationCode(
    code: code,
    client_id: client_id,
    user_id: user_id,
    session_id: session_id,
    session_iteration: session_iteration,
    redirect_uri: redirect_uri,
    scope: scope,
    code_challenge: code_challenge,
    code_challenge_method: code_challenge_method,
    nonce: nonce,
    created_at: created_at,
    expires_at: expires_at,
    used: used == 1,
  ))
}
