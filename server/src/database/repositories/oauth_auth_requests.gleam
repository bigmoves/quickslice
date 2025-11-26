/// OAuth authorization request repository operations
import database/types.{type OAuthAuthRequest, OAuthAuthRequest}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import sqlight

/// Insert a new authorization request
pub fn insert(
  conn: sqlight.Connection,
  req: OAuthAuthRequest,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO oauth_auth_request (
      session_id, client_id, redirect_uri, scope, state,
      code_challenge, code_challenge_method, response_type,
      nonce, login_hint, created_at, expires_at
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  "

  let params = [
    sqlight.text(req.session_id),
    sqlight.text(req.client_id),
    sqlight.text(req.redirect_uri),
    sqlight.nullable(sqlight.text, req.scope),
    sqlight.nullable(sqlight.text, req.state),
    sqlight.nullable(sqlight.text, req.code_challenge),
    sqlight.nullable(sqlight.text, req.code_challenge_method),
    sqlight.text(req.response_type),
    sqlight.nullable(sqlight.text, req.nonce),
    sqlight.nullable(sqlight.text, req.login_hint),
    sqlight.int(req.created_at),
    sqlight.int(req.expires_at),
  ]

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: params,
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Get an authorization request by session_id
pub fn get(
  conn: sqlight.Connection,
  session_id: String,
) -> Result(Option(OAuthAuthRequest), sqlight.Error) {
  let sql =
    "SELECT session_id, client_id, redirect_uri, scope, state,
            code_challenge, code_challenge_method, response_type,
            nonce, login_hint, created_at, expires_at
     FROM oauth_auth_request WHERE session_id = ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(session_id)],
    expecting: decoder(),
  ))

  case list.first(rows) {
    Ok(req) -> Ok(Some(req))
    Error(_) -> Ok(None)
  }
}

/// Delete an authorization request
pub fn delete(
  conn: sqlight.Connection,
  session_id: String,
) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM oauth_auth_request WHERE session_id = ?"

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(session_id)],
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Delete expired authorization requests
pub fn delete_expired(
  conn: sqlight.Connection,
  now: Int,
) -> Result(Int, sqlight.Error) {
  let sql = "DELETE FROM oauth_auth_request WHERE expires_at <= ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.int(now)],
    expecting: decode.dynamic,
  ))
  Ok(list.length(rows))
}

/// Decode authorization request from database row
fn decoder() -> decode.Decoder(OAuthAuthRequest) {
  use session_id <- decode.field(0, decode.string)
  use client_id <- decode.field(1, decode.string)
  use redirect_uri <- decode.field(2, decode.string)
  use scope <- decode.field(3, decode.optional(decode.string))
  use state <- decode.field(4, decode.optional(decode.string))
  use code_challenge <- decode.field(5, decode.optional(decode.string))
  use code_challenge_method <- decode.field(6, decode.optional(decode.string))
  use response_type <- decode.field(7, decode.string)
  use nonce <- decode.field(8, decode.optional(decode.string))
  use login_hint <- decode.field(9, decode.optional(decode.string))
  use created_at <- decode.field(10, decode.int)
  use expires_at <- decode.field(11, decode.int)

  decode.success(OAuthAuthRequest(
    session_id: session_id,
    client_id: client_id,
    redirect_uri: redirect_uri,
    scope: scope,
    state: state,
    code_challenge: code_challenge,
    code_challenge_method: code_challenge_method,
    response_type: response_type,
    nonce: nonce,
    login_hint: login_hint,
    created_at: created_at,
    expires_at: expires_at,
  ))
}
