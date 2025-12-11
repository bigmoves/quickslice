/// OAuth authorization request repository operations
import database/executor.{type DbError, type Executor, Int as DbInt, Text}
import database/types.{type OAuthAuthRequest, OAuthAuthRequest}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}

/// Insert a new authorization request
pub fn insert(exec: Executor, req: OAuthAuthRequest) -> Result(Nil, DbError) {
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
  let p12 = executor.placeholder(exec, 12)

  let sql = "INSERT INTO oauth_auth_request (
      session_id, client_id, redirect_uri, scope, state,
      code_challenge, code_challenge_method, response_type,
      nonce, login_hint, created_at, expires_at
    ) VALUES (" <> p1 <> ", " <> p2 <> ", " <> p3 <> ", " <> p4 <> ", " <> p5 <> ", " <> p6 <> ", " <> p7 <> ", " <> p8 <> ", " <> p9 <> ", " <> p10 <> ", " <> p11 <> ", " <> p12 <> ")"

  let params = [
    Text(req.session_id),
    Text(req.client_id),
    Text(req.redirect_uri),
    executor.nullable_text(req.scope),
    executor.nullable_text(req.state),
    executor.nullable_text(req.code_challenge),
    executor.nullable_text(req.code_challenge_method),
    Text(req.response_type),
    executor.nullable_text(req.nonce),
    executor.nullable_text(req.login_hint),
    DbInt(req.created_at),
    DbInt(req.expires_at),
  ]

  executor.exec(exec, sql, params)
}

/// Get an authorization request by session_id
pub fn get(
  exec: Executor,
  session_id: String,
) -> Result(Option(OAuthAuthRequest), DbError) {
  let sql = "SELECT session_id, client_id, redirect_uri, scope, state,
            code_challenge, code_challenge_method, response_type,
            nonce, login_hint, created_at, expires_at
     FROM oauth_auth_request WHERE session_id = " <> executor.placeholder(
      exec,
      1,
    )

  case executor.query(exec, sql, [Text(session_id)], decoder()) {
    Ok(rows) ->
      case list.first(rows) {
        Ok(req) -> Ok(Some(req))
        Error(_) -> Ok(None)
      }
    Error(err) -> Error(err)
  }
}

/// Delete an authorization request
pub fn delete(exec: Executor, session_id: String) -> Result(Nil, DbError) {
  let sql =
    "DELETE FROM oauth_auth_request WHERE session_id = "
    <> executor.placeholder(exec, 1)

  executor.exec(exec, sql, [Text(session_id)])
}

/// Delete expired authorization requests
pub fn delete_expired(exec: Executor, now: Int) -> Result(Nil, DbError) {
  let sql =
    "DELETE FROM oauth_auth_request WHERE expires_at <= "
    <> executor.placeholder(exec, 1)

  executor.exec(exec, sql, [DbInt(now)])
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
