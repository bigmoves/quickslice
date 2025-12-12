/// OAuth authorization code repository operations
import database/executor.{type DbError, type Executor, Int as DbInt, Text}
import database/types.{
  type OAuthAuthorizationCode, OAuthAuthorizationCode, Plain, S256,
}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}

/// Insert a new authorization code
pub fn insert(
  exec: Executor,
  code: OAuthAuthorizationCode,
) -> Result(Nil, DbError) {
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
  let p13 = executor.placeholder(exec, 13)

  let sql = "INSERT INTO oauth_authorization_code (
      code, client_id, user_id, session_id, session_iteration, redirect_uri, scope,
      code_challenge, code_challenge_method, nonce, created_at, expires_at, used
    ) VALUES (" <> p1 <> ", " <> p2 <> ", " <> p3 <> ", " <> p4 <> ", " <> p5 <> ", " <> p6 <> ", " <> p7 <> ", " <> p8 <> ", " <> p9 <> ", " <> p10 <> ", " <> p11 <> ", " <> p12 <> ", " <> p13 <> ")"

  let params = [
    Text(code.code),
    Text(code.client_id),
    Text(code.user_id),
    executor.nullable_text(code.session_id),
    executor.nullable_int(code.session_iteration),
    Text(code.redirect_uri),
    executor.nullable_text(code.scope),
    executor.nullable_text(code.code_challenge),
    executor.nullable_text(option.map(
      code.code_challenge_method,
      types.code_challenge_method_to_string,
    )),
    executor.nullable_text(code.nonce),
    DbInt(code.created_at),
    DbInt(code.expires_at),
    executor.bool_value(code.used),
  ]

  executor.exec(exec, sql, params)
}

/// Get an authorization code by code value
pub fn get(
  exec: Executor,
  code_value: String,
) -> Result(Option(OAuthAuthorizationCode), DbError) {
  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "SELECT code, client_id, user_id, session_id, session_iteration, redirect_uri, scope,
              code_challenge, code_challenge_method, nonce, created_at, expires_at, used
       FROM oauth_authorization_code WHERE code = "
      <> executor.placeholder(exec, 1)
    executor.PostgreSQL ->
      "SELECT code, client_id, user_id, session_id, session_iteration, redirect_uri, scope,
              code_challenge, code_challenge_method, nonce, created_at, expires_at, used::int
       FROM oauth_authorization_code WHERE code = "
      <> executor.placeholder(exec, 1)
  }

  case executor.query(exec, sql, [Text(code_value)], decoder()) {
    Ok(rows) ->
      case list.first(rows) {
        Ok(code) -> Ok(Some(code))
        Error(_) -> Ok(None)
      }
    Error(err) -> Error(err)
  }
}

/// Mark an authorization code as used
pub fn mark_used(exec: Executor, code_value: String) -> Result(Nil, DbError) {
  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "UPDATE oauth_authorization_code SET used = 1 WHERE code = "
      <> executor.placeholder(exec, 1)
    executor.PostgreSQL ->
      "UPDATE oauth_authorization_code SET used = TRUE WHERE code = "
      <> executor.placeholder(exec, 1)
  }

  executor.exec(exec, sql, [Text(code_value)])
}

/// Delete an authorization code
pub fn delete(exec: Executor, code_value: String) -> Result(Nil, DbError) {
  let sql =
    "DELETE FROM oauth_authorization_code WHERE code = "
    <> executor.placeholder(exec, 1)

  executor.exec(exec, sql, [Text(code_value)])
}

/// Delete expired authorization codes
pub fn delete_expired(exec: Executor, now: Int) -> Result(Nil, DbError) {
  let sql =
    "DELETE FROM oauth_authorization_code WHERE expires_at <= "
    <> executor.placeholder(exec, 1)

  executor.exec(exec, sql, [DbInt(now)])
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
