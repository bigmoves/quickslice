/// OAuth ATP request repository operations
import database/executor.{type DbError, type Executor, Int as DbInt, Text}
import database/types.{type OAuthAtpRequest, OAuthAtpRequest}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}

/// Insert a new ATP request
pub fn insert(exec: Executor, req: OAuthAtpRequest) -> Result(Nil, DbError) {
  let p1 = executor.placeholder(exec, 1)
  let p2 = executor.placeholder(exec, 2)
  let p3 = executor.placeholder(exec, 3)
  let p4 = executor.placeholder(exec, 4)
  let p5 = executor.placeholder(exec, 5)
  let p6 = executor.placeholder(exec, 6)
  let p7 = executor.placeholder(exec, 7)
  let p8 = executor.placeholder(exec, 8)

  let sql = "INSERT INTO oauth_atp_request (
      oauth_state, authorization_server, nonce, pkce_verifier,
      signing_public_key, dpop_private_key, created_at, expires_at
    ) VALUES (" <> p1 <> ", " <> p2 <> ", " <> p3 <> ", " <> p4 <> ", " <> p5 <> ", " <> p6 <> ", " <> p7 <> ", " <> p8 <> ")"

  let params = [
    Text(req.oauth_state),
    Text(req.authorization_server),
    Text(req.nonce),
    Text(req.pkce_verifier),
    Text(req.signing_public_key),
    Text(req.dpop_private_key),
    DbInt(req.created_at),
    DbInt(req.expires_at),
  ]

  executor.exec(exec, sql, params)
}

/// Get an ATP request by oauth_state
pub fn get(
  exec: Executor,
  oauth_state: String,
) -> Result(Option(OAuthAtpRequest), DbError) {
  let sql = "SELECT oauth_state, authorization_server, nonce, pkce_verifier,
            signing_public_key, dpop_private_key, created_at, expires_at
     FROM oauth_atp_request WHERE oauth_state = " <> executor.placeholder(
      exec,
      1,
    )

  case executor.query(exec, sql, [Text(oauth_state)], decoder()) {
    Ok(rows) ->
      case list.first(rows) {
        Ok(req) -> Ok(Some(req))
        Error(_) -> Ok(None)
      }
    Error(err) -> Error(err)
  }
}

/// Delete an ATP request
pub fn delete(exec: Executor, oauth_state: String) -> Result(Nil, DbError) {
  let sql =
    "DELETE FROM oauth_atp_request WHERE oauth_state = "
    <> executor.placeholder(exec, 1)

  executor.exec(exec, sql, [Text(oauth_state)])
}

/// Delete expired ATP requests
pub fn delete_expired(exec: Executor, now: Int) -> Result(Nil, DbError) {
  let sql =
    "DELETE FROM oauth_atp_request WHERE expires_at <= "
    <> executor.placeholder(exec, 1)

  executor.exec(exec, sql, [DbInt(now)])
}

/// Decode ATP request from database row
fn decoder() -> decode.Decoder(OAuthAtpRequest) {
  use oauth_state <- decode.field(0, decode.string)
  use authorization_server <- decode.field(1, decode.string)
  use nonce <- decode.field(2, decode.string)
  use pkce_verifier <- decode.field(3, decode.string)
  use signing_public_key <- decode.field(4, decode.string)
  use dpop_private_key <- decode.field(5, decode.string)
  use created_at <- decode.field(6, decode.int)
  use expires_at <- decode.field(7, decode.int)

  decode.success(OAuthAtpRequest(
    oauth_state: oauth_state,
    authorization_server: authorization_server,
    nonce: nonce,
    pkce_verifier: pkce_verifier,
    signing_public_key: signing_public_key,
    dpop_private_key: dpop_private_key,
    created_at: created_at,
    expires_at: expires_at,
  ))
}
