/// OAuth DPoP nonce repository operations
import database/executor.{type DbError, type Executor, Int as DbInt, Text}
import database/types.{type OAuthDpopNonce, OAuthDpopNonce}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}

/// Insert a new DPoP nonce
pub fn insert(exec: Executor, nonce: OAuthDpopNonce) -> Result(Nil, DbError) {
  let p1 = executor.placeholder(exec, 1)
  let p2 = executor.placeholder(exec, 2)

  let sql = "INSERT INTO oauth_dpop_nonce (nonce, expires_at)
     VALUES (" <> p1 <> ", " <> p2 <> ")"

  executor.exec(exec, sql, [Text(nonce.nonce), DbInt(nonce.expires_at)])
}

/// Get a DPoP nonce by value
pub fn get(
  exec: Executor,
  nonce_value: String,
) -> Result(Option(OAuthDpopNonce), DbError) {
  let sql =
    "SELECT nonce, expires_at FROM oauth_dpop_nonce WHERE nonce = "
    <> executor.placeholder(exec, 1)

  case executor.query(exec, sql, [Text(nonce_value)], decoder()) {
    Ok(rows) ->
      case list.first(rows) {
        Ok(nonce) -> Ok(Some(nonce))
        Error(_) -> Ok(None)
      }
    Error(err) -> Error(err)
  }
}

/// Delete a DPoP nonce
pub fn delete(exec: Executor, nonce_value: String) -> Result(Nil, DbError) {
  let sql =
    "DELETE FROM oauth_dpop_nonce WHERE nonce = "
    <> executor.placeholder(exec, 1)

  executor.exec(exec, sql, [Text(nonce_value)])
}

/// Delete expired DPoP nonces
pub fn delete_expired(exec: Executor, now: Int) -> Result(Nil, DbError) {
  let sql =
    "DELETE FROM oauth_dpop_nonce WHERE expires_at <= "
    <> executor.placeholder(exec, 1)

  executor.exec(exec, sql, [DbInt(now)])
}

/// Decode DPoP nonce from database row
fn decoder() -> decode.Decoder(OAuthDpopNonce) {
  use nonce <- decode.field(0, decode.string)
  use expires_at <- decode.field(1, decode.int)

  decode.success(OAuthDpopNonce(nonce: nonce, expires_at: expires_at))
}
