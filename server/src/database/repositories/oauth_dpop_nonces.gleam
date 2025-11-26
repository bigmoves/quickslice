/// OAuth DPoP nonce repository operations
import database/types.{type OAuthDpopNonce, OAuthDpopNonce}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import sqlight

/// Insert a new DPoP nonce
pub fn insert(
  conn: sqlight.Connection,
  nonce: OAuthDpopNonce,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO oauth_dpop_nonce (nonce, expires_at)
    VALUES (?, ?)
  "

  let params = [sqlight.text(nonce.nonce), sqlight.int(nonce.expires_at)]

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: params,
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Get a DPoP nonce by value
pub fn get(
  conn: sqlight.Connection,
  nonce_value: String,
) -> Result(Option(OAuthDpopNonce), sqlight.Error) {
  let sql = "SELECT nonce, expires_at FROM oauth_dpop_nonce WHERE nonce = ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(nonce_value)],
    expecting: decoder(),
  ))

  case list.first(rows) {
    Ok(nonce) -> Ok(Some(nonce))
    Error(_) -> Ok(None)
  }
}

/// Delete a DPoP nonce
pub fn delete(
  conn: sqlight.Connection,
  nonce_value: String,
) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM oauth_dpop_nonce WHERE nonce = ?"

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(nonce_value)],
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Delete expired DPoP nonces
pub fn delete_expired(
  conn: sqlight.Connection,
  now: Int,
) -> Result(Int, sqlight.Error) {
  let sql = "DELETE FROM oauth_dpop_nonce WHERE expires_at <= ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.int(now)],
    expecting: decode.dynamic,
  ))
  Ok(list.length(rows))
}

/// Decode DPoP nonce from database row
fn decoder() -> decode.Decoder(OAuthDpopNonce) {
  use nonce <- decode.field(0, decode.string)
  use expires_at <- decode.field(1, decode.int)

  decode.success(OAuthDpopNonce(nonce: nonce, expires_at: expires_at))
}
