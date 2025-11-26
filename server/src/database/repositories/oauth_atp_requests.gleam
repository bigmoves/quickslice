/// OAuth ATP request repository operations
import database/types.{type OAuthAtpRequest, OAuthAtpRequest}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import sqlight

/// Insert a new ATP request
pub fn insert(
  conn: sqlight.Connection,
  req: OAuthAtpRequest,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO oauth_atp_request (
      oauth_state, authorization_server, nonce, pkce_verifier,
      signing_public_key, dpop_private_key, created_at, expires_at
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
  "

  let params = [
    sqlight.text(req.oauth_state),
    sqlight.text(req.authorization_server),
    sqlight.text(req.nonce),
    sqlight.text(req.pkce_verifier),
    sqlight.text(req.signing_public_key),
    sqlight.text(req.dpop_private_key),
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

/// Get an ATP request by oauth_state
pub fn get(
  conn: sqlight.Connection,
  oauth_state: String,
) -> Result(Option(OAuthAtpRequest), sqlight.Error) {
  let sql =
    "SELECT oauth_state, authorization_server, nonce, pkce_verifier,
            signing_public_key, dpop_private_key, created_at, expires_at
     FROM oauth_atp_request WHERE oauth_state = ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(oauth_state)],
    expecting: decoder(),
  ))

  case list.first(rows) {
    Ok(req) -> Ok(Some(req))
    Error(_) -> Ok(None)
  }
}

/// Delete an ATP request
pub fn delete(
  conn: sqlight.Connection,
  oauth_state: String,
) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM oauth_atp_request WHERE oauth_state = ?"

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(oauth_state)],
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Delete expired ATP requests
pub fn delete_expired(
  conn: sqlight.Connection,
  now: Int,
) -> Result(Int, sqlight.Error) {
  let sql = "DELETE FROM oauth_atp_request WHERE expires_at <= ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.int(now)],
    expecting: decode.dynamic,
  ))
  Ok(list.length(rows))
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
