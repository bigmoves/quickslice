/// OAuth PAR (Pushed Authorization Request) repository operations
import database/types.{type OAuthParRequest, OAuthParRequest}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import sqlight

/// Insert a new PAR request
pub fn insert(
  conn: sqlight.Connection,
  par: OAuthParRequest,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO oauth_par_request (
      request_uri, authorization_request, client_id,
      created_at, expires_at, subject, metadata
    ) VALUES (?, ?, ?, ?, ?, ?, ?)
  "

  let params = [
    sqlight.text(par.request_uri),
    sqlight.text(par.authorization_request),
    sqlight.text(par.client_id),
    sqlight.int(par.created_at),
    sqlight.int(par.expires_at),
    sqlight.nullable(sqlight.text, par.subject),
    sqlight.text(par.metadata),
  ]

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: params,
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Get a PAR request by request_uri
pub fn get(
  conn: sqlight.Connection,
  request_uri: String,
) -> Result(Option(OAuthParRequest), sqlight.Error) {
  let sql =
    "SELECT request_uri, authorization_request, client_id,
            created_at, expires_at, subject, metadata
     FROM oauth_par_request WHERE request_uri = ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(request_uri)],
    expecting: decoder(),
  ))

  case list.first(rows) {
    Ok(par) -> Ok(Some(par))
    Error(_) -> Ok(None)
  }
}

/// Delete a PAR request
pub fn delete(
  conn: sqlight.Connection,
  request_uri: String,
) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM oauth_par_request WHERE request_uri = ?"

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(request_uri)],
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Delete expired PAR requests
pub fn delete_expired(
  conn: sqlight.Connection,
  now: Int,
) -> Result(Int, sqlight.Error) {
  let sql = "DELETE FROM oauth_par_request WHERE expires_at <= ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.int(now)],
    expecting: decode.dynamic,
  ))
  Ok(list.length(rows))
}

/// Decode PAR request from database row
fn decoder() -> decode.Decoder(OAuthParRequest) {
  use request_uri <- decode.field(0, decode.string)
  use authorization_request <- decode.field(1, decode.string)
  use client_id <- decode.field(2, decode.string)
  use created_at <- decode.field(3, decode.int)
  use expires_at <- decode.field(4, decode.int)
  use subject <- decode.field(5, decode.optional(decode.string))
  use metadata <- decode.field(6, decode.string)

  decode.success(OAuthParRequest(
    request_uri: request_uri,
    authorization_request: authorization_request,
    client_id: client_id,
    created_at: created_at,
    expires_at: expires_at,
    subject: subject,
    metadata: metadata,
  ))
}
