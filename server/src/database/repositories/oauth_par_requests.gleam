/// OAuth PAR (Pushed Authorization Request) repository operations
import database/executor.{type DbError, type Executor, Int as DbInt, Text}
import database/types.{type OAuthParRequest, OAuthParRequest}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}

/// Insert a new PAR request
pub fn insert(exec: Executor, par: OAuthParRequest) -> Result(Nil, DbError) {
  let p1 = executor.placeholder(exec, 1)
  let p2 = executor.placeholder(exec, 2)
  let p3 = executor.placeholder(exec, 3)
  let p4 = executor.placeholder(exec, 4)
  let p5 = executor.placeholder(exec, 5)
  let p6 = executor.placeholder(exec, 6)
  let p7 = executor.placeholder(exec, 7)

  let sql = "INSERT INTO oauth_par_request (
      request_uri, authorization_request, client_id,
      created_at, expires_at, subject, metadata
    ) VALUES (" <> p1 <> ", " <> p2 <> ", " <> p3 <> ", " <> p4 <> ", " <> p5 <> ", " <> p6 <> ", " <> p7 <> ")"

  let params = [
    Text(par.request_uri),
    Text(par.authorization_request),
    Text(par.client_id),
    DbInt(par.created_at),
    DbInt(par.expires_at),
    executor.nullable_text(par.subject),
    Text(par.metadata),
  ]

  executor.exec(exec, sql, params)
}

/// Get a PAR request by request_uri
pub fn get(
  exec: Executor,
  request_uri: String,
) -> Result(Option(OAuthParRequest), DbError) {
  // PostgreSQL: metadata is JSONB (needs ::text cast)
  let sql = case executor.dialect(exec) {
    executor.SQLite -> "SELECT request_uri, authorization_request, client_id,
              created_at, expires_at, subject, metadata
       FROM oauth_par_request WHERE request_uri = " <> executor.placeholder(
        exec,
        1,
      )
    executor.PostgreSQL ->
      "SELECT request_uri, authorization_request, client_id,
              created_at, expires_at, subject, metadata::text
       FROM oauth_par_request WHERE request_uri = " <> executor.placeholder(
        exec,
        1,
      )
  }

  case executor.query(exec, sql, [Text(request_uri)], decoder()) {
    Ok(rows) ->
      case list.first(rows) {
        Ok(par) -> Ok(Some(par))
        Error(_) -> Ok(None)
      }
    Error(err) -> Error(err)
  }
}

/// Delete a PAR request
pub fn delete(exec: Executor, request_uri: String) -> Result(Nil, DbError) {
  let sql =
    "DELETE FROM oauth_par_request WHERE request_uri = "
    <> executor.placeholder(exec, 1)

  executor.exec(exec, sql, [Text(request_uri)])
}

/// Delete expired PAR requests
pub fn delete_expired(exec: Executor, now: Int) -> Result(Nil, DbError) {
  let sql =
    "DELETE FROM oauth_par_request WHERE expires_at <= "
    <> executor.placeholder(exec, 1)

  executor.exec(exec, sql, [DbInt(now)])
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
