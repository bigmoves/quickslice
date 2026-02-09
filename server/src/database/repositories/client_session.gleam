/// Client session repository operations
/// Stores sessions for JS SDK clients using HTTP-only cookies
import database/executor.{type DbError, type Executor, Int, Text}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import lib/oauth/token_generator

/// Client session record
pub type ClientSession {
  ClientSession(
    session_id: String,
    client_id: String,
    user_did: Option(String),
    atp_session_id: Option(String),
    dpop_jkt: String,
    created_at: Int,
    last_activity_at: Int,
  )
}

/// Create a new client session
pub fn insert(
  exec: Executor,
  session_id: String,
  client_id: String,
  user_did: Option(String),
  atp_session_id: Option(String),
  dpop_jkt: String,
) -> Result(ClientSession, DbError) {
  let now = token_generator.current_timestamp()

  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "INSERT INTO client_session (session_id, client_id, user_did, atp_session_id, dpop_jkt, created_at, last_activity_at)
       VALUES (?, ?, ?, ?, ?, ?, ?)"
    executor.PostgreSQL ->
      "INSERT INTO client_session (session_id, client_id, user_did, atp_session_id, dpop_jkt, created_at, last_activity_at)
       VALUES ($1, $2, $3, $4, $5, $6, $7)"
  }

  let user_did_param = case user_did {
    Some(did) -> Text(did)
    None -> executor.Null
  }

  let atp_session_id_param = case atp_session_id {
    Some(id) -> Text(id)
    None -> executor.Null
  }

  use _ <- result.try(executor.query(
    exec,
    sql,
    [
      Text(session_id),
      Text(client_id),
      user_did_param,
      atp_session_id_param,
      Text(dpop_jkt),
      Int(now),
      Int(now),
    ],
    decode.dynamic,
  ))

  Ok(ClientSession(
    session_id: session_id,
    client_id: client_id,
    user_did: user_did,
    atp_session_id: atp_session_id,
    dpop_jkt: dpop_jkt,
    created_at: now,
    last_activity_at: now,
  ))
}

/// Get client session by session_id
pub fn get(
  exec: Executor,
  session_id: String,
) -> Result(Option(ClientSession), DbError) {
  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "SELECT session_id, client_id, user_did, atp_session_id, dpop_jkt, created_at, last_activity_at
       FROM client_session WHERE session_id = ?"
    executor.PostgreSQL ->
      "SELECT session_id, client_id, user_did, atp_session_id, dpop_jkt, created_at, last_activity_at
       FROM client_session WHERE session_id = $1"
  }

  use rows <- result.try(executor.query(
    exec,
    sql,
    [Text(session_id)],
    decoder(),
  ))

  case list.first(rows) {
    Ok(session) -> Ok(Some(session))
    Error(_) -> Ok(None)
  }
}

/// Update last activity timestamp for a session
pub fn touch(exec: Executor, session_id: String) -> Result(Nil, DbError) {
  let now = token_generator.current_timestamp()

  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "UPDATE client_session SET last_activity_at = ? WHERE session_id = ?"
    executor.PostgreSQL ->
      "UPDATE client_session SET last_activity_at = $1 WHERE session_id = $2"
  }

  use _ <- result.try(executor.query(
    exec,
    sql,
    [Int(now), Text(session_id)],
    decode.dynamic,
  ))
  Ok(Nil)
}

/// Update session with user DID and ATP session ID after OAuth completes
pub fn update_auth(
  exec: Executor,
  session_id: String,
  user_did: String,
  atp_session_id: String,
) -> Result(Nil, DbError) {
  let now = token_generator.current_timestamp()

  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "UPDATE client_session SET user_did = ?, atp_session_id = ?, last_activity_at = ? WHERE session_id = ?"
    executor.PostgreSQL ->
      "UPDATE client_session SET user_did = $1, atp_session_id = $2, last_activity_at = $3 WHERE session_id = $4"
  }

  use _ <- result.try(executor.query(
    exec,
    sql,
    [Text(user_did), Text(atp_session_id), Int(now), Text(session_id)],
    decode.dynamic,
  ))
  Ok(Nil)
}

/// Delete client session (logout)
pub fn delete(exec: Executor, session_id: String) -> Result(Nil, DbError) {
  let sql = case executor.dialect(exec) {
    executor.SQLite -> "DELETE FROM client_session WHERE session_id = ?"
    executor.PostgreSQL -> "DELETE FROM client_session WHERE session_id = $1"
  }

  use _ <- result.try(executor.query(
    exec,
    sql,
    [Text(session_id)],
    decode.dynamic,
  ))
  Ok(Nil)
}

/// Delete sessions older than the given max age (in seconds)
pub fn delete_expired(
  exec: Executor,
  max_age_seconds: Int,
) -> Result(Nil, DbError) {
  let cutoff = token_generator.current_timestamp() - max_age_seconds

  let sql = case executor.dialect(exec) {
    executor.SQLite -> "DELETE FROM client_session WHERE last_activity_at < ?"
    executor.PostgreSQL ->
      "DELETE FROM client_session WHERE last_activity_at < $1"
  }

  executor.exec(exec, sql, [Int(cutoff)])
}

/// Decode client session from database row
fn decoder() -> decode.Decoder(ClientSession) {
  use session_id <- decode.field(0, decode.string)
  use client_id <- decode.field(1, decode.string)
  use user_did <- decode.field(2, decode.optional(decode.string))
  use atp_session_id <- decode.field(3, decode.optional(decode.string))
  use dpop_jkt <- decode.field(4, decode.string)
  use created_at <- decode.field(5, decode.int)
  use last_activity_at <- decode.field(6, decode.int)

  decode.success(ClientSession(
    session_id: session_id,
    client_id: client_id,
    user_did: user_did,
    atp_session_id: atp_session_id,
    dpop_jkt: dpop_jkt,
    created_at: created_at,
    last_activity_at: last_activity_at,
  ))
}
