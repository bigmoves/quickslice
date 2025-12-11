/// Admin session repository operations
import database/executor.{type DbError, type Executor, Text}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

/// Admin session record
pub type AdminSession {
  AdminSession(session_id: String, atp_session_id: String, created_at: Int)
}

/// Create a new admin session
pub fn insert(
  exec: Executor,
  session_id: String,
  atp_session_id: String,
) -> Result(Nil, DbError) {
  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "INSERT INTO admin_session (session_id, atp_session_id) VALUES (?, ?)"
    executor.PostgreSQL ->
      "INSERT INTO admin_session (session_id, atp_session_id) VALUES ($1, $2)"
  }

  use _ <- result.try(executor.query(
    exec,
    sql,
    [Text(session_id), Text(atp_session_id)],
    decode.dynamic,
  ))
  Ok(Nil)
}

/// Get admin session by session_id (cookie ID)
pub fn get(
  exec: Executor,
  session_id: String,
) -> Result(Option(AdminSession), DbError) {
  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "SELECT session_id, atp_session_id, created_at FROM admin_session WHERE session_id = ?"
    executor.PostgreSQL ->
      "SELECT session_id, atp_session_id, created_at FROM admin_session WHERE session_id = $1"
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

/// Delete admin session (logout)
pub fn delete(exec: Executor, session_id: String) -> Result(Nil, DbError) {
  let sql = case executor.dialect(exec) {
    executor.SQLite -> "DELETE FROM admin_session WHERE session_id = ?"
    executor.PostgreSQL -> "DELETE FROM admin_session WHERE session_id = $1"
  }

  use _ <- result.try(executor.query(
    exec,
    sql,
    [Text(session_id)],
    decode.dynamic,
  ))
  Ok(Nil)
}

/// Decode admin session from database row
fn decoder() -> decode.Decoder(AdminSession) {
  use session_id <- decode.field(0, decode.string)
  use atp_session_id <- decode.field(1, decode.string)
  use created_at <- decode.field(2, decode.int)

  decode.success(AdminSession(
    session_id: session_id,
    atp_session_id: atp_session_id,
    created_at: created_at,
  ))
}
