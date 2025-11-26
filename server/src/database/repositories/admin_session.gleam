/// Admin session repository operations
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import sqlight

/// Admin session record
pub type AdminSession {
  AdminSession(session_id: String, atp_session_id: String, created_at: Int)
}

/// Create a new admin session
pub fn insert(
  conn: sqlight.Connection,
  session_id: String,
  atp_session_id: String,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO admin_session (session_id, atp_session_id)
    VALUES (?, ?)
  "

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(session_id), sqlight.text(atp_session_id)],
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Get admin session by session_id (cookie ID)
pub fn get(
  conn: sqlight.Connection,
  session_id: String,
) -> Result(Option(AdminSession), sqlight.Error) {
  let sql =
    "
    SELECT session_id, atp_session_id, created_at
    FROM admin_session
    WHERE session_id = ?
  "

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(session_id)],
    expecting: decoder(),
  ))

  case list.first(rows) {
    Ok(session) -> Ok(Some(session))
    Error(_) -> Ok(None)
  }
}

/// Delete admin session (logout)
pub fn delete(
  conn: sqlight.Connection,
  session_id: String,
) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM admin_session WHERE session_id = ?"

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(session_id)],
    expecting: decode.dynamic,
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
