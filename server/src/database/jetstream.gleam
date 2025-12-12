import database/executor.{type DbError, type Executor, Int}
import gleam/dynamic/decode
import gleam/option.{type Option, None, Some}
import gleam/result

// ===== Jetstream Cursor Functions =====

/// Gets the current Jetstream cursor value
pub fn get_cursor(exec: Executor) -> Result(Option(Int), DbError) {
  let sql = "SELECT cursor FROM jetstream_cursor WHERE id = 1"

  let decoder = {
    use cursor <- decode.field(0, decode.int)
    decode.success(cursor)
  }

  case executor.query(exec, sql, [], decoder) {
    Ok([cursor]) -> Ok(Some(cursor))
    Ok([]) -> Ok(None)
    Ok(_) -> Ok(None)
    Error(err) -> Error(err)
  }
}

/// Sets or updates the Jetstream cursor value
pub fn set_cursor(exec: Executor, cursor: Int) -> Result(Nil, DbError) {
  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "INSERT INTO jetstream_cursor (id, cursor, updated_at)
       VALUES (1, ?, datetime('now'))
       ON CONFLICT(id) DO UPDATE SET
         cursor = excluded.cursor,
         updated_at = datetime('now')"
    executor.PostgreSQL ->
      "INSERT INTO jetstream_cursor (id, cursor, updated_at)
       VALUES (1, $1, NOW())
       ON CONFLICT(id) DO UPDATE SET
         cursor = EXCLUDED.cursor,
         updated_at = NOW()"
  }

  use _ <- result.try(executor.query(exec, sql, [Int(cursor)], decode.string))
  Ok(Nil)
}

/// Clears the Jetstream cursor (for dev reset)
pub fn clear_cursor(exec: Executor) -> Result(Nil, DbError) {
  let sql = "DELETE FROM jetstream_cursor WHERE id = 1"
  executor.exec(exec, sql, [])
}
