import gleam/dynamic/decode
import gleam/option.{type Option, None, Some}
import gleam/result
import sqlight

// ===== Jetstream Cursor Functions =====

/// Gets the current Jetstream cursor value
pub fn get_cursor(
  conn: sqlight.Connection,
) -> Result(Option(Int), sqlight.Error) {
  let sql =
    "
    SELECT cursor
    FROM jetstream_cursor
    WHERE id = 1
  "

  let decoder = {
    use cursor <- decode.field(0, decode.int)
    decode.success(cursor)
  }

  case sqlight.query(sql, on: conn, with: [], expecting: decoder) {
    Ok([cursor]) -> Ok(Some(cursor))
    Ok([]) -> Ok(None)
    Ok(_) -> Ok(None)
    Error(err) -> Error(err)
  }
}

/// Sets or updates the Jetstream cursor value
pub fn set_cursor(
  conn: sqlight.Connection,
  cursor: Int,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO jetstream_cursor (id, cursor, updated_at)
    VALUES (1, ?, datetime('now'))
    ON CONFLICT(id) DO UPDATE SET
      cursor = excluded.cursor,
      updated_at = datetime('now')
  "

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.int(cursor)],
    expecting: decode.string,
  ))
  Ok(Nil)
}

/// Clears the Jetstream cursor (for dev reset)
pub fn clear_cursor(conn: sqlight.Connection) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM jetstream_cursor WHERE id = 1"

  sqlight.exec(sql, conn)
  |> result.map(fn(_) { Nil })
}
