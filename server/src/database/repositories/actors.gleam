import database/types.{type Actor, Actor}
import gleam/dynamic/decode
import gleam/result
import sqlight

// ===== Actor Functions =====

/// Inserts or updates an actor in the database
pub fn upsert(
  conn: sqlight.Connection,
  did: String,
  handle: String,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO actor (did, handle, indexed_at)
    VALUES (?, ?, datetime('now'))
    ON CONFLICT(did) DO UPDATE SET
      handle = excluded.handle,
      indexed_at = excluded.indexed_at
  "

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(did), sqlight.text(handle)],
    expecting: decode.string,
  ))
  Ok(Nil)
}

/// Gets an actor by DID
pub fn get(
  conn: sqlight.Connection,
  did: String,
) -> Result(List(Actor), sqlight.Error) {
  let sql =
    "
    SELECT did, handle, indexed_at
    FROM actor
    WHERE did = ?
  "

  let decoder = {
    use did <- decode.field(0, decode.string)
    use handle <- decode.field(1, decode.string)
    use indexed_at <- decode.field(2, decode.string)
    decode.success(Actor(did:, handle:, indexed_at:))
  }

  sqlight.query(sql, on: conn, with: [sqlight.text(did)], expecting: decoder)
}

/// Gets an actor by handle
pub fn get_by_handle(
  conn: sqlight.Connection,
  handle: String,
) -> Result(List(Actor), sqlight.Error) {
  let sql =
    "
    SELECT did, handle, indexed_at
    FROM actor
    WHERE handle = ?
  "

  let decoder = {
    use did <- decode.field(0, decode.string)
    use handle <- decode.field(1, decode.string)
    use indexed_at <- decode.field(2, decode.string)
    decode.success(Actor(did:, handle:, indexed_at:))
  }

  sqlight.query(sql, on: conn, with: [sqlight.text(handle)], expecting: decoder)
}

/// Gets the total number of actors in the database
pub fn get_count(conn: sqlight.Connection) -> Result(Int, sqlight.Error) {
  let sql =
    "
    SELECT COUNT(*) as count
    FROM actor
  "

  let decoder = {
    use count <- decode.field(0, decode.int)
    decode.success(count)
  }

  case sqlight.query(sql, on: conn, with: [], expecting: decoder) {
    Ok([count]) -> Ok(count)
    Ok(_) -> Ok(0)
    Error(err) -> Error(err)
  }
}

/// Deletes all actors from the database
pub fn delete_all(conn: sqlight.Connection) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM actor"

  sqlight.exec(sql, conn)
  |> result.map(fn(_) { Nil })
}
