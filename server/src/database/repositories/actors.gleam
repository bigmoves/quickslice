import database/types.{type Actor, Actor}
import gleam/dynamic/decode
import gleam/list
import gleam/result
import gleam/string
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

/// Batch upsert multiple actors in a single transaction
/// More efficient than individual upserts for large datasets
pub fn batch_upsert(
  conn: sqlight.Connection,
  actors: List(#(String, String)),
) -> Result(Nil, sqlight.Error) {
  case actors {
    [] -> Ok(Nil)
    _ -> {
      // Process in chunks of 100 to avoid SQLite parameter limits
      // (999 max params, 2 params per actor = 400 safe, use 100 for safety)
      let batch_size = 100

      list.sized_chunk(actors, batch_size)
      |> list.try_each(fn(batch) {
        // Build the SQL with multiple value sets
        let value_placeholders =
          list.repeat("(?, ?, datetime('now'))", list.length(batch))
          |> string.join(", ")

        let sql =
          "INSERT INTO actor (did, handle, indexed_at) VALUES "
          <> value_placeholders
          <> " ON CONFLICT(did) DO UPDATE SET handle = excluded.handle, indexed_at = excluded.indexed_at"

        // Flatten actor tuples into parameter list
        let params =
          list.flat_map(batch, fn(actor) {
            let #(did, handle) = actor
            [sqlight.text(did), sqlight.text(handle)]
          })

        use _ <- result.try(sqlight.query(
          sql,
          on: conn,
          with: params,
          expecting: decode.string,
        ))
        Ok(Nil)
      })
    }
  }
}
