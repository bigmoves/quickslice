import database/executor.{type DbError, type Executor, Text}
import database/types.{type Actor, Actor}
import gleam/dynamic/decode
import gleam/list
import gleam/string

// ===== Actor Functions =====

/// Inserts or updates an actor in the database
pub fn upsert(
  exec: Executor,
  did: String,
  handle: String,
) -> Result(Nil, DbError) {
  let now = executor.now(exec)
  let p1 = executor.placeholder(exec, 1)
  let p2 = executor.placeholder(exec, 2)

  // Use dialect-specific UPSERT syntax
  let sql = case executor.dialect(exec) {
    executor.SQLite -> "INSERT INTO actor (did, handle, indexed_at)
       VALUES (" <> p1 <> ", " <> p2 <> ", " <> now <> ")
       ON CONFLICT(did) DO UPDATE SET
         handle = excluded.handle,
         indexed_at = excluded.indexed_at"
    executor.PostgreSQL -> "INSERT INTO actor (did, handle, indexed_at)
       VALUES (" <> p1 <> ", " <> p2 <> ", " <> now <> ")
       ON CONFLICT(did) DO UPDATE SET
         handle = EXCLUDED.handle,
         indexed_at = EXCLUDED.indexed_at"
  }

  executor.exec(exec, sql, [Text(did), Text(handle)])
}

/// Gets an actor by DID
pub fn get(exec: Executor, did: String) -> Result(List(Actor), DbError) {
  // PostgreSQL: indexed_at is TIMESTAMPTZ (needs ::text cast)
  // SQLite: indexed_at is TEXT
  let sql = case executor.dialect(exec) {
    executor.SQLite -> "SELECT did, handle, indexed_at
       FROM actor
       WHERE did = " <> executor.placeholder(exec, 1)
    executor.PostgreSQL -> "SELECT did, handle, indexed_at::text
       FROM actor
       WHERE did = " <> executor.placeholder(exec, 1)
  }

  let decoder = {
    use did <- decode.field(0, decode.string)
    use handle <- decode.field(1, decode.string)
    use indexed_at <- decode.field(2, decode.string)
    decode.success(Actor(did:, handle:, indexed_at:))
  }

  executor.query(exec, sql, [Text(did)], decoder)
}

/// Gets an actor by handle
pub fn get_by_handle(
  exec: Executor,
  handle: String,
) -> Result(List(Actor), DbError) {
  // PostgreSQL: indexed_at is TIMESTAMPTZ (needs ::text cast)
  // SQLite: indexed_at is TEXT
  let sql = case executor.dialect(exec) {
    executor.SQLite -> "SELECT did, handle, indexed_at
       FROM actor
       WHERE handle = " <> executor.placeholder(exec, 1)
    executor.PostgreSQL -> "SELECT did, handle, indexed_at::text
       FROM actor
       WHERE handle = " <> executor.placeholder(exec, 1)
  }

  let decoder = {
    use did <- decode.field(0, decode.string)
    use handle <- decode.field(1, decode.string)
    use indexed_at <- decode.field(2, decode.string)
    decode.success(Actor(did:, handle:, indexed_at:))
  }

  executor.query(exec, sql, [Text(handle)], decoder)
}

/// Gets the total number of actors in the database
pub fn get_count(exec: Executor) -> Result(Int, DbError) {
  let sql = "SELECT COUNT(*) as count FROM actor"

  let decoder = {
    use count <- decode.field(0, decode.int)
    decode.success(count)
  }

  case executor.query(exec, sql, [], decoder) {
    Ok([count]) -> Ok(count)
    Ok(_) -> Ok(0)
    Error(err) -> Error(err)
  }
}

/// Deletes all actors from the database
pub fn delete_all(exec: Executor) -> Result(Nil, DbError) {
  executor.exec(exec, "DELETE FROM actor", [])
}

/// Batch upsert multiple actors in a single transaction
/// More efficient than individual upserts for large datasets
pub fn batch_upsert(
  exec: Executor,
  actors: List(#(String, String)),
) -> Result(Nil, DbError) {
  case actors {
    [] -> Ok(Nil)
    _ -> {
      // Process in chunks to avoid parameter limits
      // SQLite: 999 max params, PostgreSQL: 65535 max params
      // 2 params per actor, use 100 for safety
      let batch_size = 100

      list.sized_chunk(actors, batch_size)
      |> list.try_each(fn(batch) { batch_upsert_chunk(exec, batch) })
    }
  }
}

/// Internal: upsert a single batch of actors
fn batch_upsert_chunk(
  exec: Executor,
  batch: List(#(String, String)),
) -> Result(Nil, DbError) {
  let now = executor.now(exec)

  // Build placeholders for all actors in batch
  let value_placeholders =
    list.index_map(batch, fn(_, i) {
      let p1 = executor.placeholder(exec, i * 2 + 1)
      let p2 = executor.placeholder(exec, i * 2 + 2)
      "(" <> p1 <> ", " <> p2 <> ", " <> now <> ")"
    })
    |> string.join(", ")

  // Use dialect-specific UPSERT syntax
  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "INSERT INTO actor (did, handle, indexed_at) VALUES "
      <> value_placeholders
      <> " ON CONFLICT(did) DO UPDATE SET handle = excluded.handle, indexed_at = excluded.indexed_at"
    executor.PostgreSQL ->
      "INSERT INTO actor (did, handle, indexed_at) VALUES "
      <> value_placeholders
      <> " ON CONFLICT(did) DO UPDATE SET handle = EXCLUDED.handle, indexed_at = EXCLUDED.indexed_at"
  }

  // Flatten actor tuples into parameter list
  let params =
    list.flat_map(batch, fn(actor) {
      let #(did, handle) = actor
      [Text(did), Text(handle)]
    })

  executor.exec(exec, sql, params)
}
