import gleam/result
import sqlight

/// Creates the record table if it doesn't exist
pub fn create_record_table(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let create_table_sql =
    "
    CREATE TABLE IF NOT EXISTS record (
      uri TEXT PRIMARY KEY NOT NULL,
      cid TEXT NOT NULL,
      did TEXT NOT NULL,
      collection TEXT NOT NULL,
      json TEXT NOT NULL,
      indexed_at TEXT NOT NULL DEFAULT (datetime('now'))
    )
  "

  let create_did_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_record_did
    ON record(did)
  "

  let create_collection_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_record_collection
    ON record(collection)
  "

  let create_did_collection_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_record_did_collection
    ON record(did, collection)
  "

  let create_indexed_at_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_record_indexed_at
    ON record(indexed_at DESC)
  "

  use _ <- result.try(sqlight.exec(create_table_sql, conn))
  use _ <- result.try(sqlight.exec(create_did_index_sql, conn))
  use _ <- result.try(sqlight.exec(create_collection_index_sql, conn))
  use _ <- result.try(sqlight.exec(create_did_collection_index_sql, conn))
  use _ <- result.try(sqlight.exec(create_indexed_at_index_sql, conn))
  Ok(Nil)
}

/// Creates the actor table if it doesn't exist
pub fn create_actor_table(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let create_table_sql =
    "
    CREATE TABLE IF NOT EXISTS actor (
      did TEXT PRIMARY KEY NOT NULL,
      handle TEXT,
      indexed_at TEXT NOT NULL
    )
  "

  let create_handle_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_actor_handle
    ON actor(handle)
  "

  let create_indexed_at_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_actor_indexed_at
    ON actor(indexed_at DESC)
  "

  use _ <- result.try(sqlight.exec(create_table_sql, conn))
  use _ <- result.try(sqlight.exec(create_handle_index_sql, conn))
  use _ <- result.try(sqlight.exec(create_indexed_at_index_sql, conn))
  Ok(Nil)
}

/// Creates the lexicon table if it doesn't exist
pub fn create_lexicon_table(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let create_table_sql =
    "
    CREATE TABLE IF NOT EXISTS lexicon (
      id TEXT PRIMARY KEY NOT NULL,
      json TEXT NOT NULL,
      created_at TEXT NOT NULL DEFAULT (datetime('now'))
    )
  "

  let create_created_at_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_lexicon_created_at
    ON lexicon(created_at DESC)
  "

  use _ <- result.try(sqlight.exec(create_table_sql, conn))
  use _ <- result.try(sqlight.exec(create_created_at_index_sql, conn))
  Ok(Nil)
}

/// Creates the config table if it doesn't exist
pub fn create_config_table(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let create_table_sql =
    "
    CREATE TABLE IF NOT EXISTS config (
      key TEXT PRIMARY KEY NOT NULL,
      value TEXT NOT NULL,
      updated_at TEXT NOT NULL DEFAULT (datetime('now'))
    )
  "

  sqlight.exec(create_table_sql, conn)
}

/// Creates the CID index for record deduplication
pub fn create_cid_index(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let create_cid_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_record_cid
    ON record(cid)
  "

  sqlight.exec(create_cid_index_sql, conn)
}

/// Creates the jetstream_activity table for 24h activity log
pub fn create_jetstream_activity_table(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let create_table_sql =
    "
    CREATE TABLE IF NOT EXISTS jetstream_activity (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      timestamp TEXT NOT NULL,
      operation TEXT NOT NULL,
      collection TEXT NOT NULL,
      did TEXT NOT NULL,
      status TEXT NOT NULL,
      error_message TEXT,
      event_json TEXT NOT NULL
    )
  "

  let create_timestamp_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_jetstream_activity_timestamp
    ON jetstream_activity(timestamp DESC)
  "

  use _ <- result.try(sqlight.exec(create_table_sql, conn))
  sqlight.exec(create_timestamp_index_sql, conn)
}

/// Creates the jetstream_cursor table for cursor tracking
pub fn create_jetstream_cursor_table(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let create_table_sql =
    "
    CREATE TABLE IF NOT EXISTS jetstream_cursor (
      id INTEGER PRIMARY KEY CHECK (id = 1),
      cursor INTEGER NOT NULL,
      updated_at TEXT NOT NULL DEFAULT (datetime('now'))
    )
  "

  sqlight.exec(create_table_sql, conn)
}
