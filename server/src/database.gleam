import cursor
import gleam/dynamic/decode
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import logging
import oauth/session
import sqlight
import where_clause

pub type Record {
  Record(
    uri: String,
    cid: String,
    did: String,
    collection: String,
    json: String,
    indexed_at: String,
  )
}

pub type Actor {
  Actor(did: String, handle: String, indexed_at: String)
}

pub type Lexicon {
  Lexicon(id: String, json: String, created_at: String)
}

// ===== Schema Version Tracking =====

/// Creates the schema_version table if it doesn't exist
fn create_schema_version_table(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    CREATE TABLE IF NOT EXISTS schema_version (
      version INTEGER PRIMARY KEY,
      applied_at TEXT NOT NULL DEFAULT (datetime('now'))
    )
  "
  sqlight.exec(sql, conn)
}

/// Gets the current schema version (returns 0 if no version table exists)
fn get_current_version(conn: sqlight.Connection) -> Result(Int, sqlight.Error) {
  let sql =
    "
    SELECT version
    FROM schema_version
    ORDER BY version DESC
    LIMIT 1
  "

  let decoder = {
    use version <- decode.field(0, decode.int)
    decode.success(version)
  }

  case sqlight.query(sql, on: conn, with: [], expecting: decoder) {
    Ok([version, ..]) -> Ok(version)
    Ok([]) -> Ok(0)
    // If table doesn't exist yet, version is 0
    Error(_) -> Ok(0)
  }
}

/// Sets the schema version after a successful migration
fn set_version(
  conn: sqlight.Connection,
  version: Int,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO schema_version (version, applied_at)
    VALUES (?, datetime('now'))
  "

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.int(version)],
    expecting: decode.string,
  ))
  Ok(Nil)
}

/// Applies a migration function and updates the version on success
fn apply_migration(
  conn: sqlight.Connection,
  version: Int,
  migration: fn(sqlight.Connection) -> Result(Nil, sqlight.Error),
) -> Result(Nil, sqlight.Error) {
  // Run migration
  use _ <- result.try(migration(conn))

  // Update version on success
  use _ <- result.try(set_version(conn, version))

  logging.log(logging.Info, "Applied migration v" <> int.to_string(version))
  Ok(Nil)
}

// ===== Database Connection =====

/// Opens a connection to the SQLite database
pub fn connect(path: String) -> Result(sqlight.Connection, sqlight.Error) {
  use conn <- result.try(sqlight.open(path))

  // Enable WAL mode for better concurrency
  use _ <- result.try(sqlight.exec("PRAGMA journal_mode = WAL", conn))

  // Enable foreign key constraints
  use _ <- result.try(sqlight.exec("PRAGMA foreign_keys = ON", conn))

  Ok(conn)
}

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

// ===== Database Migrations =====

/// Migration v1: Initial schema with all tables
fn migration_v1(conn: sqlight.Connection) -> Result(Nil, sqlight.Error) {
  logging.log(logging.Info, "Running migration v1 (initial schema)...")

  // Create all tables and indexes
  use _ <- result.try(create_record_table(conn))
  use _ <- result.try(create_actor_table(conn))
  use _ <- result.try(create_lexicon_table(conn))
  use _ <- result.try(session.init_db(conn))

  Ok(Nil)
}

/// Migration v2: Add config table
fn migration_v2(conn: sqlight.Connection) -> Result(Nil, sqlight.Error) {
  logging.log(logging.Info, "Running migration v2 (config table)...")

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

/// Runs all pending migrations based on current schema version
fn run_migrations(conn: sqlight.Connection) -> Result(Nil, sqlight.Error) {
  use current_version <- result.try(get_current_version(conn))

  logging.log(
    logging.Info,
    "Current schema version: " <> int.to_string(current_version),
  )

  // Apply migrations sequentially based on current version
  case current_version {
    // Fresh database or pre-migration database - run v1
    0 -> {
      use _ <- result.try(apply_migration(conn, 1, migration_v1))
      apply_migration(conn, 2, migration_v2)
    }

    // Run v2 migration
    1 -> apply_migration(conn, 2, migration_v2)

    // Already at latest version
    2 -> {
      logging.log(logging.Info, "Schema is up to date (v2)")
      Ok(Nil)
    }

    // Future versions would be handled here:
    // 2 -> apply_migration(conn, 3, migration_v3)
    // 3 -> apply_migration(conn, 4, migration_v4)
    _ -> {
      logging.log(
        logging.Error,
        "Unknown schema version: " <> int.to_string(current_version),
      )
      Ok(Nil)
    }
  }
}

/// Initializes the database with all required tables using the migration system
pub fn initialize(path: String) -> Result(sqlight.Connection, sqlight.Error) {
  use conn <- result.try(connect(path))

  // Create schema version tracking table first
  use _ <- result.try(create_schema_version_table(conn))

  // Run any pending migrations
  use _ <- result.try(run_migrations(conn))

  logging.log(logging.Info, "Database initialized at: " <> path)
  Ok(conn)
}

// ===== Config Functions =====

/// Get a config value by key
pub fn get_config(
  conn: sqlight.Connection,
  key: String,
) -> Result(String, sqlight.Error) {
  let sql =
    "
    SELECT value
    FROM config
    WHERE key = ?
  "

  let decoder = {
    use value <- decode.field(0, decode.string)
    decode.success(value)
  }

  case sqlight.query(sql, on: conn, with: [sqlight.text(key)], expecting: decoder) {
    Ok([value, ..]) -> Ok(value)
    Ok([]) -> Error(sqlight.SqlightError(sqlight.ConstraintForeignkey, "Config key not found", -1))
    Error(err) -> Error(err)
  }
}

/// Set or update a config value
pub fn set_config(
  conn: sqlight.Connection,
  key: String,
  value: String,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO config (key, value, updated_at)
    VALUES (?, ?, datetime('now'))
    ON CONFLICT(key) DO UPDATE SET
      value = excluded.value,
      updated_at = datetime('now')
  "

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(key), sqlight.text(value)],
    expecting: decode.string,
  ))
  Ok(Nil)
}

/// Deletes the domain_authority config entry
pub fn delete_domain_authority(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM config WHERE key = ?"

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text("domain_authority")],
    expecting: decode.string,
  ))
  Ok(Nil)
}

/// Get OAuth client credentials from config table
/// Returns a tuple of (client_id, client_secret, redirect_uri) if all values exist
pub fn get_oauth_credentials(
  conn: sqlight.Connection,
) -> Result(Option(#(String, String, String)), sqlight.Error) {
  case get_config(conn, "oauth_client_id"), get_config(conn, "oauth_client_secret") {
    Ok(client_id), Ok(client_secret) -> {
      let redirect_uri = case get_config(conn, "oauth_redirect_uri") {
        Ok(uri) -> uri
        Error(_) -> ""
      }
      Ok(Some(#(client_id, client_secret, redirect_uri)))
    }
    Error(_), _ -> Ok(None)
    _, Error(_) -> Ok(None)
  }
}

/// Delete OAuth credentials from config table
pub fn delete_oauth_credentials(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  use _ <- result.try(delete_config(conn, "oauth_client_id"))
  use _ <- result.try(delete_config(conn, "oauth_client_secret"))
  use _ <- result.try(delete_config(conn, "oauth_redirect_uri"))
  Ok(Nil)
}

/// Generic config deletion function
fn delete_config(
  conn: sqlight.Connection,
  key: String,
) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM config WHERE key = ?"

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(key)],
    expecting: decode.string,
  ))
  Ok(Nil)
}

/// Deletes all lexicons from the database
pub fn delete_all_lexicons(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM lexicon"

  sqlight.exec(sql, conn)
  |> result.map(fn(_) { Nil })
}

/// Deletes all records from the database
pub fn delete_all_records(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM record"

  sqlight.exec(sql, conn)
  |> result.map(fn(_) { Nil })
}

/// Deletes all actors from the database
pub fn delete_all_actors(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM actor"

  sqlight.exec(sql, conn)
  |> result.map(fn(_) { Nil })
}

// ===== Record Functions =====

/// Inserts or updates a record in the database
pub fn insert_record(
  conn: sqlight.Connection,
  uri: String,
  cid: String,
  did: String,
  collection: String,
  json: String,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO record (uri, cid, did, collection, json)
    VALUES (?, ?, ?, ?, ?)
    ON CONFLICT(uri) DO UPDATE SET
      cid = excluded.cid,
      json = excluded.json,
      indexed_at = datetime('now')
  "

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [
      sqlight.text(uri),
      sqlight.text(cid),
      sqlight.text(did),
      sqlight.text(collection),
      sqlight.text(json),
    ],
    expecting: decode.string,
  ))
  Ok(Nil)
}

/// Batch inserts or updates multiple records in the database
/// More efficient than individual inserts for large datasets
pub fn batch_insert_records(
  conn: sqlight.Connection,
  records: List(Record),
) -> Result(Nil, sqlight.Error) {
  // Process records in smaller batches to avoid SQL parameter limits
  // SQLite has a default limit of 999 parameters
  // Each record uses 5 parameters, so we can safely do 100 records at a time (500 params)
  let batch_size = 100

  list.sized_chunk(records, batch_size)
  |> list.try_each(fn(batch) {
    // Build the SQL with multiple value sets
    let value_placeholders =
      list.repeat("(?, ?, ?, ?, ?)", list.length(batch))
      |> string.join(", ")

    let sql = "
      INSERT INTO record (uri, cid, did, collection, json)
      VALUES " <> value_placeholders <> "
      ON CONFLICT(uri) DO UPDATE SET
        cid = excluded.cid,
        json = excluded.json,
        indexed_at = datetime('now')
    "

    // Flatten all record parameters into a single list
    let params =
      list.flat_map(batch, fn(record) {
        [
          sqlight.text(record.uri),
          sqlight.text(record.cid),
          sqlight.text(record.did),
          sqlight.text(record.collection),
          sqlight.text(record.json),
        ]
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

/// Gets a record by URI
pub fn get_record(
  conn: sqlight.Connection,
  uri: String,
) -> Result(List(Record), sqlight.Error) {
  let sql =
    "
    SELECT uri, cid, did, collection, json, indexed_at
    FROM record
    WHERE uri = ?
  "

  let decoder = {
    use uri <- decode.field(0, decode.string)
    use cid <- decode.field(1, decode.string)
    use did <- decode.field(2, decode.string)
    use collection <- decode.field(3, decode.string)
    use json <- decode.field(4, decode.string)
    use indexed_at <- decode.field(5, decode.string)
    decode.success(Record(uri:, cid:, did:, collection:, json:, indexed_at:))
  }

  sqlight.query(sql, on: conn, with: [sqlight.text(uri)], expecting: decoder)
}

/// Gets all records for a specific DID
pub fn get_records_by_did(
  conn: sqlight.Connection,
  did: String,
) -> Result(List(Record), sqlight.Error) {
  let sql =
    "
    SELECT uri, cid, did, collection, json, indexed_at
    FROM record
    WHERE did = ?
    ORDER BY indexed_at DESC
  "

  let decoder = {
    use uri <- decode.field(0, decode.string)
    use cid <- decode.field(1, decode.string)
    use did <- decode.field(2, decode.string)
    use collection <- decode.field(3, decode.string)
    use json <- decode.field(4, decode.string)
    use indexed_at <- decode.field(5, decode.string)
    decode.success(Record(uri:, cid:, did:, collection:, json:, indexed_at:))
  }

  sqlight.query(sql, on: conn, with: [sqlight.text(did)], expecting: decoder)
}

/// Gets all records for a specific collection
pub fn get_records_by_collection(
  conn: sqlight.Connection,
  collection: String,
) -> Result(List(Record), sqlight.Error) {
  let sql =
    "
    SELECT uri, cid, did, collection, json, indexed_at
    FROM record
    WHERE collection = ?
    ORDER BY indexed_at DESC
    LIMIT 100
  "

  let decoder = {
    use uri <- decode.field(0, decode.string)
    use cid <- decode.field(1, decode.string)
    use did <- decode.field(2, decode.string)
    use collection <- decode.field(3, decode.string)
    use json <- decode.field(4, decode.string)
    use indexed_at <- decode.field(5, decode.string)
    decode.success(Record(uri:, cid:, did:, collection:, json:, indexed_at:))
  }

  sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(collection)],
    expecting: decoder,
  )
}

/// Deletes a record by URI (hard delete)
pub fn delete_record(
  conn: sqlight.Connection,
  uri: String,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    DELETE FROM record
    WHERE uri = ?
  "

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(uri)],
    expecting: decode.string,
  ))
  Ok(Nil)
}

/// Updates an existing record in the database
pub fn update_record(
  conn: sqlight.Connection,
  uri: String,
  cid: String,
  json: String,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    UPDATE record
    SET cid = ?, json = ?, indexed_at = datetime('now')
    WHERE uri = ?
  "

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(cid), sqlight.text(json), sqlight.text(uri)],
    expecting: decode.string,
  ))
  Ok(Nil)
}

/// Inserts or updates an actor in the database
pub fn upsert_actor(
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
pub fn get_actor(
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
pub fn get_actor_by_handle(
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

pub type CollectionStat {
  CollectionStat(collection: String, count: Int)
}

/// Gets statistics for all collections (collection name and record count)
pub fn get_collection_stats(
  conn: sqlight.Connection,
) -> Result(List(CollectionStat), sqlight.Error) {
  let sql =
    "
    SELECT collection, COUNT(*) as count
    FROM record
    GROUP BY collection
    ORDER BY count DESC
  "

  let decoder = {
    use collection <- decode.field(0, decode.string)
    use count <- decode.field(1, decode.int)
    decode.success(CollectionStat(collection:, count:))
  }

  sqlight.query(sql, on: conn, with: [], expecting: decoder)
}

/// Gets the total number of actors in the database
pub fn get_actor_count(conn: sqlight.Connection) -> Result(Int, sqlight.Error) {
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

/// Gets the total number of records in the database
pub fn get_record_count(conn: sqlight.Connection) -> Result(Int, sqlight.Error) {
  let sql =
    "
    SELECT COUNT(*) as count
    FROM record
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

/// Checks if a lexicon exists for a given collection NSID
/// First checks the dedicated lexicon table, then falls back to record table
pub fn has_lexicon_for_collection(
  conn: sqlight.Connection,
  collection: String,
) -> Result(Bool, sqlight.Error) {
  // First check lexicon table (direct lookup is faster)
  case has_lexicon(conn, collection) {
    Ok(True) -> Ok(True)
    Ok(False) -> {
      // Fall back to searching record table for backward compatibility
      let sql =
        "
        SELECT COUNT(*) as count
        FROM record
        WHERE collection = 'com.atproto.lexicon.schema'
        AND json LIKE ?
      "

      let decoder = {
        use count <- decode.field(0, decode.int)
        decode.success(count)
      }

      let pattern = "%" <> collection <> "%"

      case
        sqlight.query(
          sql,
          on: conn,
          with: [sqlight.text(pattern)],
          expecting: decoder,
        )
      {
        Ok([count]) -> Ok(count > 0)
        Ok(_) -> Ok(False)
        Error(err) -> Error(err)
      }
    }
    Error(err) -> Error(err)
  }
}

/// Inserts or updates a lexicon in the database
pub fn insert_lexicon(
  conn: sqlight.Connection,
  id: String,
  json: String,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO lexicon (id, json, created_at)
    VALUES (?, ?, datetime('now'))
    ON CONFLICT(id) DO UPDATE SET
      json = excluded.json,
      created_at = datetime('now')
  "

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(id), sqlight.text(json)],
    expecting: decode.string,
  ))
  Ok(Nil)
}

/// Gets a lexicon by ID
pub fn get_lexicon(
  conn: sqlight.Connection,
  id: String,
) -> Result(List(Lexicon), sqlight.Error) {
  let sql =
    "
    SELECT id, json, created_at
    FROM lexicon
    WHERE id = ?
  "

  let decoder = {
    use id <- decode.field(0, decode.string)
    use json <- decode.field(1, decode.string)
    use created_at <- decode.field(2, decode.string)
    decode.success(Lexicon(id:, json:, created_at:))
  }

  sqlight.query(sql, on: conn, with: [sqlight.text(id)], expecting: decoder)
}

/// Gets all lexicons from the database
pub fn get_all_lexicons(
  conn: sqlight.Connection,
) -> Result(List(Lexicon), sqlight.Error) {
  let sql =
    "
    SELECT id, json, created_at
    FROM lexicon
    ORDER BY created_at DESC
  "

  let decoder = {
    use id <- decode.field(0, decode.string)
    use json <- decode.field(1, decode.string)
    use created_at <- decode.field(2, decode.string)
    decode.success(Lexicon(id:, json:, created_at:))
  }

  sqlight.query(sql, on: conn, with: [], expecting: decoder)
}

/// Checks if a lexicon exists by ID
pub fn has_lexicon(
  conn: sqlight.Connection,
  id: String,
) -> Result(Bool, sqlight.Error) {
  let sql =
    "
    SELECT COUNT(*) as count
    FROM lexicon
    WHERE id = ?
  "

  let decoder = {
    use count <- decode.field(0, decode.int)
    decode.success(count)
  }

  case
    sqlight.query(sql, on: conn, with: [sqlight.text(id)], expecting: decoder)
  {
    Ok([count]) -> Ok(count > 0)
    Ok(_) -> Ok(False)
    Error(err) -> Error(err)
  }
}

/// Gets the total number of lexicons in the database
pub fn get_lexicon_count(conn: sqlight.Connection) -> Result(Int, sqlight.Error) {
  let sql =
    "
    SELECT COUNT(*) as count
    FROM lexicon
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

/// Gets all lexicons that are of type "record" (collections)
pub fn get_record_type_lexicons(
  conn: sqlight.Connection,
) -> Result(List(Lexicon), sqlight.Error) {
  let sql =
    "
    SELECT id, json, created_at
    FROM lexicon
    WHERE json LIKE '%\"type\":\"record\"%'
       OR json LIKE '%\"type\": \"record\"%'
    ORDER BY id ASC
  "

  let decoder = {
    use id <- decode.field(0, decode.string)
    use json <- decode.field(1, decode.string)
    use created_at <- decode.field(2, decode.string)
    decode.success(Lexicon(id:, json:, created_at:))
  }

  sqlight.query(sql, on: conn, with: [], expecting: decoder)
}

pub type ActivityPoint {
  ActivityPoint(timestamp: String, count: Int)
}

/// Gets record indexing activity over time
/// Returns hourly counts for the specified duration
pub fn get_record_activity(
  conn: sqlight.Connection,
  duration_hours: Int,
) -> Result(List(ActivityPoint), sqlight.Error) {
  // SQLite datetime calculation for cutoff time
  let sql =
    "
    WITH RECURSIVE time_series AS (
      SELECT datetime('now', '-" <> int.to_string(duration_hours) <> " hours') AS bucket
      UNION ALL
      SELECT datetime(bucket, '+1 hour')
      FROM time_series
      WHERE bucket < datetime('now')
    )
    SELECT
      strftime('%Y-%m-%dT%H:00:00Z', ts.bucket) as timestamp,
      COALESCE(COUNT(r.uri), 0) as count
    FROM time_series ts
    LEFT JOIN record r ON
      datetime(r.indexed_at) >= datetime(ts.bucket)
      AND datetime(r.indexed_at) < datetime(ts.bucket, '+1 hour')
      AND datetime(r.indexed_at) >= datetime('now', '-" <> int.to_string(duration_hours) <> " hours')
    GROUP BY ts.bucket
    ORDER BY ts.bucket ASC
  "

  let decoder = {
    use timestamp <- decode.field(0, decode.string)
    use count <- decode.field(1, decode.int)
    decode.success(ActivityPoint(timestamp:, count:))
  }

  sqlight.query(sql, on: conn, with: [], expecting: decoder)
}

/// Paginated query for records with cursor-based pagination
///
/// Supports both forward (first/after) and backward (last/before) pagination.
/// Returns a tuple of (records, next_cursor, has_next_page, has_previous_page)
pub fn get_records_by_collection_paginated(
  conn: sqlight.Connection,
  collection: String,
  first: Option(Int),
  after: Option(String),
  last: Option(Int),
  before: Option(String),
  sort_by: Option(List(#(String, String))),
) -> Result(#(List(Record), Option(String), Bool, Bool), sqlight.Error) {
  // Validate pagination arguments
  let #(limit, is_forward, cursor_opt) = case first, last {
    Some(f), None -> #(f, True, after)
    None, Some(l) -> #(l, False, before)
    Some(f), Some(_) ->
      // Both first and last specified - use first
      #(f, True, after)
    None, None ->
      // Neither specified - default to first 50
      #(50, True, None)
  }

  // Default sort order if not specified
  let sort_fields = case sort_by {
    Some(fields) -> fields
    None -> [#("indexed_at", "desc")]
  }

  // Build the ORDER BY clause (no joins in this function, so no prefix needed)
  let order_by_clause = build_order_by(sort_fields, False)

  // Build WHERE clause parts
  let where_parts = ["collection = ?"]
  let bind_values = [sqlight.text(collection)]

  // Add cursor condition if present
  let #(final_where_parts, final_bind_values) = case cursor_opt {
    Some(cursor_str) -> {
      case cursor.decode_cursor(cursor_str, sort_by) {
        Ok(decoded_cursor) -> {
          let #(cursor_where, cursor_params) =
            cursor.build_cursor_where_clause(
              decoded_cursor,
              sort_by,
              !is_forward,
            )

          let new_where = list.append(where_parts, [cursor_where])
          let new_binds =
            list.append(bind_values, list.map(cursor_params, sqlight.text))
          #(new_where, new_binds)
        }
        Error(_) -> #(where_parts, bind_values)
      }
    }
    None -> #(where_parts, bind_values)
  }

  // Fetch limit + 1 to detect if there are more pages
  let fetch_limit = limit + 1

  // Build the SQL query
  let sql = "
    SELECT uri, cid, did, collection, json, indexed_at
    FROM record
    WHERE " <> string.join(final_where_parts, " AND ") <> "
    ORDER BY " <> order_by_clause <> "
    LIMIT " <> int.to_string(fetch_limit)

  // Execute query
  let decoder = {
    use uri <- decode.field(0, decode.string)
    use cid <- decode.field(1, decode.string)
    use did <- decode.field(2, decode.string)
    use collection <- decode.field(3, decode.string)
    use json <- decode.field(4, decode.string)
    use indexed_at <- decode.field(5, decode.string)
    decode.success(Record(uri:, cid:, did:, collection:, json:, indexed_at:))
  }

  use records <- result.try(sqlight.query(
    sql,
    on: conn,
    with: final_bind_values,
    expecting: decoder,
  ))

  // Check if there are more results
  let has_more = list.length(records) > limit
  let trimmed_records = case has_more {
    True -> list.take(records, limit)
    False -> records
  }

  // For backward pagination, reverse the results to restore original order
  let final_records = case is_forward {
    True -> trimmed_records
    False -> list.reverse(trimmed_records)
  }

  // Calculate hasNextPage and hasPreviousPage
  let has_next_page = case is_forward {
    True -> has_more
    False -> option.is_some(cursor_opt)
  }

  let has_previous_page = case is_forward {
    True -> option.is_some(cursor_opt)
    False -> has_more
  }

  // Generate next cursor if there are more results
  let next_cursor = case has_more, list.last(final_records) {
    True, Ok(last_record) -> {
      let record_like = record_to_record_like(last_record)
      Some(cursor.generate_cursor_from_record(record_like, sort_by))
    }
    _, _ -> None
  }

  Ok(#(final_records, next_cursor, has_next_page, has_previous_page))
}

/// Paginated query for records with cursor-based pagination AND where clause filtering
///
/// Same as get_records_by_collection_paginated but with an additional where_clause parameter
pub fn get_records_by_collection_paginated_with_where(
  conn: sqlight.Connection,
  collection: String,
  first: Option(Int),
  after: Option(String),
  last: Option(Int),
  before: Option(String),
  sort_by: Option(List(#(String, String))),
  where: Option(where_clause.WhereClause),
) -> Result(#(List(Record), Option(String), Bool, Bool), sqlight.Error) {
  // Validate pagination arguments
  let #(limit, is_forward, cursor_opt) = case first, last {
    Some(f), None -> #(f, True, after)
    None, Some(l) -> #(l, False, before)
    Some(f), Some(_) -> #(f, True, after)
    None, None -> #(50, True, None)
  }

  // Default sort order if not specified
  let sort_fields = case sort_by {
    Some(fields) -> fields
    None -> [#("indexed_at", "desc")]
  }

  // For backward pagination (last/before), reverse the sort order
  let query_sort_fields = case is_forward {
    True -> sort_fields
    False -> reverse_sort_fields(sort_fields)
  }

  // Check if we need to join with actor table
  let needs_actor_join = case where {
    Some(wc) -> where_clause.requires_actor_join(wc)
    None -> False
  }

  // Build the ORDER BY clause (with table prefix if doing a join)
  let order_by_clause = build_order_by(query_sort_fields, needs_actor_join)

  // Build FROM clause with optional LEFT JOIN
  let from_clause = case needs_actor_join {
    True -> "record LEFT JOIN actor ON record.did = actor.did"
    False -> "record"
  }

  // Build WHERE clause parts - start with collection filter
  let mut_where_parts = ["record.collection = ?"]
  let mut_bind_values = [sqlight.text(collection)]

  // Add where clause conditions if provided
  let #(where_parts, bind_values) = case where {
    Some(wc) -> {
      case where_clause.is_clause_empty(wc) {
        True -> #(mut_where_parts, mut_bind_values)
        False -> {
          let #(where_sql, where_params) =
            where_clause.build_where_sql(wc, needs_actor_join)
          let new_where = list.append(mut_where_parts, [where_sql])
          let new_binds = list.append(mut_bind_values, where_params)
          #(new_where, new_binds)
        }
      }
    }
    None -> #(mut_where_parts, mut_bind_values)
  }

  // Add cursor condition if present
  let #(final_where_parts, final_bind_values) = case cursor_opt {
    Some(cursor_str) -> {
      case cursor.decode_cursor(cursor_str, sort_by) {
        Ok(decoded_cursor) -> {
          let #(cursor_where, cursor_params) =
            cursor.build_cursor_where_clause(
              decoded_cursor,
              sort_by,
              !is_forward,
            )

          let new_where = list.append(where_parts, [cursor_where])
          let new_binds =
            list.append(bind_values, list.map(cursor_params, sqlight.text))
          #(new_where, new_binds)
        }
        Error(_) -> #(where_parts, bind_values)
      }
    }
    None -> #(where_parts, bind_values)
  }

  // Fetch limit + 1 to detect if there are more pages
  let fetch_limit = limit + 1

  // Build the SQL query
  let sql = "
    SELECT record.uri, record.cid, record.did, record.collection, record.json, record.indexed_at
    FROM " <> from_clause <> "
    WHERE " <> string.join(final_where_parts, " AND ") <> "
    ORDER BY " <> order_by_clause <> "
    LIMIT " <> int.to_string(fetch_limit)

  // Execute query
  let decoder = {
    use uri <- decode.field(0, decode.string)
    use cid <- decode.field(1, decode.string)
    use did <- decode.field(2, decode.string)
    use collection <- decode.field(3, decode.string)
    use json <- decode.field(4, decode.string)
    use indexed_at <- decode.field(5, decode.string)
    decode.success(Record(uri:, cid:, did:, collection:, json:, indexed_at:))
  }

  use records <- result.try(sqlight.query(
    sql,
    on: conn,
    with: final_bind_values,
    expecting: decoder,
  ))

  // Check if there are more results
  let has_more = list.length(records) > limit
  let trimmed_records = case has_more {
    True -> list.take(records, limit)
    False -> records
  }

  // For backward pagination, reverse the results to restore original order
  let final_records = case is_forward {
    True -> trimmed_records
    False -> list.reverse(trimmed_records)
  }

  // Calculate hasNextPage and hasPreviousPage
  let has_next_page = case is_forward {
    True -> has_more
    False -> option.is_some(cursor_opt)
  }

  let has_previous_page = case is_forward {
    True -> option.is_some(cursor_opt)
    False -> has_more
  }

  // Generate next cursor if there are more results
  let next_cursor = case has_more, list.last(final_records) {
    True, Ok(last_record) -> {
      let record_like = record_to_record_like(last_record)
      Some(cursor.generate_cursor_from_record(record_like, sort_by))
    }
    _, _ -> None
  }

  Ok(#(final_records, next_cursor, has_next_page, has_previous_page))
}

/// Gets the total count of records for a collection with optional where clause
pub fn get_collection_count_with_where(
  conn: sqlight.Connection,
  collection: String,
  where: Option(where_clause.WhereClause),
) -> Result(Int, sqlight.Error) {
  // Check if we need to join with actor table
  let needs_actor_join = case where {
    Some(wc) -> where_clause.requires_actor_join(wc)
    None -> False
  }

  // Build FROM clause with optional LEFT JOIN
  let from_clause = case needs_actor_join {
    True -> "record LEFT JOIN actor ON record.did = actor.did"
    False -> "record"
  }

  // Build WHERE clause parts - start with collection filter
  let mut_where_parts = ["record.collection = ?"]
  let mut_bind_values = [sqlight.text(collection)]

  // Add where clause conditions if provided
  let #(where_parts, bind_values) = case where {
    Some(wc) -> {
      case where_clause.is_clause_empty(wc) {
        True -> #(mut_where_parts, mut_bind_values)
        False -> {
          let #(where_sql, where_params) =
            where_clause.build_where_sql(wc, needs_actor_join)
          let new_where = list.append(mut_where_parts, [where_sql])
          let new_binds = list.append(mut_bind_values, where_params)
          #(new_where, new_binds)
        }
      }
    }
    None -> #(mut_where_parts, mut_bind_values)
  }

  // Build the SQL query
  let sql = "
    SELECT COUNT(*) as count
    FROM " <> from_clause <> "
    WHERE " <> string.join(where_parts, " AND ")

  // Execute query
  let decoder = {
    use count <- decode.field(0, decode.int)
    decode.success(count)
  }

  case sqlight.query(sql, on: conn, with: bind_values, expecting: decoder) {
    Ok([count]) -> Ok(count)
    Ok(_) -> Ok(0)
    Error(err) -> Error(err)
  }
}

/// Converts a database Record to a cursor.RecordLike
pub fn record_to_record_like(record: Record) -> cursor.RecordLike {
  cursor.RecordLike(
    uri: record.uri,
    cid: record.cid,
    did: record.did,
    collection: record.collection,
    json: record.json,
    indexed_at: record.indexed_at,
  )
}

/// Builds an ORDER BY clause from sort fields
/// use_table_prefix: if True, prefixes table columns with "record." for joins
/// Reverse sort direction for backward pagination
fn reverse_sort_direction(direction: String) -> String {
  case string.lowercase(direction) {
    "asc" -> "desc"
    "desc" -> "asc"
    _ -> "asc"
  }
}

/// Reverse sort fields for backward pagination
fn reverse_sort_fields(
  sort_fields: List(#(String, String)),
) -> List(#(String, String)) {
  list.map(sort_fields, fn(field) {
    let #(field_name, direction) = field
    #(field_name, reverse_sort_direction(direction))
  })
}

fn build_order_by(
  sort_fields: List(#(String, String)),
  use_table_prefix: Bool,
) -> String {
  let order_parts =
    list.map(sort_fields, fn(field) {
      let #(field_name, direction) = field
      let table_prefix = case use_table_prefix {
        True -> "record."
        False -> ""
      }
      let field_ref = case field_name {
        "uri" | "cid" | "did" | "collection" | "indexed_at" ->
          table_prefix <> field_name
        // For JSON fields, check if they look like dates and handle accordingly
        "createdAt" | "indexedAt" -> {
          // Use CASE to treat invalid dates as NULL for sorting
          let json_field =
            "json_extract(" <> table_prefix <> "json, '$." <> field_name <> "')"
          "CASE
            WHEN " <> json_field <> " IS NULL THEN NULL
            WHEN datetime(" <> json_field <> ") IS NULL THEN NULL
            ELSE " <> json_field <> "
           END"
        }
        _ ->
          "json_extract(" <> table_prefix <> "json, '$." <> field_name <> "')"
      }
      let dir = case string.lowercase(direction) {
        "asc" -> "ASC"
        _ -> "DESC"
      }
      // Always put NULLs last regardless of sort direction
      field_ref <> " " <> dir <> " NULLS LAST"
    })

  case list.is_empty(order_parts) {
    True -> {
      let prefix = case use_table_prefix {
        True -> "record."
        False -> ""
      }
      prefix <> "indexed_at DESC NULLS LAST"
    }
    False -> string.join(order_parts, ", ")
  }
}

/// Get records by a list of URIs (for forward joins / DataLoader)
/// Returns records in any order - caller must group them
pub fn get_records_by_uris(
  conn: sqlight.Connection,
  uris: List(String),
) -> Result(List(Record), sqlight.Error) {
  case uris {
    [] -> Ok([])
    _ -> {
      // Build placeholders for SQL IN clause
      let placeholders =
        list.map(uris, fn(_) { "?" })
        |> string.join(", ")

      let sql = "
        SELECT uri, cid, did, collection, json, indexed_at
        FROM record
        WHERE uri IN (" <> placeholders <> ")
      "

      // Convert URIs to sqlight.Value list
      let params = list.map(uris, sqlight.text)

      let decoder = {
        use uri <- decode.field(0, decode.string)
        use cid <- decode.field(1, decode.string)
        use did <- decode.field(2, decode.string)
        use collection <- decode.field(3, decode.string)
        use json <- decode.field(4, decode.string)
        use indexed_at <- decode.field(5, decode.string)
        decode.success(Record(uri:, cid:, did:, collection:, json:, indexed_at:))
      }

      sqlight.query(sql, on: conn, with: params, expecting: decoder)
    }
  }
}

/// Get records by reference field (for reverse joins / DataLoader)
/// Finds all records in a collection where a field references one of the parent URIs
/// Note: This does a JSON field extraction, so it may be slow on large datasets
pub fn get_records_by_reference_field(
  conn: sqlight.Connection,
  collection: String,
  field_name: String,
  parent_uris: List(String),
) -> Result(List(Record), sqlight.Error) {
  case parent_uris {
    [] -> Ok([])
    _ -> {
      // Build placeholders for SQL IN clause
      let placeholders =
        list.map(parent_uris, fn(_) { "?" })
        |> string.join(", ")

      // Use SQLite JSON extraction to find records where field_name matches parent URIs
      // This supports both simple string fields and strongRef objects with a "uri" field
      let sql = "
        SELECT uri, cid, did, collection, json, indexed_at
        FROM record
        WHERE collection = ?
        AND (
          json_extract(json, '$." <> field_name <> "') IN (" <> placeholders <> ")
          OR json_extract(json, '$." <> field_name <> ".uri') IN (" <> placeholders <> ")
        )
      "

      // Build params: collection + parent_uris twice (once for direct match, once for strongRef)
      let params =
        list.flatten([
          [sqlight.text(collection)],
          list.map(parent_uris, sqlight.text),
          list.map(parent_uris, sqlight.text),
        ])

      let decoder = {
        use uri <- decode.field(0, decode.string)
        use cid <- decode.field(1, decode.string)
        use did <- decode.field(2, decode.string)
        use collection <- decode.field(3, decode.string)
        use json <- decode.field(4, decode.string)
        use indexed_at <- decode.field(5, decode.string)
        decode.success(Record(uri:, cid:, did:, collection:, json:, indexed_at:))
      }

      sqlight.query(sql, on: conn, with: params, expecting: decoder)
    }
  }
}

/// Get records by reference field with pagination (for reverse joins with connections)
/// Similar to get_records_by_reference_field but supports cursor-based pagination
/// Returns: (records, next_cursor, has_next_page, has_previous_page, total_count)
pub fn get_records_by_reference_field_paginated(
  conn: sqlight.Connection,
  collection: String,
  field_name: String,
  parent_uri: String,
  first: Option(Int),
  after: Option(String),
  last: Option(Int),
  before: Option(String),
  sort_by: Option(List(#(String, String))),
  where_clause: Option(where_clause.WhereClause),
) -> Result(
  #(List(Record), Option(String), Bool, Bool, Option(Int)),
  sqlight.Error,
) {
  // Validate pagination arguments
  let #(limit, is_forward, cursor_opt) = case first, last {
    Some(f), None -> #(f, True, after)
    None, Some(l) -> #(l, False, before)
    Some(f), Some(_) -> #(f, True, after)
    None, None -> #(50, True, None)
  }

  // Default sort order if not specified
  let sort_fields = case sort_by {
    Some(fields) -> fields
    None -> [#("indexed_at", "desc")]
  }

  // For backward pagination (last/before), reverse the sort order
  let query_sort_fields = case is_forward {
    True -> sort_fields
    False -> reverse_sort_fields(sort_fields)
  }

  // Build the ORDER BY clause
  let order_by_clause = build_order_by(query_sort_fields, False)

  // Build WHERE clause parts for reference field matching
  let base_where_parts = [
    "collection = ?",
    "(json_extract(json, '$."
      <> field_name
      <> "') = ? OR json_extract(json, '$."
      <> field_name
      <> ".uri') = ?)",
  ]
  let base_bind_values = [
    sqlight.text(collection),
    sqlight.text(parent_uri),
    sqlight.text(parent_uri),
  ]

  // Add where clause conditions if present
  let #(with_where_parts, with_where_values) = case where_clause {
    Some(clause) -> {
      let #(where_sql, where_params) =
        where_clause.build_where_sql(clause, False)
      case where_sql {
        "" -> #(base_where_parts, base_bind_values)
        _ -> #(
          list.append(base_where_parts, [where_sql]),
          list.append(base_bind_values, where_params),
        )
      }
    }
    None -> #(base_where_parts, base_bind_values)
  }

  // Add cursor condition if present
  let #(final_where_parts, final_bind_values) = case cursor_opt {
    Some(cursor_str) -> {
      case cursor.decode_cursor(cursor_str, sort_by) {
        Ok(decoded_cursor) -> {
          let #(cursor_where, cursor_params) =
            cursor.build_cursor_where_clause(
              decoded_cursor,
              sort_by,
              !is_forward,
            )

          let new_where = list.append(with_where_parts, [cursor_where])
          let new_binds =
            list.append(
              with_where_values,
              list.map(cursor_params, sqlight.text),
            )
          #(new_where, new_binds)
        }
        Error(_) -> #(with_where_parts, with_where_values)
      }
    }
    None -> #(with_where_parts, with_where_values)
  }

  // Fetch limit + 1 to detect if there are more pages
  let fetch_limit = limit + 1

  // Build the SQL query
  let sql = "
    SELECT uri, cid, did, collection, json, indexed_at
    FROM record
    WHERE " <> string.join(final_where_parts, " AND ") <> "
    ORDER BY " <> order_by_clause <> "
    LIMIT " <> int.to_string(fetch_limit)

  // Execute query
  let decoder = {
    use uri <- decode.field(0, decode.string)
    use cid <- decode.field(1, decode.string)
    use did <- decode.field(2, decode.string)
    use collection <- decode.field(3, decode.string)
    use json <- decode.field(4, decode.string)
    use indexed_at <- decode.field(5, decode.string)
    decode.success(Record(uri:, cid:, did:, collection:, json:, indexed_at:))
  }

  use records <- result.try(sqlight.query(
    sql,
    on: conn,
    with: final_bind_values,
    expecting: decoder,
  ))

  // Check if there are more results
  let has_more = list.length(records) > limit
  let trimmed_records = case has_more {
    True -> list.take(records, limit)
    False -> records
  }

  // For backward pagination, reverse the results to restore original order
  let final_records = case is_forward {
    True -> trimmed_records
    False -> list.reverse(trimmed_records)
  }

  // Calculate hasNextPage and hasPreviousPage
  let has_next_page = case is_forward {
    True -> has_more
    False -> option.is_some(cursor_opt)
  }

  let has_previous_page = case is_forward {
    True -> option.is_some(cursor_opt)
    False -> has_more
  }

  // Generate next cursor if there are more results
  let next_cursor = case has_more, list.last(final_records) {
    True, Ok(last_record) -> {
      let record_like = record_to_record_like(last_record)
      Some(cursor.generate_cursor_from_record(record_like, sort_by))
    }
    _, _ -> None
  }

  // Get total count using the WHERE clause (with where conditions, but without cursor conditions)
  let count_sql =
    "SELECT COUNT(*) FROM record WHERE "
    <> string.join(with_where_parts, " AND ")

  let count_decoder = {
    use count <- decode.field(0, decode.int)
    decode.success(count)
  }

  let total_count = case
    sqlight.query(
      count_sql,
      on: conn,
      with: with_where_values,
      expecting: count_decoder,
    )
  {
    Ok([count]) -> Some(count)
    _ -> None
  }

  Ok(#(
    final_records,
    next_cursor,
    has_next_page,
    has_previous_page,
    total_count,
  ))
}

/// Get records by DIDs and collection (for DID joins / DataLoader)
/// Finds all records in a specific collection that belong to any of the given DIDs
/// Uses the idx_record_did_collection index for efficient lookup
pub fn get_records_by_dids_and_collection(
  conn: sqlight.Connection,
  dids: List(String),
  collection: String,
) -> Result(List(Record), sqlight.Error) {
  case dids {
    [] -> Ok([])
    _ -> {
      // Build placeholders for SQL IN clause
      let placeholders =
        list.map(dids, fn(_) { "?" })
        |> string.join(", ")

      let sql = "
        SELECT uri, cid, did, collection, json, indexed_at
        FROM record
        WHERE did IN (" <> placeholders <> ")
        AND collection = ?
        ORDER BY indexed_at DESC
      "

      // Build params: DIDs + collection
      let params =
        list.flatten([
          list.map(dids, sqlight.text),
          [sqlight.text(collection)],
        ])

      let decoder = {
        use uri <- decode.field(0, decode.string)
        use cid <- decode.field(1, decode.string)
        use did <- decode.field(2, decode.string)
        use collection <- decode.field(3, decode.string)
        use json <- decode.field(4, decode.string)
        use indexed_at <- decode.field(5, decode.string)
        decode.success(Record(uri:, cid:, did:, collection:, json:, indexed_at:))
      }

      sqlight.query(sql, on: conn, with: params, expecting: decoder)
    }
  }
}

/// Get records by DID and collection with pagination (for DID joins with connections)
/// Similar to get_records_by_dids_and_collection but for a single DID with cursor-based pagination
/// Returns: (records, next_cursor, has_next_page, has_previous_page, total_count)
pub fn get_records_by_dids_and_collection_paginated(
  conn: sqlight.Connection,
  did: String,
  collection: String,
  first: Option(Int),
  after: Option(String),
  last: Option(Int),
  before: Option(String),
  sort_by: Option(List(#(String, String))),
  where_clause: Option(where_clause.WhereClause),
) -> Result(
  #(List(Record), Option(String), Bool, Bool, Option(Int)),
  sqlight.Error,
) {
  // Validate pagination arguments
  let #(limit, is_forward, cursor_opt) = case first, last {
    Some(f), None -> #(f, True, after)
    None, Some(l) -> #(l, False, before)
    Some(f), Some(_) -> #(f, True, after)
    None, None -> #(50, True, None)
  }

  // Default sort order if not specified
  let sort_fields = case sort_by {
    Some(fields) -> fields
    None -> [#("indexed_at", "desc")]
  }

  // For backward pagination (last/before), reverse the sort order
  let query_sort_fields = case is_forward {
    True -> sort_fields
    False -> reverse_sort_fields(sort_fields)
  }

  // Build the ORDER BY clause
  let order_by_clause = build_order_by(query_sort_fields, False)

  // Build WHERE clause parts for DID and collection matching
  let base_where_parts = ["did = ?", "collection = ?"]
  let base_bind_values = [sqlight.text(did), sqlight.text(collection)]

  // Add where clause conditions if present
  let #(with_where_parts, with_where_values) = case where_clause {
    Some(clause) -> {
      let #(where_sql, where_params) =
        where_clause.build_where_sql(clause, False)
      case where_sql {
        "" -> #(base_where_parts, base_bind_values)
        _ -> #(
          list.append(base_where_parts, [where_sql]),
          list.append(base_bind_values, where_params),
        )
      }
    }
    None -> #(base_where_parts, base_bind_values)
  }

  // Add cursor condition if present
  let #(final_where_parts, final_bind_values) = case cursor_opt {
    Some(cursor_str) -> {
      case cursor.decode_cursor(cursor_str, sort_by) {
        Ok(decoded_cursor) -> {
          let #(cursor_where, cursor_params) =
            cursor.build_cursor_where_clause(
              decoded_cursor,
              sort_by,
              !is_forward,
            )

          let new_where = list.append(with_where_parts, [cursor_where])
          let new_binds =
            list.append(
              with_where_values,
              list.map(cursor_params, sqlight.text),
            )
          #(new_where, new_binds)
        }
        Error(_) -> #(with_where_parts, with_where_values)
      }
    }
    None -> #(with_where_parts, with_where_values)
  }

  // Fetch limit + 1 to detect if there are more pages
  let fetch_limit = limit + 1

  // Build the SQL query
  let sql = "
    SELECT uri, cid, did, collection, json, indexed_at
    FROM record
    WHERE " <> string.join(final_where_parts, " AND ") <> "
    ORDER BY " <> order_by_clause <> "
    LIMIT " <> int.to_string(fetch_limit)

  // Execute query
  let decoder = {
    use uri <- decode.field(0, decode.string)
    use cid <- decode.field(1, decode.string)
    use did <- decode.field(2, decode.string)
    use collection <- decode.field(3, decode.string)
    use json <- decode.field(4, decode.string)
    use indexed_at <- decode.field(5, decode.string)
    decode.success(Record(uri:, cid:, did:, collection:, json:, indexed_at:))
  }

  use records <- result.try(sqlight.query(
    sql,
    on: conn,
    with: final_bind_values,
    expecting: decoder,
  ))

  // Check if there are more results
  let has_more = list.length(records) > limit
  let trimmed_records = case has_more {
    True -> list.take(records, limit)
    False -> records
  }

  // For backward pagination, reverse the results to restore original order
  let final_records = case is_forward {
    True -> trimmed_records
    False -> list.reverse(trimmed_records)
  }

  // Calculate hasNextPage and hasPreviousPage
  let has_next_page = case is_forward {
    True -> has_more
    False -> option.is_some(cursor_opt)
  }

  let has_previous_page = case is_forward {
    True -> option.is_some(cursor_opt)
    False -> has_more
  }

  // Generate next cursor if there are more results
  let next_cursor = case has_more, list.last(final_records) {
    True, Ok(last_record) -> {
      let record_like = record_to_record_like(last_record)
      Some(cursor.generate_cursor_from_record(record_like, sort_by))
    }
    _, _ -> None
  }

  // Get total count using the WHERE clause (with where conditions, but without cursor conditions)
  let count_sql =
    "SELECT COUNT(*) FROM record WHERE "
    <> string.join(with_where_parts, " AND ")

  let count_decoder = {
    use count <- decode.field(0, decode.int)
    decode.success(count)
  }

  let total_count = case
    sqlight.query(
      count_sql,
      on: conn,
      with: with_where_values,
      expecting: count_decoder,
    )
  {
    Ok([count]) -> Some(count)
    _ -> None
  }

  Ok(#(
    final_records,
    next_cursor,
    has_next_page,
    has_previous_page,
    total_count,
  ))
}
