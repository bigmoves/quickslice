import database/schema/tables
import gleam/dynamic/decode
import gleam/int
import gleam/result
import logging
import oauth/session
import sqlight

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

// ===== Database Migrations =====

/// Migration v1: Initial schema with all tables
fn migration_v1(conn: sqlight.Connection) -> Result(Nil, sqlight.Error) {
  logging.log(logging.Info, "Running migration v1 (initial schema)...")

  // Create all tables and indexes
  use _ <- result.try(tables.create_record_table(conn))
  use _ <- result.try(tables.create_actor_table(conn))
  use _ <- result.try(tables.create_lexicon_table(conn))
  use _ <- result.try(session.init_db(conn))

  Ok(Nil)
}

/// Migration v2: Add config table
fn migration_v2(conn: sqlight.Connection) -> Result(Nil, sqlight.Error) {
  logging.log(logging.Info, "Running migration v2 (config table)...")
  tables.create_config_table(conn)
}

/// Migration v3: Add CID index for deduplication
fn migration_v3(conn: sqlight.Connection) -> Result(Nil, sqlight.Error) {
  logging.log(logging.Info, "Running migration v3 (CID index)...")
  tables.create_cid_index(conn)
}

/// Migration v4: Add jetstream_activity table for 24h activity log
fn migration_v4(conn: sqlight.Connection) -> Result(Nil, sqlight.Error) {
  logging.log(
    logging.Info,
    "Running migration v4 (jetstream_activity table)...",
  )
  tables.create_jetstream_activity_table(conn)
}

/// Migration v5: Add jetstream_cursor table for cursor tracking
fn migration_v5(conn: sqlight.Connection) -> Result(Nil, sqlight.Error) {
  logging.log(
    logging.Info,
    "Running migration v5 (jetstream_cursor table)...",
  )
  tables.create_jetstream_cursor_table(conn)
}

/// Runs all pending migrations based on current schema version
pub fn run_migrations(conn: sqlight.Connection) -> Result(Nil, sqlight.Error) {
  use _ <- result.try(create_schema_version_table(conn))
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
      use _ <- result.try(apply_migration(conn, 2, migration_v2))
      use _ <- result.try(apply_migration(conn, 3, migration_v3))
      use _ <- result.try(apply_migration(conn, 4, migration_v4))
      apply_migration(conn, 5, migration_v5)
    }

    // Run v2, v3, v4, and v5 migrations
    1 -> {
      use _ <- result.try(apply_migration(conn, 2, migration_v2))
      use _ <- result.try(apply_migration(conn, 3, migration_v3))
      use _ <- result.try(apply_migration(conn, 4, migration_v4))
      apply_migration(conn, 5, migration_v5)
    }

    // Run v3, v4, and v5 migrations
    2 -> {
      use _ <- result.try(apply_migration(conn, 3, migration_v3))
      use _ <- result.try(apply_migration(conn, 4, migration_v4))
      apply_migration(conn, 5, migration_v5)
    }

    // Run v4 and v5 migrations
    3 -> {
      use _ <- result.try(apply_migration(conn, 4, migration_v4))
      apply_migration(conn, 5, migration_v5)
    }

    // Run v5 migration
    4 -> apply_migration(conn, 5, migration_v5)

    // Already at latest version
    5 -> {
      logging.log(logging.Info, "Schema is up to date (v5)")
      Ok(Nil)
    }

    // Future versions would be handled here
    _ -> {
      logging.log(
        logging.Error,
        "Unknown schema version: " <> int.to_string(current_version),
      )
      Ok(Nil)
    }
  }
}
