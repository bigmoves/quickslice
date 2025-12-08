import database/repositories/oauth_clients
import database/schema/tables
import gleam/dynamic/decode
import gleam/int
import gleam/result
import logging
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
  tables.create_lexicon_table(conn)
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
  logging.log(logging.Info, "Running migration v5 (jetstream_cursor table)...")
  tables.create_jetstream_cursor_table(conn)
}

/// Migration v6: Add OAuth tables
fn migration_v6(conn: sqlight.Connection) -> Result(Nil, sqlight.Error) {
  logging.log(logging.Info, "Running migration v6 (OAuth tables)...")

  // Create OAuth tables in dependency order (oauth_client first due to foreign keys)
  use _ <- result.try(tables.create_oauth_client_table(conn))
  use _ <- result.try(tables.create_oauth_access_token_table(conn))
  use _ <- result.try(tables.create_oauth_refresh_token_table(conn))
  use _ <- result.try(tables.create_oauth_par_request_table(conn))
  use _ <- result.try(tables.create_oauth_dpop_nonce_table(conn))
  use _ <- result.try(tables.create_oauth_auth_request_table(conn))
  use _ <- result.try(tables.create_oauth_atp_session_table(conn))
  tables.create_oauth_atp_request_table(conn)
}

/// Migration v7: Add OAuth authorization code table
fn migration_v7(conn: sqlight.Connection) -> Result(Nil, sqlight.Error) {
  logging.log(
    logging.Info,
    "Running migration v7 (authorization code table)...",
  )
  tables.create_oauth_authorization_code_table(conn)
}

/// Migration v8: Add admin_session table for admin browser sessions
fn migration_v8(conn: sqlight.Connection) -> Result(Nil, sqlight.Error) {
  logging.log(logging.Info, "Running migration v8 (admin_session table)...")
  tables.create_admin_session_table(conn)
}

/// Migration v9: Add oauth_dpop_jti table for DPoP replay protection
fn migration_v9(conn: sqlight.Connection) -> Result(Nil, sqlight.Error) {
  logging.log(logging.Info, "Running migration v9 (oauth_dpop_jti table)...")
  tables.create_oauth_dpop_jti_table(conn)
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
  use _ <- result.try(case current_version {
    // Fresh database or pre-migration database - run v1
    0 -> {
      use _ <- result.try(apply_migration(conn, 1, migration_v1))
      use _ <- result.try(apply_migration(conn, 2, migration_v2))
      use _ <- result.try(apply_migration(conn, 3, migration_v3))
      use _ <- result.try(apply_migration(conn, 4, migration_v4))
      use _ <- result.try(apply_migration(conn, 5, migration_v5))
      use _ <- result.try(apply_migration(conn, 6, migration_v6))
      use _ <- result.try(apply_migration(conn, 7, migration_v7))
      use _ <- result.try(apply_migration(conn, 8, migration_v8))
      apply_migration(conn, 9, migration_v9)
    }

    // Run v2 through v9 migrations
    1 -> {
      use _ <- result.try(apply_migration(conn, 2, migration_v2))
      use _ <- result.try(apply_migration(conn, 3, migration_v3))
      use _ <- result.try(apply_migration(conn, 4, migration_v4))
      use _ <- result.try(apply_migration(conn, 5, migration_v5))
      use _ <- result.try(apply_migration(conn, 6, migration_v6))
      use _ <- result.try(apply_migration(conn, 7, migration_v7))
      use _ <- result.try(apply_migration(conn, 8, migration_v8))
      apply_migration(conn, 9, migration_v9)
    }

    // Run v3 through v9 migrations
    2 -> {
      use _ <- result.try(apply_migration(conn, 3, migration_v3))
      use _ <- result.try(apply_migration(conn, 4, migration_v4))
      use _ <- result.try(apply_migration(conn, 5, migration_v5))
      use _ <- result.try(apply_migration(conn, 6, migration_v6))
      use _ <- result.try(apply_migration(conn, 7, migration_v7))
      use _ <- result.try(apply_migration(conn, 8, migration_v8))
      apply_migration(conn, 9, migration_v9)
    }

    // Run v4 through v9 migrations
    3 -> {
      use _ <- result.try(apply_migration(conn, 4, migration_v4))
      use _ <- result.try(apply_migration(conn, 5, migration_v5))
      use _ <- result.try(apply_migration(conn, 6, migration_v6))
      use _ <- result.try(apply_migration(conn, 7, migration_v7))
      use _ <- result.try(apply_migration(conn, 8, migration_v8))
      apply_migration(conn, 9, migration_v9)
    }

    // Run v5 through v9 migrations
    4 -> {
      use _ <- result.try(apply_migration(conn, 5, migration_v5))
      use _ <- result.try(apply_migration(conn, 6, migration_v6))
      use _ <- result.try(apply_migration(conn, 7, migration_v7))
      use _ <- result.try(apply_migration(conn, 8, migration_v8))
      apply_migration(conn, 9, migration_v9)
    }

    // Run v6 through v9 migrations
    5 -> {
      use _ <- result.try(apply_migration(conn, 6, migration_v6))
      use _ <- result.try(apply_migration(conn, 7, migration_v7))
      use _ <- result.try(apply_migration(conn, 8, migration_v8))
      apply_migration(conn, 9, migration_v9)
    }

    // Run v7 through v9 migrations
    6 -> {
      use _ <- result.try(apply_migration(conn, 7, migration_v7))
      use _ <- result.try(apply_migration(conn, 8, migration_v8))
      apply_migration(conn, 9, migration_v9)
    }

    // Run v8 and v9 migrations
    7 -> {
      use _ <- result.try(apply_migration(conn, 8, migration_v8))
      apply_migration(conn, 9, migration_v9)
    }

    // Run v9 migration
    8 -> apply_migration(conn, 9, migration_v9)

    // Already at latest version
    9 -> {
      logging.log(logging.Info, "Schema is up to date (v9)")
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
  })

  // Ensure internal clients exist (idempotent)
  oauth_clients.ensure_admin_client(conn)
}
