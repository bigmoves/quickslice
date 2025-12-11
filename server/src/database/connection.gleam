// server/src/database/connection.gleam

import database/executor.{type DbError, type Executor}
import database/postgres/connection as postgres_connection
import database/schema/migrations
import database/sqlite/connection as sqlite_connection
import gleam/result
import gleam/string
import logging
import sqlight

/// Supported database backends
pub type Backend {
  SQLite
  PostgreSQL
}

/// Parse DATABASE_URL and connect to the appropriate backend (new API)
///
/// Supported URL formats:
/// - SQLite: "sqlite:./path/to/db.sqlite", "./path/to/db.sqlite", "file:./path"
/// - PostgreSQL: "postgres://user:pass@host:port/db", "postgresql://..."
pub fn connect_executor(url: String) -> Result(Executor, DbError) {
  case detect_backend(url) {
    SQLite -> sqlite_connection.connect(url)
    PostgreSQL -> postgres_connection.connect(url)
  }
}

/// Detect the database backend from a URL
pub fn detect_backend(url: String) -> Backend {
  let url_lower = string.lowercase(url)

  case
    string.starts_with(url_lower, "postgres://")
    || string.starts_with(url_lower, "postgresql://")
  {
    True -> PostgreSQL
    False -> SQLite
  }
}

// ===== Legacy SQLite-specific API (for backward compatibility) =====
// These functions will be removed after migrating all callers to Executor

/// Opens a connection to the SQLite database (legacy API)
/// @deprecated Use connect_executor instead
pub fn connect(path: String) -> Result(sqlight.Connection, sqlight.Error) {
  use conn <- result.try(sqlight.open(path))

  // Enable WAL mode for better concurrency
  use _ <- result.try(sqlight.exec("PRAGMA journal_mode = WAL", conn))

  // Performance tuning - safe with WAL mode
  use _ <- result.try(sqlight.exec("PRAGMA synchronous = NORMAL", conn))
  use _ <- result.try(sqlight.exec("PRAGMA cache_size = -64000", conn))
  use _ <- result.try(sqlight.exec("PRAGMA mmap_size = 268435456", conn))
  use _ <- result.try(sqlight.exec("PRAGMA temp_store = MEMORY", conn))
  use _ <- result.try(sqlight.exec("PRAGMA busy_timeout = 5000", conn))

  // Enable foreign key constraints
  use _ <- result.try(sqlight.exec("PRAGMA foreign_keys = ON", conn))

  Ok(conn)
}

/// Initializes the database with all required tables (legacy API)
/// @deprecated Use connect_executor + dbmate migrations instead
pub fn initialize(path: String) -> Result(sqlight.Connection, sqlight.Error) {
  use conn <- result.try(connect(path))

  // Run any pending migrations
  use _ <- result.try(migrations.run_migrations(conn))

  logging.log(logging.Info, "Database initialized at: " <> path)
  Ok(conn)
}
