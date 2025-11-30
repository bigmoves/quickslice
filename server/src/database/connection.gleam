import database/schema/migrations
import gleam/result
import logging
import sqlight

/// Opens a connection to the SQLite database
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

/// Initializes the database with all required tables using the migration system
pub fn initialize(path: String) -> Result(sqlight.Connection, sqlight.Error) {
  use conn <- result.try(connect(path))

  // Run any pending migrations
  use _ <- result.try(migrations.run_migrations(conn))

  logging.log(logging.Info, "Database initialized at: " <> path)
  Ok(conn)
}
