import database/schema/migrations
import gleam/result
import logging
import sqlight

/// Opens a connection to the SQLite database
pub fn connect(path: String) -> Result(sqlight.Connection, sqlight.Error) {
  use conn <- result.try(sqlight.open(path))

  // Enable WAL mode for better concurrency
  use _ <- result.try(sqlight.exec("PRAGMA journal_mode = WAL", conn))

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
