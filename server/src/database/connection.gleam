// server/src/database/connection.gleam

import database/executor.{type DbError, type Executor}
import database/postgres/connection as postgres_connection
import database/sqlite/connection as sqlite_connection
import gleam/string

/// Supported database backends
pub type Backend {
  SQLite
  PostgreSQL
}

/// Parse DATABASE_URL and connect to the appropriate backend.
///
/// Note: This function connects to the database but does NOT run migrations.
/// Schema migrations should be run externally using dbmate before starting
/// the application:
///
/// ```sh
/// DATABASE_URL=sqlite:data/quickslice.db dbmate up
/// # or
/// DATABASE_URL=postgres://localhost/quickslice dbmate --migrations-dir ./db/migrations_postgres up
/// ```
///
/// Supported URL formats:
/// - SQLite: "sqlite:./path/to/db.sqlite", "./path/to/db.sqlite", "file:./path"
/// - PostgreSQL: "postgres://user:pass@host:port/db", "postgresql://..."
pub fn connect(url: String) -> Result(Executor, DbError) {
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
