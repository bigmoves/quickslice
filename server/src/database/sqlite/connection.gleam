// server/src/database/sqlite/connection.gleam

import database/executor.{type DbError, type Executor, ConnectionError}
import database/sqlite/executor as sqlite_executor
import gleam/result
import gleam/string
import logging
import sqlight

/// Connect to SQLite database and return an Executor
/// Handles SQLite-specific connection setup (PRAGMAs, etc.)
pub fn connect(url: String) -> Result(Executor, DbError) {
  let path = parse_path(url)

  use conn <- result.try(
    sqlight.open(path)
    |> result.map_error(fn(e) {
      ConnectionError(
        "Failed to open SQLite database: " <> sqlight_error_message(e),
      )
    }),
  )

  // Enable WAL mode for better concurrency
  use _ <- result.try(exec_pragma(conn, "PRAGMA journal_mode = WAL"))

  // Performance tuning - safe with WAL mode
  use _ <- result.try(exec_pragma(conn, "PRAGMA synchronous = NORMAL"))
  use _ <- result.try(exec_pragma(conn, "PRAGMA cache_size = -64000"))
  use _ <- result.try(exec_pragma(conn, "PRAGMA mmap_size = 268435456"))
  use _ <- result.try(exec_pragma(conn, "PRAGMA temp_store = MEMORY"))
  use _ <- result.try(exec_pragma(conn, "PRAGMA busy_timeout = 5000"))

  // Enable foreign key constraints
  use _ <- result.try(exec_pragma(conn, "PRAGMA foreign_keys = ON"))

  logging.log(logging.Info, "Connected to SQLite database: " <> path)

  Ok(sqlite_executor.new(conn))
}

/// Parse the path from a SQLite URL
/// Supports: "sqlite:./path/to/db", "file:./path/to/db", or just "./path/to/db"
fn parse_path(url: String) -> String {
  case string.split_once(url, ":") {
    Ok(#(scheme, rest)) ->
      case scheme {
        "sqlite" | "file" ->
          case string.starts_with(rest, "//") {
            True -> string.drop_start(rest, 2)
            False -> rest
          }
        _ -> url
      }
    Error(_) -> url
  }
}

/// Execute a PRAGMA statement
fn exec_pragma(conn: sqlight.Connection, pragma: String) -> Result(Nil, DbError) {
  sqlight.exec(pragma, conn)
  |> result.map_error(fn(e) {
    ConnectionError(
      "Failed to execute " <> pragma <> ": " <> sqlight_error_message(e),
    )
  })
}

/// Get error message from sqlight error
fn sqlight_error_message(err: sqlight.Error) -> String {
  case err {
    sqlight.SqlightError(_, message, _) -> message
  }
}
