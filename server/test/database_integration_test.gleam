/// Integration tests for database executors
///
/// These tests verify that both SQLite and PostgreSQL executors work correctly
/// with actual database connections.
///
/// SQLite tests run with in-memory databases automatically.
/// PostgreSQL tests require POSTGRES_TEST_URL environment variable to be set.
/// If not set, PostgreSQL tests will be skipped.
import database/executor.{type Executor, Bool, Int, PostgreSQL, SQLite, Text}
import database/postgres/connection as pg_connection
import database/sqlite/connection as sqlite_connection
import envoy
import gleam/dynamic/decode
import gleam/int
import gleam/io
import gleam/option.{None, Some}
import gleeunit/should

// ===== SQLite Integration Tests =====

pub fn sqlite_connect_test() {
  let result = sqlite_connection.connect("sqlite::memory:")
  result |> should.be_ok
}

pub fn sqlite_create_table_test() {
  let assert Ok(exec) = sqlite_connection.connect("sqlite::memory:")

  let result =
    executor.exec(
      exec,
      "CREATE TABLE test_table (id INTEGER PRIMARY KEY, name TEXT NOT NULL)",
      [],
    )

  result |> should.be_ok
}

pub fn sqlite_insert_and_select_test() {
  let assert Ok(exec) = sqlite_connection.connect("sqlite::memory:")

  // Create table
  let assert Ok(_) =
    executor.exec(
      exec,
      "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT NOT NULL, active INTEGER NOT NULL)",
      [],
    )

  // Insert data
  let assert Ok(_) =
    executor.exec(
      exec,
      "INSERT INTO users (id, name, active) VALUES (?, ?, ?)",
      [
        Int(1),
        Text("Alice"),
        Bool(True),
      ],
    )

  let assert Ok(_) =
    executor.exec(
      exec,
      "INSERT INTO users (id, name, active) VALUES (?, ?, ?)",
      [
        Int(2),
        Text("Bob"),
        Bool(False),
      ],
    )

  // Query data
  let decoder = {
    use id <- decode.field(0, decode.int)
    use name <- decode.field(1, decode.string)
    use active <- decode.field(2, decode.int)
    decode.success(#(id, name, active))
  }

  let result =
    executor.query(
      exec,
      "SELECT id, name, active FROM users ORDER BY id",
      [],
      decoder,
    )

  result |> should.be_ok
  let assert Ok(rows) = result
  rows |> should.equal([#(1, "Alice", 1), #(2, "Bob", 0)])
}

pub fn sqlite_placeholder_format_test() {
  let assert Ok(exec) = sqlite_connection.connect("sqlite::memory:")

  // SQLite should use ? placeholders
  executor.placeholder(exec, 1) |> should.equal("?")
  executor.placeholder(exec, 5) |> should.equal("?")
}

pub fn sqlite_dialect_test() {
  let assert Ok(exec) = sqlite_connection.connect("sqlite::memory:")
  executor.dialect(exec) |> should.equal(SQLite)
}

// ===== PostgreSQL Integration Tests =====
// These tests require POSTGRES_TEST_URL to be set

fn get_postgres_exec() -> Result(Executor, Nil) {
  case envoy.get("POSTGRES_TEST_URL") {
    Ok(url) -> {
      case pg_connection.connect(url) {
        Ok(exec) -> Ok(exec)
        Error(_err) -> {
          io.println("PostgreSQL connection failed")
          Error(Nil)
        }
      }
    }
    Error(_) -> Error(Nil)
  }
}

fn skip_if_no_postgres(test_fn: fn(Executor) -> Nil) -> Nil {
  case get_postgres_exec() {
    Ok(exec) -> test_fn(exec)
    Error(_) -> {
      // Silently skip - PostgreSQL not configured
      Nil
    }
  }
}

pub fn postgres_connect_test() {
  skip_if_no_postgres(fn(exec) {
    // If we got here, connection succeeded
    executor.dialect(exec) |> should.equal(PostgreSQL)
  })
}

pub fn postgres_placeholder_format_test() {
  skip_if_no_postgres(fn(exec) {
    // PostgreSQL should use $1, $2, etc. placeholders
    executor.placeholder(exec, 1) |> should.equal("$1")
    executor.placeholder(exec, 5) |> should.equal("$5")
  })
}

pub fn postgres_create_and_drop_table_test() {
  skip_if_no_postgres(fn(exec) {
    // Use a unique table name to avoid conflicts
    let table_name = "test_integration_" <> int.to_string(erlang_timestamp())

    // Create table
    let create_result =
      executor.exec(
        exec,
        "CREATE TABLE "
          <> table_name
          <> " (id SERIAL PRIMARY KEY, name TEXT NOT NULL)",
        [],
      )
    create_result |> should.be_ok

    // Drop table (cleanup)
    let drop_result = executor.exec(exec, "DROP TABLE " <> table_name, [])
    drop_result |> should.be_ok
  })
}

pub fn postgres_insert_and_select_test() {
  skip_if_no_postgres(fn(exec) {
    let table_name = "test_crud_" <> int.to_string(erlang_timestamp())

    // Create table
    let assert Ok(_) =
      executor.exec(
        exec,
        "CREATE TABLE "
          <> table_name
          <> " (id SERIAL PRIMARY KEY, name TEXT NOT NULL, active BOOLEAN NOT NULL)",
        [],
      )

    // Insert data using PostgreSQL placeholders
    let assert Ok(_) =
      executor.exec(
        exec,
        "INSERT INTO " <> table_name <> " (name, active) VALUES ($1, $2)",
        [Text("Alice"), Bool(True)],
      )

    let assert Ok(_) =
      executor.exec(
        exec,
        "INSERT INTO " <> table_name <> " (name, active) VALUES ($1, $2)",
        [Text("Bob"), Bool(False)],
      )

    // Query data
    let decoder = {
      use name <- decode.field(0, decode.string)
      use active <- decode.field(1, decode.bool)
      decode.success(#(name, active))
    }

    let result =
      executor.query(
        exec,
        "SELECT name, active FROM " <> table_name <> " ORDER BY name",
        [],
        decoder,
      )

    result |> should.be_ok
    let assert Ok(rows) = result
    rows |> should.equal([#("Alice", True), #("Bob", False)])

    // Cleanup
    let assert Ok(_) = executor.exec(exec, "DROP TABLE " <> table_name, [])
    Nil
  })
}

pub fn postgres_null_handling_test() {
  skip_if_no_postgres(fn(exec) {
    let table_name = "test_null_" <> int.to_string(erlang_timestamp())

    // Create table with nullable column
    let assert Ok(_) =
      executor.exec(
        exec,
        "CREATE TABLE " <> table_name <> " (id SERIAL PRIMARY KEY, value TEXT)",
        [],
      )

    // Insert NULL value
    let assert Ok(_) =
      executor.exec(
        exec,
        "INSERT INTO " <> table_name <> " (value) VALUES ($1)",
        [executor.Null],
      )

    // Insert actual value
    let assert Ok(_) =
      executor.exec(
        exec,
        "INSERT INTO " <> table_name <> " (value) VALUES ($1)",
        [Text("not null")],
      )

    // Query and check
    let decoder = {
      use value <- decode.field(0, decode.optional(decode.string))
      decode.success(value)
    }

    let result =
      executor.query(
        exec,
        "SELECT value FROM " <> table_name <> " ORDER BY id",
        [],
        decoder,
      )

    result |> should.be_ok
    let assert Ok(rows) = result
    rows |> should.equal([None, Some("not null")])

    // Cleanup
    let assert Ok(_) = executor.exec(exec, "DROP TABLE " <> table_name, [])
    Nil
  })
}

// Helper to get a unique timestamp for table names
@external(erlang, "os", "system_time")
fn erlang_timestamp() -> Int
