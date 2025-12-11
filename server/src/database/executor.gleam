// server/src/database/executor.gleam

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// Unified error type for all database operations
pub type DbError {
  ConnectionError(message: String)
  QueryError(message: String)
  DecodeError(message: String)
  ConstraintError(message: String)
}

/// Parameter values for database queries
pub type Value {
  Text(String)
  Int(Int)
  Float(Float)
  Bool(Bool)
  Null
  Blob(BitArray)
  /// ISO 8601 timestamp string - PostgreSQL treats as TIMESTAMPTZ, SQLite as TEXT
  Timestamptz(String)
}

/// Database dialect identifier
pub type Dialect {
  SQLite
  PostgreSQL
}

/// The Executor provides a unified interface for database operations
/// across different backends (SQLite, PostgreSQL, etc.)
pub opaque type Executor {
  Executor(
    dialect: Dialect,
    /// Execute a query and return raw dynamic results
    query_raw: fn(String, List(Value)) -> Result(List(Dynamic), DbError),
    /// Execute a statement without returning results
    exec: fn(String, List(Value)) -> Result(Nil, DbError),
    /// Generate a placeholder for the given parameter index (1-based)
    /// SQLite: "?" (ignores index), PostgreSQL: "$1", "$2", etc.
    placeholder: fn(Int) -> String,
    /// Generate SQL for extracting a field from a JSON column
    /// SQLite: json_extract(column, '$.field')
    /// PostgreSQL: column->>'field'
    json_extract: fn(String, String) -> String,
    /// Generate SQL for extracting a nested JSON path
    /// SQLite: json_extract(column, '$.path.to.field')
    /// PostgreSQL: column->'path'->'to'->>'field'
    json_extract_path: fn(String, List(String)) -> String,
    /// Generate SQL for current timestamp
    /// SQLite: datetime('now'), PostgreSQL: NOW()
    now: fn() -> String,
  )
}

// ===== Executor Constructor =====

/// Create an Executor (used by backend implementations)
pub fn new(
  dialect: Dialect,
  query_raw: fn(String, List(Value)) -> Result(List(Dynamic), DbError),
  exec: fn(String, List(Value)) -> Result(Nil, DbError),
  placeholder: fn(Int) -> String,
  json_extract: fn(String, String) -> String,
  json_extract_path: fn(String, List(String)) -> String,
  now: fn() -> String,
) -> Executor {
  Executor(
    dialect: dialect,
    query_raw: query_raw,
    exec: exec,
    placeholder: placeholder,
    json_extract: json_extract,
    json_extract_path: json_extract_path,
    now: now,
  )
}

// ===== Executor Accessors =====

/// Get the dialect of the executor
pub fn dialect(exec: Executor) -> Dialect {
  exec.dialect
}

/// Execute a query with the given SQL, parameters, and decoder
pub fn query(
  exec: Executor,
  sql: String,
  params: List(Value),
  decoder: Decoder(a),
) -> Result(List(a), DbError) {
  case exec.query_raw(sql, params) {
    Ok(rows) -> {
      let results =
        list.try_map(rows, fn(row) {
          case decode.run(row, decoder) {
            Ok(decoded) -> Ok(decoded)
            Error(errors) ->
              Error(DecodeError(
                "Failed to decode row: " <> string.inspect(errors),
              ))
          }
        })
      results
    }
    Error(err) -> Error(err)
  }
}

/// Execute a statement without returning results
pub fn exec(
  exec: Executor,
  sql: String,
  params: List(Value),
) -> Result(Nil, DbError) {
  exec.exec(sql, params)
}

/// Generate a placeholder for parameter at given index (1-based)
pub fn placeholder(exec: Executor, index: Int) -> String {
  exec.placeholder(index)
}

/// Generate SQL for extracting a JSON field
pub fn json_extract(exec: Executor, column: String, field: String) -> String {
  exec.json_extract(column, field)
}

/// Generate SQL for extracting a nested JSON path
pub fn json_extract_path(
  exec: Executor,
  column: String,
  path: List(String),
) -> String {
  exec.json_extract_path(column, path)
}

/// Generate SQL for current timestamp
pub fn now(exec: Executor) -> String {
  exec.now()
}

// ===== Helper Functions =====

/// Build a list of placeholders for N parameters starting at offset
pub fn placeholders(exec: Executor, count: Int, start_index: Int) -> String {
  case count {
    0 -> ""
    _ -> {
      list.range(start_index, start_index + count - 1)
      |> list.map(fn(i) { placeholder(exec, i) })
      |> string.join(", ")
    }
  }
}

/// Convert an optional text value to a Value (Text or Null)
pub fn nullable_text(value: Option(String)) -> Value {
  case value {
    Some(s) -> Text(s)
    None -> Null
  }
}

/// Convert an optional int value to a Value (Int or Null)
pub fn nullable_int(value: Option(Int)) -> Value {
  case value {
    Some(i) -> Int(i)
    None -> Null
  }
}

/// Convert a boolean to a Value
/// PostgreSQL uses native BOOLEAN, SQLite uses INTEGER (0/1)
/// Each executor handles this appropriately
pub fn bool_value(value: Bool) -> Value {
  Bool(value)
}
