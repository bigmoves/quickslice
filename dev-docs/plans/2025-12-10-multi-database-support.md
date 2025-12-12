# Multi-Database Support Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add PostgreSQL support alongside existing SQLite, using an Executor abstraction pattern that allows future database backends (DuckDB, LibSQL, etc.).

**Architecture:** Opaque Executor type containing dialect-specific functions (query, exec, placeholder, json_extract). Repositories take an Executor and use its functions for database operations. Connection auto-detects backend from DATABASE_URL scheme. Schema migrations handled by dbmate (external tool) with dialect-specific SQL files.

**Tech Stack:** Gleam, sqlight (existing), pog (new), envoy (existing), dbmate (new - external CLI tool)

---

## Task 1: Create Executor Type and Error Definitions

**Files:**
- Create: `server/src/database/executor.gleam`

**Step 1: Write the Executor type definition**

```gleam
// server/src/database/executor.gleam

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/option.{type Option}

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
    /// Execute a query and decode results
    query: fn(String, List(Value), Decoder(a)) -> Result(List(a), DbError),
    /// Execute a statement without returning results
    exec: fn(String, List(Value)) -> Result(Nil, DbError),
    /// Generate a placeholder for the given parameter index (1-based)
    /// SQLite: "?" (ignores index), PostgreSQL: "$1", "$2", etc.
    placeholder: fn(Int) -> String,
    /// Generate SQL for extracting a field from a JSON column
    /// SQLite: json_extract(column, '$.field')
    /// PostgreSQL: column->>'field' or column->'field'
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
  exec.query(sql, params, decoder)
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

import gleam/list
import gleam/string
```

**Step 2: Run the build to verify syntax**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds (or fails only on missing imports we'll add)

**Step 3: Commit**

```bash
git add server/src/database/executor.gleam
git commit -m "feat(database): add Executor type for multi-database abstraction"
```

---

## Task 2: Create SQLite Executor Implementation

**Files:**
- Create: `server/src/database/sqlite/executor.gleam`

**Step 1: Write the SQLite executor factory**

```gleam
// server/src/database/sqlite/executor.gleam

import database/executor.{
  type DbError, type Decoder, type Executor, type Value, Bool, Blob,
  ConnectionError, ConstraintError, DecodeError, Float, Int, Null,
  QueryError, SQLite, Text,
}
import gleam/dynamic/decode
import gleam/list
import gleam/result
import gleam/string
import sqlight

/// Create an Executor for SQLite from an open connection
pub fn new(conn: sqlight.Connection) -> Executor {
  executor.Executor(
    dialect: SQLite,
    query: fn(sql, params, decoder) {
      sqlight.query(
        sql,
        on: conn,
        with: to_sqlight_values(params),
        expecting: decoder,
      )
      |> result.map_error(sqlight_error_to_db_error)
    },
    exec: fn(sql, params) {
      sqlight.query(
        sql,
        on: conn,
        with: to_sqlight_values(params),
        expecting: decode.dynamic,
      )
      |> result.map(fn(_) { Nil })
      |> result.map_error(sqlight_error_to_db_error)
    },
    placeholder: fn(_index) { "?" },
    json_extract: fn(column, field) {
      "json_extract(" <> column <> ", '$." <> field <> "')"
    },
    json_extract_path: fn(column, path) {
      let path_str = string.join(path, ".")
      "json_extract(" <> column <> ", '$." <> path_str <> "')"
    },
    now: fn() { "datetime('now')" },
  )
}

/// Convert our Value type to sqlight.Value
fn to_sqlight_values(values: List(Value)) -> List(sqlight.Value) {
  list.map(values, fn(v) {
    case v {
      Text(s) -> sqlight.text(s)
      Int(i) -> sqlight.int(i)
      Float(f) -> sqlight.float(f)
      Bool(b) ->
        case b {
          True -> sqlight.int(1)
          False -> sqlight.int(0)
        }
      Null -> sqlight.null()
      Blob(b) -> sqlight.blob(b)
    }
  })
}

/// Convert sqlight.Error to our DbError type
fn sqlight_error_to_db_error(err: sqlight.Error) -> DbError {
  case err {
    sqlight.SqlightError(code, message, _) ->
      case code {
        sqlight.ConstraintCheck
        | sqlight.ConstraintCommithook
        | sqlight.ConstraintDatatype
        | sqlight.ConstraintForeignkey
        | sqlight.ConstraintFunction
        | sqlight.ConstraintNotnull
        | sqlight.ConstraintPinned
        | sqlight.ConstraintPrimarykey
        | sqlight.ConstraintRowid
        | sqlight.ConstraintTrigger
        | sqlight.ConstraintUnique
        | sqlight.ConstraintVtab
        -> ConstraintError(message)
        _ -> QueryError(message)
      }
  }
}
```

**Step 2: Create the sqlite directory**

Run: `mkdir -p /Users/chadmiller/code/quickslice/server/src/database/sqlite`

**Step 3: Run the build to verify**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add server/src/database/sqlite/
git commit -m "feat(database): add SQLite executor implementation"
```

---

## Task 3: Create SQLite Connection Module

**Files:**
- Create: `server/src/database/sqlite/connection.gleam`

**Step 1: Write the SQLite connection module**

```gleam
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
      ConnectionError("Failed to open SQLite database: " <> sqlight_error_message(e))
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
        "sqlite" | "file" -> string.trim_start(rest, "//")
        _ -> url
      }
    Error(_) -> url
  }
}

/// Execute a PRAGMA statement
fn exec_pragma(conn: sqlight.Connection, pragma: String) -> Result(Nil, DbError) {
  sqlight.exec(pragma, conn)
  |> result.map_error(fn(e) {
    ConnectionError("Failed to execute " <> pragma <> ": " <> sqlight_error_message(e))
  })
}

/// Get error message from sqlight error
fn sqlight_error_message(err: sqlight.Error) -> String {
  case err {
    sqlight.SqlightError(_, message, _) -> message
  }
}
```

**Step 2: Run the build to verify**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add server/src/database/sqlite/connection.gleam
git commit -m "feat(database): add SQLite connection module with PRAGMA setup"
```

---

## Task 4: Create Main Connection Module with URL Parsing

**Files:**
- Modify: `server/src/database/connection.gleam` (replace existing)

**Step 1: Read the existing connection.gleam to understand current usage**

The existing `server/src/database/connection.gleam` contains SQLite-specific code that will be moved to the sqlite module. We'll replace it with the unified connection interface.

**Step 2: Write the new unified connection module**

```gleam
// server/src/database/connection.gleam

import database/executor.{type DbError, type Executor, ConnectionError}
import database/sqlite/connection as sqlite_connection
import gleam/string

/// Supported database backends
pub type Backend {
  SQLite
  PostgreSQL
}

/// Parse DATABASE_URL and connect to the appropriate backend
///
/// Supported URL formats:
/// - SQLite: "sqlite:./path/to/db.sqlite", "./path/to/db.sqlite", "file:./path"
/// - PostgreSQL: "postgres://user:pass@host:port/db", "postgresql://..."
pub fn connect(url: String) -> Result(Executor, DbError) {
  case detect_backend(url) {
    SQLite -> sqlite_connection.connect(url)
    PostgreSQL ->
      Error(ConnectionError(
        "PostgreSQL support not yet implemented. Use SQLite for now.",
      ))
  }
}

/// Detect the database backend from a URL
pub fn detect_backend(url: String) -> Backend {
  let url_lower = string.lowercase(url)

  case string.starts_with(url_lower, "postgres://")
    || string.starts_with(url_lower, "postgresql://")
  {
    True -> PostgreSQL
    False -> SQLite
  }
}
```

**Step 3: Run the build to verify**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add server/src/database/connection.gleam
git commit -m "feat(database): add unified connection module with URL detection"
```

---

## Task 5: Add pog Dependency

**Files:**
- Modify: `server/gleam.toml`

**Step 1: Add pog to dependencies**

Add to the `[dependencies]` section:

```toml
pog = ">= 1.0.0 and < 2.0.0"
```

**Step 2: Run gleam deps to fetch**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam deps download`
Expected: pog package downloaded

**Step 3: Commit**

```bash
git add server/gleam.toml
git commit -m "chore(deps): add pog PostgreSQL driver"
```

---

## Task 6: Create PostgreSQL Executor Implementation

**Files:**
- Create: `server/src/database/postgres/executor.gleam`

**Step 1: Create the postgres directory**

Run: `mkdir -p /Users/chadmiller/code/quickslice/server/src/database/postgres`

**Step 2: Write the PostgreSQL executor factory**

```gleam
// server/src/database/postgres/executor.gleam

import database/executor.{
  type DbError, type Decoder, type Executor, type Value, Bool, Blob,
  ConnectionError, ConstraintError, DecodeError, Float, Int, Null,
  PostgreSQL, QueryError, Text,
}
import gleam/dynamic/decode
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import pog

/// Create an Executor for PostgreSQL from a connection pool
pub fn new(pool: pog.Connection) -> Executor {
  executor.Executor(
    dialect: PostgreSQL,
    query: fn(sql, params, decoder) {
      pog.query(sql)
      |> set_pog_params(params)
      |> pog.returning(decoder)
      |> pog.execute(pool)
      |> result.map(fn(returned) { returned.rows })
      |> result.map_error(pog_error_to_db_error)
    },
    exec: fn(sql, params) {
      pog.query(sql)
      |> set_pog_params(params)
      |> pog.execute(pool)
      |> result.map(fn(_) { Nil })
      |> result.map_error(pog_error_to_db_error)
    },
    placeholder: fn(index) { "$" <> int.to_string(index) },
    json_extract: fn(column, field) {
      column <> "->>'" <> field <> "'"
    },
    json_extract_path: fn(column, path) {
      case path {
        [] -> column
        [single] -> column <> "->>'" <> single <> "'"
        _ -> {
          // All but last use ->, last uses ->>
          let path_parts = list.index_map(path, fn(part, i) {
            case i == list.length(path) - 1 {
              True -> "->>'" <> part <> "'"
              False -> "->'" <> part <> "'"
            }
          })
          column <> string.join(path_parts, "")
        }
      }
    },
    now: fn() { "NOW()" },
  )
}

/// Set parameters on a pog query
fn set_pog_params(query: pog.Query(a), params: List(Value)) -> pog.Query(a) {
  list.fold(params, query, fn(q, param) {
    case param {
      Text(s) -> pog.parameter(q, pog.text(s))
      Int(i) -> pog.parameter(q, pog.int(i))
      Float(f) -> pog.parameter(q, pog.float(f))
      Bool(b) -> pog.parameter(q, pog.bool(b))
      Null -> pog.parameter(q, pog.null())
      Blob(b) -> pog.parameter(q, pog.bytea(b))
    }
  })
}

/// Convert pog.QueryError to our DbError type
fn pog_error_to_db_error(err: pog.QueryError) -> DbError {
  case err {
    pog.ConstraintViolated(message, constraint, _detail) ->
      ConstraintError(message <> ": " <> constraint)
    pog.PostgresqlError(code, name, message) ->
      QueryError("[" <> code <> " " <> name <> "] " <> message)
    pog.UnexpectedArgumentCount(expected, got) ->
      QueryError(
        "Expected " <> int.to_string(expected) <> " arguments, got " <> int.to_string(got),
      )
    pog.UnexpectedArgumentType(expected, got) ->
      QueryError("Expected argument type " <> expected <> ", got " <> got)
    pog.UnexpectedResultType(errors) ->
      DecodeError("Failed to decode result: " <> string.inspect(errors))
    pog.ConnectionUnavailable ->
      ConnectionError("Database connection unavailable")
  }
}
```

**Step 3: Run the build to verify**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add server/src/database/postgres/
git commit -m "feat(database): add PostgreSQL executor implementation"
```

---

## Task 7: Create PostgreSQL Connection Module

**Files:**
- Create: `server/src/database/postgres/connection.gleam`

**Step 1: Write the PostgreSQL connection module**

```gleam
// server/src/database/postgres/connection.gleam

import database/executor.{type DbError, type Executor, ConnectionError}
import database/postgres/executor as postgres_executor
import gleam/option.{None, Some}
import gleam/result
import gleam/uri
import logging
import pog

/// Default connection pool size
const default_pool_size = 10

/// Connect to PostgreSQL database and return an Executor
pub fn connect(url: String) -> Result(Executor, DbError) {
  use config <- result.try(parse_url(url))

  let pool =
    pog.default_config()
    |> pog.host(config.host)
    |> pog.port(config.port)
    |> pog.database(config.database)
    |> pog.user(config.user)
    |> pog.password(config.password)
    |> pog.pool_size(config.pool_size)
    |> pog.connect

  logging.log(
    logging.Info,
    "Connected to PostgreSQL database: " <> config.host <> "/" <> config.database,
  )

  Ok(postgres_executor.new(pool))
}

/// Parsed PostgreSQL connection config
type PgConfig {
  PgConfig(
    host: String,
    port: Int,
    database: String,
    user: String,
    password: option.Option(String),
    pool_size: Int,
  )
}

/// Parse a PostgreSQL URL into connection config
/// Format: postgres://user:password@host:port/database?pool_size=N
fn parse_url(url: String) -> Result(PgConfig, DbError) {
  case uri.parse(url) {
    Ok(parsed) -> {
      let host = option.unwrap(parsed.host, "localhost")
      let port = option.unwrap(parsed.port, 5432)
      let database = case parsed.path {
        "/" <> db -> db
        db -> db
      }

      // Parse userinfo (user:password)
      let #(user, password) = case parsed.userinfo {
        Some(userinfo) ->
          case string.split_once(userinfo, ":") {
            Ok(#(u, p)) -> #(u, Some(p))
            Error(_) -> #(userinfo, None)
          }
        None -> #("postgres", None)
      }

      // Parse query params for pool_size
      let pool_size = case parsed.query {
        Some(query) -> parse_pool_size(query)
        None -> default_pool_size
      }

      case database {
        "" ->
          Error(ConnectionError("No database specified in PostgreSQL URL"))
        _ ->
          Ok(PgConfig(
            host: host,
            port: port,
            database: database,
            user: user,
            password: password,
            pool_size: pool_size,
          ))
      }
    }
    Error(_) -> Error(ConnectionError("Invalid PostgreSQL URL: " <> url))
  }
}

import gleam/int
import gleam/string

/// Parse pool_size from query string
fn parse_pool_size(query: String) -> Int {
  query
  |> string.split("&")
  |> list.find_map(fn(param) {
    case string.split_once(param, "=") {
      Ok(#("pool_size", value)) ->
        case int.parse(value) {
          Ok(n) -> Ok(n)
          Error(_) -> Error(Nil)
        }
      _ -> Error(Nil)
    }
  })
  |> result.unwrap(default_pool_size)
}

import gleam/list
```

**Step 2: Update the main connection module to use PostgreSQL**

Modify `server/src/database/connection.gleam`:

```gleam
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

/// Parse DATABASE_URL and connect to the appropriate backend
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

  case string.starts_with(url_lower, "postgres://")
    || string.starts_with(url_lower, "postgresql://")
  {
    True -> PostgreSQL
    False -> SQLite
  }
}
```

**Step 3: Run the build to verify**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add server/src/database/postgres/connection.gleam server/src/database/connection.gleam
git commit -m "feat(database): add PostgreSQL connection module"
```

---

## Task 8: Migrate config Repository (Pilot Migration)

**Files:**
- Modify: `server/src/database/repositories/config.gleam`

This is the simplest repository and will serve as the template for migrating others.

**Step 1: Update config.gleam to use Executor**

```gleam
// server/src/database/repositories/config.gleam

import database/executor.{type DbError, type Executor, Text}
import envoy
import gleam/dynamic/decode
import gleam/list
import gleam/result
import gleam/string

// ===== Default Values =====

pub const default_relay_url = "https://relay1.us-west.bsky.network"

pub const default_plc_directory_url = "https://plc.directory"

pub const default_jetstream_url = "wss://jetstream2.us-west.bsky.network/subscribe"

pub const default_oauth_supported_scopes = "atproto transition:generic"

// ===== Config Functions =====

/// Get a config value by key
pub fn get(exec: Executor, key: String) -> Result(String, DbError) {
  let sql =
    "SELECT value FROM config WHERE key = " <> executor.placeholder(exec, 1)

  let decoder = {
    use value <- decode.field(0, decode.string)
    decode.success(value)
  }

  case executor.query(exec, sql, [Text(key)], decoder) {
    Ok([value, ..]) -> Ok(value)
    Ok([]) ->
      Error(executor.ConstraintError("Config key not found: " <> key))
    Error(err) -> Error(err)
  }
}

/// Set or update a config value
pub fn set(exec: Executor, key: String, value: String) -> Result(Nil, DbError) {
  let now = executor.now(exec)
  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "INSERT INTO config (key, value, updated_at)
       VALUES (" <> executor.placeholder(exec, 1) <> ", " <> executor.placeholder(exec, 2) <> ", " <> now <> ")
       ON CONFLICT(key) DO UPDATE SET
         value = excluded.value,
         updated_at = " <> now
    executor.PostgreSQL ->
      "INSERT INTO config (key, value, updated_at)
       VALUES (" <> executor.placeholder(exec, 1) <> ", " <> executor.placeholder(exec, 2) <> ", " <> now <> ")
       ON CONFLICT(key) DO UPDATE SET
         value = EXCLUDED.value,
         updated_at = " <> now
  }

  executor.exec(exec, sql, [Text(key), Text(value)])
}

/// Delete a config value by key
pub fn delete(exec: Executor, key: String) -> Result(Nil, DbError) {
  let sql = "DELETE FROM config WHERE key = " <> executor.placeholder(exec, 1)

  executor.exec(exec, sql, [Text(key)])
}

/// Deletes the domain_authority config entry
pub fn delete_domain_authority(exec: Executor) -> Result(Nil, DbError) {
  delete(exec, "domain_authority")
}

// ===== Admin DID Functions =====

/// Get admin DIDs from config
pub fn get_admin_dids(exec: Executor) -> List(String) {
  case get(exec, "admin_dids") {
    Ok(value) -> {
      value
      |> string.split(",")
      |> list.map(string.trim)
      |> list.filter(fn(did) { !string.is_empty(did) })
    }
    Error(_) -> []
  }
}

/// Add an admin DID to the list
pub fn add_admin_did(exec: Executor, did: String) -> Result(Nil, DbError) {
  let current = get_admin_dids(exec)
  case list.contains(current, did) {
    True -> Ok(Nil)
    False -> {
      let new_list = list.append(current, [did])
      let value = string.join(new_list, ",")
      set(exec, "admin_dids", value)
    }
  }
}

pub type RemoveAdminError {
  LastAdminError
  NotFoundError
  DatabaseError(DbError)
}

/// Remove an admin DID from the list
/// Returns error if trying to remove the last admin
pub fn remove_admin_did(
  exec: Executor,
  did: String,
) -> Result(List(String), RemoveAdminError) {
  let current = get_admin_dids(exec)
  case list.contains(current, did) {
    False -> Error(NotFoundError)
    True -> {
      let new_list = list.filter(current, fn(d) { d != did })
      case new_list {
        [] -> Error(LastAdminError)
        _ -> {
          let value = string.join(new_list, ",")
          case set(exec, "admin_dids", value) {
            Ok(_) -> Ok(new_list)
            Error(err) -> Error(DatabaseError(err))
          }
        }
      }
    }
  }
}

/// Set the full admin DIDs list (replaces existing)
pub fn set_admin_dids(
  exec: Executor,
  dids: List(String),
) -> Result(Nil, DbError) {
  let value = string.join(dids, ",")
  set(exec, "admin_dids", value)
}

/// Check if a DID is an admin
pub fn is_admin(exec: Executor, did: String) -> Bool {
  let admins = get_admin_dids(exec)
  list.contains(admins, did)
}

/// Check if any admins are configured
pub fn has_admins(exec: Executor) -> Bool {
  case get_admin_dids(exec) {
    [] -> False
    _ -> True
  }
}

// ===== External Services Configuration =====

/// Get relay URL from config, with default fallback
pub fn get_relay_url(exec: Executor) -> String {
  case get(exec, "relay_url") {
    Ok(url) -> url
    Error(_) -> default_relay_url
  }
}

/// Get PLC directory URL with precedence: env var -> database -> default
pub fn get_plc_directory_url(exec: Executor) -> String {
  case envoy.get("PLC_DIRECTORY_URL") {
    Ok(url) -> url
    Error(_) -> {
      case get(exec, "plc_directory_url") {
        Ok(url) -> url
        Error(_) -> default_plc_directory_url
      }
    }
  }
}

/// Get Jetstream URL from config, with default fallback
pub fn get_jetstream_url(exec: Executor) -> String {
  case get(exec, "jetstream_url") {
    Ok(url) -> url
    Error(_) -> default_jetstream_url
  }
}

/// Get OAuth supported scopes from config, with default fallback
pub fn get_oauth_supported_scopes(exec: Executor) -> String {
  case get(exec, "oauth_supported_scopes") {
    Ok(scopes) -> scopes
    Error(_) -> default_oauth_supported_scopes
  }
}

/// Parse OAuth supported scopes into List(String)
pub fn get_oauth_supported_scopes_list(exec: Executor) -> List(String) {
  let scopes_str = get_oauth_supported_scopes(exec)
  scopes_str
  |> string.split(" ")
  |> list.map(string.trim)
  |> list.filter(fn(s) { !string.is_empty(s) })
}

/// Set relay URL
pub fn set_relay_url(exec: Executor, url: String) -> Result(Nil, DbError) {
  set(exec, "relay_url", url)
}

/// Set PLC directory URL
pub fn set_plc_directory_url(
  exec: Executor,
  url: String,
) -> Result(Nil, DbError) {
  set(exec, "plc_directory_url", url)
}

/// Set Jetstream URL
pub fn set_jetstream_url(exec: Executor, url: String) -> Result(Nil, DbError) {
  set(exec, "jetstream_url", url)
}

/// Set OAuth supported scopes (space-separated string)
pub fn set_oauth_supported_scopes(
  exec: Executor,
  scopes: String,
) -> Result(Nil, DbError) {
  set(exec, "oauth_supported_scopes", scopes)
}

/// Initialize config with defaults if not already set
pub fn initialize_config_defaults(exec: Executor) -> Result(Nil, DbError) {
  case get(exec, "relay_url") {
    Error(_) -> {
      let _ = set(exec, "relay_url", default_relay_url)
      Nil
    }
    Ok(_) -> Nil
  }

  case get(exec, "plc_directory_url") {
    Error(_) -> {
      let _ = set(exec, "plc_directory_url", default_plc_directory_url)
      Nil
    }
    Ok(_) -> Nil
  }

  case get(exec, "jetstream_url") {
    Error(_) -> {
      let _ = set(exec, "jetstream_url", default_jetstream_url)
      Nil
    }
    Ok(_) -> Nil
  }

  case get(exec, "oauth_supported_scopes") {
    Error(_) -> {
      let _ = set(exec, "oauth_supported_scopes", default_oauth_supported_scopes)
      Nil
    }
    Ok(_) -> Nil
  }

  Ok(Nil)
}
```

**Step 2: Run the build to check for errors**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build may fail due to callers still using old signature - that's expected

**Step 3: Commit the config migration**

```bash
git add server/src/database/repositories/config.gleam
git commit -m "refactor(database): migrate config repository to Executor pattern"
```

---

## Task 9: Update where_clause to Support Dialects

**Files:**
- Modify: `server/src/database/queries/where_clause.gleam`

**Step 1: Update where_clause.gleam to accept Executor**

The key changes:
- Replace `sqlight.Value` with `executor.Value`
- Replace hardcoded `json_extract()` with `executor.json_extract()`
- Replace `"?"` placeholders with `executor.placeholder()`
- Add Executor parameter to `build_where_sql`

This is a larger refactor. The core pattern:

```gleam
// Before
"json_extract(" <> table_name <> "json, '$." <> field <> "')"

// After
executor.json_extract(exec, table_name <> "json", field)
```

```gleam
// Before
sqlight.text(search_text)

// After
executor.Text(search_text)
```

Due to the complexity of this file, this task involves:
1. Changing imports
2. Changing WhereCondition to use executor.Value
3. Changing build_where_sql signature to take Executor
4. Updating all SQL generation to use Executor helpers

**Step 2: Run the build**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`

**Step 3: Commit**

```bash
git add server/src/database/queries/where_clause.gleam
git commit -m "refactor(database): update where_clause to use Executor for dialect support"
```

---

## Task 10: Migrate Remaining Simple Repositories

**Files:**
- Modify: `server/src/database/repositories/actors.gleam`
- Modify: `server/src/database/repositories/lexicons.gleam`
- Modify: `server/src/database/repositories/jetstream_activity.gleam`

For each repository, follow the same pattern as config.gleam:
1. Change `sqlight.Connection` to `Executor`
2. Change `sqlight.text(x)` to `Text(x)`, etc.
3. Replace `"?"` with `executor.placeholder(exec, n)`
4. Replace `datetime('now')` with `executor.now(exec)`

**Step 1-3: Update each repository**

Each commit:
```bash
git add server/src/database/repositories/<name>.gleam
git commit -m "refactor(database): migrate <name> repository to Executor pattern"
```

---

## Task 11: Migrate OAuth Repositories

**Files:**
- Modify all `oauth_*.gleam` files in `server/src/database/repositories/`

Same pattern as Task 10. These are more complex but follow the same transformation rules.

---

## Task 12: Migrate records Repository (Most Complex)

**Files:**
- Modify: `server/src/database/repositories/records.gleam`

This is the largest repository with JSON queries. Key transformations:
- All `json_extract()` calls use `executor.json_extract()`
- Batch queries need placeholder numbering for PostgreSQL
- The `get_by_reference_field` functions need special attention for JSON operators

---

## Task 13: Set Up dbmate for Schema Migrations

**Background:**
Replace the Gleam-based migration system with dbmate, a lightweight CLI tool that manages database migrations using plain SQL files. This gives us:
- Plain SQL migrations (no Gleam code for schema)
- Separate migration files per dialect
- Rollback support (down migrations)
- Auto-generated schema.sql for reference
- Standard tooling that works in CI/CD, Docker, etc.

**Files:**
- Create: `db/migrations/sqlite/` directory
- Create: `db/migrations/postgres/` directory
- Create: `.dbmate.env` (or use existing `.env`)
- Delete (later): `server/src/database/schema/migrations.gleam`
- Delete (later): `server/src/database/schema/tables.gleam`

**Step 1: Install dbmate**

Run (macOS):
```bash
brew install dbmate
```

Or download binary from https://github.com/amacneil/dbmate/releases

**Step 2: Create dbmate configuration**

Create `db/.dbmate.toml`:
```toml
# dbmate configuration
# DATABASE_URL is read from environment

[dbmate]
migrations-dir = "./db/migrations"
schema-file = "./db/schema.sql"
no-dump-schema = false
```

**Step 3: Create migration directories**

```bash
mkdir -p db/migrations/sqlite db/migrations/postgres
```

**Step 4: Commit dbmate setup**

```bash
git add db/.dbmate.toml db/migrations/
git commit -m "chore: add dbmate configuration for database migrations"
```

---

## Task 14: Convert Existing Migrations to dbmate Format (SQLite)

**Files:**
- Create: `db/migrations/sqlite/20231001000001_initial_schema.sql`
- Create: `db/migrations/sqlite/20231001000002_config_table.sql`
- Create: `db/migrations/sqlite/20231001000003_cid_index.sql`
- Create: `db/migrations/sqlite/20231001000004_jetstream_activity.sql`
- Create: `db/migrations/sqlite/20231001000005_jetstream_cursor.sql`
- Create: `db/migrations/sqlite/20231001000006_oauth_tables.sql`
- Create: `db/migrations/sqlite/20231001000007_authorization_code.sql`
- Create: `db/migrations/sqlite/20231001000008_admin_session.sql`
- Create: `db/migrations/sqlite/20231001000009_dpop_jti.sql`

**Step 1: Create initial schema migration**

```sql
-- db/migrations/sqlite/20231001000001_initial_schema.sql

-- migrate:up
CREATE TABLE IF NOT EXISTS record (
  uri TEXT PRIMARY KEY,
  cid TEXT NOT NULL,
  did TEXT NOT NULL,
  collection TEXT NOT NULL,
  json TEXT NOT NULL,
  indexed_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX IF NOT EXISTS idx_record_did ON record(did);
CREATE INDEX IF NOT EXISTS idx_record_collection ON record(collection);
CREATE INDEX IF NOT EXISTS idx_record_did_collection ON record(did, collection);
CREATE INDEX IF NOT EXISTS idx_record_indexed_at ON record(indexed_at);

CREATE TABLE IF NOT EXISTS actor (
  did TEXT PRIMARY KEY,
  handle TEXT NOT NULL,
  indexed_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX IF NOT EXISTS idx_actor_handle ON actor(handle);

CREATE TABLE IF NOT EXISTS lexicon (
  id TEXT PRIMARY KEY,
  json TEXT NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

-- migrate:down
DROP TABLE IF EXISTS lexicon;
DROP TABLE IF EXISTS actor;
DROP TABLE IF EXISTS record;
```

**Step 2: Create remaining migration files**

Each migration follows the pattern:
```sql
-- migrate:up
<create statements>

-- migrate:down
<drop statements>
```

Convert each migration from `server/src/database/schema/tables.gleam` to SQL.

**Step 3: Test migrations**

```bash
# Set DATABASE_URL for SQLite
export DATABASE_URL="sqlite:./data/test.db"

# Run migrations
dbmate --migrations-dir ./db/migrations/sqlite up

# Verify
dbmate --migrations-dir ./db/migrations/sqlite status
```

**Step 4: Commit SQLite migrations**

```bash
git add db/migrations/sqlite/
git commit -m "feat(database): add SQLite migrations in dbmate format"
```

---

## Task 15: Create PostgreSQL Migrations

**Files:**
- Create: `db/migrations/postgres/20231001000001_initial_schema.sql`
- Create: `db/migrations/postgres/20231001000002_config_table.sql`
- (etc. - matching SQLite migrations)

**Step 1: Create initial schema migration for Postgres**

```sql
-- db/migrations/postgres/20231001000001_initial_schema.sql

-- migrate:up
CREATE TABLE IF NOT EXISTS record (
  uri TEXT PRIMARY KEY,
  cid TEXT NOT NULL,
  did TEXT NOT NULL,
  collection TEXT NOT NULL,
  json JSONB NOT NULL,
  indexed_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_record_did ON record(did);
CREATE INDEX IF NOT EXISTS idx_record_collection ON record(collection);
CREATE INDEX IF NOT EXISTS idx_record_did_collection ON record(did, collection);
CREATE INDEX IF NOT EXISTS idx_record_indexed_at ON record(indexed_at);
CREATE INDEX IF NOT EXISTS idx_record_json_gin ON record USING GIN (json);

CREATE TABLE IF NOT EXISTS actor (
  did TEXT PRIMARY KEY,
  handle TEXT NOT NULL,
  indexed_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_actor_handle ON actor(handle);

CREATE TABLE IF NOT EXISTS lexicon (
  id TEXT PRIMARY KEY,
  json JSONB NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- migrate:down
DROP TABLE IF EXISTS lexicon;
DROP TABLE IF EXISTS actor;
DROP TABLE IF EXISTS record;
```

**Key Postgres differences from SQLite:**
- `JSONB` instead of `TEXT` for JSON columns (better indexing, operators)
- `TIMESTAMPTZ` instead of `TEXT` for timestamps
- `NOW()` instead of `datetime('now')`
- GIN indexes for JSONB columns
- `SERIAL`/`BIGSERIAL` for auto-increment (where applicable)

**Step 2: Create all Postgres migration files**

Mirror the SQLite migrations with Postgres-specific syntax.

**Step 3: Test with Postgres**

```bash
export DATABASE_URL="postgres://user:pass@localhost:5432/quickslice_test"
dbmate --migrations-dir ./db/migrations/postgres up
dbmate --migrations-dir ./db/migrations/postgres status
```

**Step 4: Commit Postgres migrations**

```bash
git add db/migrations/postgres/
git commit -m "feat(database): add PostgreSQL migrations in dbmate format"
```

---

## Task 16: Add Makefile/Script for Database Operations

**Files:**
- Modify: `Makefile`

**Step 1: Add database targets to Makefile**

```makefile
# Database migrations
.PHONY: db-up db-down db-status db-new

# Detect dialect from DATABASE_URL
DB_DIALECT := $(shell echo "$(DATABASE_URL)" | grep -q "^postgres" && echo "postgres" || echo "sqlite")

db-up:
	dbmate --migrations-dir ./db/migrations/$(DB_DIALECT) up

db-down:
	dbmate --migrations-dir ./db/migrations/$(DB_DIALECT) down

db-status:
	dbmate --migrations-dir ./db/migrations/$(DB_DIALECT) status

db-new:
	@read -p "Migration name: " name; \
	dbmate --migrations-dir ./db/migrations/sqlite new $$name; \
	dbmate --migrations-dir ./db/migrations/postgres new $$name
```

**Step 2: Commit Makefile changes**

```bash
git add Makefile
git commit -m "chore: add database migration targets to Makefile"
```

---

## Task 17: Remove Gleam Migration System

**Files:**
- Delete: `server/src/database/schema/migrations.gleam`
- Delete: `server/src/database/schema/tables.gleam`
- Modify: `server/src/database/connection.gleam` (remove migration calls)

**Step 1: Update connection.gleam to not run migrations**

The server should assume migrations have been run by dbmate before startup.

```gleam
// Remove this from sqlite/connection.gleam:
// use _ <- result.try(migrations.run_migrations(conn))

// Server startup now expects database to be migrated
```

**Step 2: Delete old migration files**

```bash
rm server/src/database/schema/migrations.gleam
rm server/src/database/schema/tables.gleam
```

**Step 3: Update any imports**

Search for imports of the deleted modules and remove them.

**Step 4: Commit removal**

```bash
git add -A
git commit -m "refactor(database): remove Gleam migration system in favor of dbmate"
```

---

## Task 18: Update Server Startup to Use New Connection

**Files:**
- Modify: `server/src/server.gleam`

Update the server startup to:
1. Read `DATABASE_URL` from environment
2. Use `connection.connect()` to get an Executor
3. Pass Executor to all handlers/context

```gleam
// server.gleam startup pattern
import database/connection
import envoy

pub fn main() {
  // Load .env file
  dotenv_gleam.config()

  // Get DATABASE_URL (defaults to SQLite for local dev)
  let database_url =
    envoy.get("DATABASE_URL")
    |> result.unwrap("sqlite:./data/quickslice.db")

  // Connect - auto-detects SQLite vs Postgres from URL
  use exec <- result.try(connection.connect(database_url))

  // Pass executor to context/handlers
  start_server(exec)
}
```

---

## Task 19: Update All Callers (Handlers, GraphQL Resolvers)

**Files:**
- All files that import repository modules

Search for all files importing from `database/repositories/` and update them to pass `Executor` instead of `sqlight.Connection`.

**Step 1: Find all callers**

```bash
grep -r "sqlight.Connection" server/src/ --include="*.gleam"
```

**Step 2: Update each file**

Replace:
- `conn: sqlight.Connection` → `exec: Executor`
- `import sqlight` → `import database/executor.{type Executor}`
- Function calls: `repo.get(conn, ...)` → `repo.get(exec, ...)`

---

## Task 20: Add Integration Tests

**Files:**
- Create: `server/test/database/executor_test.gleam`
- Create: `server/test/database/sqlite_test.gleam`

Test the Executor abstraction with SQLite to ensure:
- Connection works
- Queries work
- Placeholders are correct
- JSON extraction works

```gleam
// server/test/database/executor_test.gleam

import database/connection
import database/executor.{Text}
import gleam/dynamic/decode
import gleeunit/should

pub fn sqlite_placeholder_test() {
  let assert Ok(exec) = connection.connect("sqlite::memory:")

  executor.placeholder(exec, 1) |> should.equal("?")
  executor.placeholder(exec, 5) |> should.equal("?")
}

pub fn sqlite_json_extract_test() {
  let assert Ok(exec) = connection.connect("sqlite::memory:")

  executor.json_extract(exec, "json", "name")
  |> should.equal("json_extract(json, '$.name')")
}

pub fn sqlite_now_test() {
  let assert Ok(exec) = connection.connect("sqlite::memory:")

  executor.now(exec) |> should.equal("datetime('now')")
}
```

---

## Task 21: Documentation

**Files:**
- Create: `docs/database.md`

**Content:**

```markdown
# Database Configuration

## Supported Databases

- **SQLite** (default) - Embedded, zero-config, great for development and small deployments
- **PostgreSQL** - Production-ready, full-featured, recommended for larger deployments

## Configuration

Set `DATABASE_URL` environment variable:

### SQLite
\`\`\`bash
DATABASE_URL="sqlite:./data/quickslice.db"
DATABASE_URL="./data/quickslice.db"  # Also works
\`\`\`

### PostgreSQL
\`\`\`bash
DATABASE_URL="postgres://user:password@localhost:5432/quickslice"
DATABASE_URL="postgresql://user:password@host:5432/db?pool_size=20"
\`\`\`

## Running Migrations

Migrations are managed by [dbmate](https://github.com/amacneil/dbmate).

\`\`\`bash
# Apply pending migrations
make db-up

# Rollback last migration
make db-down

# Check migration status
make db-status

# Create new migration (creates for both SQLite and Postgres)
make db-new
\`\`\`

## Adding a New Database Backend

1. Create `server/src/database/<backend>/executor.gleam` implementing the Executor interface
2. Create `server/src/database/<backend>/connection.gleam` with connect function
3. Update `server/src/database/connection.gleam` to detect and route to new backend
4. Create `db/migrations/<backend>/` with dialect-specific migrations
5. Test thoroughly

See `database/sqlite/` as a reference implementation.
```

---

## Task 22: Add Docker/CI Support for Multiple Databases

**Files:**
- Modify: `docker-compose.yml`
- Create: `docker-compose.postgres.yml` (optional override)

**Step 1: Add Postgres service to docker-compose**

```yaml
# docker-compose.yml addition
services:
  postgres:
    image: postgres:16-alpine
    environment:
      POSTGRES_USER: quickslice
      POSTGRES_PASSWORD: quickslice
      POSTGRES_DB: quickslice
    ports:
      - "5432:5432"
    volumes:
      - postgres_data:/var/lib/postgresql/data

volumes:
  postgres_data:
```

**Step 2: Add dbmate to Dockerfile**

```dockerfile
# Add to Dockerfile
RUN curl -fsSL -o /usr/local/bin/dbmate https://github.com/amacneil/dbmate/releases/latest/download/dbmate-linux-amd64 \
    && chmod +x /usr/local/bin/dbmate
```

---

## Summary

| Phase | Tasks | Description |
|-------|-------|-------------|
| **Phase 1** | 1-4 | Core abstraction layer (non-breaking) |
| **Phase 2** | 5-7 | Add PostgreSQL driver (non-breaking) |
| **Phase 3** | 8-12 | Migrate repositories (breaking, incremental) |
| **Phase 4** | 13-17 | dbmate migrations, remove Gleam migrations |
| **Phase 5** | 18-19 | Wire everything together |
| **Phase 6** | 20-22 | Testing, docs, Docker/CI |

**Key Benefits:**
- **Extensible:** Adding LibSQL, DuckDB, or other SQL databases = implement one executor
- **Standard tooling:** dbmate is language-agnostic, works in CI/CD
- **Clean separation:** Gleam code handles queries, dbmate handles schema
- **Rollback support:** dbmate provides down migrations
- **Parallel development:** SQLite and Postgres migrations don't interfere

Each task is independently testable and committable.
