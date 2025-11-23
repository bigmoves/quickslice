# Database Layer Refactoring Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Refactor the monolithic 2417-line `database.gleam` file into a well-structured, modular database layer following repository pattern best practices.

**Architecture:** Split database layer by domain aggregates (records, actors, lexicons, config) with dedicated repositories, extract schema/migration logic, separate query utilities (pagination, where clauses, aggregates), and create shared type definitions. Follows functional programming patterns with Result types, immutable data, and clear separation of concerns.

**Tech Stack:** Gleam, SQLite (via sqlight), cursor-based pagination, JSON field extraction

---

## Overview

The current `server/src/database.gleam` is 2417 lines containing:
- Schema versioning & migrations (lines 34-397)
- Config storage (lines 398-554)
- Jetstream cursor tracking (lines 555-613)
- Records, Actors, Lexicons CRUD (lines 614-2243)
- Aggregation queries (lines 2244-2417)

15 files currently import from `database`, requiring careful migration of public API.

**New Structure:**
```
database/
├── types.gleam              # Shared types
├── connection.gleam         # DB connection & initialization
├── schema/
│   ├── migrations.gleam     # Migration framework
│   └── tables.gleam         # Table creation DDL
├── repositories/
│   ├── records.gleam        # Record CRUD & queries
│   ├── actors.gleam         # Actor CRUD
│   ├── lexicons.gleam       # Lexicon CRUD
│   └── config.gleam         # Config storage
├── queries/
│   ├── pagination.gleam     # Reusable pagination logic
│   └── aggregates.gleam     # Aggregate queries
└── jetstream.gleam          # Jetstream cursor operations
```

---

## Task 1: Create database/ directory structure

**Files:**
- Create: `server/src/database/` (directory)
- Create: `server/src/database/schema/` (directory)
- Create: `server/src/database/repositories/` (directory)
- Create: `server/src/database/queries/` (directory)

**Step 1: Create directory structure**

Run the following command to create all necessary directories:

```bash
mkdir -p server/src/database/schema server/src/database/repositories server/src/database/queries
```

**Step 2: Verify directories exist**

Run: `ls -la server/src/database/`
Expected: Should show `schema/`, `repositories/`, and `queries/` subdirectories

**Step 3: Commit**

```bash
git add server/src/database/
git commit -m "feat(database): create database layer directory structure

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

## Task 2: Extract shared types

**Files:**
- Create: `server/src/database/types.gleam`
- Reference: `server/src/database.gleam:15-32` (current types)

**Step 1: Create types module with shared domain types**

Create `server/src/database/types.gleam`:

```gleam
// Shared database type definitions

/// A record stored in the database
pub type Record {
  Record(
    uri: String,
    cid: String,
    did: String,
    collection: String,
    json: String,
    indexed_at: String,
  )
}

/// An actor (user) stored in the database
pub type Actor {
  Actor(did: String, handle: String, indexed_at: String)
}

/// A lexicon schema definition
pub type Lexicon {
  Lexicon(id: String, json: String, created_at: String)
}

/// Collection statistics
pub type CollectionStat {
  CollectionStat(collection: String, count: Int)
}

/// Result of inserting a record
pub type InsertResult {
  /// Record was newly inserted or updated
  Inserted
  /// Record was skipped (duplicate CID or unchanged)
  Skipped
}

/// Date interval for date truncation in aggregations
pub type DateInterval {
  Hour
  Day
  Week
  Month
}

/// A field to group by with optional date truncation
pub type GroupByField {
  SimpleField(field: String)
  TruncatedField(field: String, interval: DateInterval)
}
```

**Step 2: Build to verify syntax**

Run: `gleam build --target erlang`
Expected: Compilation successful

**Step 3: Commit**

```bash
git add server/src/database/types.gleam
git commit -m "feat(database): add shared type definitions

Extract Record, Actor, Lexicon, and related types into dedicated module

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

## Task 3: Create schema migration framework

**Files:**
- Create: `server/src/database/schema/migrations.gleam`
- Reference: `server/src/database.gleam:34-397` (migration code)

**Step 1: Create migrations module**

Create `server/src/database/schema/migrations.gleam`:

```gleam
import gleam/dynamic/decode
import gleam/int
import gleam/list
import gleam/result
import gleam/string
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

  // Import from tables module
  use _ <- result.try(database/schema/tables.create_record_table(conn))
  use _ <- result.try(database/schema/tables.create_actor_table(conn))
  use _ <- result.try(database/schema/tables.create_lexicon_table(conn))
  use _ <- result.try(session.init_db(conn))

  Ok(Nil)
}

/// Migration v2: Add config table
fn migration_v2(conn: sqlight.Connection) -> Result(Nil, sqlight.Error) {
  logging.log(logging.Info, "Running migration v2 (config table)...")

  let create_table_sql =
    "
    CREATE TABLE IF NOT EXISTS config (
      key TEXT PRIMARY KEY NOT NULL,
      value TEXT NOT NULL,
      updated_at TEXT NOT NULL DEFAULT (datetime('now'))
    )
  "

  sqlight.exec(create_table_sql, conn)
}

/// Migration v3: Add CID index for deduplication
fn migration_v3(conn: sqlight.Connection) -> Result(Nil, sqlight.Error) {
  logging.log(logging.Info, "Running migration v3 (CID index)...")

  let create_cid_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_record_cid
    ON record(cid)
  "

  sqlight.exec(create_cid_index_sql, conn)
}

/// Migration v4: Add jetstream_activity table for 24h activity log
fn migration_v4(conn: sqlight.Connection) -> Result(Nil, sqlight.Error) {
  logging.log(
    logging.Info,
    "Running migration v4 (jetstream_activity table)...",
  )

  let create_table_sql =
    "
    CREATE TABLE IF NOT EXISTS jetstream_activity (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      timestamp TEXT NOT NULL,
      operation TEXT NOT NULL,
      collection TEXT NOT NULL,
      did TEXT NOT NULL,
      status TEXT NOT NULL,
      error_message TEXT,
      event_json TEXT NOT NULL
    )
  "

  let create_timestamp_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_jetstream_activity_timestamp
    ON jetstream_activity(timestamp DESC)
  "

  use _ <- result.try(sqlight.exec(create_table_sql, conn))
  sqlight.exec(create_timestamp_index_sql, conn)
}

/// Migration v5: Add jetstream_cursor table for cursor tracking
fn migration_v5(conn: sqlight.Connection) -> Result(Nil, sqlight.Error) {
  logging.log(
    logging.Info,
    "Running migration v5 (jetstream_cursor table)...",
  )

  let create_table_sql =
    "
    CREATE TABLE IF NOT EXISTS jetstream_cursor (
      id INTEGER PRIMARY KEY CHECK (id = 1),
      cursor INTEGER NOT NULL,
      updated_at TEXT NOT NULL DEFAULT (datetime('now'))
    )
  "

  sqlight.exec(create_table_sql, conn)
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
```

**Step 2: Create tables module**

Create `server/src/database/schema/tables.gleam`:

```gleam
import gleam/result
import sqlight

/// Creates the record table if it doesn't exist
pub fn create_record_table(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let create_table_sql =
    "
    CREATE TABLE IF NOT EXISTS record (
      uri TEXT PRIMARY KEY NOT NULL,
      cid TEXT NOT NULL,
      did TEXT NOT NULL,
      collection TEXT NOT NULL,
      json TEXT NOT NULL,
      indexed_at TEXT NOT NULL DEFAULT (datetime('now'))
    )
  "

  let create_did_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_record_did
    ON record(did)
  "

  let create_collection_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_record_collection
    ON record(collection)
  "

  let create_did_collection_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_record_did_collection
    ON record(did, collection)
  "

  let create_indexed_at_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_record_indexed_at
    ON record(indexed_at DESC)
  "

  use _ <- result.try(sqlight.exec(create_table_sql, conn))
  use _ <- result.try(sqlight.exec(create_did_index_sql, conn))
  use _ <- result.try(sqlight.exec(create_collection_index_sql, conn))
  use _ <- result.try(sqlight.exec(create_did_collection_index_sql, conn))
  use _ <- result.try(sqlight.exec(create_indexed_at_index_sql, conn))
  Ok(Nil)
}

/// Creates the actor table if it doesn't exist
pub fn create_actor_table(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let create_table_sql =
    "
    CREATE TABLE IF NOT EXISTS actor (
      did TEXT PRIMARY KEY NOT NULL,
      handle TEXT,
      indexed_at TEXT NOT NULL
    )
  "

  let create_handle_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_actor_handle
    ON actor(handle)
  "

  let create_indexed_at_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_actor_indexed_at
    ON actor(indexed_at DESC)
  "

  use _ <- result.try(sqlight.exec(create_table_sql, conn))
  use _ <- result.try(sqlight.exec(create_handle_index_sql, conn))
  use _ <- result.try(sqlight.exec(create_indexed_at_index_sql, conn))
  Ok(Nil)
}

/// Creates the lexicon table if it doesn't exist
pub fn create_lexicon_table(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let create_table_sql =
    "
    CREATE TABLE IF NOT EXISTS lexicon (
      id TEXT PRIMARY KEY NOT NULL,
      json TEXT NOT NULL,
      created_at TEXT NOT NULL DEFAULT (datetime('now'))
    )
  "

  let create_created_at_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_lexicon_created_at
    ON lexicon(created_at DESC)
  "

  use _ <- result.try(sqlight.exec(create_table_sql, conn))
  use _ <- result.try(sqlight.exec(create_created_at_index_sql, conn))
  Ok(Nil)
}
```

**Step 3: Build to verify**

Run: `gleam build --target erlang`
Expected: Compilation successful

**Step 4: Commit**

```bash
git add server/src/database/schema/
git commit -m "feat(database): add schema migration framework

Extract migration system and table DDL into dedicated modules

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

## Task 4: Create database connection module

**Files:**
- Create: `server/src/database/connection.gleam`
- Reference: `server/src/database.gleam:109-396` (connection & init code)

**Step 1: Create connection module**

Create `server/src/database/connection.gleam`:

```gleam
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
```

**Step 2: Build to verify**

Run: `gleam build --target erlang`
Expected: Compilation successful

**Step 3: Commit**

```bash
git add server/src/database/connection.gleam
git commit -m "feat(database): add connection management module

Extract database connection and initialization logic

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

## Task 5: Create config repository

**Files:**
- Create: `server/src/database/repositories/config.gleam`
- Reference: `server/src/database.gleam:398-554` (config functions)

**Step 1: Create config repository**

Create `server/src/database/repositories/config.gleam`:

```gleam
import gleam/dynamic/decode
import gleam/option.{type Option, None, Some}
import gleam/result
import sqlight

// ===== Config Functions =====

/// Get a config value by key
pub fn get(
  conn: sqlight.Connection,
  key: String,
) -> Result(String, sqlight.Error) {
  let sql =
    "
    SELECT value
    FROM config
    WHERE key = ?
  "

  let decoder = {
    use value <- decode.field(0, decode.string)
    decode.success(value)
  }

  case
    sqlight.query(sql, on: conn, with: [sqlight.text(key)], expecting: decoder)
  {
    Ok([value, ..]) -> Ok(value)
    Ok([]) ->
      Error(sqlight.SqlightError(
        sqlight.ConstraintForeignkey,
        "Config key not found",
        -1,
      ))
    Error(err) -> Error(err)
  }
}

/// Set or update a config value
pub fn set(
  conn: sqlight.Connection,
  key: String,
  value: String,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO config (key, value, updated_at)
    VALUES (?, ?, datetime('now'))
    ON CONFLICT(key) DO UPDATE SET
      value = excluded.value,
      updated_at = datetime('now')
  "

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(key), sqlight.text(value)],
    expecting: decode.string,
  ))
  Ok(Nil)
}

/// Delete a config value by key
pub fn delete(
  conn: sqlight.Connection,
  key: String,
) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM config WHERE key = ?"

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(key)],
    expecting: decode.string,
  ))
  Ok(Nil)
}

/// Deletes the domain_authority config entry
pub fn delete_domain_authority(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  delete(conn, "domain_authority")
}

/// Get OAuth client credentials from config table
/// Returns a tuple of (client_id, client_secret, redirect_uri) if all values exist
pub fn get_oauth_credentials(
  conn: sqlight.Connection,
) -> Result(Option(#(String, String, String)), sqlight.Error) {
  case get(conn, "oauth_client_id"), get(conn, "oauth_client_secret") {
    Ok(client_id), Ok(client_secret) -> {
      let redirect_uri = case get(conn, "oauth_redirect_uri") {
        Ok(uri) -> uri
        Error(_) -> ""
      }
      Ok(Some(#(client_id, client_secret, redirect_uri)))
    }
    Error(_), _ -> Ok(None)
    _, Error(_) -> Ok(None)
  }
}

/// Delete OAuth credentials from config table
pub fn delete_oauth_credentials(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  use _ <- result.try(delete(conn, "oauth_client_id"))
  use _ <- result.try(delete(conn, "oauth_client_secret"))
  use _ <- result.try(delete(conn, "oauth_redirect_uri"))
  Ok(Nil)
}
```

**Step 2: Build to verify**

Run: `gleam build --target erlang`
Expected: Compilation successful

**Step 3: Commit**

```bash
git add server/src/database/repositories/config.gleam
git commit -m "feat(database): add config repository

Extract config storage operations into repository module

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

## Task 6: Create Jetstream cursor module

**Files:**
- Create: `server/src/database/jetstream.gleam`
- Reference: `server/src/database.gleam:555-613` (cursor functions)

**Step 1: Create jetstream module**

Create `server/src/database/jetstream.gleam`:

```gleam
import gleam/dynamic/decode
import gleam/option.{type Option, None, Some}
import gleam/result
import sqlight

// ===== Jetstream Cursor Functions =====

/// Gets the current Jetstream cursor value
pub fn get_cursor(
  conn: sqlight.Connection,
) -> Result(Option(Int), sqlight.Error) {
  let sql =
    "
    SELECT cursor
    FROM jetstream_cursor
    WHERE id = 1
  "

  let decoder = {
    use cursor <- decode.field(0, decode.int)
    decode.success(cursor)
  }

  case sqlight.query(sql, on: conn, with: [], expecting: decoder) {
    Ok([cursor]) -> Ok(Some(cursor))
    Ok([]) -> Ok(None)
    Ok(_) -> Ok(None)
    Error(err) -> Error(err)
  }
}

/// Sets or updates the Jetstream cursor value
pub fn set_cursor(
  conn: sqlight.Connection,
  cursor: Int,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO jetstream_cursor (id, cursor, updated_at)
    VALUES (1, ?, datetime('now'))
    ON CONFLICT(id) DO UPDATE SET
      cursor = excluded.cursor,
      updated_at = datetime('now')
  "

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.int(cursor)],
    expecting: decode.string,
  ))
  Ok(Nil)
}

/// Clears the Jetstream cursor (for dev reset)
pub fn clear_cursor(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM jetstream_cursor WHERE id = 1"

  sqlight.exec(sql, conn)
  |> result.map(fn(_) { Nil })
}
```

**Step 2: Build to verify**

Run: `gleam build --target erlang`
Expected: Compilation successful

**Step 3: Commit**

```bash
git add server/src/database/jetstream.gleam
git commit -m "feat(database): add jetstream cursor module

Extract jetstream cursor operations into dedicated module

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

## Task 7: Create lexicons repository

**Files:**
- Create: `server/src/database/repositories/lexicons.gleam`
- Reference: `server/src/database.gleam:1181-1314` (lexicon functions)

**Implementation:** Extract all lexicon-related functions from database.gleam:
- `insert` (was `insert_lexicon`)
- `get` (was `get_lexicon`)
- `get_all` (was `get_all_lexicons`)
- `has` (was `has_lexicon`)
- `has_for_collection` (was `has_lexicon_for_collection`)
- `get_count` (was `get_lexicon_count`)
- `get_record_types` (was `get_record_type_lexicons`)
- `delete_all` (was `delete_all_lexicons`)

Use `database/types.Lexicon` type. Follow same pattern as config repository.

**Commit:** `feat(database): add lexicons repository`

---

## Task 8: Create actors repository

**Files:**
- Create: `server/src/database/repositories/actors.gleam`
- Reference: `server/src/database.gleam:1005-1115` (actor functions)

**Implementation:** Extract all actor-related functions from database.gleam:
- `upsert` (was `upsert_actor`)
- `get` (was `get_actor`)
- `get_by_handle` (was `get_actor_by_handle`)
- `get_count` (was `get_actor_count`)
- `delete_all` (was `delete_all_actors`)

Use `database/types.Actor` type. Follow same pattern as config repository.

**Commit:** `feat(database): add actors repository`

---

## Task 9: Create pagination query utilities

**Files:**
- Create: `server/src/database/queries/pagination.gleam`
- Reference: `server/src/database.gleam:1658-1736` (pagination helpers)

**Implementation:** Extract pagination utility functions:
- `record_to_record_like` - Convert Record to cursor.RecordLike
- `reverse_sort_direction` - Reverse ASC/DESC for backward pagination
- `reverse_sort_fields` - Reverse all sort fields
- `build_order_by` - Build ORDER BY clause with optional table prefix
- `is_table_column` - Check if field is table column vs JSON field

These are shared utilities used by multiple query functions. Keep as internal module helpers.

**Commit:** `feat(database): add pagination query utilities`

---

## Task 10: Create records repository (Part 1 - CRUD)

**Files:**
- Create: `server/src/database/repositories/records.gleam`
- Reference: `server/src/database.gleam:614-2242` (record functions)

**Implementation:** This is the largest module. Start with basic CRUD operations:
- `insert` (was `insert_record`)
- `batch_insert` (was `batch_insert_records`)
- `get` (was `get_record`)
- `get_by_did` (was `get_records_by_did`)
- `get_by_collection` (was `get_records_by_collection`)
- `update` (was `update_record`)
- `delete` (was `delete_record`)
- `delete_all` (was `delete_all_records`)
- Helper functions: `get_existing_cids`, `get_existing_cids_batch`

Use `database/types.Record` and `database/types.InsertResult`.

**Commit:** `feat(database): add records repository with CRUD operations`

---

## Task 11: Create records repository (Part 2 - Complex Queries)

**Files:**
- Modify: `server/src/database/repositories/records.gleam`
- Reference: `server/src/database.gleam:1320-2242` (paginated queries)

**Implementation:** Add complex query functions to records repository:
- `get_by_collection_paginated`
- `get_by_collection_paginated_with_where`
- `get_count_with_where`
- `get_by_uris` (for DataLoader/joins)
- `get_by_reference_field` (for reverse joins)
- `get_by_reference_field_paginated`
- `get_by_dids_and_collection`
- `get_by_dids_and_collection_paginated`

Import pagination utilities from `database/queries/pagination`.

**Commit:** `feat(database): add complex query operations to records repository`

---

## Task 12: Create records repository (Part 3 - Stats)

**Files:**
- Modify: `server/src/database/repositories/records.gleam`
- Reference: `server/src/database.gleam:1077-1135` (stats functions)

**Implementation:** Add statistics functions:
- `get_collection_stats`
- `get_count`

Use `database/types.CollectionStat`.

**Commit:** `feat(database): add statistics operations to records repository`

---

## Task 13: Create aggregates query module

**Files:**
- Create: `server/src/database/queries/aggregates.gleam`
- Reference: `server/src/database.gleam:2244-2417` (aggregation support)

**Implementation:** Extract aggregation query logic:
- `get_aggregated_records` - Main aggregation function
- Helper functions: `build_field_select`, `build_date_truncate_select`, `is_table_column_for_aggregate`

Use `database/types.DateInterval`, `database/types.GroupByField`.
Import `lexicon_graphql/aggregate_types.AggregateResult`.

**Commit:** `feat(database): add aggregation query module`

---

## Task 14: Create backward compatibility module

**Files:**
- Create: `server/src/database.gleam` (new, simplified version)

**Implementation:** Create a compatibility layer that re-exports all public functions with their original names. This allows existing code to continue working while we update imports incrementally.

```gleam
// Backward compatibility module
// TODO: Remove this once all imports are updated to use specific database modules

// Re-export types
pub type Record = database/types.Record
pub type Actor = database/types.Actor
pub type Lexicon = database/types.Lexicon
pub type CollectionStat = database/types.CollectionStat
pub type InsertResult = database/types.InsertResult
pub type DateInterval = database/types.DateInterval
pub type GroupByField = database/types.GroupByField

// Re-export connection functions
pub const connect = database/connection.connect
pub const initialize = database/connection.initialize

// Re-export config functions
pub const get_config = database/repositories/config.get
pub const set_config = database/repositories/config.set
pub const delete_domain_authority = database/repositories/config.delete_domain_authority
pub const get_oauth_credentials = database/repositories/config.get_oauth_credentials
pub const delete_oauth_credentials = database/repositories/config.delete_oauth_credentials

// Re-export jetstream functions
pub const get_jetstream_cursor = database/jetstream.get_cursor
pub const set_jetstream_cursor = database/jetstream.set_cursor
pub const clear_jetstream_cursor = database/jetstream.clear_cursor

// Re-export lexicon functions
pub const insert_lexicon = database/repositories/lexicons.insert
pub const get_lexicon = database/repositories/lexicons.get
pub const get_all_lexicons = database/repositories/lexicons.get_all
pub const has_lexicon = database/repositories/lexicons.has
pub const has_lexicon_for_collection = database/repositories/lexicons.has_for_collection
pub const get_lexicon_count = database/repositories/lexicons.get_count
pub const get_record_type_lexicons = database/repositories/lexicons.get_record_types
pub const delete_all_lexicons = database/repositories/lexicons.delete_all

// Re-export actor functions
pub const upsert_actor = database/repositories/actors.upsert
pub const get_actor = database/repositories/actors.get
pub const get_actor_by_handle = database/repositories/actors.get_by_handle
pub const get_actor_count = database/repositories/actors.get_count
pub const delete_all_actors = database/repositories/actors.delete_all

// Re-export record functions (many - see database/repositories/records.gleam for full list)
pub const insert_record = database/repositories/records.insert
pub const batch_insert_records = database/repositories/records.batch_insert
pub const get_record = database/repositories/records.get
// ... (continue for all record functions)

// Re-export aggregate functions
pub const get_aggregated_records = database/queries/aggregates.get_aggregated_records
```

**Commit:** `feat(database): add backward compatibility layer`

---

## Task 15: Test the refactored database layer

**Files:**
- Run: `gleam test`

**Step 1: Run all tests**

Run: `gleam test --target erlang`
Expected: All tests pass

**Step 2: Run build**

Run: `gleam build --target erlang`
Expected: Compilation successful with no errors

**Step 3: Manual smoke test (optional)**

Start the server and verify:
- Database initializes correctly
- Migrations run
- Basic operations work (if you have a test dataset)

---

## Task 16: Update imports across codebase (Incremental)

**Files to update (15 files):**
- `server/src/xrpc_handlers.gleam`
- `server/src/server.gleam`
- `server/src/mutation_resolvers.gleam`
- `server/src/graphql_ws_handler.gleam`
- `server/src/graphql_gleam.gleam`
- `server/src/backfill.gleam`
- `server/src/settings_handler.gleam`
- `server/src/client_schema.gleam`
- `server/src/jetstream_consumer.gleam`
- `server/src/importer.gleam`
- `server/src/event_handler.gleam`
- `server/src/config.gleam`
- `server/src/actor_validator.gleam`
- `server/src/oauth/registration.gleam`
- `server/src/xrpc_router.gleam`

**Strategy:** Update imports incrementally, one file at a time. For each file:

**Step 1:** Change import from `import database` to specific module imports:
```gleam
// Old
import database

// New
import database/types.{type Record, type Actor}
import database/repositories/records
import database/repositories/actors
```

**Step 2:** Update function calls to use module-qualified names:
```gleam
// Old
database.insert_record(conn, ...)

// New
records.insert(conn, ...)
```

**Step 3:** Test after each file:
```bash
gleam build --target erlang
```

**Step 4:** Commit after each file:
```bash
git add server/src/<filename>.gleam
git commit -m "refactor: update <filename> to use new database modules"
```

**Note:** Thanks to the backward compatibility layer in Task 14, the codebase will continue to work even if some files aren't updated yet. This allows incremental migration.

---

## Task 17: Remove backward compatibility layer

**Files:**
- Delete: `server/src/database.gleam` (compatibility layer created in Task 14)

**Step 1: Verify all imports updated**

Run: `grep -r "import database$" server/src/ --exclude-dir=build`
Expected: No results (all files now use specific imports)

**Step 2: Remove compatibility module**

```bash
rm server/src/database.gleam
```

**Step 3: Build to verify**

Run: `gleam build --target erlang`
Expected: Compilation successful

**Step 4: Run tests**

Run: `gleam test --target erlang`
Expected: All tests pass

**Step 5: Commit**

```bash
git add server/src/database.gleam
git commit -m "refactor: remove database backward compatibility layer

All imports have been migrated to use specific database modules

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

## Task 18: Final verification

**Step 1: Run full test suite**

Run: `gleam test --target erlang`
Expected: All tests pass

**Step 2: Build production target**

Run: `gleam build --target erlang`
Expected: Successful build with no warnings

**Step 3: Verify file structure**

Run: `tree server/src/database -L 2`
Expected output:
```
server/src/database
├── connection.gleam
├── jetstream.gleam
├── queries
│   ├── aggregates.gleam
│   └── pagination.gleam
├── repositories
│   ├── actors.gleam
│   ├── config.gleam
│   ├── lexicons.gleam
│   └── records.gleam
├── schema
│   ├── migrations.gleam
│   └── tables.gleam
└── types.gleam
```

**Step 4: Verify no old database.gleam exists**

Run: `ls server/src/database.gleam`
Expected: File not found

**Step 5: Create final commit**

```bash
git add .
git commit -m "docs: database refactoring complete

Successfully refactored 2417-line database.gleam into modular structure:
- database/types.gleam - Shared type definitions
- database/connection.gleam - Connection management
- database/schema/* - Migration framework and table DDL
- database/repositories/* - Domain repositories (records, actors, lexicons, config)
- database/queries/* - Query utilities (pagination, aggregates)
- database/jetstream.gleam - Jetstream cursor operations

All 15 dependent files updated to use new structure.
All tests passing.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

## Summary

This plan refactors the monolithic 2417-line `database.gleam` into a well-structured, modular database layer:

**Benefits:**
- **Separation of Concerns**: Each repository handles one domain aggregate
- **Maintainability**: Smaller, focused modules easier to understand and modify
- **Testability**: Can test repositories independently
- **Discoverability**: Clear module structure makes finding functions easier
- **Scalability**: Easy to add new repositories or queries

**Structure Created:**
- 1 types module (shared definitions)
- 1 connection module (DB initialization)
- 2 schema modules (migrations, table DDL)
- 4 repository modules (records, actors, lexicons, config)
- 2 query modules (pagination, aggregates)
- 1 jetstream module (cursor operations)

**Total:** 11 focused modules replacing 1 monolithic file

---

## Execution Options

**Plan complete and saved to `docs/plans/2025-11-23-database-refactoring.md`.**

**Two execution options:**

**1. Subagent-Driven (this session)** - I dispatch fresh subagent per task, review between tasks, fast iteration. Use @superpowers:subagent-driven-development skill.

**2. Parallel Session (separate)** - Open new session with @superpowers:executing-plans, batch execution with checkpoints. Better for long-running refactorings.

**Which approach?**
