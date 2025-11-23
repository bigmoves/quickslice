# Jetstream Activity Repository Migration Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Refactor jetstream_activity module into the modular database structure by moving types to database/types.gleam and operations to database/repositories/jetstream_activity.gleam

**Architecture:** Follow the established repository pattern where type definitions live in `database/types.gleam` and database operations are encapsulated in repository modules under `database/repositories/`. This migration will move `ActivityEntry` and `ActivityBucket` types to the types module, create a new repository module for all database operations, and update all import statements across the codebase.

**Tech Stack:** Gleam, SQLite (via sqlight), existing repository pattern

---

## Task 1: Move Type Definitions to database/types.gleam

**Files:**
- Modify: `server/src/database/types.gleam:51`
- Read: `server/src/jetstream_activity.gleam:8-19` and `server/src/jetstream_activity.gleam:163-170`

**Step 1: Add ActivityEntry type to database/types.gleam**

Add after the `GroupByField` type definition at line 51:

```gleam
/// A jetstream activity log entry
pub type ActivityEntry {
  ActivityEntry(
    id: Int,
    timestamp: String,
    operation: String,
    collection: String,
    did: String,
    status: String,
    error_message: Option(String),
    event_json: String,
  )
}

/// Activity bucket for aggregated data
pub type ActivityBucket {
  ActivityBucket(
    timestamp: String,
    create_count: Int,
    update_count: Int,
    delete_count: Int,
  )
}
```

**Step 2: Add import for Option type**

Ensure `option.{type Option}` is imported at the top of `database/types.gleam`. Check the existing imports and add if missing:

```gleam
import gleam/option.{type Option}
```

**Step 3: Verify the file compiles**

Run: `cd server && gleam build`
Expected: Build succeeds with no errors

**Step 4: Commit**

```bash
git add server/src/database/types.gleam
git commit -m "feat: add ActivityEntry and ActivityBucket types to database/types"
```

---

## Task 2: Create database/repositories/jetstream_activity.gleam

**Files:**
- Create: `server/src/database/repositories/jetstream_activity.gleam`
- Read: `server/src/jetstream_activity.gleam` (entire file for reference)

**Step 1: Create the repository file with imports and basic structure**

Create `server/src/database/repositories/jetstream_activity.gleam`:

```gleam
import database/types.{type ActivityBucket, type ActivityEntry, ActivityBucket, ActivityEntry}
import gleam/dynamic/decode
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/result
import logging
import sqlight

// ===== Jetstream Activity Functions =====
```

**Step 2: Add the log_activity function**

Add after the comment block:

```gleam
/// Logs a new JetStream activity entry
pub fn log_activity(
  conn: sqlight.Connection,
  timestamp: String,
  operation: String,
  collection: String,
  did: String,
  event_json: String,
) -> Result(Int, sqlight.Error) {
  let sql =
    "
    INSERT INTO jetstream_activity (timestamp, operation, collection, did, status, event_json)
    VALUES (?, ?, ?, ?, 'processing', ?)
    RETURNING id
  "

  let decoder = {
    use id <- decode.field(0, decode.int)
    decode.success(id)
  }

  use results <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [
      sqlight.text(timestamp),
      sqlight.text(operation),
      sqlight.text(collection),
      sqlight.text(did),
      sqlight.text(event_json),
    ],
    expecting: decoder,
  ))

  case results {
    [id] -> Ok(id)
    _ ->
      Error(sqlight.SqlightError(
        sqlight.ConstraintForeignkey,
        "Failed to insert activity",
        -1,
      ))
  }
}
```

**Step 3: Add the update_status function**

```gleam
/// Updates the status of an activity entry
pub fn update_status(
  conn: sqlight.Connection,
  id: Int,
  status: String,
  error_message: Option(String),
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    UPDATE jetstream_activity
    SET status = ?, error_message = ?
    WHERE id = ?
  "

  let error_value = case error_message {
    Some(msg) -> sqlight.text(msg)
    None -> sqlight.null()
  }

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(status), error_value, sqlight.int(id)],
    expecting: decode.string,
  ))
  Ok(Nil)
}
```

**Step 4: Add the get_recent_activity function**

```gleam
/// Gets recent activity entries for the last N hours
pub fn get_recent_activity(
  conn: sqlight.Connection,
  hours: Int,
) -> Result(List(ActivityEntry), sqlight.Error) {
  let sql = "
    SELECT id, timestamp, operation, collection, did, status, error_message, event_json
    FROM jetstream_activity
    WHERE datetime(timestamp) >= datetime('now', '-" <> int.to_string(hours) <> " hours')
    ORDER BY timestamp DESC
    LIMIT 1000
  "

  let decoder = {
    use id <- decode.field(0, decode.int)
    use timestamp <- decode.field(1, decode.string)
    use operation <- decode.field(2, decode.string)
    use collection <- decode.field(3, decode.string)
    use did <- decode.field(4, decode.string)
    use status <- decode.field(5, decode.string)
    use error_message <- decode.field(6, decode.optional(decode.string))
    use event_json <- decode.field(7, decode.string)
    decode.success(ActivityEntry(
      id:,
      timestamp:,
      operation:,
      collection:,
      did:,
      status:,
      error_message:,
      event_json:,
    ))
  }

  sqlight.query(sql, on: conn, with: [], expecting: decoder)
}
```

**Step 5: Add the cleanup_old_activity function**

```gleam
/// Deletes activity entries older than N hours
pub fn cleanup_old_activity(
  conn: sqlight.Connection,
  hours: Int,
) -> Result(Nil, sqlight.Error) {
  let sql = "
    DELETE FROM jetstream_activity
    WHERE datetime(timestamp) < datetime('now', '-" <> int.to_string(hours) <> " hours')
  "

  use _ <- result.try(sqlight.exec(sql, conn))
  logging.log(
    logging.Info,
    "Cleaned up jetstream_activity entries older than "
      <> int.to_string(hours)
      <> " hours",
  )
  Ok(Nil)
}
```

**Step 6: Add the extract_display_info helper function**

```gleam
/// Extracts display information from event JSON
/// Note: This is a simplified version that doesn't parse JSON yet
/// Full JSON parsing would need proper decoders
pub fn extract_display_info(
  _event_json_str: String,
) -> #(Option(String), Option(String), Option(String)) {
  // TODO: Implement proper JSON parsing with gleam_json or similar
  // For now, return None for all fields - the JSON is stored and can be viewed raw
  #(None, None, None)
}
```

**Step 7: Add the delete_all function**

```gleam
/// Deletes all jetstream activity records from the database
pub fn delete_all(conn: sqlight.Connection) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM jetstream_activity"
  sqlight.exec(sql, conn)
  |> result.map(fn(_) { Nil })
}
```

**Step 8: Verify the file compiles**

Run: `cd server && gleam build`
Expected: Build succeeds with no errors

**Step 9: Commit**

```bash
git add server/src/database/repositories/jetstream_activity.gleam
git commit -m "feat: create jetstream_activity repository with core CRUD functions"
```

---

## Task 3: Add Aggregation Functions to Repository

**Files:**
- Modify: `server/src/database/repositories/jetstream_activity.gleam` (append to end)
- Read: `server/src/jetstream_activity.gleam:173-290` (for reference)

**Step 1: Add the get_activity_bucketed helper function**

Add at the end of the repository file:

```gleam
/// Helper function to get activity bucketed by a specific interval
fn get_activity_bucketed(
  conn: sqlight.Connection,
  hours: Int,
  interval: String,
  expected_buckets: Int,
) -> Result(List(ActivityBucket), sqlight.Error) {
  let hours_str = int.to_string(hours)
  let max_n = int.to_string(expected_buckets)

  // Build the SQL dynamically based on interval
  let sql = "
    WITH RECURSIVE time_series(bucket, n) AS (
      SELECT datetime('now', '-" <> hours_str <> " hours'), 0
      UNION ALL
      SELECT datetime(bucket, '+" <> interval <> "'), n + 1
      FROM time_series
      WHERE n < " <> max_n <> "
    )
    SELECT
      strftime('%Y-%m-%dT%H:%M:00Z', ts.bucket) as timestamp,
      COALESCE(SUM(CASE WHEN a.operation = 'create' THEN 1 ELSE 0 END), 0) as create_count,
      COALESCE(SUM(CASE WHEN a.operation = 'update' THEN 1 ELSE 0 END), 0) as update_count,
      COALESCE(SUM(CASE WHEN a.operation = 'delete' THEN 1 ELSE 0 END), 0) as delete_count
    FROM time_series ts
    LEFT JOIN jetstream_activity a ON
      datetime(a.timestamp) >= ts.bucket
      AND datetime(a.timestamp) < datetime(ts.bucket, '+" <> interval <> "')
      AND a.status = 'success'
    GROUP BY ts.bucket
    ORDER BY ts.bucket
  "

  let decoder = {
    use timestamp <- decode.field(0, decode.string)
    use create_count <- decode.field(1, decode.int)
    use update_count <- decode.field(2, decode.int)
    use delete_count <- decode.field(3, decode.int)
    decode.success(ActivityBucket(
      timestamp:,
      create_count:,
      update_count:,
      delete_count:,
    ))
  }

  sqlight.query(sql, on: conn, with: [], expecting: decoder)
}
```

**Step 2: Add the public aggregation functions**

Add before the `get_activity_bucketed` helper function:

```gleam
/// Get activity aggregated into 5-minute buckets for the last hour
pub fn get_activity_1hr(
  conn: sqlight.Connection,
) -> Result(List(ActivityBucket), sqlight.Error) {
  get_activity_bucketed(conn, 1, "5 minutes", 12)
}

/// Get activity aggregated into 15-minute buckets for the last 3 hours
pub fn get_activity_3hr(
  conn: sqlight.Connection,
) -> Result(List(ActivityBucket), sqlight.Error) {
  get_activity_bucketed(conn, 3, "15 minutes", 12)
}

/// Get activity aggregated into 30-minute buckets for the last 6 hours
pub fn get_activity_6hr(
  conn: sqlight.Connection,
) -> Result(List(ActivityBucket), sqlight.Error) {
  get_activity_bucketed(conn, 6, "30 minutes", 12)
}

/// Get activity aggregated into hourly buckets for the last day
pub fn get_activity_1day(
  conn: sqlight.Connection,
) -> Result(List(ActivityBucket), sqlight.Error) {
  get_activity_bucketed(conn, 24, "1 hour", 24)
}
```

**Step 3: Add the get_activity_7day function**

Add after the other aggregation functions:

```gleam
/// Get activity aggregated into daily buckets for the last 7 days
pub fn get_activity_7day(
  conn: sqlight.Connection,
) -> Result(List(ActivityBucket), sqlight.Error) {
  // For 7 days, we need to bucket by day (6 days ago through today = 7 days)
  let sql =
    "
    WITH RECURSIVE time_series(bucket, n) AS (
      SELECT datetime('now', 'start of day', '-6 days'), 0
      UNION ALL
      SELECT datetime(bucket, '+1 day'), n + 1
      FROM time_series
      WHERE n <= 5
    )
    SELECT
      strftime('%Y-%m-%dT00:00:00Z', ts.bucket) as timestamp,
      COALESCE(SUM(CASE WHEN a.operation = 'create' THEN 1 ELSE 0 END), 0) as create_count,
      COALESCE(SUM(CASE WHEN a.operation = 'update' THEN 1 ELSE 0 END), 0) as update_count,
      COALESCE(SUM(CASE WHEN a.operation = 'delete' THEN 1 ELSE 0 END), 0) as delete_count
    FROM time_series ts
    LEFT JOIN jetstream_activity a ON
      date(a.timestamp) = date(ts.bucket)
      AND a.status = 'success'
    GROUP BY ts.bucket
    ORDER BY ts.bucket
  "

  let decoder = {
    use timestamp <- decode.field(0, decode.string)
    use create_count <- decode.field(1, decode.int)
    use update_count <- decode.field(2, decode.int)
    use delete_count <- decode.field(3, decode.int)
    decode.success(ActivityBucket(
      timestamp:,
      create_count:,
      update_count:,
      delete_count:,
    ))
  }

  sqlight.query(sql, on: conn, with: [], expecting: decoder)
}
```

**Step 4: Verify the file compiles**

Run: `cd server && gleam build`
Expected: Build succeeds with no errors

**Step 5: Commit**

```bash
git add server/src/database/repositories/jetstream_activity.gleam
git commit -m "feat: add activity aggregation functions to jetstream_activity repository"
```

---

## Task 4: Update event_handler.gleam to Use New Repository

**Files:**
- Modify: `server/src/event_handler.gleam:15` (import statement)
- Modify: `server/src/event_handler.gleam` (all jetstream_activity function calls)

**Step 1: Update the import statement**

Replace line 15:

```gleam
import jetstream_activity
```

With:

```gleam
import database/repositories/jetstream_activity
```

**Step 2: Verify the file compiles**

Run: `cd server && gleam build`
Expected: Build succeeds - all function calls remain the same, only the import path changed

**Step 3: Commit**

```bash
git add server/src/event_handler.gleam
git commit -m "refactor: update event_handler to use jetstream_activity repository"
```

---

## Task 5: Update activity_cleanup.gleam to Use New Repository

**Files:**
- Modify: `server/src/activity_cleanup.gleam:4` (import statement)

**Step 1: Update the import statement**

Replace line 4:

```gleam
import jetstream_activity
```

With:

```gleam
import database/repositories/jetstream_activity
```

**Step 2: Verify the file compiles**

Run: `cd server && gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add server/src/activity_cleanup.gleam
git commit -m "refactor: update activity_cleanup to use jetstream_activity repository"
```

---

## Task 6: Update client_schema.gleam to Use New Repository and Types

**Files:**
- Modify: `server/src/client_schema.gleam:15` (import statement)
- Modify: `server/src/client_schema.gleam:488` (type reference)
- Modify: `server/src/client_schema.gleam:501` (type reference)

**Step 1: Update the import statements**

Replace line 15:

```gleam
import jetstream_activity
```

With two separate imports (maintain alphabetical order with existing imports):

```gleam
import database/repositories/jetstream_activity
import database/types.{type ActivityBucket, type ActivityEntry}
```

**Step 2: Update the ActivityBucket type reference on line 488**

Replace:

```gleam
bucket: jetstream_activity.ActivityBucket,
```

With:

```gleam
bucket: ActivityBucket,
```

**Step 3: Update the ActivityEntry type reference on line 501**

Replace:

```gleam
entry: jetstream_activity.ActivityEntry,
```

With:

```gleam
entry: ActivityEntry,
```

**Step 4: Verify the file compiles**

Run: `cd server && gleam build`
Expected: Build succeeds

**Step 5: Commit**

```bash
git add server/src/client_schema.gleam
git commit -m "refactor: update client_schema to use jetstream_activity repository and types"
```

---

## Task 7: Verify No Other Files Reference the Old Module

**Files:**
- Search across codebase

**Step 1: Search for remaining references**

Run: `cd server && grep -r "import jetstream_activity" src/`
Expected: No matches (all imports updated)

**Step 2: Search for qualified type references**

Run: `cd server && grep -r "jetstream_activity\\.Activity" src/`
Expected: No matches (all type references updated)

**Step 3: Full build verification**

Run: `cd server && gleam build`
Expected: Clean build with no errors or warnings

---

## Task 8: Remove the Old jetstream_activity.gleam File

**Files:**
- Delete: `server/src/jetstream_activity.gleam`

**Step 1: Delete the old module file**

Run: `rm server/src/jetstream_activity.gleam`

**Step 2: Verify the build still succeeds**

Run: `cd server && gleam build`
Expected: Clean build - proves all references have been migrated

**Step 3: Run any existing tests**

Run: `cd server && gleam test`
Expected: All tests pass (if tests exist)

**Step 4: Commit**

```bash
git add server/src/jetstream_activity.gleam
git commit -m "refactor: remove old jetstream_activity module after migration"
```

---

## Task 9: Final Verification and Documentation

**Files:**
- Verify: All repository files follow consistent pattern
- Optional: Update any documentation

**Step 1: Verify repository structure**

Run: `ls -la server/src/database/repositories/`
Expected: Should see actors.gleam, config.gleam, jetstream_activity.gleam, lexicons.gleam, records.gleam

**Step 2: Verify types structure**

Run: `grep -E "^pub type" server/src/database/types.gleam`
Expected: Should see Record, Actor, Lexicon, CollectionStat, InsertResult, DateInterval, GroupByField, ActivityEntry, ActivityBucket

**Step 3: Final full build**

Run: `cd server && gleam clean && gleam build`
Expected: Clean build from scratch succeeds

**Step 4: Check git status**

Run: `git status`
Expected: Working directory clean (all changes committed)

**Step 5: Review commit history**

Run: `git log --oneline -9`
Expected: Should see all 8 commits from this refactoring

---

## Summary

This plan migrates the `jetstream_activity` module into the established repository pattern:

1. **Types centralized** - `ActivityEntry` and `ActivityBucket` moved to `database/types.gleam`
2. **Operations encapsulated** - All database functions moved to `database/repositories/jetstream_activity.gleam`
3. **Imports updated** - All consuming modules updated to use new paths
4. **Old module removed** - Original file deleted after migration complete
5. **Pattern consistency** - Repository now matches the structure of actors, config, lexicons, and records repositories

The migration is broken into small, testable steps with verification at each stage to ensure correctness.
