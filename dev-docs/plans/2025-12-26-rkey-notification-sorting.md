# Rkey-Based Notification Sorting Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Sort notifications by TID (timestamp identifier) extracted from record rkey for chronological ordering across collections.

**Architecture:** Add a generated `rkey` column to the record table that extracts the last path segment from the URI. Use this for sorting notifications instead of `indexed_at`, since TIDs are lexicographically sortable and encode creation timestamps.

**Tech Stack:** Gleam, SQLite, GraphQL (swell)

---

### Task 1: Add Migration for rkey Generated Column

**Files:**
- Create: `server/db/migrations/20241227000001_add_rkey_column.sql`

**Step 1: Create the migration file**

```sql
-- migrate:up

-- Add rkey as a generated column extracted from uri
-- URI format: at://did/collection/rkey
-- We extract everything after the last '/'
ALTER TABLE record ADD COLUMN rkey TEXT
  GENERATED ALWAYS AS (
    substr(uri, instr(substr(uri, instr(substr(uri, 6), '/') + 6), '/') + instr(substr(uri, 6), '/') + 6)
  ) STORED;

-- Index for efficient sorting by rkey (TID-based chronological order)
CREATE INDEX IF NOT EXISTS idx_record_rkey ON record(rkey DESC);

-- migrate:down

DROP INDEX IF EXISTS idx_record_rkey;
-- Note: SQLite doesn't support DROP COLUMN, would need table rebuild
```

**Step 2: Test the SQL extraction logic manually**

Run in SQLite to verify the extraction works:
```sql
SELECT
  uri,
  substr(uri, instr(substr(uri, instr(substr(uri, 6), '/') + 6), '/') + instr(substr(uri, 6), '/') + 6) as rkey
FROM (
  SELECT 'at://did:plc:abc123/social.grain.photo/3maungnepas2t' as uri
);
```

Expected: `3maungnepas2t`

**Step 3: Commit**

```bash
git add server/db/migrations/20241227000001_add_rkey_column.sql
git commit -m "feat(db): add rkey generated column for TID-based sorting"
```

---

### Task 2: Update Record Type to Include rkey

**Files:**
- Modify: `server/src/database/types.gleam`

**Step 1: Add rkey field to Record type**

In `server/src/database/types.gleam`, update the Record type:

```gleam
pub type Record {
  Record(
    uri: String,
    cid: String,
    did: String,
    collection: String,
    json: String,
    indexed_at: String,
    rkey: String,
  )
}
```

**Step 2: Run build to find all places that need updating**

```bash
cd /Users/chadmiller/code/quickslice/server && gleam build 2>&1
```

Expected: Compile errors showing all places constructing Record

**Step 3: Commit**

```bash
git add server/src/database/types.gleam
git commit -m "feat(types): add rkey field to Record type"
```

---

### Task 3: Update Record Decoder and Columns

**Files:**
- Modify: `server/src/database/repositories/records.gleam`

**Step 1: Update record_columns function**

Find `record_columns` and add `rkey`:

```gleam
fn record_columns(exec: Executor) -> String {
  "uri, cid, did, collection, json, indexed_at, rkey"
}
```

**Step 2: Update record_decoder function**

Find `record_decoder` and add rkey decoding:

```gleam
fn record_decoder() -> decode.Decoder(Record) {
  use uri <- decode.field(0, decode.string)
  use cid <- decode.field(1, decode.string)
  use did <- decode.field(2, decode.string)
  use collection <- decode.field(3, decode.string)
  use json <- decode.field(4, decode.string)
  use indexed_at <- decode.field(5, decode.string)
  use rkey <- decode.field(6, decode.string)
  decode.success(Record(uri:, cid:, did:, collection:, json:, indexed_at:, rkey:))
}
```

**Step 3: Fix any remaining Record constructor calls**

Search for `Record(` and update any direct constructions to include rkey. For test fixtures, extract rkey from uri:

```gleam
// Helper to extract rkey from uri for tests
fn extract_rkey(uri: String) -> String {
  uri
  |> string.split("/")
  |> list.last
  |> result.unwrap("")
}
```

**Step 4: Run tests to verify**

```bash
cd /Users/chadmiller/code/quickslice/server && gleam test 2>&1 | tail -20
```

**Step 5: Commit**

```bash
git add server/src/database/repositories/records.gleam
git commit -m "feat(records): update decoder to include rkey column"
```

---

### Task 4: Update get_notifications to Sort by rkey

**Files:**
- Modify: `server/src/database/repositories/records.gleam`

**Step 1: Update ORDER BY clause**

In `get_notifications` function (around line 1143), change:

```gleam
// Before:
<> " ORDER BY indexed_at DESC, uri DESC LIMIT "

// After:
<> " ORDER BY rkey DESC, uri DESC LIMIT "
```

**Step 2: Update cursor WHERE clause**

In the cursor clause building (around line 1118), change:

```gleam
// Before:
#(" AND (indexed_at, uri) < (" <> p1 <> ", " <> p2 <> ")", [
  Text(indexed_at_value),
  Text(cid_value),
])

// After:
#(" AND (rkey, uri) < (" <> p1 <> ", " <> p2 <> ")", [
  Text(rkey_value),
  Text(uri_value),
])
```

Note: The cursor now uses `(rkey, uri)` tuple instead of `(indexed_at, cid)`.

**Step 3: Update cursor decoding**

Update the cursor value extraction:

```gleam
// The cursor format is now: rkey|uri (encoded as base64)
let rkey_value =
  decoded.field_values |> list.first |> result.unwrap("")
let uri_value = decoded.cid  // cid field now holds uri for this cursor
```

**Step 4: Run tests**

```bash
cd /Users/chadmiller/code/quickslice/server && gleam test 2>&1 | tail -20
```

**Step 5: Commit**

```bash
git add server/src/database/repositories/records.gleam
git commit -m "feat(notifications): sort by rkey for TID-based chronological order"
```

---

### Task 5: Update Notification Cursor Generation

**Files:**
- Modify: `server/src/database/repositories/records.gleam`

**Step 1: Update end_cursor generation**

In `get_notifications` (around line 1156-1162), update cursor generation:

```gleam
let end_cursor = case list.last(trimmed) {
  Ok(record) -> {
    // Encode cursor as rkey|uri for notification pagination
    let cursor_content = record.rkey <> "|" <> record.uri
    Some(pagination.encode_base64(cursor_content))
  }
  Error(_) -> None
}
```

**Step 2: Run tests**

```bash
cd /Users/chadmiller/code/quickslice/server && gleam test 2>&1 | tail -20
```

**Step 3: Commit**

```bash
git add server/src/database/repositories/records.gleam
git commit -m "feat(notifications): update cursor to use rkey|uri format"
```

---

### Task 6: Add rkey to Pagination Field Extraction

**Files:**
- Modify: `server/src/database/queries/pagination.gleam`

**Step 1: Add rkey to extract_field_value**

Update the `extract_field_value` function:

```gleam
pub fn extract_field_value(record: Record, field: String) -> String {
  case field {
    "uri" -> record.uri
    "cid" -> record.cid
    "did" -> record.did
    "collection" -> record.collection
    "indexed_at" -> record.indexed_at
    "rkey" -> record.rkey
    _ -> extract_json_field(record.json, field)
  }
}
```

**Step 2: Add rkey to build_cursor_field_reference**

```gleam
fn build_cursor_field_reference(exec: Executor, field: String) -> String {
  case field {
    "uri" | "cid" | "did" | "collection" | "indexed_at" | "rkey" -> field
    _ -> executor.json_extract(exec, "json", field)
  }
}
```

**Step 3: Add rkey to build_order_by**

```gleam
// In build_order_by, update the field_ref case:
let field_ref = case field_name {
  "uri" | "cid" | "did" | "collection" | "indexed_at" | "rkey" ->
    table_prefix <> field_name
  // ... rest unchanged
}
```

**Step 4: Run tests**

```bash
cd /Users/chadmiller/code/quickslice/server && gleam test 2>&1 | tail -20
```

**Step 5: Commit**

```bash
git add server/src/database/queries/pagination.gleam
git commit -m "feat(pagination): add rkey field support"
```

---

### Task 7: Update Notification E2E Tests

**Files:**
- Modify: `server/test/graphql/notifications_e2e_test.gleam`

**Step 1: Update test fixtures to use TID-like rkeys**

Ensure test records use TID-format rkeys so sorting is predictable:

```gleam
// Use TIDs that sort in a known order
// Earlier TID (should appear second when sorted DESC)
let earlier_rkey = "3kaungnepas2t"
// Later TID (should appear first when sorted DESC)
let later_rkey = "3maungnepas2t"
```

**Step 2: Update assertions to verify rkey-based ordering**

```gleam
// Records should be ordered by rkey DESC
// So later_rkey record should come before earlier_rkey record
```

**Step 3: Run notification tests**

```bash
cd /Users/chadmiller/code/quickslice/server && gleam test 2>&1 | grep -A5 "notification"
```

**Step 4: Commit**

```bash
git add server/test/graphql/notifications_e2e_test.gleam
git commit -m "test(notifications): update e2e tests for rkey-based sorting"
```

---

### Task 8: Run Full Test Suite and Verify

**Step 1: Run all tests**

```bash
cd /Users/chadmiller/code/quickslice/server && gleam test 2>&1
```

Expected: All tests pass

**Step 2: Verify migration works on fresh database**

```bash
cd /Users/chadmiller/code/quickslice/server && rm -f quickslice.db && gleam run -- migrate
```

**Step 3: Final commit if any fixes needed**

```bash
git add -A
git commit -m "fix: address test failures from rkey sorting migration"
```

---

## Summary

After completing these tasks:

1. **Records table** has a generated `rkey` column that extracts the last URI segment
2. **Notifications** are sorted by `rkey DESC` instead of `indexed_at DESC`
3. **Cursor pagination** uses `(rkey, uri)` tuple for stable pagination
4. **TID-based records** (majority) sort chronologically by creation time
5. **Non-TID rkeys** sort lexicographically (deterministic, just not chronological)

This provides chronological notification ordering across collections without relying on arbitrary `indexed_at` timestamps.
