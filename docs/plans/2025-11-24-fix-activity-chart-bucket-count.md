# Fix Activity Chart Bucket Count Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix the off-by-one error causing the rightmost bar in the activity chart to always be empty for 1hr, 3hr, 6hr, and 1 day time ranges.

**Architecture:** The `get_activity_bucketed()` function generates time buckets via a recursive SQL CTE. The condition `WHERE n < expected_buckets` creates one extra bucket extending into the future, which always has zero data.

**Tech Stack:** Gleam, SQLite, gleeunit

---

### Task 1: Write Failing Test for Bucket Count

**Files:**
- Create: `server/test/jetstream_activity_bucket_test.gleam`

**Step 1: Create the test file with bucket count test**

```gleam
import database/repositories/jetstream_activity
import database/schema/tables
import gleam/list
import gleeunit
import gleeunit/should
import sqlight

pub fn main() {
  gleeunit.main()
}

fn setup_test_db() -> sqlight.Connection {
  let assert Ok(conn) = sqlight.open(":memory:")
  let assert Ok(_) = tables.create_jetstream_activity_table(conn)
  conn
}

pub fn test_1hr_returns_exactly_12_buckets() {
  let conn = setup_test_db()

  let assert Ok(buckets) = jetstream_activity.get_activity_1hr(conn)

  list.length(buckets)
  |> should.equal(12)
}

pub fn test_3hr_returns_exactly_12_buckets() {
  let conn = setup_test_db()

  let assert Ok(buckets) = jetstream_activity.get_activity_3hr(conn)

  list.length(buckets)
  |> should.equal(12)
}

pub fn test_6hr_returns_exactly_12_buckets() {
  let conn = setup_test_db()

  let assert Ok(buckets) = jetstream_activity.get_activity_6hr(conn)

  list.length(buckets)
  |> should.equal(12)
}

pub fn test_1day_returns_exactly_24_buckets() {
  let conn = setup_test_db()

  let assert Ok(buckets) = jetstream_activity.get_activity_1day(conn)

  list.length(buckets)
  |> should.equal(24)
}

pub fn test_7day_returns_exactly_7_buckets() {
  let conn = setup_test_db()

  let assert Ok(buckets) = jetstream_activity.get_activity_7day(conn)

  list.length(buckets)
  |> should.equal(7)
}
```

**Step 2: Run test to verify it fails**

Run from `/Users/chadmiller/code/quickslice/server`:
```bash
gleam test -- --only jetstream_activity_bucket_test
```

Expected: Tests for 1hr, 3hr, 6hr, 1day should FAIL with `left: 13, right: 12` (or similar for 1day: `left: 25, right: 24`). The 7day test may also fail with `left: 6, right: 7`.

---

### Task 2: Fix the Bucket Count in get_activity_bucketed

**Files:**
- Modify: `server/src/database/repositories/jetstream_activity.gleam:238-249`

**Step 1: Fix the WHERE clause**

Change line 249 from:
```gleam
      WHERE n < " <> max_n <> "
```

To:
```gleam
      WHERE n < " <> int.to_string(expected_buckets - 1) <> "
```

The full function after the fix (lines 232-279):
```gleam
/// Helper function to get activity bucketed by a specific interval
fn get_activity_bucketed(
  conn: sqlight.Connection,
  hours: Int,
  interval: String,
  expected_buckets: Int,
) -> Result(List(ActivityBucket), sqlight.Error) {
  let hours_str = int.to_string(hours)

  // Build the SQL dynamically based on interval
  let sql = "
    WITH RECURSIVE time_series(bucket, n) AS (
      SELECT datetime('now', '-" <> hours_str <> " hours'), 0
      UNION ALL
      SELECT datetime(bucket, '+" <> interval <> "'), n + 1
      FROM time_series
      WHERE n < " <> int.to_string(expected_buckets - 1) <> "
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

Note: We also need to remove the now-unused `max_n` variable from line 240.

**Step 2: Run tests to verify they pass**

Run from `/Users/chadmiller/code/quickslice/server`:
```bash
gleam test -- --only jetstream_activity_bucket_test
```

Expected: Tests for 1hr, 3hr, 6hr, 1day should PASS.

---

### Task 3: Fix the 7-Day Bucket Count

**Files:**
- Modify: `server/src/database/repositories/jetstream_activity.gleam:190-214`

**Step 1: Fix the 7-day WHERE clause**

The 7-day function has its own SQL. Change line 201 from:
```gleam
      WHERE n <= 5
```

To:
```gleam
      WHERE n < 7
```

This changes the CTE from generating 6 buckets (n=0 through n=5) to 7 buckets (n=0 through n=6).

**Step 2: Run all bucket tests**

Run from `/Users/chadmiller/code/quickslice/server`:
```bash
gleam test -- --only jetstream_activity_bucket_test
```

Expected: All 5 tests PASS.

---

### Task 4: Run Full Test Suite

**Step 1: Run all server tests**

Run from `/Users/chadmiller/code/quickslice/server`:
```bash
gleam test
```

Expected: All 179 tests PASS.

---

### Task 5: Manual Verification

**Step 1: Start the server and verify in browser**

Run from `/Users/chadmiller/code/quickslice`:
```bash
make run
```

**Step 2: Check the activity chart**

Open the client UI and:
1. Click the "1hr" tab - rightmost bar should now show data (if there's recent activity)
2. Click the "3hr" tab - verify data appears correctly
3. Click the "6hr" tab - verify data appears correctly
4. Click the "1 day" tab - verify data appears correctly
5. Click the "7 day" tab - verify 7 bars are shown

---

### Task 6: Commit

**Step 1: Stage and commit**

```bash
git add server/test/jetstream_activity_bucket_test.gleam server/src/database/repositories/jetstream_activity.gleam
git commit -m "fix: correct activity chart bucket count off-by-one error

The recursive CTE was generating one extra bucket extending into the
future, causing the rightmost bar to always be empty. Fixed by adjusting
the WHERE clause to generate exactly the expected number of buckets.

Also fixed 7-day view which was generating 6 buckets instead of 7."
```
