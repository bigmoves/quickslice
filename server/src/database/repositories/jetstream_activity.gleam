import database/executor.{
  type DbError, type Executor, ConstraintError, Int as DbInt, Null, Text,
  Timestamptz,
}
import database/types.{
  type ActivityBucket, type ActivityEntry, ActivityBucket, ActivityEntry,
}
import gleam/dynamic/decode
import gleam/int
import gleam/option.{type Option, None, Some}
import logging

// ===== Jetstream Activity Functions =====

/// Logs a new JetStream activity entry
pub fn log_activity(
  exec: Executor,
  timestamp: String,
  operation: String,
  collection: String,
  did: String,
  event_json: String,
) -> Result(Int, DbError) {
  let p1 = executor.placeholder(exec, 1)
  let p2 = executor.placeholder(exec, 2)
  let p3 = executor.placeholder(exec, 3)
  let p4 = executor.placeholder(exec, 4)
  let p5 = executor.placeholder(exec, 5)

  // SQLite: both are TEXT
  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "INSERT INTO jetstream_activity (timestamp, operation, collection, did, status, event_json)
       VALUES ("
      <> p1
      <> ", "
      <> p2
      <> ", "
      <> p3
      <> ", "
      <> p4
      <> ", 'processing', "
      <> p5
      <> ")
       RETURNING id"
    executor.PostgreSQL ->
      "INSERT INTO jetstream_activity (timestamp, operation, collection, did, status, event_json)
       VALUES ("
      <> p1
      <> ", "
      <> p2
      <> ", "
      <> p3
      <> ", "
      <> p4
      <> ", 'processing', "
      <> p5
      <> "::jsonb)
       RETURNING id"
  }

  let decoder = {
    use id <- decode.field(0, decode.int)
    decode.success(id)
  }

  case
    executor.query(
      exec,
      sql,
      [
        Timestamptz(timestamp),
        Text(operation),
        Text(collection),
        Text(did),
        Text(event_json),
      ],
      decoder,
    )
  {
    Ok([id]) -> Ok(id)
    Ok(_) -> Error(ConstraintError("Failed to insert activity"))
    Error(err) -> Error(err)
  }
}

/// Updates the status of an activity entry
pub fn update_status(
  exec: Executor,
  id: Int,
  status: String,
  error_message: Option(String),
) -> Result(Nil, DbError) {
  let p1 = executor.placeholder(exec, 1)
  let p2 = executor.placeholder(exec, 2)
  let p3 = executor.placeholder(exec, 3)

  let sql = "UPDATE jetstream_activity
     SET status = " <> p1 <> ", error_message = " <> p2 <> "
     WHERE id = " <> p3

  let error_value = case error_message {
    Some(msg) -> Text(msg)
    None -> Null
  }

  executor.exec(exec, sql, [Text(status), error_value, DbInt(id)])
}

/// Gets recent activity entries for the last N hours
pub fn get_recent_activity(
  exec: Executor,
  hours: Int,
) -> Result(List(ActivityEntry), DbError) {
  let hours_str = int.to_string(hours)

  // Use dialect-specific datetime comparison
  // SQLite: both are TEXT
  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "SELECT id, timestamp, operation, collection, did, status, error_message, event_json
       FROM jetstream_activity
       WHERE datetime(timestamp) >= datetime('now', '-"
      <> hours_str
      <> " hours')
       ORDER BY timestamp DESC
       LIMIT 1000"
    executor.PostgreSQL ->
      "SELECT id, timestamp::text, operation, collection, did, status, error_message, event_json::text
       FROM jetstream_activity
       WHERE timestamp >= NOW() - INTERVAL '"
      <> hours_str
      <> " hours'
       ORDER BY timestamp DESC
       LIMIT 1000"
  }

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

  executor.query(exec, sql, [], decoder)
}

/// Deletes activity entries older than N hours
pub fn cleanup_old_activity(exec: Executor, hours: Int) -> Result(Nil, DbError) {
  let hours_str = int.to_string(hours)

  // Use dialect-specific datetime comparison
  let sql = case executor.dialect(exec) {
    executor.SQLite -> "DELETE FROM jetstream_activity
       WHERE datetime(timestamp) < datetime('now', '-" <> hours_str <> " hours')"
    executor.PostgreSQL -> "DELETE FROM jetstream_activity
       WHERE timestamp::timestamp < NOW() - INTERVAL '" <> hours_str <> " hours'"
  }

  case executor.exec(exec, sql, []) {
    Ok(_) -> {
      logging.log(
        logging.Info,
        "Cleaned up jetstream_activity entries older than "
          <> hours_str
          <> " hours",
      )
      Ok(Nil)
    }
    Error(err) -> Error(err)
  }
}

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

/// Deletes all jetstream activity records from the database
pub fn delete_all(exec: Executor) -> Result(Nil, DbError) {
  executor.exec(exec, "DELETE FROM jetstream_activity", [])
}

/// Get activity aggregated into 5-minute buckets for the last hour
pub fn get_activity_1hr(exec: Executor) -> Result(List(ActivityBucket), DbError) {
  get_activity_bucketed(exec, 1, 5, 12)
}

/// Get activity aggregated into 15-minute buckets for the last 3 hours
pub fn get_activity_3hr(exec: Executor) -> Result(List(ActivityBucket), DbError) {
  get_activity_bucketed(exec, 3, 15, 12)
}

/// Get activity aggregated into 30-minute buckets for the last 6 hours
pub fn get_activity_6hr(exec: Executor) -> Result(List(ActivityBucket), DbError) {
  get_activity_bucketed(exec, 6, 30, 12)
}

/// Get activity aggregated into hourly buckets for the last day
pub fn get_activity_1day(
  exec: Executor,
) -> Result(List(ActivityBucket), DbError) {
  get_activity_bucketed(exec, 24, 60, 24)
}

/// Get activity aggregated into daily buckets for the last 7 days
pub fn get_activity_7day(
  exec: Executor,
) -> Result(List(ActivityBucket), DbError) {
  // Use dialect-specific time series generation for daily buckets
  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "WITH RECURSIVE time_series(bucket, n) AS (
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
      ORDER BY ts.bucket"
    executor.PostgreSQL ->
      "WITH time_series AS (
        SELECT generate_series(
          DATE_TRUNC('day', NOW()) - INTERVAL '6 days',
          DATE_TRUNC('day', NOW()),
          INTERVAL '1 day'
        ) AS bucket
      )
      SELECT
        TO_CHAR(ts.bucket, 'YYYY-MM-DD\"T\"00:00:00\"Z\"') as timestamp,
        COALESCE(SUM(CASE WHEN a.operation = 'create' THEN 1 ELSE 0 END), 0)::INT as create_count,
        COALESCE(SUM(CASE WHEN a.operation = 'update' THEN 1 ELSE 0 END), 0)::INT as update_count,
        COALESCE(SUM(CASE WHEN a.operation = 'delete' THEN 1 ELSE 0 END), 0)::INT as delete_count
      FROM time_series ts
      LEFT JOIN jetstream_activity a ON
        DATE(a.timestamp::timestamp) = DATE(ts.bucket)
        AND a.status = 'success'
      GROUP BY ts.bucket
      ORDER BY ts.bucket"
  }

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

  executor.query(exec, sql, [], decoder)
}

/// Helper function to get activity bucketed by a specific interval
/// interval_minutes: the bucket size in minutes
fn get_activity_bucketed(
  exec: Executor,
  hours: Int,
  interval_minutes: Int,
  expected_buckets: Int,
) -> Result(List(ActivityBucket), DbError) {
  let hours_str = int.to_string(hours)
  let interval_str = int.to_string(interval_minutes)
  let buckets_str = int.to_string(expected_buckets - 1)

  // Use dialect-specific time series generation
  let sql = case executor.dialect(exec) {
    executor.SQLite -> {
      let interval = int.to_string(interval_minutes) <> " minutes"
      "WITH RECURSIVE time_series(bucket, n) AS (
        SELECT datetime('now', '-" <> hours_str <> " hours'), 0
        UNION ALL
        SELECT datetime(bucket, '+" <> interval <> "'), n + 1
        FROM time_series
        WHERE n < " <> buckets_str <> "
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
      ORDER BY ts.bucket"
    }
    executor.PostgreSQL ->
      // generate_series is inclusive, so end at NOW() - 1 interval to get expected_buckets count
      "WITH time_series AS (
        SELECT generate_series(
          NOW() - INTERVAL '" <> hours_str <> " hours',
          NOW() - INTERVAL '" <> interval_str <> " minutes',
          INTERVAL '" <> interval_str <> " minutes'
        ) AS bucket
      )
      SELECT
        TO_CHAR(ts.bucket, 'YYYY-MM-DD\"T\"HH24:MI:00\"Z\"') as timestamp,
        COALESCE(SUM(CASE WHEN a.operation = 'create' THEN 1 ELSE 0 END), 0)::INT as create_count,
        COALESCE(SUM(CASE WHEN a.operation = 'update' THEN 1 ELSE 0 END), 0)::INT as update_count,
        COALESCE(SUM(CASE WHEN a.operation = 'delete' THEN 1 ELSE 0 END), 0)::INT as delete_count
      FROM time_series ts
      LEFT JOIN jetstream_activity a ON
        a.timestamp::timestamp >= ts.bucket
        AND a.timestamp::timestamp < ts.bucket + INTERVAL '" <> interval_str <> " minutes'
        AND a.status = 'success'
      GROUP BY ts.bucket
      ORDER BY ts.bucket"
  }

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

  executor.query(exec, sql, [], decoder)
}
