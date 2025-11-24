import database/types.{type ActivityBucket, type ActivityEntry, ActivityBucket, ActivityEntry}
import gleam/dynamic/decode
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/result
import logging
import sqlight

// ===== Jetstream Activity Functions =====

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
pub fn delete_all(conn: sqlight.Connection) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM jetstream_activity"
  sqlight.exec(sql, conn)
  |> result.map(fn(_) { Nil })
}

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
