/// Repository for moderation reports
import database/executor.{
  type DbError, type Executor, type Value, Int, Null, Text,
}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// Report domain type
pub type Report {
  Report(
    id: Int,
    reporter_did: String,
    subject_uri: String,
    reason_type: String,
    reason: Option(String),
    status: String,
    resolved_by: Option(String),
    resolved_at: Option(String),
    created_at: String,
  )
}

/// Insert a new report
pub fn insert(
  exec: Executor,
  reporter_did: String,
  subject_uri: String,
  reason_type: String,
  reason: Option(String),
) -> Result(Report, DbError) {
  let p1 = executor.placeholder(exec, 1)
  let p2 = executor.placeholder(exec, 2)
  let p3 = executor.placeholder(exec, 3)
  let p4 = executor.placeholder(exec, 4)

  let reason_value = case reason {
    Some(r) -> Text(r)
    None -> Null
  }

  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "INSERT INTO report (reporter_did, subject_uri, reason_type, reason) VALUES ("
      <> p1
      <> ", "
      <> p2
      <> ", "
      <> p3
      <> ", "
      <> p4
      <> ") RETURNING id, reporter_did, subject_uri, reason_type, reason, status, resolved_by, resolved_at, created_at"
    executor.PostgreSQL ->
      "INSERT INTO report (reporter_did, subject_uri, reason_type, reason) VALUES ("
      <> p1
      <> ", "
      <> p2
      <> ", "
      <> p3
      <> ", "
      <> p4
      <> ") RETURNING id, reporter_did, subject_uri, reason_type, reason, status, resolved_by, resolved_at::text, created_at::text"
  }

  case
    executor.query(
      exec,
      sql,
      [Text(reporter_did), Text(subject_uri), Text(reason_type), reason_value],
      report_decoder(),
    )
  {
    Ok([report]) -> Ok(report)
    Ok(_) -> Error(executor.QueryError("Insert did not return report"))
    Error(e) -> Error(e)
  }
}

/// Get a report by ID
pub fn get(exec: Executor, id: Int) -> Result(Option(Report), DbError) {
  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "SELECT id, reporter_did, subject_uri, reason_type, reason, status, resolved_by, resolved_at, created_at FROM report WHERE id = "
      <> executor.placeholder(exec, 1)
    executor.PostgreSQL ->
      "SELECT id, reporter_did, subject_uri, reason_type, reason, status, resolved_by, resolved_at::text, created_at::text FROM report WHERE id = "
      <> executor.placeholder(exec, 1)
  }
  case executor.query(exec, sql, [Int(id)], report_decoder()) {
    Ok([report]) -> Ok(Some(report))
    Ok(_) -> Ok(None)
    Error(e) -> Error(e)
  }
}

/// Get reports with optional status filter and pagination
pub fn get_all(
  exec: Executor,
  status_filter: Option(String),
  limit: Int,
  cursor: Option(Int),
) -> Result(List(Report), DbError) {
  let mut_where_parts: List(String) = []
  let mut_params: List(Value) = []
  let mut_param_count = 0

  // Add status filter if provided
  let #(where_parts, params, param_count) = case status_filter {
    Some(s) -> {
      let p = executor.placeholder(exec, mut_param_count + 1)
      #(["status = " <> p], [Text(s)], mut_param_count + 1)
    }
    None -> #(mut_where_parts, mut_params, mut_param_count)
  }

  // Add cursor filter if provided
  let #(where_parts2, params2, param_count2) = case cursor {
    Some(c) -> {
      let p = executor.placeholder(exec, param_count + 1)
      #(["id < " <> p, ..where_parts], [Int(c), ..params], param_count + 1)
    }
    None -> #(where_parts, params, param_count)
  }

  let where_clause = case where_parts2 {
    [] -> ""
    parts -> " WHERE " <> string.join(list.reverse(parts), " AND ")
  }

  let limit_p = executor.placeholder(exec, param_count2 + 1)
  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "SELECT id, reporter_did, subject_uri, reason_type, reason, status, resolved_by, resolved_at, created_at FROM report"
      <> where_clause
      <> " ORDER BY id DESC LIMIT "
      <> limit_p
    executor.PostgreSQL ->
      "SELECT id, reporter_did, subject_uri, reason_type, reason, status, resolved_by, resolved_at::text, created_at::text FROM report"
      <> where_clause
      <> " ORDER BY id DESC LIMIT "
      <> limit_p
  }

  executor.query(
    exec,
    sql,
    list.append(list.reverse(params2), [Int(limit)]),
    report_decoder(),
  )
}

/// Result type for paginated report queries
pub type PaginatedReports {
  PaginatedReports(reports: List(Report), has_next_page: Bool, total_count: Int)
}

/// Get reports with connection-style pagination
/// Returns reports, whether there's a next page, and total count
pub fn get_paginated(
  exec: Executor,
  status_filter: Option(String),
  first: Int,
  after_id: Option(Int),
) -> Result(PaginatedReports, DbError) {
  // Fetch first + 1 to detect hasNextPage
  let fetch_limit = first + 1

  let mut_where_parts: List(String) = []
  let mut_params: List(Value) = []
  let mut_param_count = 0

  // Add status filter if provided
  let #(where_parts, params, param_count) = case status_filter {
    Some(s) -> {
      let p = executor.placeholder(exec, mut_param_count + 1)
      #(["status = " <> p], [Text(s)], mut_param_count + 1)
    }
    None -> #(mut_where_parts, mut_params, mut_param_count)
  }

  // Add cursor filter if provided (only for main query, not for count)
  let #(where_parts_with_cursor, params_with_cursor, param_count2) = case
    after_id
  {
    Some(c) -> {
      let p = executor.placeholder(exec, param_count + 1)
      #(["id < " <> p, ..where_parts], [Int(c), ..params], param_count + 1)
    }
    None -> #(where_parts, params, param_count)
  }

  // Build WHERE clause for main query (with cursor)
  let where_clause = case where_parts_with_cursor {
    [] -> ""
    parts -> " WHERE " <> string.join(list.reverse(parts), " AND ")
  }

  // Build WHERE clause for count query (without cursor)
  let count_where_clause = case where_parts {
    [] -> ""
    parts -> " WHERE " <> string.join(list.reverse(parts), " AND ")
  }

  let limit_p = executor.placeholder(exec, param_count2 + 1)
  let main_sql = case executor.dialect(exec) {
    executor.SQLite ->
      "SELECT id, reporter_did, subject_uri, reason_type, reason, status, resolved_by, resolved_at, created_at FROM report"
      <> where_clause
      <> " ORDER BY id DESC LIMIT "
      <> limit_p
    executor.PostgreSQL ->
      "SELECT id, reporter_did, subject_uri, reason_type, reason, status, resolved_by, resolved_at::text, created_at::text FROM report"
      <> where_clause
      <> " ORDER BY id DESC LIMIT "
      <> limit_p
  }

  let count_sql = "SELECT COUNT(*) FROM report" <> count_where_clause

  // Execute main query
  let main_params =
    list.append(list.reverse(params_with_cursor), [Int(fetch_limit)])
  case executor.query(exec, main_sql, main_params, report_decoder()) {
    Ok(reports_result) -> {
      // Execute count query (uses params without cursor)
      let count_params = list.reverse(params)
      let count_decoder = {
        use count <- decode.field(0, decode.int)
        decode.success(count)
      }

      case executor.query(exec, count_sql, count_params, count_decoder) {
        Ok([total_count]) -> {
          // Determine if there's a next page
          let has_next = list.length(reports_result) > first
          let reports = case has_next {
            True -> list.take(reports_result, first)
            False -> reports_result
          }
          Ok(PaginatedReports(reports:, has_next_page: has_next, total_count:))
        }
        Ok(_) ->
          Ok(PaginatedReports(reports: [], has_next_page: False, total_count: 0))
        Error(e) -> Error(e)
      }
    }
    Error(e) -> Error(e)
  }
}

/// Resolve a report (apply label or dismiss)
pub fn resolve(
  exec: Executor,
  id: Int,
  status: String,
  resolved_by: String,
) -> Result(Report, DbError) {
  let p1 = executor.placeholder(exec, 1)
  let p2 = executor.placeholder(exec, 2)
  let p3 = executor.placeholder(exec, 3)

  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "UPDATE report SET status = "
      <> p1
      <> ", resolved_by = "
      <> p2
      <> ", resolved_at = datetime('now') WHERE id = "
      <> p3
      <> " RETURNING id, reporter_did, subject_uri, reason_type, reason, status, resolved_by, resolved_at, created_at"
    executor.PostgreSQL ->
      "UPDATE report SET status = "
      <> p1
      <> ", resolved_by = "
      <> p2
      <> ", resolved_at = NOW() WHERE id = "
      <> p3
      <> " RETURNING id, reporter_did, subject_uri, reason_type, reason, status, resolved_by, resolved_at::text, created_at::text"
  }

  case
    executor.query(
      exec,
      sql,
      [Text(status), Text(resolved_by), Int(id)],
      report_decoder(),
    )
  {
    Ok([report]) -> Ok(report)
    Ok(_) -> Error(executor.QueryError("Update did not return report"))
    Error(e) -> Error(e)
  }
}

/// Decoder for Report
fn report_decoder() -> decode.Decoder(Report) {
  use id <- decode.field(0, decode.int)
  use reporter_did <- decode.field(1, decode.string)
  use subject_uri <- decode.field(2, decode.string)
  use reason_type <- decode.field(3, decode.string)
  use reason <- decode.field(4, decode.optional(decode.string))
  use status <- decode.field(5, decode.string)
  use resolved_by <- decode.field(6, decode.optional(decode.string))
  use resolved_at <- decode.field(7, decode.optional(decode.string))
  use created_at <- decode.field(8, decode.string)
  decode.success(Report(
    id:,
    reporter_did:,
    subject_uri:,
    reason_type:,
    reason:,
    status:,
    resolved_by:,
    resolved_at:,
    created_at:,
  ))
}
