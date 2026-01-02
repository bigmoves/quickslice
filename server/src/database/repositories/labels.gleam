/// Repository for labels
import database/executor.{
  type DbError, type Executor, type Value, Int, Null, Text,
}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// Validate that a URI is a valid AT Protocol subject URI
/// Valid formats:
/// - at://did:xxx/collection/rkey (record URI)
/// - did:plc:xxx or did:web:xxx (account DID)
pub fn is_valid_subject_uri(uri: String) -> Bool {
  case string.starts_with(uri, "at://") {
    True -> {
      // AT URI format: at://did/collection/rkey
      let without_prefix = string.drop_start(uri, 5)
      case string.split(without_prefix, "/") {
        [did, _collection, _rkey] -> string.starts_with(did, "did:")
        [did, _collection] -> string.starts_with(did, "did:")
        _ -> False
      }
    }
    False -> {
      // Allow bare DIDs for account labels
      string.starts_with(uri, "did:plc:") || string.starts_with(uri, "did:web:")
    }
  }
}

/// Label domain type
pub type Label {
  Label(
    id: Int,
    src: String,
    uri: String,
    cid: Option(String),
    val: String,
    neg: Bool,
    cts: String,
    exp: Option(String),
  )
}

/// Insert a new label
pub fn insert(
  exec: Executor,
  src: String,
  uri: String,
  cid: Option(String),
  val: String,
  exp: Option(String),
) -> Result(Label, DbError) {
  let p1 = executor.placeholder(exec, 1)
  let p2 = executor.placeholder(exec, 2)
  let p3 = executor.placeholder(exec, 3)
  let p4 = executor.placeholder(exec, 4)
  let p5 = executor.placeholder(exec, 5)

  let cid_value = case cid {
    Some(c) -> Text(c)
    None -> Null
  }
  let exp_value = case exp {
    Some(e) -> Text(e)
    None -> Null
  }

  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "INSERT INTO label (src, uri, cid, val, exp) VALUES ("
      <> p1
      <> ", "
      <> p2
      <> ", "
      <> p3
      <> ", "
      <> p4
      <> ", "
      <> p5
      <> ") RETURNING id, src, uri, cid, val, neg, cts, exp"
    executor.PostgreSQL ->
      "INSERT INTO label (src, uri, cid, val, exp) VALUES ("
      <> p1
      <> ", "
      <> p2
      <> ", "
      <> p3
      <> ", "
      <> p4
      <> ", "
      <> p5
      <> ") RETURNING id, src, uri, cid, val, neg::int, cts::text, exp::text"
  }

  case
    executor.query(
      exec,
      sql,
      [Text(src), Text(uri), cid_value, Text(val), exp_value],
      label_decoder(),
    )
  {
    Ok([label]) -> Ok(label)
    Ok(_) -> Error(executor.QueryError("Insert did not return label"))
    Error(e) -> Error(e)
  }
}

/// Insert a negation label (retraction)
pub fn insert_negation(
  exec: Executor,
  src: String,
  uri: String,
  val: String,
) -> Result(Label, DbError) {
  let p1 = executor.placeholder(exec, 1)
  let p2 = executor.placeholder(exec, 2)
  let p3 = executor.placeholder(exec, 3)

  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "INSERT INTO label (src, uri, val, neg) VALUES ("
      <> p1
      <> ", "
      <> p2
      <> ", "
      <> p3
      <> ", 1) RETURNING id, src, uri, cid, val, neg, cts, exp"
    executor.PostgreSQL ->
      "INSERT INTO label (src, uri, val, neg) VALUES ("
      <> p1
      <> ", "
      <> p2
      <> ", "
      <> p3
      <> ", TRUE) RETURNING id, src, uri, cid, val, neg::int, cts::text, exp::text"
  }

  case
    executor.query(
      exec,
      sql,
      [Text(src), Text(uri), Text(val)],
      label_decoder(),
    )
  {
    Ok([label]) -> Ok(label)
    Ok(_) -> Error(executor.QueryError("Insert did not return label"))
    Error(e) -> Error(e)
  }
}

/// Get labels for a list of URIs (batch fetch for GraphQL)
/// Returns only active labels (non-negated, non-expired, and not retracted by a later negation)
pub fn get_by_uris(
  exec: Executor,
  uris: List(String),
) -> Result(List(Label), DbError) {
  case uris {
    [] -> Ok([])
    _ -> {
      let placeholders = executor.placeholders(exec, list.length(uris), 1)
      // Exclude labels that have been retracted by a subsequent negation label
      // Use (cts, id) for ordering to handle same-timestamp inserts
      let sql = case executor.dialect(exec) {
        executor.SQLite ->
          "SELECT l.id, l.src, l.uri, l.cid, l.val, l.neg, l.cts, l.exp FROM label l WHERE l.uri IN ("
          <> placeholders
          <> ") AND l.neg = 0 AND (l.exp IS NULL OR l.exp > datetime('now'))"
          <> " AND NOT EXISTS (SELECT 1 FROM label n WHERE n.uri = l.uri AND n.val = l.val AND n.neg = 1 AND (n.cts > l.cts OR (n.cts = l.cts AND n.id > l.id)))"
          <> " ORDER BY l.cts DESC"
        executor.PostgreSQL ->
          "SELECT l.id, l.src, l.uri, l.cid, l.val, l.neg::int, l.cts::text, l.exp::text FROM label l WHERE l.uri IN ("
          <> placeholders
          <> ") AND l.neg = FALSE AND (l.exp IS NULL OR l.exp > NOW())"
          <> " AND NOT EXISTS (SELECT 1 FROM label n WHERE n.uri = l.uri AND n.val = l.val AND n.neg = TRUE AND (n.cts > l.cts OR (n.cts = l.cts AND n.id > l.id)))"
          <> " ORDER BY l.cts DESC"
      }
      executor.query(exec, sql, list.map(uris, Text), label_decoder())
    }
  }
}

/// Get all labels (admin query with optional filters)
pub fn get_all(
  exec: Executor,
  uri_filter: Option(String),
  val_filter: Option(String),
  limit: Int,
  cursor: Option(Int),
) -> Result(List(Label), DbError) {
  let mut_where_parts: List(String) = []
  let mut_params: List(Value) = []
  let mut_param_count = 0

  // Add URI filter if provided
  let #(where_parts, params, param_count) = case uri_filter {
    Some(uri) -> {
      let p = executor.placeholder(exec, mut_param_count + 1)
      #(
        ["uri = " <> p, ..mut_where_parts],
        [Text(uri), ..mut_params],
        mut_param_count + 1,
      )
    }
    None -> #(mut_where_parts, mut_params, mut_param_count)
  }

  // Add val filter if provided
  let #(where_parts2, params2, param_count2) = case val_filter {
    Some(v) -> {
      let p = executor.placeholder(exec, param_count + 1)
      #(["val = " <> p, ..where_parts], [Text(v), ..params], param_count + 1)
    }
    None -> #(where_parts, params, param_count)
  }

  // Add cursor filter if provided
  let #(where_parts3, params3, param_count3) = case cursor {
    Some(c) -> {
      let p = executor.placeholder(exec, param_count2 + 1)
      #(["id < " <> p, ..where_parts2], [Int(c), ..params2], param_count2 + 1)
    }
    None -> #(where_parts2, params2, param_count2)
  }

  let where_clause = case where_parts3 {
    [] -> ""
    parts -> " WHERE " <> string.join(list.reverse(parts), " AND ")
  }

  let limit_p = executor.placeholder(exec, param_count3 + 1)
  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "SELECT id, src, uri, cid, val, neg, cts, exp FROM label"
      <> where_clause
      <> " ORDER BY id DESC LIMIT "
      <> limit_p
    executor.PostgreSQL ->
      "SELECT id, src, uri, cid, val, neg::int, cts::text, exp::text FROM label"
      <> where_clause
      <> " ORDER BY id DESC LIMIT "
      <> limit_p
  }

  executor.query(
    exec,
    sql,
    list.append(list.reverse(params3), [Int(limit)]),
    label_decoder(),
  )
}

/// Result type for paginated label queries
pub type PaginatedLabels {
  PaginatedLabels(labels: List(Label), has_next_page: Bool, total_count: Int)
}

/// Get labels with connection-style pagination
/// Returns labels, whether there's a next page, and total count
pub fn get_paginated(
  exec: Executor,
  uri_filter: Option(String),
  val_filter: Option(String),
  first: Int,
  after_id: Option(Int),
) -> Result(PaginatedLabels, DbError) {
  // Fetch first + 1 to detect hasNextPage
  let fetch_limit = first + 1

  let mut_where_parts: List(String) = []
  let mut_params: List(Value) = []
  let mut_param_count = 0

  // Add URI filter if provided
  let #(where_parts, params, param_count) = case uri_filter {
    Some(uri) -> {
      let p = executor.placeholder(exec, mut_param_count + 1)
      #(
        ["uri = " <> p, ..mut_where_parts],
        [Text(uri), ..mut_params],
        mut_param_count + 1,
      )
    }
    None -> #(mut_where_parts, mut_params, mut_param_count)
  }

  // Add val filter if provided
  let #(where_parts2, params2, param_count2) = case val_filter {
    Some(v) -> {
      let p = executor.placeholder(exec, param_count + 1)
      #(["val = " <> p, ..where_parts], [Text(v), ..params], param_count + 1)
    }
    None -> #(where_parts, params, param_count)
  }

  // Add cursor filter if provided (only for main query, not for count)
  let #(where_parts_with_cursor, params_with_cursor, param_count3) = case
    after_id
  {
    Some(c) -> {
      let p = executor.placeholder(exec, param_count2 + 1)
      #(["id < " <> p, ..where_parts2], [Int(c), ..params2], param_count2 + 1)
    }
    None -> #(where_parts2, params2, param_count2)
  }

  // Build WHERE clause for main query (with cursor)
  let where_clause = case where_parts_with_cursor {
    [] -> ""
    parts -> " WHERE " <> string.join(list.reverse(parts), " AND ")
  }

  // Build WHERE clause for count query (without cursor)
  let count_where_clause = case where_parts2 {
    [] -> ""
    parts -> " WHERE " <> string.join(list.reverse(parts), " AND ")
  }

  let limit_p = executor.placeholder(exec, param_count3 + 1)
  let main_sql = case executor.dialect(exec) {
    executor.SQLite ->
      "SELECT id, src, uri, cid, val, neg, cts, exp FROM label"
      <> where_clause
      <> " ORDER BY id DESC LIMIT "
      <> limit_p
    executor.PostgreSQL ->
      "SELECT id, src, uri, cid, val, neg::int, cts::text, exp::text FROM label"
      <> where_clause
      <> " ORDER BY id DESC LIMIT "
      <> limit_p
  }

  let count_sql = "SELECT COUNT(*) FROM label" <> count_where_clause

  // Execute main query
  let main_params =
    list.append(list.reverse(params_with_cursor), [Int(fetch_limit)])
  case executor.query(exec, main_sql, main_params, label_decoder()) {
    Ok(labels_result) -> {
      // Execute count query (uses params without cursor)
      let count_params = list.reverse(params2)
      let count_decoder = {
        use count <- decode.field(0, decode.int)
        decode.success(count)
      }

      case executor.query(exec, count_sql, count_params, count_decoder) {
        Ok([total_count]) -> {
          // Determine if there's a next page
          let has_next = list.length(labels_result) > first
          let labels = case has_next {
            True -> list.take(labels_result, first)
            False -> labels_result
          }
          Ok(PaginatedLabels(labels:, has_next_page: has_next, total_count:))
        }
        Ok(_) ->
          Ok(PaginatedLabels(labels: [], has_next_page: False, total_count: 0))
        Error(e) -> Error(e)
      }
    }
    Error(e) -> Error(e)
  }
}

/// Check if a URI has an active takedown label (not retracted by negation)
pub fn has_takedown(exec: Executor, uri: String) -> Result(Bool, DbError) {
  let p1 = executor.placeholder(exec, 1)
  let p2 = executor.placeholder(exec, 2)
  // Use (cts, id) for ordering to handle same-timestamp inserts
  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "SELECT 1 FROM label l WHERE l.uri = "
      <> p1
      <> " AND l.val IN ('!takedown', '!suspend') AND l.neg = 0 AND (l.exp IS NULL OR l.exp > datetime('now'))"
      <> " AND NOT EXISTS (SELECT 1 FROM label n WHERE n.uri = "
      <> p2
      <> " AND n.val = l.val AND n.neg = 1 AND (n.cts > l.cts OR (n.cts = l.cts AND n.id > l.id)))"
      <> " LIMIT 1"
    executor.PostgreSQL ->
      "SELECT 1 FROM label l WHERE l.uri = "
      <> p1
      <> " AND l.val IN ('!takedown', '!suspend') AND l.neg = FALSE AND (l.exp IS NULL OR l.exp > NOW())"
      <> " AND NOT EXISTS (SELECT 1 FROM label n WHERE n.uri = "
      <> p2
      <> " AND n.val = l.val AND n.neg = TRUE AND (n.cts > l.cts OR (n.cts = l.cts AND n.id > l.id)))"
      <> " LIMIT 1"
  }

  case executor.query(exec, sql, [Text(uri), Text(uri)], decode.dynamic) {
    Ok([_]) -> Ok(True)
    Ok([]) -> Ok(False)
    Ok(_) -> Ok(True)
    Error(e) -> Error(e)
  }
}

/// Batch check for takedown labels on multiple URIs
/// Returns list of URIs that have active takedown labels (not retracted by negation)
pub fn get_takedown_uris(
  exec: Executor,
  uris: List(String),
) -> Result(List(String), DbError) {
  case uris {
    [] -> Ok([])
    _ -> {
      let placeholders = executor.placeholders(exec, list.length(uris), 1)
      // Exclude takedown labels that have been retracted by a subsequent negation
      // Use (cts, id) for ordering to handle same-timestamp inserts
      let sql = case executor.dialect(exec) {
        executor.SQLite ->
          "SELECT DISTINCT l.uri FROM label l WHERE l.uri IN ("
          <> placeholders
          <> ") AND l.val IN ('!takedown', '!suspend') AND l.neg = 0 AND (l.exp IS NULL OR l.exp > datetime('now'))"
          <> " AND NOT EXISTS (SELECT 1 FROM label n WHERE n.uri = l.uri AND n.val = l.val AND n.neg = 1 AND (n.cts > l.cts OR (n.cts = l.cts AND n.id > l.id)))"
        executor.PostgreSQL ->
          "SELECT DISTINCT l.uri FROM label l WHERE l.uri IN ("
          <> placeholders
          <> ") AND l.val IN ('!takedown', '!suspend') AND l.neg = FALSE AND (l.exp IS NULL OR l.exp > NOW())"
          <> " AND NOT EXISTS (SELECT 1 FROM label n WHERE n.uri = l.uri AND n.val = l.val AND n.neg = TRUE AND (n.cts > l.cts OR (n.cts = l.cts AND n.id > l.id)))"
      }

      let uri_decoder = {
        use uri <- decode.field(0, decode.string)
        decode.success(uri)
      }

      executor.query(exec, sql, list.map(uris, Text), uri_decoder)
    }
  }
}

/// Count active takedown labels for a collection (for accurate pagination counts)
/// This counts records with !takedown or !suspend that haven't been retracted
pub fn count_takedowns_for_collection(
  exec: Executor,
  collection: String,
) -> Result(Int, DbError) {
  let p1 = executor.placeholder(exec, 1)
  // Count distinct URIs with active takedown labels for records in this collection
  // Uses LIKE pattern matching on URI: at://did/collection/rkey
  // Use (cts, id) for ordering to handle same-timestamp inserts
  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "SELECT COUNT(DISTINCT l.uri) FROM label l "
      <> "WHERE l.uri LIKE '%/' || "
      <> p1
      <> " || '/%' "
      <> "AND l.val IN ('!takedown', '!suspend') AND l.neg = 0 "
      <> "AND (l.exp IS NULL OR l.exp > datetime('now')) "
      <> "AND NOT EXISTS (SELECT 1 FROM label n WHERE n.uri = l.uri AND n.val = l.val AND n.neg = 1 AND (n.cts > l.cts OR (n.cts = l.cts AND n.id > l.id)))"
    executor.PostgreSQL ->
      "SELECT COUNT(DISTINCT l.uri) FROM label l "
      <> "WHERE l.uri LIKE '%/' || "
      <> p1
      <> " || '/%' "
      <> "AND l.val IN ('!takedown', '!suspend') AND l.neg = FALSE "
      <> "AND (l.exp IS NULL OR l.exp > NOW()) "
      <> "AND NOT EXISTS (SELECT 1 FROM label n WHERE n.uri = l.uri AND n.val = l.val AND n.neg = TRUE AND (n.cts > l.cts OR (n.cts = l.cts AND n.id > l.id)))"
  }

  let count_decoder = {
    use count <- decode.field(0, decode.int)
    decode.success(count)
  }

  case executor.query(exec, sql, [Text(collection)], count_decoder) {
    Ok([count]) -> Ok(count)
    Ok(_) -> Ok(0)
    Error(e) -> Error(e)
  }
}

/// Decoder for Label
fn label_decoder() -> decode.Decoder(Label) {
  use id <- decode.field(0, decode.int)
  use src <- decode.field(1, decode.string)
  use uri <- decode.field(2, decode.string)
  use cid <- decode.field(3, decode.optional(decode.string))
  use val <- decode.field(4, decode.string)
  use neg_int <- decode.field(5, decode.int)
  use cts <- decode.field(6, decode.string)
  use exp <- decode.field(7, decode.optional(decode.string))
  let neg = neg_int == 1
  decode.success(Label(id:, src:, uri:, cid:, val:, neg:, cts:, exp:))
}
