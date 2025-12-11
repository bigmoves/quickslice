import database/executor.{
  type DbError, type Executor, type Value, Int as DbInt, Text,
}
import database/queries/pagination
import database/queries/where_clause
import database/types.{
  type CollectionStat, type InsertResult, type Record, CollectionStat, Inserted,
  Record, Skipped,
}
import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

// ===== Helper Functions =====

/// Gets existing URIs and their CIDs from the database
/// Returns a Dict mapping URI -> CID for records that exist
fn get_existing_cids(
  exec: Executor,
  uris: List(String),
) -> Result(Dict(String, String), DbError) {
  case uris {
    [] -> Ok(dict.new())
    _ -> {
      // Process in batches to avoid SQL parameter limits
      // SQLite: max 999, PostgreSQL: max 65535
      let batch_size = 900
      let batches = list.sized_chunk(uris, batch_size)

      // Process each batch and merge results
      use accumulated_dict <- result.try(
        list.try_fold(batches, dict.new(), fn(acc_dict, batch) {
          // Build placeholders for SQL IN clause
          let placeholders = executor.placeholders(exec, list.length(batch), 1)

          let sql =
            "SELECT uri, cid FROM record WHERE uri IN (" <> placeholders <> ")"

          // Convert URIs to Value list
          let params = list.map(batch, Text)

          let decoder = {
            use uri <- decode.field(0, decode.string)
            use cid <- decode.field(1, decode.string)
            decode.success(#(uri, cid))
          }

          use results <- result.try(executor.query(exec, sql, params, decoder))

          // Merge with accumulated dictionary
          let batch_dict = dict.from_list(results)
          Ok(dict.merge(acc_dict, batch_dict))
        }),
      )

      Ok(accumulated_dict)
    }
  }
}

/// Gets existing CIDs from the database (checks if any CID exists, regardless of URI)
/// Returns a list of CIDs that exist in the database
fn get_existing_cids_batch(
  exec: Executor,
  cids: List(String),
) -> Result(List(String), DbError) {
  case cids {
    [] -> Ok([])
    _ -> {
      // Process in batches to avoid SQL parameter limits
      let batch_size = 900
      let batches = list.sized_chunk(cids, batch_size)

      // Process each batch and collect results
      use all_results <- result.try(
        list.try_fold(batches, [], fn(acc_results, batch) {
          // Build placeholders for SQL IN clause
          let placeholders = executor.placeholders(exec, list.length(batch), 1)

          let sql =
            "SELECT cid FROM record WHERE cid IN (" <> placeholders <> ")"

          let cid_decoder = {
            use cid <- decode.field(0, decode.string)
            decode.success(cid)
          }

          use results <- result.try(executor.query(
            exec,
            sql,
            list.map(batch, Text),
            cid_decoder,
          ))

          // Append to accumulated results
          Ok(list.append(acc_results, results))
        }),
      )

      Ok(all_results)
    }
  }
}

// ===== CRUD Operations =====

/// Inserts or updates a record in the database
/// Skips insertion if the CID already exists in the database (for any URI)
/// Also skips update if the URI exists with the same CID (content unchanged)
pub fn insert(
  exec: Executor,
  uri: String,
  cid: String,
  did: String,
  collection: String,
  json: String,
) -> Result(InsertResult, DbError) {
  // Check if this CID already exists in the database
  use existing_cids <- result.try(get_existing_cids(exec, [uri]))

  case dict.get(existing_cids, uri) {
    // URI exists with same CID - skip update (content unchanged)
    Ok(existing_cid) if existing_cid == cid -> Ok(Skipped)
    // URI exists with different CID - proceed with update
    // URI doesn't exist - proceed with insert
    _ -> {
      let p1 = executor.placeholder(exec, 1)
      let p2 = executor.placeholder(exec, 2)
      let p3 = executor.placeholder(exec, 3)
      let p4 = executor.placeholder(exec, 4)
      let p5 = executor.placeholder(exec, 5)

      // Use dialect-specific UPSERT syntax
      let sql = case executor.dialect(exec) {
        executor.SQLite -> "INSERT INTO record (uri, cid, did, collection, json)
         VALUES (" <> p1 <> ", " <> p2 <> ", " <> p3 <> ", " <> p4 <> ", " <> p5 <> ")
         ON CONFLICT(uri) DO UPDATE SET
           cid = excluded.cid,
           json = excluded.json,
           indexed_at = datetime('now')"
        executor.PostgreSQL ->
          "INSERT INTO record (uri, cid, did, collection, json)
         VALUES (" <> p1 <> ", " <> p2 <> ", " <> p3 <> ", " <> p4 <> ", " <> p5 <> ")
         ON CONFLICT(uri) DO UPDATE SET
           cid = EXCLUDED.cid,
           json = EXCLUDED.json,
           indexed_at = NOW()"
      }

      use _ <- result.try(
        executor.exec(exec, sql, [
          Text(uri),
          Text(cid),
          Text(did),
          Text(collection),
          Text(json),
        ]),
      )
      Ok(Inserted)
    }
  }
}

/// Batch inserts or updates multiple records in the database
/// More efficient than individual inserts for large datasets
/// Filters out records where CID already exists or is unchanged
pub fn batch_insert(
  exec: Executor,
  records: List(Record),
) -> Result(Nil, DbError) {
  case records {
    [] -> Ok(Nil)
    _ -> {
      // Get all URIs from the incoming records
      let uris = list.map(records, fn(record) { record.uri })

      // Fetch existing CIDs for these URIs (batched to avoid SQL parameter limits)
      use existing_cids <- result.try(get_existing_cids(exec, uris))

      // Get all CIDs that already exist in the database (for any URI)
      // Check in batches to avoid exceeding SQL parameter limits
      let all_incoming_cids =
        list.map(records, fn(record) { record.cid })
        |> list.unique()

      use existing_cids_in_db <- result.try(get_existing_cids_batch(
        exec,
        all_incoming_cids,
      ))

      // Create a set of existing CIDs for fast lookup
      let existing_cid_set =
        dict.from_list(list.map(existing_cids_in_db, fn(cid) { #(cid, True) }))

      // Filter out records where:
      // 1. URI exists with same CID (unchanged)
      // 2. CID already exists for a different URI (duplicate content)
      let filtered_records =
        list.filter(records, fn(record) {
          case dict.get(existing_cids, record.uri) {
            // URI exists with same CID - skip
            Ok(existing_cid) if existing_cid == record.cid -> False
            // URI exists with different CID - include (content changed)
            Ok(_) ->
              case dict.get(existing_cid_set, record.cid) {
                Ok(_) -> False
                Error(_) -> True
              }
            // URI doesn't exist - check if CID exists elsewhere
            Error(_) ->
              case dict.get(existing_cid_set, record.cid) {
                Ok(_) -> False
                Error(_) -> True
              }
          }
        })

      case filtered_records {
        [] -> Ok(Nil)
        _ -> {
          // Process records in smaller batches to avoid SQL parameter limits
          // Each record uses 5 parameters, so we can safely do 100 records at a time
          let batch_size = 100

          list.sized_chunk(filtered_records, batch_size)
          |> list.try_each(fn(batch) {
            // Build the SQL with multiple value sets using numbered placeholders
            let value_placeholders =
              list.index_map(batch, fn(_, i) {
                let base = i * 5
                "("
                <> executor.placeholder(exec, base + 1)
                <> ", "
                <> executor.placeholder(exec, base + 2)
                <> ", "
                <> executor.placeholder(exec, base + 3)
                <> ", "
                <> executor.placeholder(exec, base + 4)
                <> ", "
                <> executor.placeholder(exec, base + 5)
                <> ")"
              })
              |> string.join(", ")

            // Use dialect-specific UPSERT syntax
            let sql = case executor.dialect(exec) {
              executor.SQLite ->
                "INSERT INTO record (uri, cid, did, collection, json)
               VALUES " <> value_placeholders <> "
               ON CONFLICT(uri) DO UPDATE SET
                 cid = excluded.cid,
                 json = excluded.json,
                 indexed_at = datetime('now')"
              executor.PostgreSQL ->
                "INSERT INTO record (uri, cid, did, collection, json)
               VALUES " <> value_placeholders <> "
               ON CONFLICT(uri) DO UPDATE SET
                 cid = EXCLUDED.cid,
                 json = EXCLUDED.json,
                 indexed_at = NOW()"
            }

            // Flatten all record parameters into a single list
            let params: List(Value) =
              list.flat_map(batch, fn(record) {
                [
                  Text(record.uri),
                  Text(record.cid),
                  Text(record.did),
                  Text(record.collection),
                  Text(record.json),
                ]
              })

            use _ <- result.try(executor.exec(exec, sql, params))
            Ok(Nil)
          })
        }
      }
    }
  }
}

/// Gets a record by URI
pub fn get(exec: Executor, uri: String) -> Result(List(Record), DbError) {
  let sql =
    "SELECT uri, cid, did, collection, json, indexed_at FROM record WHERE uri = "
    <> executor.placeholder(exec, 1)

  executor.query(exec, sql, [Text(uri)], record_decoder())
}

/// Gets all records for a specific DID
pub fn get_by_did(exec: Executor, did: String) -> Result(List(Record), DbError) {
  let sql =
    "SELECT uri, cid, did, collection, json, indexed_at FROM record WHERE did = "
    <> executor.placeholder(exec, 1)
    <> " ORDER BY indexed_at DESC"

  executor.query(exec, sql, [Text(did)], record_decoder())
}

/// Gets all records for a specific collection
pub fn get_by_collection(
  exec: Executor,
  collection: String,
) -> Result(List(Record), DbError) {
  let sql =
    "SELECT uri, cid, did, collection, json, indexed_at FROM record WHERE collection = "
    <> executor.placeholder(exec, 1)
    <> " ORDER BY indexed_at DESC LIMIT 100"

  executor.query(exec, sql, [Text(collection)], record_decoder())
}

/// Updates an existing record in the database
pub fn update(
  exec: Executor,
  uri: String,
  cid: String,
  json: String,
) -> Result(Nil, DbError) {
  let p1 = executor.placeholder(exec, 1)
  let p2 = executor.placeholder(exec, 2)
  let p3 = executor.placeholder(exec, 3)

  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "UPDATE record SET cid = "
      <> p1
      <> ", json = "
      <> p2
      <> ", indexed_at = datetime('now') WHERE uri = "
      <> p3
    executor.PostgreSQL ->
      "UPDATE record SET cid = "
      <> p1
      <> ", json = "
      <> p2
      <> ", indexed_at = NOW() WHERE uri = "
      <> p3
  }

  executor.exec(exec, sql, [Text(cid), Text(json), Text(uri)])
}

/// Deletes a record by URI (hard delete)
pub fn delete(exec: Executor, uri: String) -> Result(Nil, DbError) {
  let sql = "DELETE FROM record WHERE uri = " <> executor.placeholder(exec, 1)

  executor.exec(exec, sql, [Text(uri)])
}

/// Deletes all records from the database
pub fn delete_all(exec: Executor) -> Result(Nil, DbError) {
  executor.exec(exec, "DELETE FROM record", [])
}

/// Common decoder for Record type
fn record_decoder() -> decode.Decoder(Record) {
  use uri <- decode.field(0, decode.string)
  use cid <- decode.field(1, decode.string)
  use did <- decode.field(2, decode.string)
  use collection <- decode.field(3, decode.string)
  use json <- decode.field(4, decode.string)
  use indexed_at <- decode.field(5, decode.string)
  decode.success(Record(uri:, cid:, did:, collection:, json:, indexed_at:))
}

// ===== Statistics Functions =====

/// Gets statistics for all collections (collection name and record count)
pub fn get_collection_stats(
  exec: Executor,
) -> Result(List(CollectionStat), DbError) {
  let sql =
    "SELECT collection, COUNT(*) as count FROM record GROUP BY collection ORDER BY count DESC"

  let decoder = {
    use collection <- decode.field(0, decode.string)
    use count <- decode.field(1, decode.int)
    decode.success(CollectionStat(collection:, count:))
  }

  executor.query(exec, sql, [], decoder)
}

/// Gets the total number of records in the database
pub fn get_count(exec: Executor) -> Result(Int, DbError) {
  let sql = "SELECT COUNT(*) as count FROM record"

  let decoder = {
    use count <- decode.field(0, decode.int)
    decode.success(count)
  }

  case executor.query(exec, sql, [], decoder) {
    Ok([count]) -> Ok(count)
    Ok(_) -> Ok(0)
    Error(err) -> Error(err)
  }
}

// ===== Complex Query Functions =====

/// Gets the total count of records for a collection with optional where clause
pub fn get_collection_count_with_where(
  exec: Executor,
  collection: String,
  where: Option(where_clause.WhereClause),
) -> Result(Int, DbError) {
  // Check if we need to join with actor table
  let needs_actor_join = case where {
    Some(wc) -> where_clause.requires_actor_join(wc)
    None -> False
  }

  // Build FROM clause with optional LEFT JOIN
  let from_clause = case needs_actor_join {
    True -> "record LEFT JOIN actor ON record.did = actor.did"
    False -> "record"
  }

  // Build WHERE clause parts - start with collection filter
  let mut_where_parts = [
    "record.collection = " <> executor.placeholder(exec, 1),
  ]
  let mut_bind_values: List(Value) = [Text(collection)]

  // Add where clause conditions if provided
  let #(where_parts, bind_values) = case where {
    Some(wc) -> {
      case where_clause.is_clause_empty(wc) {
        True -> #(mut_where_parts, mut_bind_values)
        False -> {
          let #(where_sql, where_params) =
            where_clause.build_where_sql(exec, wc, needs_actor_join)
          let new_where = list.append(mut_where_parts, [where_sql])
          let new_binds = list.append(mut_bind_values, where_params)
          #(new_where, new_binds)
        }
      }
    }
    None -> #(mut_where_parts, mut_bind_values)
  }

  // Build the SQL query
  let sql =
    "SELECT COUNT(*) as count FROM "
    <> from_clause
    <> " WHERE "
    <> string.join(where_parts, " AND ")

  // Execute query
  let decoder = {
    use count <- decode.field(0, decode.int)
    decode.success(count)
  }

  case executor.query(exec, sql, bind_values, decoder) {
    Ok([count]) -> Ok(count)
    Ok(_) -> Ok(0)
    Error(err) -> Error(err)
  }
}

/// Paginated query for records with cursor-based pagination
///
/// Supports both forward (first/after) and backward (last/before) pagination.
/// Returns a tuple of (records, next_cursor, has_next_page, has_previous_page)
pub fn get_by_collection_paginated(
  exec: Executor,
  collection: String,
  first: Option(Int),
  after: Option(String),
  last: Option(Int),
  before: Option(String),
  sort_by: Option(List(#(String, String))),
) -> Result(#(List(Record), Option(String), Bool, Bool), DbError) {
  // Validate pagination arguments
  let #(limit, is_forward, cursor_opt) = case first, last {
    Some(f), None -> #(f, True, after)
    None, Some(l) -> #(l, False, before)
    Some(f), Some(_) ->
      // Both first and last specified - use first
      #(f, True, after)
    None, None ->
      // Neither specified - default to first 50
      #(50, True, None)
  }

  // Default sort order if not specified
  let sort_fields = case sort_by {
    Some(fields) -> fields
    None -> [#("indexed_at", "desc")]
  }

  // Build the ORDER BY clause (no joins in this function, so no prefix needed)
  let order_by_clause = pagination.build_order_by(exec, sort_fields, False)

  // Build WHERE clause parts
  let where_parts = ["collection = " <> executor.placeholder(exec, 1)]
  let bind_values: List(Value) = [Text(collection)]

  // Add cursor condition if present
  let #(final_where_parts, final_bind_values) = case cursor_opt {
    Some(cursor_str) -> {
      case pagination.decode_cursor(cursor_str, sort_by) {
        Ok(decoded_cursor) -> {
          let #(cursor_where, cursor_params) =
            pagination.build_cursor_where_clause(
              exec,
              decoded_cursor,
              sort_by,
              !is_forward,
            )

          let new_where = list.append(where_parts, [cursor_where])
          let new_binds =
            list.append(bind_values, list.map(cursor_params, Text))
          #(new_where, new_binds)
        }
        Error(_) -> #(where_parts, bind_values)
      }
    }
    None -> #(where_parts, bind_values)
  }

  // Fetch limit + 1 to detect if there are more pages
  let fetch_limit = limit + 1

  // Build the SQL query
  let sql =
    "SELECT uri, cid, did, collection, json, indexed_at FROM record WHERE "
    <> string.join(final_where_parts, " AND ")
    <> " ORDER BY "
    <> order_by_clause
    <> " LIMIT "
    <> int.to_string(fetch_limit)

  // Execute query
  use records <- result.try(executor.query(
    exec,
    sql,
    final_bind_values,
    record_decoder(),
  ))

  // Check if there are more results
  let has_more = list.length(records) > limit
  let trimmed_records = case has_more {
    True -> list.take(records, limit)
    False -> records
  }

  // For backward pagination, reverse the results to restore original order
  let final_records = case is_forward {
    True -> trimmed_records
    False -> list.reverse(trimmed_records)
  }

  // Calculate hasNextPage and hasPreviousPage
  let has_next_page = case is_forward {
    True -> has_more
    False -> option.is_some(cursor_opt)
  }

  let has_previous_page = case is_forward {
    True -> option.is_some(cursor_opt)
    False -> has_more
  }

  // Generate next cursor if there are more results
  let next_cursor = case has_more, list.last(final_records) {
    True, Ok(last_record) -> {
      Some(pagination.generate_cursor_from_record(last_record, sort_by))
    }
    _, _ -> None
  }

  Ok(#(final_records, next_cursor, has_next_page, has_previous_page))
}

/// Paginated query for records with cursor-based pagination AND where clause filtering
///
/// Same as get_by_collection_paginated but with an additional where_clause parameter
pub fn get_by_collection_paginated_with_where(
  exec: Executor,
  collection: String,
  first: Option(Int),
  after: Option(String),
  last: Option(Int),
  before: Option(String),
  sort_by: Option(List(#(String, String))),
  where: Option(where_clause.WhereClause),
) -> Result(#(List(Record), Option(String), Bool, Bool), DbError) {
  // Validate pagination arguments
  let #(limit, is_forward, cursor_opt) = case first, last {
    Some(f), None -> #(f, True, after)
    None, Some(l) -> #(l, False, before)
    Some(f), Some(_) -> #(f, True, after)
    None, None -> #(50, True, None)
  }

  // Default sort order if not specified
  let sort_fields = case sort_by {
    Some(fields) -> fields
    None -> [#("indexed_at", "desc")]
  }

  // For backward pagination (last/before), reverse the sort order
  let query_sort_fields = case is_forward {
    True -> sort_fields
    False -> pagination.reverse_sort_fields(sort_fields)
  }

  // Check if we need to join with actor table
  let needs_actor_join = case where {
    Some(wc) -> where_clause.requires_actor_join(wc)
    None -> False
  }

  // Build the ORDER BY clause (with table prefix if doing a join)
  let order_by_clause =
    pagination.build_order_by(exec, query_sort_fields, needs_actor_join)

  // Build FROM clause with optional LEFT JOIN
  let from_clause = case needs_actor_join {
    True -> "record LEFT JOIN actor ON record.did = actor.did"
    False -> "record"
  }

  // Build WHERE clause parts - start with collection filter
  let mut_where_parts = [
    "record.collection = " <> executor.placeholder(exec, 1),
  ]
  let mut_bind_values: List(Value) = [Text(collection)]

  // Add where clause conditions if provided
  let #(where_parts, bind_values) = case where {
    Some(wc) -> {
      case where_clause.is_clause_empty(wc) {
        True -> #(mut_where_parts, mut_bind_values)
        False -> {
          let #(where_sql, where_params) =
            where_clause.build_where_sql(exec, wc, needs_actor_join)
          let new_where = list.append(mut_where_parts, [where_sql])
          let new_binds = list.append(mut_bind_values, where_params)
          #(new_where, new_binds)
        }
      }
    }
    None -> #(mut_where_parts, mut_bind_values)
  }

  // Add cursor condition if present
  let #(final_where_parts, final_bind_values) = case cursor_opt {
    Some(cursor_str) -> {
      case pagination.decode_cursor(cursor_str, sort_by) {
        Ok(decoded_cursor) -> {
          let #(cursor_where, cursor_params) =
            pagination.build_cursor_where_clause(
              exec,
              decoded_cursor,
              sort_by,
              !is_forward,
            )

          let new_where = list.append(where_parts, [cursor_where])
          let new_binds =
            list.append(bind_values, list.map(cursor_params, Text))
          #(new_where, new_binds)
        }
        Error(_) -> #(where_parts, bind_values)
      }
    }
    None -> #(where_parts, bind_values)
  }

  // Fetch limit + 1 to detect if there are more pages
  let fetch_limit = limit + 1

  // Build the SQL query
  let sql =
    "SELECT record.uri, record.cid, record.did, record.collection, record.json, record.indexed_at FROM "
    <> from_clause
    <> " WHERE "
    <> string.join(final_where_parts, " AND ")
    <> " ORDER BY "
    <> order_by_clause
    <> " LIMIT "
    <> int.to_string(fetch_limit)

  // Execute query
  use records <- result.try(executor.query(
    exec,
    sql,
    final_bind_values,
    record_decoder(),
  ))

  // Check if there are more results
  let has_more = list.length(records) > limit
  let trimmed_records = case has_more {
    True -> list.take(records, limit)
    False -> records
  }

  // For backward pagination, reverse the results to restore original order
  let final_records = case is_forward {
    True -> trimmed_records
    False -> list.reverse(trimmed_records)
  }

  // Calculate hasNextPage and hasPreviousPage
  let has_next_page = case is_forward {
    True -> has_more
    False -> option.is_some(cursor_opt)
  }

  let has_previous_page = case is_forward {
    True -> option.is_some(cursor_opt)
    False -> has_more
  }

  // Generate next cursor if there are more results
  let next_cursor = case has_more, list.last(final_records) {
    True, Ok(last_record) -> {
      Some(pagination.generate_cursor_from_record(last_record, sort_by))
    }
    _, _ -> None
  }

  Ok(#(final_records, next_cursor, has_next_page, has_previous_page))
}

/// Get records by a list of URIs (for forward joins / DataLoader)
/// Returns records in any order - caller must group them
pub fn get_by_uris(
  exec: Executor,
  uris: List(String),
) -> Result(List(Record), DbError) {
  case uris {
    [] -> Ok([])
    _ -> {
      // Build placeholders for SQL IN clause
      let placeholders = executor.placeholders(exec, list.length(uris), 1)

      let sql =
        "SELECT uri, cid, did, collection, json, indexed_at FROM record WHERE uri IN ("
        <> placeholders
        <> ")"

      // Convert URIs to Value list
      let params = list.map(uris, Text)

      executor.query(exec, sql, params, record_decoder())
    }
  }
}

/// Get records by reference field (for reverse joins / DataLoader)
/// Finds all records in a collection where a field references one of the parent URIs
/// Note: This does a JSON field extraction, so it may be slow on large datasets
pub fn get_by_reference_field(
  exec: Executor,
  collection: String,
  field_name: String,
  parent_uris: List(String),
) -> Result(List(Record), DbError) {
  case parent_uris {
    [] -> Ok([])
    _ -> {
      let uri_count = list.length(parent_uris)
      // Placeholder 1 is collection
      // Placeholders 2 to uri_count+1 are first set of URIs
      // Placeholders uri_count+2 to 2*uri_count+1 are second set of URIs
      let placeholders1 = executor.placeholders(exec, uri_count, 2)
      let placeholders2 = executor.placeholders(exec, uri_count, uri_count + 2)

      // Use dialect-specific JSON extraction
      let json_field = executor.json_extract(exec, "json", field_name)
      let json_uri_field =
        executor.json_extract_path(exec, "json", [field_name, "uri"])

      let sql =
        "SELECT uri, cid, did, collection, json, indexed_at FROM record WHERE collection = "
        <> executor.placeholder(exec, 1)
        <> " AND ("
        <> json_field
        <> " IN ("
        <> placeholders1
        <> ") OR "
        <> json_uri_field
        <> " IN ("
        <> placeholders2
        <> "))"

      // Build params: collection + parent_uris twice (once for direct match, once for strongRef)
      let params: List(Value) =
        list.flatten([
          [Text(collection)],
          list.map(parent_uris, Text),
          list.map(parent_uris, Text),
        ])

      executor.query(exec, sql, params, record_decoder())
    }
  }
}

/// Get records by reference field with pagination (for reverse joins with connections)
/// Similar to get_by_reference_field but supports cursor-based pagination
/// Returns: (records, next_cursor, has_next_page, has_previous_page, total_count)
pub fn get_by_reference_field_paginated(
  exec: Executor,
  collection: String,
  field_name: String,
  parent_uri: String,
  first: Option(Int),
  after: Option(String),
  last: Option(Int),
  before: Option(String),
  sort_by: Option(List(#(String, String))),
  wc: Option(where_clause.WhereClause),
) -> Result(#(List(Record), Option(String), Bool, Bool, Option(Int)), DbError) {
  // Validate pagination arguments
  let #(limit, is_forward, cursor_opt) = case first, last {
    Some(f), None -> #(f, True, after)
    None, Some(l) -> #(l, False, before)
    Some(f), Some(_) -> #(f, True, after)
    None, None -> #(50, True, None)
  }

  // Default sort order if not specified
  let sort_fields = case sort_by {
    Some(fields) -> fields
    None -> [#("indexed_at", "desc")]
  }

  // For backward pagination (last/before), reverse the sort order
  let query_sort_fields = case is_forward {
    True -> sort_fields
    False -> pagination.reverse_sort_fields(sort_fields)
  }

  // Build the ORDER BY clause
  let order_by_clause =
    pagination.build_order_by(exec, query_sort_fields, False)

  // Use dialect-specific JSON extraction
  let json_field = executor.json_extract(exec, "json", field_name)
  let json_uri_field =
    executor.json_extract_path(exec, "json", [field_name, "uri"])

  // Build WHERE clause parts for reference field matching
  let base_where_parts = [
    "collection = " <> executor.placeholder(exec, 1),
    "("
      <> json_field
      <> " = "
      <> executor.placeholder(exec, 2)
      <> " OR "
      <> json_uri_field
      <> " = "
      <> executor.placeholder(exec, 3)
      <> ")",
  ]
  let base_bind_values: List(Value) = [
    Text(collection),
    Text(parent_uri),
    Text(parent_uri),
  ]

  // Add where clause conditions if present
  let #(with_where_parts, with_where_values) = case wc {
    Some(clause) -> {
      let #(where_sql, where_params) =
        where_clause.build_where_sql(exec, clause, False)
      case where_sql {
        "" -> #(base_where_parts, base_bind_values)
        _ -> #(
          list.append(base_where_parts, [where_sql]),
          list.append(base_bind_values, where_params),
        )
      }
    }
    None -> #(base_where_parts, base_bind_values)
  }

  // Add cursor condition if present
  let #(final_where_parts, final_bind_values) = case cursor_opt {
    Some(cursor_str) -> {
      case pagination.decode_cursor(cursor_str, sort_by) {
        Ok(decoded_cursor) -> {
          let #(cursor_where, cursor_params) =
            pagination.build_cursor_where_clause(
              exec,
              decoded_cursor,
              sort_by,
              !is_forward,
            )

          let new_where = list.append(with_where_parts, [cursor_where])
          let new_binds =
            list.append(with_where_values, list.map(cursor_params, Text))
          #(new_where, new_binds)
        }
        Error(_) -> #(with_where_parts, with_where_values)
      }
    }
    None -> #(with_where_parts, with_where_values)
  }

  // Fetch limit + 1 to detect if there are more pages
  let fetch_limit = limit + 1

  // Build the SQL query
  let sql =
    "SELECT uri, cid, did, collection, json, indexed_at FROM record WHERE "
    <> string.join(final_where_parts, " AND ")
    <> " ORDER BY "
    <> order_by_clause
    <> " LIMIT "
    <> int.to_string(fetch_limit)

  // Execute query
  use records <- result.try(executor.query(
    exec,
    sql,
    final_bind_values,
    record_decoder(),
  ))

  // Check if there are more results
  let has_more = list.length(records) > limit
  let trimmed_records = case has_more {
    True -> list.take(records, limit)
    False -> records
  }

  // For backward pagination, reverse the results to restore original order
  let final_records = case is_forward {
    True -> trimmed_records
    False -> list.reverse(trimmed_records)
  }

  // Calculate hasNextPage and hasPreviousPage
  let has_next_page = case is_forward {
    True -> has_more
    False -> option.is_some(cursor_opt)
  }

  let has_previous_page = case is_forward {
    True -> option.is_some(cursor_opt)
    False -> has_more
  }

  // Generate next cursor if there are more results
  let next_cursor = case has_more, list.last(final_records) {
    True, Ok(last_record) -> {
      Some(pagination.generate_cursor_from_record(last_record, sort_by))
    }
    _, _ -> None
  }

  // Get total count using the WHERE clause (with where conditions, but without cursor conditions)
  let count_sql =
    "SELECT COUNT(*) FROM record WHERE "
    <> string.join(with_where_parts, " AND ")

  let count_decoder = {
    use count <- decode.field(0, decode.int)
    decode.success(count)
  }

  let total_count = case
    executor.query(exec, count_sql, with_where_values, count_decoder)
  {
    Ok([count]) -> Some(count)
    _ -> None
  }

  Ok(#(
    final_records,
    next_cursor,
    has_next_page,
    has_previous_page,
    total_count,
  ))
}

/// Get records by DIDs and collection (for DID joins / DataLoader)
/// Finds all records in a specific collection that belong to any of the given DIDs
/// Uses the idx_record_did_collection index for efficient lookup
pub fn get_by_dids_and_collection(
  exec: Executor,
  dids: List(String),
  collection: String,
) -> Result(List(Record), DbError) {
  case dids {
    [] -> Ok([])
    _ -> {
      let did_count = list.length(dids)
      // Build placeholders for SQL IN clause
      let placeholders = executor.placeholders(exec, did_count, 1)

      let sql =
        "SELECT uri, cid, did, collection, json, indexed_at FROM record WHERE did IN ("
        <> placeholders
        <> ") AND collection = "
        <> executor.placeholder(exec, did_count + 1)
        <> " ORDER BY indexed_at DESC"

      // Build params: DIDs + collection
      let params: List(Value) =
        list.flatten([list.map(dids, Text), [Text(collection)]])

      executor.query(exec, sql, params, record_decoder())
    }
  }
}

/// Get records by DID and collection with pagination (for DID joins with connections)
/// Similar to get_by_dids_and_collection but for a single DID with cursor-based pagination
/// Returns: (records, next_cursor, has_next_page, has_previous_page, total_count)
pub fn get_by_dids_and_collection_paginated(
  exec: Executor,
  did: String,
  collection: String,
  first: Option(Int),
  after: Option(String),
  last: Option(Int),
  before: Option(String),
  sort_by: Option(List(#(String, String))),
  wc: Option(where_clause.WhereClause),
) -> Result(#(List(Record), Option(String), Bool, Bool, Option(Int)), DbError) {
  // Validate pagination arguments
  let #(limit, is_forward, cursor_opt) = case first, last {
    Some(f), None -> #(f, True, after)
    None, Some(l) -> #(l, False, before)
    Some(f), Some(_) -> #(f, True, after)
    None, None -> #(50, True, None)
  }

  // Default sort order if not specified
  let sort_fields = case sort_by {
    Some(fields) -> fields
    None -> [#("indexed_at", "desc")]
  }

  // For backward pagination (last/before), reverse the sort order
  let query_sort_fields = case is_forward {
    True -> sort_fields
    False -> pagination.reverse_sort_fields(sort_fields)
  }

  // Build the ORDER BY clause
  let order_by_clause =
    pagination.build_order_by(exec, query_sort_fields, False)

  // Build WHERE clause parts for DID and collection matching
  let base_where_parts = [
    "did = " <> executor.placeholder(exec, 1),
    "collection = " <> executor.placeholder(exec, 2),
  ]
  let base_bind_values: List(Value) = [Text(did), Text(collection)]

  // Add where clause conditions if present
  let #(with_where_parts, with_where_values) = case wc {
    Some(clause) -> {
      let #(where_sql, where_params) =
        where_clause.build_where_sql(exec, clause, False)
      case where_sql {
        "" -> #(base_where_parts, base_bind_values)
        _ -> #(
          list.append(base_where_parts, [where_sql]),
          list.append(base_bind_values, where_params),
        )
      }
    }
    None -> #(base_where_parts, base_bind_values)
  }

  // Add cursor condition if present
  let #(final_where_parts, final_bind_values) = case cursor_opt {
    Some(cursor_str) -> {
      case pagination.decode_cursor(cursor_str, sort_by) {
        Ok(decoded_cursor) -> {
          let #(cursor_where, cursor_params) =
            pagination.build_cursor_where_clause(
              exec,
              decoded_cursor,
              sort_by,
              !is_forward,
            )

          let new_where = list.append(with_where_parts, [cursor_where])
          let new_binds =
            list.append(with_where_values, list.map(cursor_params, Text))
          #(new_where, new_binds)
        }
        Error(_) -> #(with_where_parts, with_where_values)
      }
    }
    None -> #(with_where_parts, with_where_values)
  }

  // Fetch limit + 1 to detect if there are more pages
  let fetch_limit = limit + 1

  // Build the SQL query
  let sql =
    "SELECT uri, cid, did, collection, json, indexed_at FROM record WHERE "
    <> string.join(final_where_parts, " AND ")
    <> " ORDER BY "
    <> order_by_clause
    <> " LIMIT "
    <> int.to_string(fetch_limit)

  // Execute query
  use records <- result.try(executor.query(
    exec,
    sql,
    final_bind_values,
    record_decoder(),
  ))

  // Check if there are more results
  let has_more = list.length(records) > limit
  let trimmed_records = case has_more {
    True -> list.take(records, limit)
    False -> records
  }

  // For backward pagination, reverse the results to restore original order
  let final_records = case is_forward {
    True -> trimmed_records
    False -> list.reverse(trimmed_records)
  }

  // Calculate hasNextPage and hasPreviousPage
  let has_next_page = case is_forward {
    True -> has_more
    False -> option.is_some(cursor_opt)
  }

  let has_previous_page = case is_forward {
    True -> option.is_some(cursor_opt)
    False -> has_more
  }

  // Generate next cursor if there are more results
  let next_cursor = case has_more, list.last(final_records) {
    True, Ok(last_record) -> {
      Some(pagination.generate_cursor_from_record(last_record, sort_by))
    }
    _, _ -> None
  }

  // Get total count using the WHERE clause (with where conditions, but without cursor conditions)
  let count_sql =
    "SELECT COUNT(*) FROM record WHERE "
    <> string.join(with_where_parts, " AND ")

  let count_decoder = {
    use count <- decode.field(0, decode.int)
    decode.success(count)
  }

  let total_count = case
    executor.query(exec, count_sql, with_where_values, count_decoder)
  {
    Ok([count]) -> Some(count)
    _ -> None
  }

  Ok(#(
    final_records,
    next_cursor,
    has_next_page,
    has_previous_page,
    total_count,
  ))
}
