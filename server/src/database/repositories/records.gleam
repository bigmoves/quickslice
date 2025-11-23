import cursor
import database/queries/pagination
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
import sqlight
import where_clause

// ===== Helper Functions =====

/// Gets existing URIs and their CIDs from the database
/// Returns a Dict mapping URI -> CID for records that exist
fn get_existing_cids(
  conn: sqlight.Connection,
  uris: List(String),
) -> Result(Dict(String, String), sqlight.Error) {
  case uris {
    [] -> Ok(dict.new())
    _ -> {
      // Process in batches to avoid SQL parameter limits (max 999 params)
      let batch_size = 900
      let batches = list.sized_chunk(uris, batch_size)

      // Process each batch and merge results
      use accumulated_dict <- result.try(
        list.try_fold(batches, dict.new(), fn(acc_dict, batch) {
          // Build placeholders for SQL IN clause
          let placeholders =
            list.map(batch, fn(_) { "?" })
            |> string.join(", ")

          let sql = "
            SELECT uri, cid
            FROM record
            WHERE uri IN (" <> placeholders <> ")
          "

          // Convert URIs to sqlight.Value list
          let params = list.map(batch, sqlight.text)

          let decoder = {
            use uri <- decode.field(0, decode.string)
            use cid <- decode.field(1, decode.string)
            decode.success(#(uri, cid))
          }

          use results <- result.try(sqlight.query(
            sql,
            on: conn,
            with: params,
            expecting: decoder,
          ))

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
  conn: sqlight.Connection,
  cids: List(String),
) -> Result(List(String), sqlight.Error) {
  case cids {
    [] -> Ok([])
    _ -> {
      // Process in batches to avoid SQL parameter limits (max 999 params)
      let batch_size = 900
      let batches = list.sized_chunk(cids, batch_size)

      // Process each batch and collect results
      use all_results <- result.try(
        list.try_fold(batches, [], fn(acc_results, batch) {
          // Build placeholders for SQL IN clause
          let placeholders =
            list.map(batch, fn(_) { "?" })
            |> string.join(", ")

          let sql = "
            SELECT cid
            FROM record
            WHERE cid IN (" <> placeholders <> ")
          "

          let cid_decoder = {
            use cid <- decode.field(0, decode.string)
            decode.success(cid)
          }

          use results <- result.try(sqlight.query(
            sql,
            on: conn,
            with: list.map(batch, sqlight.text),
            expecting: cid_decoder,
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
  conn: sqlight.Connection,
  uri: String,
  cid: String,
  did: String,
  collection: String,
  json: String,
) -> Result(InsertResult, sqlight.Error) {
  // Check if this CID already exists in the database
  use existing_cids <- result.try(get_existing_cids(conn, [uri]))

  case dict.get(existing_cids, uri) {
    // URI exists with same CID - skip update (content unchanged)
    Ok(existing_cid) if existing_cid == cid -> Ok(Skipped)
    // URI exists with different CID - proceed with update
    // URI doesn't exist - proceed with insert
    _ -> {
      let sql =
        "
        INSERT INTO record (uri, cid, did, collection, json)
        VALUES (?, ?, ?, ?, ?)
        ON CONFLICT(uri) DO UPDATE SET
          cid = excluded.cid,
          json = excluded.json,
          indexed_at = datetime('now')
      "

      use _ <- result.try(sqlight.query(
        sql,
        on: conn,
        with: [
          sqlight.text(uri),
          sqlight.text(cid),
          sqlight.text(did),
          sqlight.text(collection),
          sqlight.text(json),
        ],
        expecting: decode.string,
      ))
      Ok(Inserted)
    }
  }
}

/// Batch inserts or updates multiple records in the database
/// More efficient than individual inserts for large datasets
/// Filters out records where CID already exists or is unchanged
pub fn batch_insert(
  conn: sqlight.Connection,
  records: List(Record),
) -> Result(Nil, sqlight.Error) {
  case records {
    [] -> Ok(Nil)
    _ -> {
      // Get all URIs from the incoming records
      let uris = list.map(records, fn(record) { record.uri })

      // Fetch existing CIDs for these URIs (batched to avoid SQL parameter limits)
      use existing_cids <- result.try(get_existing_cids(conn, uris))

      // Get all CIDs that already exist in the database (for any URI)
      // Check in batches to avoid exceeding SQL parameter limits
      let all_incoming_cids =
        list.map(records, fn(record) { record.cid })
        |> list.unique()

      use existing_cids_in_db <- result.try(get_existing_cids_batch(
        conn,
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
          // SQLite has a default limit of 999 parameters
          // Each record uses 5 parameters, so we can safely do 100 records at a time (500 params)
          let batch_size = 100

          list.sized_chunk(filtered_records, batch_size)
          |> list.try_each(fn(batch) {
            // Build the SQL with multiple value sets
            let value_placeholders =
              list.repeat("(?, ?, ?, ?, ?)", list.length(batch))
              |> string.join(", ")

            let sql = "
              INSERT INTO record (uri, cid, did, collection, json)
              VALUES " <> value_placeholders <> "
              ON CONFLICT(uri) DO UPDATE SET
                cid = excluded.cid,
                json = excluded.json,
                indexed_at = datetime('now')
            "

            // Flatten all record parameters into a single list
            let params =
              list.flat_map(batch, fn(record) {
                [
                  sqlight.text(record.uri),
                  sqlight.text(record.cid),
                  sqlight.text(record.did),
                  sqlight.text(record.collection),
                  sqlight.text(record.json),
                ]
              })

            use _ <- result.try(sqlight.query(
              sql,
              on: conn,
              with: params,
              expecting: decode.string,
            ))
            Ok(Nil)
          })
        }
      }
    }
  }
}

/// Gets a record by URI
pub fn get(
  conn: sqlight.Connection,
  uri: String,
) -> Result(List(Record), sqlight.Error) {
  let sql =
    "
    SELECT uri, cid, did, collection, json, indexed_at
    FROM record
    WHERE uri = ?
  "

  let decoder = {
    use uri <- decode.field(0, decode.string)
    use cid <- decode.field(1, decode.string)
    use did <- decode.field(2, decode.string)
    use collection <- decode.field(3, decode.string)
    use json <- decode.field(4, decode.string)
    use indexed_at <- decode.field(5, decode.string)
    decode.success(Record(uri:, cid:, did:, collection:, json:, indexed_at:))
  }

  sqlight.query(sql, on: conn, with: [sqlight.text(uri)], expecting: decoder)
}

/// Gets all records for a specific DID
pub fn get_by_did(
  conn: sqlight.Connection,
  did: String,
) -> Result(List(Record), sqlight.Error) {
  let sql =
    "
    SELECT uri, cid, did, collection, json, indexed_at
    FROM record
    WHERE did = ?
    ORDER BY indexed_at DESC
  "

  let decoder = {
    use uri <- decode.field(0, decode.string)
    use cid <- decode.field(1, decode.string)
    use did <- decode.field(2, decode.string)
    use collection <- decode.field(3, decode.string)
    use json <- decode.field(4, decode.string)
    use indexed_at <- decode.field(5, decode.string)
    decode.success(Record(uri:, cid:, did:, collection:, json:, indexed_at:))
  }

  sqlight.query(sql, on: conn, with: [sqlight.text(did)], expecting: decoder)
}

/// Gets all records for a specific collection
pub fn get_by_collection(
  conn: sqlight.Connection,
  collection: String,
) -> Result(List(Record), sqlight.Error) {
  let sql =
    "
    SELECT uri, cid, did, collection, json, indexed_at
    FROM record
    WHERE collection = ?
    ORDER BY indexed_at DESC
    LIMIT 100
  "

  let decoder = {
    use uri <- decode.field(0, decode.string)
    use cid <- decode.field(1, decode.string)
    use did <- decode.field(2, decode.string)
    use collection <- decode.field(3, decode.string)
    use json <- decode.field(4, decode.string)
    use indexed_at <- decode.field(5, decode.string)
    decode.success(Record(uri:, cid:, did:, collection:, json:, indexed_at:))
  }

  sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(collection)],
    expecting: decoder,
  )
}

/// Updates an existing record in the database
pub fn update(
  conn: sqlight.Connection,
  uri: String,
  cid: String,
  json: String,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    UPDATE record
    SET cid = ?, json = ?, indexed_at = datetime('now')
    WHERE uri = ?
  "

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(cid), sqlight.text(json), sqlight.text(uri)],
    expecting: decode.string,
  ))
  Ok(Nil)
}

/// Deletes a record by URI (hard delete)
pub fn delete(
  conn: sqlight.Connection,
  uri: String,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    DELETE FROM record
    WHERE uri = ?
  "

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(uri)],
    expecting: decode.string,
  ))
  Ok(Nil)
}

/// Deletes all records from the database
pub fn delete_all(conn: sqlight.Connection) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM record"

  sqlight.exec(sql, conn)
  |> result.map(fn(_) { Nil })
}

// ===== Statistics Functions =====

/// Gets statistics for all collections (collection name and record count)
pub fn get_collection_stats(
  conn: sqlight.Connection,
) -> Result(List(CollectionStat), sqlight.Error) {
  let sql =
    "
    SELECT collection, COUNT(*) as count
    FROM record
    GROUP BY collection
    ORDER BY count DESC
  "

  let decoder = {
    use collection <- decode.field(0, decode.string)
    use count <- decode.field(1, decode.int)
    decode.success(CollectionStat(collection:, count:))
  }

  sqlight.query(sql, on: conn, with: [], expecting: decoder)
}

/// Gets the total number of records in the database
pub fn get_count(conn: sqlight.Connection) -> Result(Int, sqlight.Error) {
  let sql =
    "
    SELECT COUNT(*) as count
    FROM record
  "

  let decoder = {
    use count <- decode.field(0, decode.int)
    decode.success(count)
  }

  case sqlight.query(sql, on: conn, with: [], expecting: decoder) {
    Ok([count]) -> Ok(count)
    Ok(_) -> Ok(0)
    Error(err) -> Error(err)
  }
}

// ===== Complex Query Functions =====

/// Gets the total count of records for a collection with optional where clause
pub fn get_collection_count_with_where(
  conn: sqlight.Connection,
  collection: String,
  where: Option(where_clause.WhereClause),
) -> Result(Int, sqlight.Error) {
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
  let mut_where_parts = ["record.collection = ?"]
  let mut_bind_values = [sqlight.text(collection)]

  // Add where clause conditions if provided
  let #(where_parts, bind_values) = case where {
    Some(wc) -> {
      case where_clause.is_clause_empty(wc) {
        True -> #(mut_where_parts, mut_bind_values)
        False -> {
          let #(where_sql, where_params) =
            where_clause.build_where_sql(wc, needs_actor_join)
          let new_where = list.append(mut_where_parts, [where_sql])
          let new_binds = list.append(mut_bind_values, where_params)
          #(new_where, new_binds)
        }
      }
    }
    None -> #(mut_where_parts, mut_bind_values)
  }

  // Build the SQL query
  let sql = "
    SELECT COUNT(*) as count
    FROM " <> from_clause <> "
    WHERE " <> string.join(where_parts, " AND ")

  // Execute query
  let decoder = {
    use count <- decode.field(0, decode.int)
    decode.success(count)
  }

  case sqlight.query(sql, on: conn, with: bind_values, expecting: decoder) {
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
  conn: sqlight.Connection,
  collection: String,
  first: Option(Int),
  after: Option(String),
  last: Option(Int),
  before: Option(String),
  sort_by: Option(List(#(String, String))),
) -> Result(#(List(Record), Option(String), Bool, Bool), sqlight.Error) {
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
  let order_by_clause = pagination.build_order_by(sort_fields, False)

  // Build WHERE clause parts
  let where_parts = ["collection = ?"]
  let bind_values = [sqlight.text(collection)]

  // Add cursor condition if present
  let #(final_where_parts, final_bind_values) = case cursor_opt {
    Some(cursor_str) -> {
      case cursor.decode_cursor(cursor_str, sort_by) {
        Ok(decoded_cursor) -> {
          let #(cursor_where, cursor_params) =
            cursor.build_cursor_where_clause(
              decoded_cursor,
              sort_by,
              !is_forward,
            )

          let new_where = list.append(where_parts, [cursor_where])
          let new_binds =
            list.append(bind_values, list.map(cursor_params, sqlight.text))
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
  let sql = "
    SELECT uri, cid, did, collection, json, indexed_at
    FROM record
    WHERE " <> string.join(final_where_parts, " AND ") <> "
    ORDER BY " <> order_by_clause <> "
    LIMIT " <> int.to_string(fetch_limit)

  // Execute query
  let decoder = {
    use uri <- decode.field(0, decode.string)
    use cid <- decode.field(1, decode.string)
    use did <- decode.field(2, decode.string)
    use collection <- decode.field(3, decode.string)
    use json <- decode.field(4, decode.string)
    use indexed_at <- decode.field(5, decode.string)
    decode.success(Record(uri:, cid:, did:, collection:, json:, indexed_at:))
  }

  use records <- result.try(sqlight.query(
    sql,
    on: conn,
    with: final_bind_values,
    expecting: decoder,
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
      let record_like = pagination.record_to_record_like(last_record)
      Some(cursor.generate_cursor_from_record(record_like, sort_by))
    }
    _, _ -> None
  }

  Ok(#(final_records, next_cursor, has_next_page, has_previous_page))
}

/// Paginated query for records with cursor-based pagination AND where clause filtering
///
/// Same as get_by_collection_paginated but with an additional where_clause parameter
pub fn get_by_collection_paginated_with_where(
  conn: sqlight.Connection,
  collection: String,
  first: Option(Int),
  after: Option(String),
  last: Option(Int),
  before: Option(String),
  sort_by: Option(List(#(String, String))),
  where: Option(where_clause.WhereClause),
) -> Result(#(List(Record), Option(String), Bool, Bool), sqlight.Error) {
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
    pagination.build_order_by(query_sort_fields, needs_actor_join)

  // Build FROM clause with optional LEFT JOIN
  let from_clause = case needs_actor_join {
    True -> "record LEFT JOIN actor ON record.did = actor.did"
    False -> "record"
  }

  // Build WHERE clause parts - start with collection filter
  let mut_where_parts = ["record.collection = ?"]
  let mut_bind_values = [sqlight.text(collection)]

  // Add where clause conditions if provided
  let #(where_parts, bind_values) = case where {
    Some(wc) -> {
      case where_clause.is_clause_empty(wc) {
        True -> #(mut_where_parts, mut_bind_values)
        False -> {
          let #(where_sql, where_params) =
            where_clause.build_where_sql(wc, needs_actor_join)
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
      case cursor.decode_cursor(cursor_str, sort_by) {
        Ok(decoded_cursor) -> {
          let #(cursor_where, cursor_params) =
            cursor.build_cursor_where_clause(
              decoded_cursor,
              sort_by,
              !is_forward,
            )

          let new_where = list.append(where_parts, [cursor_where])
          let new_binds =
            list.append(bind_values, list.map(cursor_params, sqlight.text))
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
  let sql = "
    SELECT record.uri, record.cid, record.did, record.collection, record.json, record.indexed_at
    FROM " <> from_clause <> "
    WHERE " <> string.join(final_where_parts, " AND ") <> "
    ORDER BY " <> order_by_clause <> "
    LIMIT " <> int.to_string(fetch_limit)

  // Execute query
  let decoder = {
    use uri <- decode.field(0, decode.string)
    use cid <- decode.field(1, decode.string)
    use did <- decode.field(2, decode.string)
    use collection <- decode.field(3, decode.string)
    use json <- decode.field(4, decode.string)
    use indexed_at <- decode.field(5, decode.string)
    decode.success(Record(uri:, cid:, did:, collection:, json:, indexed_at:))
  }

  use records <- result.try(sqlight.query(
    sql,
    on: conn,
    with: final_bind_values,
    expecting: decoder,
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
      let record_like = pagination.record_to_record_like(last_record)
      Some(cursor.generate_cursor_from_record(record_like, sort_by))
    }
    _, _ -> None
  }

  Ok(#(final_records, next_cursor, has_next_page, has_previous_page))
}

/// Get records by a list of URIs (for forward joins / DataLoader)
/// Returns records in any order - caller must group them
pub fn get_by_uris(
  conn: sqlight.Connection,
  uris: List(String),
) -> Result(List(Record), sqlight.Error) {
  case uris {
    [] -> Ok([])
    _ -> {
      // Build placeholders for SQL IN clause
      let placeholders =
        list.map(uris, fn(_) { "?" })
        |> string.join(", ")

      let sql = "
        SELECT uri, cid, did, collection, json, indexed_at
        FROM record
        WHERE uri IN (" <> placeholders <> ")
      "

      // Convert URIs to sqlight.Value list
      let params = list.map(uris, sqlight.text)

      let decoder = {
        use uri <- decode.field(0, decode.string)
        use cid <- decode.field(1, decode.string)
        use did <- decode.field(2, decode.string)
        use collection <- decode.field(3, decode.string)
        use json <- decode.field(4, decode.string)
        use indexed_at <- decode.field(5, decode.string)
        decode.success(Record(uri:, cid:, did:, collection:, json:, indexed_at:))
      }

      sqlight.query(sql, on: conn, with: params, expecting: decoder)
    }
  }
}

/// Get records by reference field (for reverse joins / DataLoader)
/// Finds all records in a collection where a field references one of the parent URIs
/// Note: This does a JSON field extraction, so it may be slow on large datasets
pub fn get_by_reference_field(
  conn: sqlight.Connection,
  collection: String,
  field_name: String,
  parent_uris: List(String),
) -> Result(List(Record), sqlight.Error) {
  case parent_uris {
    [] -> Ok([])
    _ -> {
      // Build placeholders for SQL IN clause
      let placeholders =
        list.map(parent_uris, fn(_) { "?" })
        |> string.join(", ")

      // Use SQLite JSON extraction to find records where field_name matches parent URIs
      // This supports both simple string fields and strongRef objects with a "uri" field
      let sql = "
        SELECT uri, cid, did, collection, json, indexed_at
        FROM record
        WHERE collection = ?
        AND (
          json_extract(json, '$." <> field_name <> "') IN (" <> placeholders <> ")
          OR json_extract(json, '$." <> field_name <> ".uri') IN (" <> placeholders <> ")
        )
      "

      // Build params: collection + parent_uris twice (once for direct match, once for strongRef)
      let params =
        list.flatten([
          [sqlight.text(collection)],
          list.map(parent_uris, sqlight.text),
          list.map(parent_uris, sqlight.text),
        ])

      let decoder = {
        use uri <- decode.field(0, decode.string)
        use cid <- decode.field(1, decode.string)
        use did <- decode.field(2, decode.string)
        use collection <- decode.field(3, decode.string)
        use json <- decode.field(4, decode.string)
        use indexed_at <- decode.field(5, decode.string)
        decode.success(Record(uri:, cid:, did:, collection:, json:, indexed_at:))
      }

      sqlight.query(sql, on: conn, with: params, expecting: decoder)
    }
  }
}

/// Get records by reference field with pagination (for reverse joins with connections)
/// Similar to get_by_reference_field but supports cursor-based pagination
/// Returns: (records, next_cursor, has_next_page, has_previous_page, total_count)
pub fn get_by_reference_field_paginated(
  conn: sqlight.Connection,
  collection: String,
  field_name: String,
  parent_uri: String,
  first: Option(Int),
  after: Option(String),
  last: Option(Int),
  before: Option(String),
  sort_by: Option(List(#(String, String))),
  where_clause: Option(where_clause.WhereClause),
) -> Result(
  #(List(Record), Option(String), Bool, Bool, Option(Int)),
  sqlight.Error,
) {
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
  let order_by_clause = pagination.build_order_by(query_sort_fields, False)

  // Build WHERE clause parts for reference field matching
  let base_where_parts = [
    "collection = ?",
    "(json_extract(json, '$."
      <> field_name
      <> "') = ? OR json_extract(json, '$."
      <> field_name
      <> ".uri') = ?)",
  ]
  let base_bind_values = [
    sqlight.text(collection),
    sqlight.text(parent_uri),
    sqlight.text(parent_uri),
  ]

  // Add where clause conditions if present
  let #(with_where_parts, with_where_values) = case where_clause {
    Some(clause) -> {
      let #(where_sql, where_params) =
        where_clause.build_where_sql(clause, False)
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
      case cursor.decode_cursor(cursor_str, sort_by) {
        Ok(decoded_cursor) -> {
          let #(cursor_where, cursor_params) =
            cursor.build_cursor_where_clause(
              decoded_cursor,
              sort_by,
              !is_forward,
            )

          let new_where = list.append(with_where_parts, [cursor_where])
          let new_binds =
            list.append(
              with_where_values,
              list.map(cursor_params, sqlight.text),
            )
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
  let sql = "
    SELECT uri, cid, did, collection, json, indexed_at
    FROM record
    WHERE " <> string.join(final_where_parts, " AND ") <> "
    ORDER BY " <> order_by_clause <> "
    LIMIT " <> int.to_string(fetch_limit)

  // Execute query
  let decoder = {
    use uri <- decode.field(0, decode.string)
    use cid <- decode.field(1, decode.string)
    use did <- decode.field(2, decode.string)
    use collection <- decode.field(3, decode.string)
    use json <- decode.field(4, decode.string)
    use indexed_at <- decode.field(5, decode.string)
    decode.success(Record(uri:, cid:, did:, collection:, json:, indexed_at:))
  }

  use records <- result.try(sqlight.query(
    sql,
    on: conn,
    with: final_bind_values,
    expecting: decoder,
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
      let record_like = pagination.record_to_record_like(last_record)
      Some(cursor.generate_cursor_from_record(record_like, sort_by))
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
    sqlight.query(
      count_sql,
      on: conn,
      with: with_where_values,
      expecting: count_decoder,
    )
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
  conn: sqlight.Connection,
  dids: List(String),
  collection: String,
) -> Result(List(Record), sqlight.Error) {
  case dids {
    [] -> Ok([])
    _ -> {
      // Build placeholders for SQL IN clause
      let placeholders =
        list.map(dids, fn(_) { "?" })
        |> string.join(", ")

      let sql = "
        SELECT uri, cid, did, collection, json, indexed_at
        FROM record
        WHERE did IN (" <> placeholders <> ")
        AND collection = ?
        ORDER BY indexed_at DESC
      "

      // Build params: DIDs + collection
      let params =
        list.flatten([
          list.map(dids, sqlight.text),
          [sqlight.text(collection)],
        ])

      let decoder = {
        use uri <- decode.field(0, decode.string)
        use cid <- decode.field(1, decode.string)
        use did <- decode.field(2, decode.string)
        use collection <- decode.field(3, decode.string)
        use json <- decode.field(4, decode.string)
        use indexed_at <- decode.field(5, decode.string)
        decode.success(Record(uri:, cid:, did:, collection:, json:, indexed_at:))
      }

      sqlight.query(sql, on: conn, with: params, expecting: decoder)
    }
  }
}

/// Get records by DID and collection with pagination (for DID joins with connections)
/// Similar to get_by_dids_and_collection but for a single DID with cursor-based pagination
/// Returns: (records, next_cursor, has_next_page, has_previous_page, total_count)
pub fn get_by_dids_and_collection_paginated(
  conn: sqlight.Connection,
  did: String,
  collection: String,
  first: Option(Int),
  after: Option(String),
  last: Option(Int),
  before: Option(String),
  sort_by: Option(List(#(String, String))),
  where_clause: Option(where_clause.WhereClause),
) -> Result(
  #(List(Record), Option(String), Bool, Bool, Option(Int)),
  sqlight.Error,
) {
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
  let order_by_clause = pagination.build_order_by(query_sort_fields, False)

  // Build WHERE clause parts for DID and collection matching
  let base_where_parts = ["did = ?", "collection = ?"]
  let base_bind_values = [sqlight.text(did), sqlight.text(collection)]

  // Add where clause conditions if present
  let #(with_where_parts, with_where_values) = case where_clause {
    Some(clause) -> {
      let #(where_sql, where_params) =
        where_clause.build_where_sql(clause, False)
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
      case cursor.decode_cursor(cursor_str, sort_by) {
        Ok(decoded_cursor) -> {
          let #(cursor_where, cursor_params) =
            cursor.build_cursor_where_clause(
              decoded_cursor,
              sort_by,
              !is_forward,
            )

          let new_where = list.append(with_where_parts, [cursor_where])
          let new_binds =
            list.append(
              with_where_values,
              list.map(cursor_params, sqlight.text),
            )
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
  let sql = "
    SELECT uri, cid, did, collection, json, indexed_at
    FROM record
    WHERE " <> string.join(final_where_parts, " AND ") <> "
    ORDER BY " <> order_by_clause <> "
    LIMIT " <> int.to_string(fetch_limit)

  // Execute query
  let decoder = {
    use uri <- decode.field(0, decode.string)
    use cid <- decode.field(1, decode.string)
    use did <- decode.field(2, decode.string)
    use collection <- decode.field(3, decode.string)
    use json <- decode.field(4, decode.string)
    use indexed_at <- decode.field(5, decode.string)
    decode.success(Record(uri:, cid:, did:, collection:, json:, indexed_at:))
  }

  use records <- result.try(sqlight.query(
    sql,
    on: conn,
    with: final_bind_values,
    expecting: decoder,
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
      let record_like = pagination.record_to_record_like(last_record)
      Some(cursor.generate_cursor_from_record(record_like, sort_by))
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
    sqlight.query(
      count_sql,
      on: conn,
      with: with_where_values,
      expecting: count_decoder,
    )
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
