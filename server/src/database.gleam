import cursor
import gleam/dynamic/decode
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import sqlight
import where_clause

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

pub type Actor {
  Actor(did: String, handle: String, indexed_at: String)
}

pub type Lexicon {
  Lexicon(id: String, json: String, created_at: String)
}

/// Opens a connection to the SQLite database
pub fn connect(path: String) -> Result(sqlight.Connection, sqlight.Error) {
  sqlight.open(path)
}

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

/// Initializes the database with all required tables
pub fn initialize(path: String) -> Result(sqlight.Connection, sqlight.Error) {
  use conn <- result.try(connect(path))
  use _ <- result.try(create_record_table(conn))
  use _ <- result.try(create_actor_table(conn))
  use _ <- result.try(create_lexicon_table(conn))

  io.println("✅ Database initialized at: " <> path)
  Ok(conn)
}

/// Inserts or updates a record in the database
pub fn insert_record(
  conn: sqlight.Connection,
  uri: String,
  cid: String,
  did: String,
  collection: String,
  json: String,
) -> Result(Nil, sqlight.Error) {
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
  Ok(Nil)
}

/// Gets a record by URI
pub fn get_record(
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
pub fn get_records_by_did(
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
pub fn get_records_by_collection(
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

/// Deletes a record by URI (hard delete)
pub fn delete_record(
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

/// Updates an existing record in the database
pub fn update_record(
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

/// Inserts or updates an actor in the database
pub fn upsert_actor(
  conn: sqlight.Connection,
  did: String,
  handle: String,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO actor (did, handle, indexed_at)
    VALUES (?, ?, datetime('now'))
    ON CONFLICT(did) DO UPDATE SET
      handle = excluded.handle,
      indexed_at = excluded.indexed_at
  "

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(did), sqlight.text(handle)],
    expecting: decode.string,
  ))
  Ok(Nil)
}

/// Gets an actor by DID
pub fn get_actor(
  conn: sqlight.Connection,
  did: String,
) -> Result(List(Actor), sqlight.Error) {
  let sql =
    "
    SELECT did, handle, indexed_at
    FROM actor
    WHERE did = ?
  "

  let decoder = {
    use did <- decode.field(0, decode.string)
    use handle <- decode.field(1, decode.string)
    use indexed_at <- decode.field(2, decode.string)
    decode.success(Actor(did:, handle:, indexed_at:))
  }

  sqlight.query(sql, on: conn, with: [sqlight.text(did)], expecting: decoder)
}

/// Gets an actor by handle
pub fn get_actor_by_handle(
  conn: sqlight.Connection,
  handle: String,
) -> Result(List(Actor), sqlight.Error) {
  let sql =
    "
    SELECT did, handle, indexed_at
    FROM actor
    WHERE handle = ?
  "

  let decoder = {
    use did <- decode.field(0, decode.string)
    use handle <- decode.field(1, decode.string)
    use indexed_at <- decode.field(2, decode.string)
    decode.success(Actor(did:, handle:, indexed_at:))
  }

  sqlight.query(sql, on: conn, with: [sqlight.text(handle)], expecting: decoder)
}

pub type CollectionStat {
  CollectionStat(collection: String, count: Int)
}

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

/// Gets the total number of actors in the database
pub fn get_actor_count(conn: sqlight.Connection) -> Result(Int, sqlight.Error) {
  let sql =
    "
    SELECT COUNT(*) as count
    FROM actor
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

/// Checks if a lexicon exists for a given collection NSID
/// First checks the dedicated lexicon table, then falls back to record table
pub fn has_lexicon_for_collection(
  conn: sqlight.Connection,
  collection: String,
) -> Result(Bool, sqlight.Error) {
  // First check lexicon table (direct lookup is faster)
  case has_lexicon(conn, collection) {
    Ok(True) -> Ok(True)
    Ok(False) -> {
      // Fall back to searching record table for backward compatibility
      let sql =
        "
        SELECT COUNT(*) as count
        FROM record
        WHERE collection = 'com.atproto.lexicon.schema'
        AND json LIKE ?
      "

      let decoder = {
        use count <- decode.field(0, decode.int)
        decode.success(count)
      }

      let pattern = "%" <> collection <> "%"

      case
        sqlight.query(
          sql,
          on: conn,
          with: [sqlight.text(pattern)],
          expecting: decoder,
        )
      {
        Ok([count]) -> Ok(count > 0)
        Ok(_) -> Ok(False)
        Error(err) -> Error(err)
      }
    }
    Error(err) -> Error(err)
  }
}

/// Inserts or updates a lexicon in the database
pub fn insert_lexicon(
  conn: sqlight.Connection,
  id: String,
  json: String,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO lexicon (id, json, created_at)
    VALUES (?, ?, datetime('now'))
    ON CONFLICT(id) DO UPDATE SET
      json = excluded.json,
      created_at = datetime('now')
  "

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(id), sqlight.text(json)],
    expecting: decode.string,
  ))
  Ok(Nil)
}

/// Gets a lexicon by ID
pub fn get_lexicon(
  conn: sqlight.Connection,
  id: String,
) -> Result(List(Lexicon), sqlight.Error) {
  let sql =
    "
    SELECT id, json, created_at
    FROM lexicon
    WHERE id = ?
  "

  let decoder = {
    use id <- decode.field(0, decode.string)
    use json <- decode.field(1, decode.string)
    use created_at <- decode.field(2, decode.string)
    decode.success(Lexicon(id:, json:, created_at:))
  }

  sqlight.query(sql, on: conn, with: [sqlight.text(id)], expecting: decoder)
}

/// Gets all lexicons from the database
pub fn get_all_lexicons(
  conn: sqlight.Connection,
) -> Result(List(Lexicon), sqlight.Error) {
  let sql =
    "
    SELECT id, json, created_at
    FROM lexicon
    ORDER BY created_at DESC
  "

  let decoder = {
    use id <- decode.field(0, decode.string)
    use json <- decode.field(1, decode.string)
    use created_at <- decode.field(2, decode.string)
    decode.success(Lexicon(id:, json:, created_at:))
  }

  sqlight.query(sql, on: conn, with: [], expecting: decoder)
}

/// Checks if a lexicon exists by ID
pub fn has_lexicon(
  conn: sqlight.Connection,
  id: String,
) -> Result(Bool, sqlight.Error) {
  let sql =
    "
    SELECT COUNT(*) as count
    FROM lexicon
    WHERE id = ?
  "

  let decoder = {
    use count <- decode.field(0, decode.int)
    decode.success(count)
  }

  case
    sqlight.query(sql, on: conn, with: [sqlight.text(id)], expecting: decoder)
  {
    Ok([count]) -> Ok(count > 0)
    Ok(_) -> Ok(False)
    Error(err) -> Error(err)
  }
}

/// Gets the total number of lexicons in the database
pub fn get_lexicon_count(conn: sqlight.Connection) -> Result(Int, sqlight.Error) {
  let sql =
    "
    SELECT COUNT(*) as count
    FROM lexicon
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

/// Gets all lexicons that are of type "record" (collections)
pub fn get_record_type_lexicons(
  conn: sqlight.Connection,
) -> Result(List(Lexicon), sqlight.Error) {
  let sql =
    "
    SELECT id, json, created_at
    FROM lexicon
    WHERE json LIKE '%\"type\":\"record\"%'
       OR json LIKE '%\"type\": \"record\"%'
    ORDER BY id ASC
  "

  let decoder = {
    use id <- decode.field(0, decode.string)
    use json <- decode.field(1, decode.string)
    use created_at <- decode.field(2, decode.string)
    decode.success(Lexicon(id:, json:, created_at:))
  }

  sqlight.query(sql, on: conn, with: [], expecting: decoder)
}

/// Paginated query for records with cursor-based pagination
///
/// Supports both forward (first/after) and backward (last/before) pagination.
/// Returns a tuple of (records, next_cursor, has_next_page, has_previous_page)
pub fn get_records_by_collection_paginated(
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
  let order_by_clause = build_order_by(sort_fields, False)

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
            list.append(
              bind_values,
              list.map(cursor_params, sqlight.text),
            )
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
    "
    SELECT uri, cid, did, collection, json, indexed_at
    FROM record
    WHERE "
    <> string.join(final_where_parts, " AND ")
    <> "
    ORDER BY "
    <> order_by_clause
    <> "
    LIMIT "
    <> int.to_string(fetch_limit)

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
  let final_records = case has_more {
    True -> list.take(records, limit)
    False -> records
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
      let record_like = record_to_record_like(last_record)
      Some(cursor.generate_cursor_from_record(record_like, sort_by))
    }
    _, _ -> None
  }

  Ok(#(final_records, next_cursor, has_next_page, has_previous_page))
}

/// Paginated query for records with cursor-based pagination AND where clause filtering
///
/// Same as get_records_by_collection_paginated but with an additional where_clause parameter
pub fn get_records_by_collection_paginated_with_where(
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

  // Check if we need to join with actor table
  let needs_actor_join = case where {
    Some(wc) -> where_clause.requires_actor_join(wc)
    None -> False
  }

  // Build the ORDER BY clause (with table prefix if doing a join)
  let order_by_clause = build_order_by(sort_fields, needs_actor_join)

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
            list.append(
              bind_values,
              list.map(cursor_params, sqlight.text),
            )
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
    "
    SELECT record.uri, record.cid, record.did, record.collection, record.json, record.indexed_at
    FROM "
    <> from_clause
    <> "
    WHERE "
    <> string.join(final_where_parts, " AND ")
    <> "
    ORDER BY "
    <> order_by_clause
    <> "
    LIMIT "
    <> int.to_string(fetch_limit)

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
  let final_records = case has_more {
    True -> list.take(records, limit)
    False -> records
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
      let record_like = record_to_record_like(last_record)
      Some(cursor.generate_cursor_from_record(record_like, sort_by))
    }
    _, _ -> None
  }

  Ok(#(final_records, next_cursor, has_next_page, has_previous_page))
}

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
  let sql =
    "
    SELECT COUNT(*) as count
    FROM "
    <> from_clause
    <> "
    WHERE "
    <> string.join(where_parts, " AND ")

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

/// Converts a database Record to a cursor.RecordLike
pub fn record_to_record_like(record: Record) -> cursor.RecordLike {
  cursor.RecordLike(
    uri: record.uri,
    cid: record.cid,
    did: record.did,
    collection: record.collection,
    json: record.json,
    indexed_at: record.indexed_at,
  )
}

/// Builds an ORDER BY clause from sort fields
/// use_table_prefix: if True, prefixes table columns with "record." for joins
fn build_order_by(
  sort_fields: List(#(String, String)),
  use_table_prefix: Bool,
) -> String {
  let order_parts =
    list.map(sort_fields, fn(field) {
      let #(field_name, direction) = field
      let table_prefix = case use_table_prefix {
        True -> "record."
        False -> ""
      }
      let field_ref = case field_name {
        "uri" | "cid" | "did" | "collection" | "indexed_at" ->
          table_prefix <> field_name
        // For JSON fields, check if they look like dates and handle accordingly
        "createdAt" | "indexedAt" -> {
          // Use CASE to treat invalid dates as NULL for sorting
          let json_field =
            "json_extract(" <> table_prefix <> "json, '$." <> field_name <> "')"
          "CASE
            WHEN " <> json_field <> " IS NULL THEN NULL
            WHEN datetime(" <> json_field <> ") IS NULL THEN NULL
            ELSE " <> json_field <> "
           END"
        }
        _ ->
          "json_extract(" <> table_prefix <> "json, '$." <> field_name <> "')"
      }
      let dir = case string.lowercase(direction) {
        "asc" -> "ASC"
        _ -> "DESC"
      }
      // Always put NULLs last regardless of sort direction
      field_ref <> " " <> dir <> " NULLS LAST"
    })

  case list.is_empty(order_parts) {
    True -> {
      let prefix = case use_table_prefix {
        True -> "record."
        False -> ""
      }
      prefix <> "indexed_at DESC NULLS LAST"
    }
    False -> string.join(order_parts, ", ")
  }
}
