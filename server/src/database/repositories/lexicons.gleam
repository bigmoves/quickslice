import database/types.{type Lexicon, Lexicon}
import gleam/dynamic/decode
import gleam/result
import sqlight

// ===== Lexicon Functions =====

/// Inserts or updates a lexicon in the database
pub fn insert(
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
pub fn get(
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
pub fn get_all(conn: sqlight.Connection) -> Result(List(Lexicon), sqlight.Error) {
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
pub fn has(
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

/// Checks if a lexicon exists for a collection (with fallback to record table)
pub fn has_for_collection(
  conn: sqlight.Connection,
  collection: String,
) -> Result(Bool, sqlight.Error) {
  // First check lexicon table (direct lookup is faster)
  case has(conn, collection) {
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

/// Gets the total number of lexicons in the database
pub fn get_count(conn: sqlight.Connection) -> Result(Int, sqlight.Error) {
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
pub fn get_record_types(
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

/// Deletes all lexicons from the database
pub fn delete_all(conn: sqlight.Connection) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM lexicon"

  sqlight.exec(sql, conn)
  |> result.map(fn(_) { Nil })
}
