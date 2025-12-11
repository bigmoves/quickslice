import database/executor.{type DbError, type Executor, Text}
import database/types.{type Lexicon, Lexicon}
import gleam/dynamic/decode

// ===== Lexicon Functions =====

/// Inserts or updates a lexicon in the database
pub fn insert(exec: Executor, id: String, json: String) -> Result(Nil, DbError) {
  let now = executor.now(exec)
  let p1 = executor.placeholder(exec, 1)
  let p2 = executor.placeholder(exec, 2)

  // Use dialect-specific UPSERT syntax
  let sql = case executor.dialect(exec) {
    executor.SQLite -> "INSERT INTO lexicon (id, json, created_at)
       VALUES (" <> p1 <> ", " <> p2 <> ", " <> now <> ")
       ON CONFLICT(id) DO UPDATE SET
         json = excluded.json,
         created_at = " <> now
    executor.PostgreSQL -> "INSERT INTO lexicon (id, json, created_at)
       VALUES (" <> p1 <> ", " <> p2 <> ", " <> now <> ")
       ON CONFLICT(id) DO UPDATE SET
         json = EXCLUDED.json,
         created_at = " <> now
  }

  executor.exec(exec, sql, [Text(id), Text(json)])
}

/// Gets a lexicon by ID
pub fn get(exec: Executor, id: String) -> Result(List(Lexicon), DbError) {
  let sql = "SELECT id, json, created_at
     FROM lexicon
     WHERE id = " <> executor.placeholder(exec, 1)

  let decoder = {
    use id <- decode.field(0, decode.string)
    use json <- decode.field(1, decode.string)
    use created_at <- decode.field(2, decode.string)
    decode.success(Lexicon(id:, json:, created_at:))
  }

  executor.query(exec, sql, [Text(id)], decoder)
}

/// Gets all lexicons from the database
pub fn get_all(exec: Executor) -> Result(List(Lexicon), DbError) {
  let sql =
    "SELECT id, json, created_at
     FROM lexicon
     ORDER BY created_at DESC"

  let decoder = {
    use id <- decode.field(0, decode.string)
    use json <- decode.field(1, decode.string)
    use created_at <- decode.field(2, decode.string)
    decode.success(Lexicon(id:, json:, created_at:))
  }

  executor.query(exec, sql, [], decoder)
}

/// Checks if a lexicon exists by ID
pub fn has(exec: Executor, id: String) -> Result(Bool, DbError) {
  let sql = "SELECT COUNT(*) as count
     FROM lexicon
     WHERE id = " <> executor.placeholder(exec, 1)

  let decoder = {
    use count <- decode.field(0, decode.int)
    decode.success(count)
  }

  case executor.query(exec, sql, [Text(id)], decoder) {
    Ok([count]) -> Ok(count > 0)
    Ok(_) -> Ok(False)
    Error(err) -> Error(err)
  }
}

/// Checks if a lexicon exists for a collection (with fallback to record table)
pub fn has_for_collection(
  exec: Executor,
  collection: String,
) -> Result(Bool, DbError) {
  // First check lexicon table (direct lookup is faster)
  case has(exec, collection) {
    Ok(True) -> Ok(True)
    Ok(False) -> {
      // Fall back to searching record table for backward compatibility
      // Use dialect-specific LIKE operator
      let like_op = case executor.dialect(exec) {
        executor.SQLite -> " LIKE "
        executor.PostgreSQL -> " ILIKE "
      }

      let sql = "SELECT COUNT(*) as count
         FROM record
         WHERE collection = 'com.atproto.lexicon.schema'
         AND json" <> like_op <> executor.placeholder(exec, 1)

      let decoder = {
        use count <- decode.field(0, decode.int)
        decode.success(count)
      }

      let pattern = "%" <> collection <> "%"

      case executor.query(exec, sql, [Text(pattern)], decoder) {
        Ok([count]) -> Ok(count > 0)
        Ok(_) -> Ok(False)
        Error(err) -> Error(err)
      }
    }
    Error(err) -> Error(err)
  }
}

/// Gets the total number of lexicons in the database
pub fn get_count(exec: Executor) -> Result(Int, DbError) {
  let sql = "SELECT COUNT(*) as count FROM lexicon"

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

/// Gets all lexicons that are of type "record" (collections)
pub fn get_record_types(exec: Executor) -> Result(List(Lexicon), DbError) {
  let sql =
    "SELECT id, json, created_at
     FROM lexicon
     WHERE json LIKE '%\"type\":\"record\"%'
        OR json LIKE '%\"type\": \"record\"%'
     ORDER BY id ASC"

  let decoder = {
    use id <- decode.field(0, decode.string)
    use json <- decode.field(1, decode.string)
    use created_at <- decode.field(2, decode.string)
    decode.success(Lexicon(id:, json:, created_at:))
  }

  executor.query(exec, sql, [], decoder)
}

/// Deletes all lexicons from the database
pub fn delete_all(exec: Executor) -> Result(Nil, DbError) {
  executor.exec(exec, "DELETE FROM lexicon", [])
}
