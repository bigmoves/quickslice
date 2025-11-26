import gleam/dynamic/decode
import gleam/result
import sqlight

// ===== Config Functions =====

/// Get a config value by key
pub fn get(
  conn: sqlight.Connection,
  key: String,
) -> Result(String, sqlight.Error) {
  let sql =
    "
    SELECT value
    FROM config
    WHERE key = ?
  "

  let decoder = {
    use value <- decode.field(0, decode.string)
    decode.success(value)
  }

  case
    sqlight.query(sql, on: conn, with: [sqlight.text(key)], expecting: decoder)
  {
    Ok([value, ..]) -> Ok(value)
    Ok([]) ->
      Error(sqlight.SqlightError(
        sqlight.ConstraintForeignkey,
        "Config key not found",
        -1,
      ))
    Error(err) -> Error(err)
  }
}

/// Set or update a config value
pub fn set(
  conn: sqlight.Connection,
  key: String,
  value: String,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO config (key, value, updated_at)
    VALUES (?, ?, datetime('now'))
    ON CONFLICT(key) DO UPDATE SET
      value = excluded.value,
      updated_at = datetime('now')
  "

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(key), sqlight.text(value)],
    expecting: decode.string,
  ))
  Ok(Nil)
}

/// Delete a config value by key
pub fn delete(
  conn: sqlight.Connection,
  key: String,
) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM config WHERE key = ?"

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(key)],
    expecting: decode.string,
  ))
  Ok(Nil)
}

/// Deletes the domain_authority config entry
pub fn delete_domain_authority(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  delete(conn, "domain_authority")
}
