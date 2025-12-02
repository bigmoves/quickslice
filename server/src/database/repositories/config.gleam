import gleam/dynamic/decode
import gleam/list
import gleam/result
import gleam/string
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

// ===== Admin DID Functions =====

/// Get admin DIDs from config
pub fn get_admin_dids(conn: sqlight.Connection) -> List(String) {
  case get(conn, "admin_dids") {
    Ok(value) -> {
      value
      |> string.split(",")
      |> list.map(string.trim)
      |> list.filter(fn(did) { !string.is_empty(did) })
    }
    Error(_) -> []
  }
}

/// Add an admin DID to the list
pub fn add_admin_did(
  conn: sqlight.Connection,
  did: String,
) -> Result(Nil, sqlight.Error) {
  let current = get_admin_dids(conn)
  case list.contains(current, did) {
    True -> Ok(Nil)
    // Already exists, idempotent
    False -> {
      let new_list = list.append(current, [did])
      let value = string.join(new_list, ",")
      set(conn, "admin_dids", value)
    }
  }
}

pub type RemoveAdminError {
  LastAdminError
  NotFoundError
  DatabaseError(sqlight.Error)
}

/// Remove an admin DID from the list
/// Returns error if trying to remove the last admin
pub fn remove_admin_did(
  conn: sqlight.Connection,
  did: String,
) -> Result(List(String), RemoveAdminError) {
  let current = get_admin_dids(conn)
  case list.contains(current, did) {
    False -> Error(NotFoundError)
    True -> {
      let new_list = list.filter(current, fn(d) { d != did })
      case new_list {
        [] -> Error(LastAdminError)
        _ -> {
          let value = string.join(new_list, ",")
          case set(conn, "admin_dids", value) {
            Ok(_) -> Ok(new_list)
            Error(err) -> Error(DatabaseError(err))
          }
        }
      }
    }
  }
}

/// Set the full admin DIDs list (replaces existing)
pub fn set_admin_dids(
  conn: sqlight.Connection,
  dids: List(String),
) -> Result(Nil, sqlight.Error) {
  let value = string.join(dids, ",")
  set(conn, "admin_dids", value)
}

/// Check if a DID is an admin
pub fn is_admin(conn: sqlight.Connection, did: String) -> Bool {
  let admins = get_admin_dids(conn)
  list.contains(admins, did)
}

/// Check if any admins are configured
pub fn has_admins(conn: sqlight.Connection) -> Bool {
  case get_admin_dids(conn) {
    [] -> False
    _ -> True
  }
}
