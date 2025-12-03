import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/list
import gleam/result
import gleam/string
import sqlight

// ===== Default Values =====

pub const default_relay_url = "https://relay1.us-west.bsky.network"

pub const default_plc_directory_url = "https://plc.directory"

pub const default_jetstream_url = "wss://jetstream2.us-west.bsky.network/subscribe"

pub const default_oauth_supported_scopes = "atproto transition:generic"

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

/// Get all config values as a dictionary
pub fn get_all(conn: sqlight.Connection) -> Dict(String, String) {
  let sql = "SELECT key, value FROM config"

  let decoder = {
    use key <- decode.field(0, decode.string)
    use value <- decode.field(1, decode.string)
    decode.success(#(key, value))
  }

  case sqlight.query(sql, on: conn, with: [], expecting: decoder) {
    Ok(rows) -> dict.from_list(rows)
    Error(_) -> dict.new()
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

// ===== External Services Configuration =====

/// Get relay URL from config, with default fallback
pub fn get_relay_url(conn: sqlight.Connection) -> String {
  case get(conn, "relay_url") {
    Ok(url) -> url
    Error(_) -> default_relay_url
  }
}

/// Get PLC directory URL from config, with default fallback
pub fn get_plc_directory_url(conn: sqlight.Connection) -> String {
  case get(conn, "plc_directory_url") {
    Ok(url) -> url
    Error(_) -> default_plc_directory_url
  }
}

/// Get Jetstream URL from config, with default fallback
pub fn get_jetstream_url(conn: sqlight.Connection) -> String {
  case get(conn, "jetstream_url") {
    Ok(url) -> url
    Error(_) -> default_jetstream_url
  }
}

/// Get OAuth supported scopes from config, with default fallback
/// Returns space-separated string
pub fn get_oauth_supported_scopes(conn: sqlight.Connection) -> String {
  case get(conn, "oauth_supported_scopes") {
    Ok(scopes) -> scopes
    Error(_) -> default_oauth_supported_scopes
  }
}

/// Parse OAuth supported scopes into List(String)
pub fn get_oauth_supported_scopes_list(conn: sqlight.Connection) -> List(String) {
  let scopes_str = get_oauth_supported_scopes(conn)
  scopes_str
  |> string.split(" ")
  |> list.map(string.trim)
  |> list.filter(fn(s) { !string.is_empty(s) })
}

/// Set relay URL
pub fn set_relay_url(
  conn: sqlight.Connection,
  url: String,
) -> Result(Nil, sqlight.Error) {
  set(conn, "relay_url", url)
}

/// Set PLC directory URL
pub fn set_plc_directory_url(
  conn: sqlight.Connection,
  url: String,
) -> Result(Nil, sqlight.Error) {
  set(conn, "plc_directory_url", url)
}

/// Set Jetstream URL
pub fn set_jetstream_url(
  conn: sqlight.Connection,
  url: String,
) -> Result(Nil, sqlight.Error) {
  set(conn, "jetstream_url", url)
}

/// Set OAuth supported scopes (space-separated string)
pub fn set_oauth_supported_scopes(
  conn: sqlight.Connection,
  scopes: String,
) -> Result(Nil, sqlight.Error) {
  set(conn, "oauth_supported_scopes", scopes)
}

/// Initialize config with defaults if not already set
/// Should be called once during server startup
pub fn initialize_config_defaults(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  // Only set if the key doesn't exist (don't overwrite user settings)

  // Relay URL
  case get(conn, "relay_url") {
    Error(_) -> {
      let _ = set(conn, "relay_url", default_relay_url)
      Nil
    }
    Ok(_) -> Nil
  }

  // PLC Directory URL
  case get(conn, "plc_directory_url") {
    Error(_) -> {
      let _ = set(conn, "plc_directory_url", default_plc_directory_url)
      Nil
    }
    Ok(_) -> Nil
  }

  // Jetstream URL
  case get(conn, "jetstream_url") {
    Error(_) -> {
      let _ = set(conn, "jetstream_url", default_jetstream_url)
      Nil
    }
    Ok(_) -> Nil
  }

  // OAuth Supported Scopes
  case get(conn, "oauth_supported_scopes") {
    Error(_) -> {
      let _ =
        set(conn, "oauth_supported_scopes", default_oauth_supported_scopes)
      Nil
    }
    Ok(_) -> Nil
  }

  Ok(Nil)
}
