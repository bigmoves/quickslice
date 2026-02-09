import database/executor.{type DbError, type Executor, ConstraintError, Text}
import envoy
import gleam/dynamic/decode
import gleam/list
import gleam/string

// ===== Default Values =====

pub const default_relay_url = "https://relay1.us-west.bsky.network"

pub const default_plc_directory_url = "https://plc.directory"

pub const default_jetstream_url = "wss://jetstream2.us-west.bsky.network/subscribe"

pub const default_oauth_supported_scopes = "atproto transition:generic"

// ===== Config Functions =====

/// Get a config value by key
pub fn get(exec: Executor, key: String) -> Result(String, DbError) {
  let sql =
    "SELECT value FROM config WHERE key = " <> executor.placeholder(exec, 1)

  let decoder = {
    use value <- decode.field(0, decode.string)
    decode.success(value)
  }

  case executor.query(exec, sql, [Text(key)], decoder) {
    Ok([value, ..]) -> Ok(value)
    Ok([]) -> Error(ConstraintError("Config key not found: " <> key))
    Error(err) -> Error(err)
  }
}

/// Set or update a config value
pub fn set(exec: Executor, key: String, value: String) -> Result(Nil, DbError) {
  let now = executor.now(exec)
  let p1 = executor.placeholder(exec, 1)
  let p2 = executor.placeholder(exec, 2)

  // Use dialect-specific syntax for UPSERT
  let sql = case executor.dialect(exec) {
    executor.SQLite -> "INSERT INTO config (key, value, updated_at)
       VALUES (" <> p1 <> ", " <> p2 <> ", " <> now <> ")
       ON CONFLICT(key) DO UPDATE SET
         value = excluded.value,
         updated_at = " <> now
    executor.PostgreSQL -> "INSERT INTO config (key, value, updated_at)
       VALUES (" <> p1 <> ", " <> p2 <> ", " <> now <> ")
       ON CONFLICT(key) DO UPDATE SET
         value = EXCLUDED.value,
         updated_at = " <> now
  }

  executor.exec(exec, sql, [Text(key), Text(value)])
}

/// Delete a config value by key
pub fn delete(exec: Executor, key: String) -> Result(Nil, DbError) {
  let sql = "DELETE FROM config WHERE key = " <> executor.placeholder(exec, 1)

  executor.exec(exec, sql, [Text(key)])
}

/// Deletes the domain_authority config entry
pub fn delete_domain_authority(exec: Executor) -> Result(Nil, DbError) {
  delete(exec, "domain_authority")
}

// ===== Admin DID Functions =====

/// Get admin DIDs from config
pub fn get_admin_dids(exec: Executor) -> List(String) {
  case get(exec, "admin_dids") {
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
pub fn add_admin_did(exec: Executor, did: String) -> Result(Nil, DbError) {
  let current = get_admin_dids(exec)
  case list.contains(current, did) {
    True -> Ok(Nil)
    False -> {
      let new_list = list.append(current, [did])
      let value = string.join(new_list, ",")
      set(exec, "admin_dids", value)
    }
  }
}

pub type RemoveAdminError {
  LastAdminError
  NotFoundError
  DatabaseError(DbError)
}

/// Remove an admin DID from the list
/// Returns error if trying to remove the last admin
pub fn remove_admin_did(
  exec: Executor,
  did: String,
) -> Result(List(String), RemoveAdminError) {
  let current = get_admin_dids(exec)
  case list.contains(current, did) {
    False -> Error(NotFoundError)
    True -> {
      let new_list = list.filter(current, fn(d) { d != did })
      case new_list {
        [] -> Error(LastAdminError)
        _ -> {
          let value = string.join(new_list, ",")
          case set(exec, "admin_dids", value) {
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
  exec: Executor,
  dids: List(String),
) -> Result(Nil, DbError) {
  let value = string.join(dids, ",")
  set(exec, "admin_dids", value)
}

/// Check if a DID is an admin
pub fn is_admin(exec: Executor, did: String) -> Bool {
  let admins = get_admin_dids(exec)
  list.contains(admins, did)
}

/// Check if any admins are configured
pub fn has_admins(exec: Executor) -> Bool {
  case get_admin_dids(exec) {
    [] -> False
    _ -> True
  }
}

// ===== External Services Configuration =====

/// Get relay URL from config, with default fallback
pub fn get_relay_url(exec: Executor) -> String {
  case get(exec, "relay_url") {
    Ok(url) -> url
    Error(_) -> default_relay_url
  }
}

/// Get PLC directory URL with precedence: env var -> database -> default
pub fn get_plc_directory_url(exec: Executor) -> String {
  case envoy.get("PLC_DIRECTORY_URL") {
    Ok(url) -> url
    Error(_) -> {
      case get(exec, "plc_directory_url") {
        Ok(url) -> url
        Error(_) -> default_plc_directory_url
      }
    }
  }
}

/// Get Jetstream URL from config, with default fallback
pub fn get_jetstream_url(exec: Executor) -> String {
  case get(exec, "jetstream_url") {
    Ok(url) -> url
    Error(_) -> default_jetstream_url
  }
}

/// Get OAuth supported scopes from config, with default fallback
pub fn get_oauth_supported_scopes(exec: Executor) -> String {
  case get(exec, "oauth_supported_scopes") {
    Ok(scopes) -> scopes
    Error(_) -> default_oauth_supported_scopes
  }
}

/// Parse OAuth supported scopes into List(String)
pub fn get_oauth_supported_scopes_list(exec: Executor) -> List(String) {
  let scopes_str = get_oauth_supported_scopes(exec)
  scopes_str
  |> string.split(" ")
  |> list.map(string.trim)
  |> list.filter(fn(s) { !string.is_empty(s) })
}

/// Set relay URL
pub fn set_relay_url(exec: Executor, url: String) -> Result(Nil, DbError) {
  set(exec, "relay_url", url)
}

/// Set PLC directory URL
pub fn set_plc_directory_url(
  exec: Executor,
  url: String,
) -> Result(Nil, DbError) {
  set(exec, "plc_directory_url", url)
}

/// Set Jetstream URL
pub fn set_jetstream_url(exec: Executor, url: String) -> Result(Nil, DbError) {
  set(exec, "jetstream_url", url)
}

/// Set OAuth supported scopes (space-separated string)
pub fn set_oauth_supported_scopes(
  exec: Executor,
  scopes: String,
) -> Result(Nil, DbError) {
  set(exec, "oauth_supported_scopes", scopes)
}

// ===== Cookie Configuration =====

/// Valid SameSite values for cookies
pub type CookieSameSite {
  Strict
  Lax
  CookieSameSiteNone
}

/// Parse SameSite string to type
pub fn parse_same_site(s: String) -> Result(CookieSameSite, Nil) {
  case string.lowercase(s) {
    "strict" -> Ok(Strict)
    "lax" -> Ok(Lax)
    "none" -> Ok(CookieSameSiteNone)
    _ -> Error(Nil)
  }
}

/// Convert SameSite to string
pub fn same_site_to_string(ss: CookieSameSite) -> String {
  case ss {
    Strict -> "strict"
    Lax -> "lax"
    CookieSameSiteNone -> "none"
  }
}

/// Valid Secure flag modes for cookies
pub type CookieSecure {
  Auto
  Always
  Never
}

/// Parse Secure string to type
pub fn parse_secure(s: String) -> Result(CookieSecure, Nil) {
  case string.lowercase(s) {
    "auto" -> Ok(Auto)
    "always" -> Ok(Always)
    "never" -> Ok(Never)
    _ -> Error(Nil)
  }
}

/// Convert Secure to string
pub fn secure_to_string(sec: CookieSecure) -> String {
  case sec {
    Auto -> "auto"
    Always -> "always"
    Never -> "never"
  }
}

/// Get cookie SameSite setting (defaults to Strict)
pub fn get_cookie_same_site(exec: Executor) -> CookieSameSite {
  case get(exec, "cookie_same_site") {
    Ok(value) ->
      case parse_same_site(value) {
        Ok(ss) -> ss
        Error(_) -> Strict
      }
    Error(_) -> Strict
  }
}

/// Set cookie SameSite setting
pub fn set_cookie_same_site(
  exec: Executor,
  same_site: CookieSameSite,
) -> Result(Nil, DbError) {
  set(exec, "cookie_same_site", same_site_to_string(same_site))
}

/// Get cookie Secure setting (defaults to Auto)
pub fn get_cookie_secure(exec: Executor) -> CookieSecure {
  case get(exec, "cookie_secure") {
    Ok(value) ->
      case parse_secure(value) {
        Ok(sec) -> sec
        Error(_) -> Auto
      }
    Error(_) -> Auto
  }
}

/// Set cookie Secure setting
pub fn set_cookie_secure(
  exec: Executor,
  secure: CookieSecure,
) -> Result(Nil, DbError) {
  set(exec, "cookie_secure", secure_to_string(secure))
}

/// Get cookie domain (optional, for subdomain sharing)
pub fn get_cookie_domain(exec: Executor) -> Result(String, Nil) {
  case get(exec, "cookie_domain") {
    Ok(domain) if domain != "" -> Ok(domain)
    _ -> Error(Nil)
  }
}

/// Set cookie domain
pub fn set_cookie_domain(exec: Executor, domain: String) -> Result(Nil, DbError) {
  case string.trim(domain) {
    "" -> delete(exec, "cookie_domain")
    trimmed -> set(exec, "cookie_domain", trimmed)
  }
}

/// Clear cookie domain setting
pub fn clear_cookie_domain(exec: Executor) -> Result(Nil, DbError) {
  delete(exec, "cookie_domain")
}

/// Initialize config with defaults if not already set
pub fn initialize_config_defaults(exec: Executor) -> Result(Nil, DbError) {
  case get(exec, "relay_url") {
    Error(_) -> {
      let _ = set(exec, "relay_url", default_relay_url)
      Nil
    }
    Ok(_) -> Nil
  }

  case get(exec, "plc_directory_url") {
    Error(_) -> {
      let _ = set(exec, "plc_directory_url", default_plc_directory_url)
      Nil
    }
    Ok(_) -> Nil
  }

  case get(exec, "jetstream_url") {
    Error(_) -> {
      let _ = set(exec, "jetstream_url", default_jetstream_url)
      Nil
    }
    Ok(_) -> Nil
  }

  case get(exec, "oauth_supported_scopes") {
    Error(_) -> {
      let _ =
        set(exec, "oauth_supported_scopes", default_oauth_supported_scopes)
      Nil
    }
    Ok(_) -> Nil
  }

  Ok(Nil)
}
