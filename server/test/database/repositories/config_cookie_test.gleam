/// Tests for cookie configuration helpers in config repository
import database/executor
import database/repositories/config
import database/sqlite/connection as db_connection
import gleeunit/should

fn setup_test_db() {
  let assert Ok(exec) = db_connection.connect("sqlite::memory:")

  // Create config table
  let assert Ok(_) =
    executor.exec(
      exec,
      "CREATE TABLE config (
        key TEXT PRIMARY KEY,
        value TEXT NOT NULL,
        updated_at INTEGER
      )",
      [],
    )

  exec
}

// ===== CookieSameSite Parsing Tests =====

pub fn parse_same_site_strict_test() {
  let result = config.parse_same_site("strict")
  result |> should.be_ok()
  let assert Ok(ss) = result
  ss |> should.equal(config.Strict)
}

pub fn parse_same_site_lax_test() {
  let result = config.parse_same_site("lax")
  result |> should.be_ok()
  let assert Ok(ss) = result
  ss |> should.equal(config.Lax)
}

pub fn parse_same_site_none_test() {
  let result = config.parse_same_site("none")
  result |> should.be_ok()
  let assert Ok(ss) = result
  ss |> should.equal(config.CookieSameSiteNone)
}

pub fn parse_same_site_case_insensitive_test() {
  config.parse_same_site("STRICT") |> should.be_ok()
  config.parse_same_site("Lax") |> should.be_ok()
  config.parse_same_site("NONE") |> should.be_ok()
}

pub fn parse_same_site_invalid_test() {
  let result = config.parse_same_site("invalid")
  result |> should.be_error()
}

pub fn same_site_to_string_test() {
  config.same_site_to_string(config.Strict) |> should.equal("strict")
  config.same_site_to_string(config.Lax) |> should.equal("lax")
  config.same_site_to_string(config.CookieSameSiteNone) |> should.equal("none")
}

// ===== CookieSecure Parsing Tests =====

pub fn parse_secure_auto_test() {
  let result = config.parse_secure("auto")
  result |> should.be_ok()
  let assert Ok(sec) = result
  sec |> should.equal(config.Auto)
}

pub fn parse_secure_always_test() {
  let result = config.parse_secure("always")
  result |> should.be_ok()
  let assert Ok(sec) = result
  sec |> should.equal(config.Always)
}

pub fn parse_secure_never_test() {
  let result = config.parse_secure("never")
  result |> should.be_ok()
  let assert Ok(sec) = result
  sec |> should.equal(config.Never)
}

pub fn parse_secure_case_insensitive_test() {
  config.parse_secure("AUTO") |> should.be_ok()
  config.parse_secure("Always") |> should.be_ok()
  config.parse_secure("NEVER") |> should.be_ok()
}

pub fn parse_secure_invalid_test() {
  let result = config.parse_secure("invalid")
  result |> should.be_error()
}

pub fn secure_to_string_test() {
  config.secure_to_string(config.Auto) |> should.equal("auto")
  config.secure_to_string(config.Always) |> should.equal("always")
  config.secure_to_string(config.Never) |> should.equal("never")
}

// ===== Database Cookie Config Tests =====

pub fn get_cookie_same_site_defaults_to_strict_test() {
  let exec = setup_test_db()
  let result = config.get_cookie_same_site(exec)
  result |> should.equal(config.Strict)
}

pub fn set_and_get_cookie_same_site_test() {
  let exec = setup_test_db()

  // Set to Lax
  let set_result = config.set_cookie_same_site(exec, config.Lax)
  set_result |> should.be_ok()

  // Get should return Lax
  let result = config.get_cookie_same_site(exec)
  result |> should.equal(config.Lax)
}

pub fn set_cookie_same_site_to_none_test() {
  let exec = setup_test_db()

  let set_result = config.set_cookie_same_site(exec, config.CookieSameSiteNone)
  set_result |> should.be_ok()

  let result = config.get_cookie_same_site(exec)
  result |> should.equal(config.CookieSameSiteNone)
}

pub fn get_cookie_secure_defaults_to_auto_test() {
  let exec = setup_test_db()
  let result = config.get_cookie_secure(exec)
  result |> should.equal(config.Auto)
}

pub fn set_and_get_cookie_secure_test() {
  let exec = setup_test_db()

  // Set to Always
  let set_result = config.set_cookie_secure(exec, config.Always)
  set_result |> should.be_ok()

  // Get should return Always
  let result = config.get_cookie_secure(exec)
  result |> should.equal(config.Always)
}

pub fn set_cookie_secure_to_never_test() {
  let exec = setup_test_db()

  let set_result = config.set_cookie_secure(exec, config.Never)
  set_result |> should.be_ok()

  let result = config.get_cookie_secure(exec)
  result |> should.equal(config.Never)
}

pub fn get_cookie_domain_defaults_to_error_test() {
  let exec = setup_test_db()
  let result = config.get_cookie_domain(exec)
  result |> should.be_error()
}

pub fn set_and_get_cookie_domain_test() {
  let exec = setup_test_db()

  // Set domain
  let set_result = config.set_cookie_domain(exec, ".example.com")
  set_result |> should.be_ok()

  // Get should return the domain
  let result = config.get_cookie_domain(exec)
  result |> should.be_ok()
  let assert Ok(domain) = result
  domain |> should.equal(".example.com")
}

pub fn set_empty_cookie_domain_deletes_it_test() {
  let exec = setup_test_db()

  // Set a domain first
  let assert Ok(_) = config.set_cookie_domain(exec, ".example.com")

  // Setting empty should delete it
  let clear_result = config.set_cookie_domain(exec, "")
  clear_result |> should.be_ok()

  // Get should return error now
  let result = config.get_cookie_domain(exec)
  result |> should.be_error()
}

pub fn clear_cookie_domain_test() {
  let exec = setup_test_db()

  // Set a domain first
  let assert Ok(_) = config.set_cookie_domain(exec, ".example.com")

  // Clear it
  let clear_result = config.clear_cookie_domain(exec)
  clear_result |> should.be_ok()

  // Get should return error now
  let result = config.get_cookie_domain(exec)
  result |> should.be_error()
}

pub fn get_cookie_domain_returns_error_for_empty_string_test() {
  let exec = setup_test_db()

  // Manually set an empty string in the database
  let assert Ok(_) = config.set(exec, "cookie_domain", "")

  // get_cookie_domain should treat empty string as not set
  let result = config.get_cookie_domain(exec)
  result |> should.be_error()
}
