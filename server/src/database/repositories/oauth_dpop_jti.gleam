/// OAuth DPoP JTI replay protection repository
/// Tracks used JTI values to prevent replay attacks
import gleam/dynamic/decode
import gleam/list
import gleam/result
import sqlight

/// Check if a JTI has been used and mark it as used atomically
/// Returns Ok(True) if the JTI was successfully recorded (not previously used)
/// Returns Ok(False) if the JTI was already used (replay attack)
pub fn use_jti(
  conn: sqlight.Connection,
  jti: String,
  created_at: Int,
) -> Result(Bool, sqlight.Error) {
  // Try to insert - will fail if JTI already exists due to PRIMARY KEY constraint
  let sql =
    "INSERT OR IGNORE INTO oauth_dpop_jti (jti, created_at) VALUES (?, ?)"

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(jti), sqlight.int(created_at)],
    expecting: decode.dynamic,
  ))

  // Check if insert succeeded by checking changes
  let check_sql = "SELECT changes()"
  use changes_rows <- result.try(sqlight.query(
    check_sql,
    on: conn,
    with: [],
    expecting: decode.at([0], decode.int),
  ))

  case list.first(changes_rows) {
    Ok(1) -> Ok(True)
    // Insert succeeded, JTI was new
    Ok(0) -> Ok(False)
    // Insert ignored, JTI was duplicate
    _ -> Ok(False)
  }
}

/// Delete expired JTI entries
/// Should be called periodically to clean up old entries
pub fn delete_expired(
  conn: sqlight.Connection,
  before: Int,
) -> Result(Int, sqlight.Error) {
  let sql = "DELETE FROM oauth_dpop_jti WHERE created_at < ?"

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.int(before)],
    expecting: decode.dynamic,
  ))

  // Get count of deleted rows
  let check_sql = "SELECT changes()"
  use changes_rows <- result.try(sqlight.query(
    check_sql,
    on: conn,
    with: [],
    expecting: decode.at([0], decode.int),
  ))

  case list.first(changes_rows) {
    Ok(count) -> Ok(count)
    Error(_) -> Ok(0)
  }
}
