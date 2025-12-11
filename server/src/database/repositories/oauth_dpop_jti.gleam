/// OAuth DPoP JTI replay protection repository
/// Tracks used JTI values to prevent replay attacks
import database/executor.{type DbError, type Executor, Int as DbInt, Text}
import gleam/dynamic/decode

/// Check if a JTI has been used and mark it as used atomically
/// Returns Ok(True) if the JTI was successfully recorded (not previously used)
/// Returns Ok(False) if the JTI was already used (replay attack)
pub fn use_jti(
  exec: Executor,
  jti: String,
  created_at: Int,
) -> Result(Bool, DbError) {
  let p1 = executor.placeholder(exec, 1)
  let p2 = executor.placeholder(exec, 2)

  // Use dialect-specific insert-or-ignore syntax
  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "INSERT OR IGNORE INTO oauth_dpop_jti (jti, created_at) VALUES ("
      <> p1
      <> ", "
      <> p2
      <> ")"
    executor.PostgreSQL ->
      "INSERT INTO oauth_dpop_jti (jti, created_at) VALUES ("
      <> p1
      <> ", "
      <> p2
      <> ") ON CONFLICT (jti) DO NOTHING"
  }

  case executor.exec(exec, sql, [Text(jti), DbInt(created_at)]) {
    Ok(_) -> {
      // Check if the insert succeeded by trying to select the row we just inserted
      // and verifying it has our created_at value
      let check_sql =
        "SELECT created_at FROM oauth_dpop_jti WHERE jti = "
        <> executor.placeholder(exec, 1)

      let check_decoder = {
        use ts <- decode.field(0, decode.int)
        decode.success(ts)
      }

      case executor.query(exec, check_sql, [Text(jti)], check_decoder) {
        Ok([stored_ts]) ->
          // If our timestamp matches, we were the one who inserted it
          Ok(stored_ts == created_at)
        Ok(_) -> Ok(False)
        Error(_) -> Ok(False)
      }
    }
    Error(_) -> Ok(False)
  }
}

/// Delete expired JTI entries
/// Should be called periodically to clean up old entries
pub fn delete_expired(exec: Executor, before: Int) -> Result(Nil, DbError) {
  let sql =
    "DELETE FROM oauth_dpop_jti WHERE created_at < "
    <> executor.placeholder(exec, 1)

  executor.exec(exec, sql, [DbInt(before)])
}
