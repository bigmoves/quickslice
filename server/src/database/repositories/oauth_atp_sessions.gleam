/// OAuth ATP session repository operations
import database/executor.{type DbError, type Executor, Int as DbInt, Text}
import database/types.{type OAuthAtpSession, OAuthAtpSession}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}

/// Insert a new ATP session (or replace existing)
pub fn insert(exec: Executor, session: OAuthAtpSession) -> Result(Nil, DbError) {
  let p1 = executor.placeholder(exec, 1)
  let p2 = executor.placeholder(exec, 2)
  let p3 = executor.placeholder(exec, 3)
  let p4 = executor.placeholder(exec, 4)
  let p5 = executor.placeholder(exec, 5)
  let p6 = executor.placeholder(exec, 6)
  let p7 = executor.placeholder(exec, 7)
  let p8 = executor.placeholder(exec, 8)
  let p9 = executor.placeholder(exec, 9)
  let p10 = executor.placeholder(exec, 10)
  let p11 = executor.placeholder(exec, 11)
  let p12 = executor.placeholder(exec, 12)
  let p13 = executor.placeholder(exec, 13)
  let p14 = executor.placeholder(exec, 14)

  // Use dialect-specific INSERT OR REPLACE syntax
  let sql = case executor.dialect(exec) {
    executor.SQLite -> "INSERT OR REPLACE INTO oauth_atp_session (
        session_id, iteration, did, session_created_at, atp_oauth_state,
        signing_key_jkt, dpop_key, access_token, refresh_token,
        access_token_created_at, access_token_expires_at, access_token_scopes,
        session_exchanged_at, exchange_error
      ) VALUES (" <> p1 <> ", " <> p2 <> ", " <> p3 <> ", " <> p4 <> ", " <> p5 <> ", " <> p6 <> ", " <> p7 <> ", " <> p8 <> ", " <> p9 <> ", " <> p10 <> ", " <> p11 <> ", " <> p12 <> ", " <> p13 <> ", " <> p14 <> ")"
    executor.PostgreSQL -> "INSERT INTO oauth_atp_session (
        session_id, iteration, did, session_created_at, atp_oauth_state,
        signing_key_jkt, dpop_key, access_token, refresh_token,
        access_token_created_at, access_token_expires_at, access_token_scopes,
        session_exchanged_at, exchange_error
      ) VALUES (" <> p1 <> ", " <> p2 <> ", " <> p3 <> ", " <> p4 <> ", " <> p5 <> ", " <> p6 <> ", " <> p7 <> ", " <> p8 <> ", " <> p9 <> ", " <> p10 <> ", " <> p11 <> ", " <> p12 <> ", " <> p13 <> ", " <> p14 <> ")
      ON CONFLICT (session_id, iteration) DO UPDATE SET
        did = EXCLUDED.did,
        session_created_at = EXCLUDED.session_created_at,
        atp_oauth_state = EXCLUDED.atp_oauth_state,
        signing_key_jkt = EXCLUDED.signing_key_jkt,
        dpop_key = EXCLUDED.dpop_key,
        access_token = EXCLUDED.access_token,
        refresh_token = EXCLUDED.refresh_token,
        access_token_created_at = EXCLUDED.access_token_created_at,
        access_token_expires_at = EXCLUDED.access_token_expires_at,
        access_token_scopes = EXCLUDED.access_token_scopes,
        session_exchanged_at = EXCLUDED.session_exchanged_at,
        exchange_error = EXCLUDED.exchange_error"
  }

  let params = [
    Text(session.session_id),
    DbInt(session.iteration),
    executor.nullable_text(session.did),
    DbInt(session.session_created_at),
    Text(session.atp_oauth_state),
    Text(session.signing_key_jkt),
    Text(session.dpop_key),
    executor.nullable_text(session.access_token),
    executor.nullable_text(session.refresh_token),
    executor.nullable_int(session.access_token_created_at),
    executor.nullable_int(session.access_token_expires_at),
    executor.nullable_text(session.access_token_scopes),
    executor.nullable_int(session.session_exchanged_at),
    executor.nullable_text(session.exchange_error),
  ]

  executor.exec(exec, sql, params)
}

/// Get an ATP session by session_id and iteration
pub fn get(
  exec: Executor,
  session_id: String,
  iteration: Int,
) -> Result(Option(OAuthAtpSession), DbError) {
  let sql =
    "SELECT session_id, iteration, did, session_created_at, atp_oauth_state,
            signing_key_jkt, dpop_key, access_token, refresh_token,
            access_token_created_at, access_token_expires_at, access_token_scopes,
            session_exchanged_at, exchange_error
     FROM oauth_atp_session
     WHERE session_id = " <> executor.placeholder(exec, 1) <> " AND iteration = " <> executor.placeholder(
      exec,
      2,
    )

  case
    executor.query(exec, sql, [Text(session_id), DbInt(iteration)], decoder())
  {
    Ok(rows) ->
      case list.first(rows) {
        Ok(session) -> Ok(Some(session))
        Error(_) -> Ok(None)
      }
    Error(err) -> Error(err)
  }
}

/// Get the latest ATP session iteration for a session_id
pub fn get_latest(
  exec: Executor,
  session_id: String,
) -> Result(Option(OAuthAtpSession), DbError) {
  let sql =
    "SELECT session_id, iteration, did, session_created_at, atp_oauth_state,
            signing_key_jkt, dpop_key, access_token, refresh_token,
            access_token_created_at, access_token_expires_at, access_token_scopes,
            session_exchanged_at, exchange_error
     FROM oauth_atp_session
     WHERE session_id = " <> executor.placeholder(exec, 1) <> " ORDER BY iteration DESC LIMIT 1"

  case executor.query(exec, sql, [Text(session_id)], decoder()) {
    Ok(rows) ->
      case list.first(rows) {
        Ok(session) -> Ok(Some(session))
        Error(_) -> Ok(None)
      }
    Error(err) -> Error(err)
  }
}

/// Get an ATP session by atp_oauth_state (returns latest iteration)
pub fn get_by_state(
  exec: Executor,
  atp_oauth_state: String,
) -> Result(Option(OAuthAtpSession), DbError) {
  let sql =
    "SELECT session_id, iteration, did, session_created_at, atp_oauth_state,
            signing_key_jkt, dpop_key, access_token, refresh_token,
            access_token_created_at, access_token_expires_at, access_token_scopes,
            session_exchanged_at, exchange_error
     FROM oauth_atp_session
     WHERE atp_oauth_state = " <> executor.placeholder(exec, 1) <> " ORDER BY iteration DESC LIMIT 1"

  case executor.query(exec, sql, [Text(atp_oauth_state)], decoder()) {
    Ok(rows) ->
      case list.first(rows) {
        Ok(session) -> Ok(Some(session))
        Error(_) -> Ok(None)
      }
    Error(err) -> Error(err)
  }
}

/// Decode ATP session from database row
fn decoder() -> decode.Decoder(OAuthAtpSession) {
  use session_id <- decode.field(0, decode.string)
  use iteration <- decode.field(1, decode.int)
  use did <- decode.field(2, decode.optional(decode.string))
  use session_created_at <- decode.field(3, decode.int)
  use atp_oauth_state <- decode.field(4, decode.string)
  use signing_key_jkt <- decode.field(5, decode.string)
  use dpop_key <- decode.field(6, decode.string)
  use access_token <- decode.field(7, decode.optional(decode.string))
  use refresh_token <- decode.field(8, decode.optional(decode.string))
  use access_token_created_at <- decode.field(9, decode.optional(decode.int))
  use access_token_expires_at <- decode.field(10, decode.optional(decode.int))
  use access_token_scopes <- decode.field(11, decode.optional(decode.string))
  use session_exchanged_at <- decode.field(12, decode.optional(decode.int))
  use exchange_error <- decode.field(13, decode.optional(decode.string))

  decode.success(OAuthAtpSession(
    session_id: session_id,
    iteration: iteration,
    did: did,
    session_created_at: session_created_at,
    atp_oauth_state: atp_oauth_state,
    signing_key_jkt: signing_key_jkt,
    dpop_key: dpop_key,
    access_token: access_token,
    refresh_token: refresh_token,
    access_token_created_at: access_token_created_at,
    access_token_expires_at: access_token_expires_at,
    access_token_scopes: access_token_scopes,
    session_exchanged_at: session_exchanged_at,
    exchange_error: exchange_error,
  ))
}
