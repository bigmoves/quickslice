/// OAuth ATP session repository operations
import database/types.{type OAuthAtpSession, OAuthAtpSession}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import sqlight

/// Insert a new ATP session (or replace existing)
pub fn insert(
  conn: sqlight.Connection,
  session: OAuthAtpSession,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT OR REPLACE INTO oauth_atp_session (
      session_id, iteration, did, session_created_at, atp_oauth_state,
      signing_key_jkt, dpop_key, access_token, refresh_token,
      access_token_created_at, access_token_expires_at, access_token_scopes,
      session_exchanged_at, exchange_error
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  "

  let params = [
    sqlight.text(session.session_id),
    sqlight.int(session.iteration),
    sqlight.nullable(sqlight.text, session.did),
    sqlight.int(session.session_created_at),
    sqlight.text(session.atp_oauth_state),
    sqlight.text(session.signing_key_jkt),
    sqlight.text(session.dpop_key),
    sqlight.nullable(sqlight.text, session.access_token),
    sqlight.nullable(sqlight.text, session.refresh_token),
    sqlight.nullable(sqlight.int, session.access_token_created_at),
    sqlight.nullable(sqlight.int, session.access_token_expires_at),
    sqlight.nullable(sqlight.text, session.access_token_scopes),
    sqlight.nullable(sqlight.int, session.session_exchanged_at),
    sqlight.nullable(sqlight.text, session.exchange_error),
  ]

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: params,
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Get an ATP session by session_id and iteration
pub fn get(
  conn: sqlight.Connection,
  session_id: String,
  iteration: Int,
) -> Result(Option(OAuthAtpSession), sqlight.Error) {
  let sql =
    "SELECT session_id, iteration, did, session_created_at, atp_oauth_state,
            signing_key_jkt, dpop_key, access_token, refresh_token,
            access_token_created_at, access_token_expires_at, access_token_scopes,
            session_exchanged_at, exchange_error
     FROM oauth_atp_session
     WHERE session_id = ? AND iteration = ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(session_id), sqlight.int(iteration)],
    expecting: decoder(),
  ))

  case list.first(rows) {
    Ok(session) -> Ok(Some(session))
    Error(_) -> Ok(None)
  }
}

/// Get the latest ATP session iteration for a session_id
pub fn get_latest(
  conn: sqlight.Connection,
  session_id: String,
) -> Result(Option(OAuthAtpSession), sqlight.Error) {
  let sql =
    "SELECT session_id, iteration, did, session_created_at, atp_oauth_state,
            signing_key_jkt, dpop_key, access_token, refresh_token,
            access_token_created_at, access_token_expires_at, access_token_scopes,
            session_exchanged_at, exchange_error
     FROM oauth_atp_session
     WHERE session_id = ?
     ORDER BY iteration DESC
     LIMIT 1"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(session_id)],
    expecting: decoder(),
  ))

  case list.first(rows) {
    Ok(session) -> Ok(Some(session))
    Error(_) -> Ok(None)
  }
}

/// Get an ATP session by atp_oauth_state (returns latest iteration)
pub fn get_by_state(
  conn: sqlight.Connection,
  atp_oauth_state: String,
) -> Result(Option(OAuthAtpSession), sqlight.Error) {
  let sql =
    "SELECT session_id, iteration, did, session_created_at, atp_oauth_state,
            signing_key_jkt, dpop_key, access_token, refresh_token,
            access_token_created_at, access_token_expires_at, access_token_scopes,
            session_exchanged_at, exchange_error
     FROM oauth_atp_session
     WHERE atp_oauth_state = ?
     ORDER BY iteration DESC
     LIMIT 1"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(atp_oauth_state)],
    expecting: decoder(),
  ))

  case list.first(rows) {
    Ok(session) -> Ok(Some(session))
    Error(_) -> Ok(None)
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
