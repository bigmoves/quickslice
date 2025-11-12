import gleam/bit_array
import gleam/crypto
import gleam/dynamic/decode
import gleam/http/cookie
import gleam/http/response
import gleam/int
import gleam/option.{type Option}
import gleam/result
import sqlight.{type Connection}
import wisp.{type Request, type Response}

/// OAuth session data stored server-side
pub type OAuthSession {
  OAuthSession(
    session_id: String,
    access_token: String,
    refresh_token: Option(String),
    did: String,
    handle: String,
    expires_at: Option(Int),
  )
}

/// Temporary OAuth state during authorization flow
pub type OAuthState {
  OAuthState(code_verifier: String, code_challenge: String, login_hint: String)
}

const session_cookie_name = "quickslice_session"

/// Initialize the session database tables
pub fn init_db(db: Connection) -> Result(Nil, sqlight.Error) {
  let sessions_sql =
    "
    CREATE TABLE IF NOT EXISTS oauth_sessions (
      session_id TEXT PRIMARY KEY,
      access_token TEXT NOT NULL,
      refresh_token TEXT,
      did TEXT NOT NULL,
      handle TEXT NOT NULL,
      expires_at INTEGER,
      created_at INTEGER NOT NULL DEFAULT (unixepoch())
    )
  "

  let states_sql =
    "
    CREATE TABLE IF NOT EXISTS oauth_states (
      state_id TEXT PRIMARY KEY,
      code_verifier TEXT NOT NULL,
      code_challenge TEXT NOT NULL,
      login_hint TEXT NOT NULL,
      created_at INTEGER NOT NULL DEFAULT (unixepoch())
    )
  "

  use _ <- result.try(sqlight.exec(sessions_sql, db))
  sqlight.exec(states_sql, db)
}

/// Generate a new session ID
pub fn generate_session_id() -> String {
  let random_bytes = crypto.strong_random_bytes(32)
  bit_array.base64_url_encode(random_bytes, False)
}

/// Create a new OAuth session
pub fn create_session(
  db: Connection,
  access_token: String,
  refresh_token: Option(String),
  did: String,
  handle: String,
  expires_in: Option(Int),
) -> Result(String, sqlight.Error) {
  let session_id = generate_session_id()

  let sql = case expires_in {
    option.Some(seconds) -> "
      INSERT INTO oauth_sessions (session_id, access_token, refresh_token, did, handle, expires_at)
      VALUES (?, ?, ?, ?, ?, unixepoch() + " <> int.to_string(seconds) <> ")
    "
    option.None ->
      "
      INSERT INTO oauth_sessions (session_id, access_token, refresh_token, did, handle, expires_at)
      VALUES (?, ?, ?, ?, ?, NULL)
    "
  }

  let refresh_token_value = case refresh_token {
    option.Some(token) -> sqlight.text(token)
    option.None -> sqlight.null()
  }

  use _ <- result.try(sqlight.query(
    sql,
    db,
    [
      sqlight.text(session_id),
      sqlight.text(access_token),
      refresh_token_value,
      sqlight.text(did),
      sqlight.text(handle),
    ],
    decode.success(Nil),
  ))

  Ok(session_id)
}

/// Get a session by ID
pub fn get_session(
  db: Connection,
  session_id: String,
) -> Result(OAuthSession, sqlight.Error) {
  let sql =
    "
    SELECT session_id, access_token, refresh_token, did, handle, expires_at
    FROM oauth_sessions
    WHERE session_id = ?
  "

  let decoder = {
    use session_id <- decode.field(0, decode.string)
    use access_token <- decode.field(1, decode.string)
    use refresh_token <- decode.field(2, decode.optional(decode.string))
    use did <- decode.field(3, decode.string)
    use handle <- decode.field(4, decode.string)
    use expires_at <- decode.field(5, decode.optional(decode.int))

    decode.success(OAuthSession(
      session_id: session_id,
      access_token: access_token,
      refresh_token: refresh_token,
      did: did,
      handle: handle,
      expires_at: expires_at,
    ))
  }

  case sqlight.query(sql, db, [sqlight.text(session_id)], decoder) {
    Ok([session]) -> Ok(session)
    Ok([]) ->
      Error(sqlight.SqlightError(
        sqlight.ConstraintForeignkey,
        "Session not found",
        0,
      ))
    Ok(_) ->
      Error(sqlight.SqlightError(
        sqlight.ConstraintForeignkey,
        "Multiple sessions found",
        0,
      ))
    Error(e) -> Error(e)
  }
}

/// Update session tokens after refresh
pub fn update_session_tokens(
  db: Connection,
  session_id: String,
  access_token: String,
  refresh_token: Option(String),
  expires_in: Option(Int),
) -> Result(Nil, sqlight.Error) {
  let sql = case expires_in {
    option.Some(seconds) -> "
      UPDATE oauth_sessions
      SET access_token = ?,
          refresh_token = ?,
          expires_at = unixepoch() + " <> int.to_string(seconds) <> "
      WHERE session_id = ?
    "
    option.None ->
      "
      UPDATE oauth_sessions
      SET access_token = ?,
          refresh_token = ?,
          expires_at = NULL
      WHERE session_id = ?
    "
  }

  let refresh_token_value = case refresh_token {
    option.Some(token) -> sqlight.text(token)
    option.None -> sqlight.null()
  }

  use _ <- result.try(sqlight.query(
    sql,
    db,
    [sqlight.text(access_token), refresh_token_value, sqlight.text(session_id)],
    decode.success(Nil),
  ))
  Ok(Nil)
}

/// Delete a session by ID
pub fn delete_session(
  db: Connection,
  session_id: String,
) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM oauth_sessions WHERE session_id = ?"

  use _ <- result.try(sqlight.query(
    sql,
    db,
    [sqlight.text(session_id)],
    decode.success(Nil),
  ))
  Ok(Nil)
}

/// Set session cookie on response with SameSite=None for fetch with credentials
pub fn set_session_cookie(
  response: Response,
  req: Request,
  session_id: String,
) -> Response {
  // Sign the session ID the same way wisp does
  let signed_value = wisp.sign_message(req, <<session_id:utf8>>, crypto.Sha512)

  // Create cookie attributes without SameSite restriction
  let attributes =
    cookie.Attributes(
      max_age: option.Some(60 * 60 * 24 * 14),
      domain: option.None,
      path: option.Some("/"),
      secure: False,
      // False for localhost HTTP
      http_only: True,
      same_site: option.None,
      // No SameSite restriction for JavaScript fetch
    )

  response.set_cookie(response, session_cookie_name, signed_value, attributes)
}

/// Get session ID from request cookies
pub fn get_session_id(req: Request) -> Result(String, Nil) {
  wisp.get_cookie(req, session_cookie_name, wisp.Signed)
}

/// Clear session cookie on response
pub fn clear_session_cookie(response: Response, req: Request) -> Response {
  wisp.set_cookie(response, req, session_cookie_name, "", wisp.Signed, 0)
}

/// Get the current user from session with automatic token refresh
/// Returns (did, handle, access_token)
/// This is the main function you should use - it handles token refresh automatically
pub fn get_current_user(
  req: Request,
  db: Connection,
  refresh_fn: fn(String) ->
    Result(#(String, Option(String), Option(Int)), String),
) -> Result(#(String, String, String), Nil) {
  // Get the full session to check expiration
  use sess <- result.try(get_current_session(req, db))

  // Check if token should be refreshed
  case should_refresh_token(db, sess) && option.is_some(sess.refresh_token) {
    True -> {
      // Attempt to refresh the token
      case option.unwrap(sess.refresh_token, "") {
        "" -> Ok(#(sess.did, sess.handle, sess.access_token))
        refresh_tok -> {
          case refresh_fn(refresh_tok) {
            Ok(#(new_access_token, new_refresh_token, expires_in)) -> {
              // Update session with new tokens
              let final_refresh =
                option.or(new_refresh_token, sess.refresh_token)
              let _ =
                update_session_tokens(
                  db,
                  sess.session_id,
                  new_access_token,
                  final_refresh,
                  expires_in,
                )

              Ok(#(sess.did, sess.handle, new_access_token))
            }
            Error(_err) -> {
              // Return old token and let it fail naturally
              Ok(#(sess.did, sess.handle, sess.access_token))
            }
          }
        }
      }
    }
    False -> {
      // Token doesn't need refresh
      Ok(#(sess.did, sess.handle, sess.access_token))
    }
  }
}

/// Get the current user session with expiration check
/// Returns the full session object for refresh checking
pub fn get_current_session(
  req: Request,
  db: Connection,
) -> Result(OAuthSession, Nil) {
  use session_id <- result.try(get_session_id(req))
  get_session(db, session_id) |> result.replace_error(Nil)
}

/// Check if a session token is expired or will expire soon (within 5 minutes)
/// Returns True if token should be refreshed
pub fn should_refresh_token(db: Connection, session: OAuthSession) -> Bool {
  case session.expires_at {
    option.None -> False
    option.Some(expires_at) -> {
      // Check if token expires within 5 minutes (300 seconds)
      let sql = "SELECT (? - unixepoch()) < 300"

      let decoder = {
        use should_refresh <- decode.field(0, decode.int)
        decode.success(should_refresh != 0)
      }

      case sqlight.query(sql, db, [sqlight.int(expires_at)], decoder) {
        Ok([True]) -> True
        _ -> False
      }
    }
  }
}

/// Save OAuth state for authorization flow
pub fn save_oauth_state(
  db: Connection,
  state_id: String,
  state: OAuthState,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO oauth_states (state_id, code_verifier, code_challenge, login_hint)
    VALUES (?, ?, ?, ?)
  "

  use _ <- result.try(sqlight.query(
    sql,
    db,
    [
      sqlight.text(state_id),
      sqlight.text(state.code_verifier),
      sqlight.text(state.code_challenge),
      sqlight.text(state.login_hint),
    ],
    decode.success(Nil),
  ))

  Ok(Nil)
}

/// Get OAuth state by ID
pub fn get_oauth_state(
  db: Connection,
  state_id: String,
) -> Result(OAuthState, sqlight.Error) {
  let sql =
    "
    SELECT code_verifier, code_challenge, login_hint
    FROM oauth_states
    WHERE state_id = ?
  "

  let decoder = {
    use code_verifier <- decode.field(0, decode.string)
    use code_challenge <- decode.field(1, decode.string)
    use login_hint <- decode.field(2, decode.string)

    decode.success(OAuthState(
      code_verifier: code_verifier,
      code_challenge: code_challenge,
      login_hint: login_hint,
    ))
  }

  case sqlight.query(sql, db, [sqlight.text(state_id)], decoder) {
    Ok([state]) -> Ok(state)
    Ok([]) ->
      Error(sqlight.SqlightError(
        sqlight.ConstraintForeignkey,
        "State not found",
        0,
      ))
    Ok(_) ->
      Error(sqlight.SqlightError(
        sqlight.ConstraintForeignkey,
        "Multiple states found",
        0,
      ))
    Error(e) -> Error(e)
  }
}

/// Delete OAuth state after use
pub fn delete_oauth_state(
  db: Connection,
  state_id: String,
) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM oauth_states WHERE state_id = ?"

  use _ <- result.try(sqlight.query(
    sql,
    db,
    [sqlight.text(state_id)],
    decode.success(Nil),
  ))
  Ok(Nil)
}
