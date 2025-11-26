import database/repositories/admin_session
import database/repositories/oauth_access_tokens
import database/repositories/oauth_atp_sessions
import database/repositories/oauth_refresh_tokens
import gleam/bit_array
import gleam/crypto
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/http/cookie
import gleam/http/response
import gleam/option.{type Option, None, Some}
import gleam/result
import lib/oauth/atproto/did_resolver
import lib/oauth/did_cache
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

const session_cookie_name = "quickslice_session"

/// Generate a new session ID
pub fn generate_session_id() -> String {
  let random_bytes = crypto.strong_random_bytes(32)
  bit_array.base64_url_encode(random_bytes, False)
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

/// Get the current user from session
/// Returns (did, handle, access_token)
pub fn get_current_user(
  req: Request,
  db: Connection,
  did_cache: Subject(did_cache.Message),
) -> Result(#(String, String, String), Nil) {
  use sess <- result.try(get_current_session(req, db, did_cache))
  Ok(#(sess.did, sess.handle, sess.access_token))
}

/// Get the current user session from admin_session + ATP session tables
/// Returns OAuthSession for compatibility with existing callers
pub fn get_current_session(
  req: Request,
  db: Connection,
  did_cache: Subject(did_cache.Message),
) -> Result(OAuthSession, Nil) {
  use session_id <- result.try(get_session_id(req))

  // Look up admin session
  use admin_sess_opt <- result.try(
    admin_session.get(db, session_id) |> result.replace_error(Nil),
  )
  use admin_sess <- result.try(case admin_sess_opt {
    Some(s) -> Ok(s)
    None -> Error(Nil)
  })

  // Look up ATP session (get latest iteration)
  use atp_sess_opt <- result.try(
    oauth_atp_sessions.get_latest(db, admin_sess.atp_session_id)
    |> result.replace_error(Nil),
  )
  use atp_sess <- result.try(case atp_sess_opt {
    Some(s) -> Ok(s)
    None -> Error(Nil)
  })

  // Get the DID
  let did = option.unwrap(atp_sess.did, "")

  // Look up OAuth access token by session_id (atp_session_id)
  // This is our OAuth token (e.g., tok-xxx), not the ATP token from PDS
  let oauth_access_token_opt =
    oauth_access_tokens.get_by_session_id(db, admin_sess.atp_session_id)
    |> result.unwrap(None)

  // Look up OAuth refresh token by session_id (atp_session_id)
  let oauth_refresh_token_opt =
    oauth_refresh_tokens.get_by_session_id(db, admin_sess.atp_session_id)
    |> result.unwrap(None)

  // Resolve handle from DID document (falls back to DID if resolution fails)
  let handle = case did {
    "" -> ""
    _ -> {
      case did_resolver.resolve_did_with_cache(did_cache, did, False) {
        Ok(doc) -> option.unwrap(did_resolver.get_handle(doc), did)
        Error(_) -> did
      }
    }
  }

  // Get OAuth token values (or empty string if not found)
  let access_token = case oauth_access_token_opt {
    Some(t) -> t.token
    None -> ""
  }

  let refresh_token = case oauth_refresh_token_opt {
    Some(t) -> Some(t.token)
    None -> None
  }

  // Get expiration from OAuth access token
  let expires_at = case oauth_access_token_opt {
    Some(t) -> Some(t.expires_at)
    None -> None
  }

  // Convert to OAuthSession format
  Ok(OAuthSession(
    session_id: session_id,
    access_token: access_token,
    refresh_token: refresh_token,
    did: did,
    handle: handle,
    expires_at: expires_at,
  ))
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
