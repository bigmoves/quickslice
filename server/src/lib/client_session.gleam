/// Client session management for JS SDK cookie-based authentication
///
/// Handles HTTP-only cookie creation and validation with configurable
/// SameSite, Secure, and Domain settings from database config.
import database/executor.{type Executor}
import database/repositories/client_session as client_session_repo
import database/repositories/config as config_repo
import database/repositories/oauth_access_tokens
import gleam/bit_array
import gleam/crypto
import gleam/erlang/process.{type Subject}
import gleam/http/cookie
import gleam/http/request
import gleam/http/response
import gleam/option.{type Option, None, Some}
import gleam/result
import lib/oauth/atproto/did_resolver
import lib/oauth/did_cache
import wisp.{type Request, type Response}

/// Session data returned to clients
pub type SessionInfo {
  SessionInfo(did: Option(String), handle: Option(String), authenticated: Bool)
}

const session_cookie_name = "quickslice_client_session"

// 14 days in seconds
const session_max_age_seconds = 1_209_600

/// Generate a new session ID using cryptographically secure random bytes
pub fn generate_session_id() -> String {
  let random_bytes = crypto.strong_random_bytes(32)
  bit_array.base64_url_encode(random_bytes, False)
}

/// Set session cookie on response with configurable settings from database
pub fn set_session_cookie(
  response: Response,
  req: Request,
  db: Executor,
  session_id: String,
) -> Response {
  // Get cookie settings from database
  let same_site = config_repo.get_cookie_same_site(db)
  let secure_mode = config_repo.get_cookie_secure(db)
  let domain = config_repo.get_cookie_domain(db)

  // Determine Secure flag based on mode and request
  let is_secure = case secure_mode {
    config_repo.Always -> True
    config_repo.Never -> False
    config_repo.Auto -> {
      // Check if request came over HTTPS by looking at headers
      case request.get_header(req, "x-forwarded-proto") {
        Ok("https") -> True
        _ -> False
      }
    }
  }

  // Convert SameSite to cookie option
  let same_site_opt = case same_site {
    config_repo.Strict -> Some(cookie.Strict)
    config_repo.Lax -> Some(cookie.Lax)
    config_repo.CookieSameSiteNone -> Some(cookie.None)
  }

  // Sign the session ID
  let signed_value = wisp.sign_message(req, <<session_id:utf8>>, crypto.Sha512)

  // Build cookie attributes
  let attributes =
    cookie.Attributes(
      max_age: Some(session_max_age_seconds),
      domain: case domain {
        Ok(d) -> Some(d)
        Error(_) -> None
      },
      path: Some("/"),
      secure: is_secure,
      http_only: True,
      same_site: same_site_opt,
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

/// Get session info for the current request
/// Returns session data if valid session cookie exists
pub fn get_session_info(
  req: Request,
  db: Executor,
  did_cache: Subject(did_cache.Message),
) -> Result(SessionInfo, Nil) {
  use session_id <- result.try(get_session_id(req))
  use session_opt <- result.try(
    client_session_repo.get(db, session_id) |> result.replace_error(Nil),
  )
  use session <- result.try(case session_opt {
    Some(s) -> Ok(s)
    None -> Error(Nil)
  })

  // Touch session to update last activity
  let _ = client_session_repo.touch(db, session_id)

  case session.user_did {
    None ->
      // Session exists but user not authenticated yet
      Ok(SessionInfo(did: None, handle: None, authenticated: False))
    Some(did) -> {
      // Resolve handle from DID
      let handle = case
        did_resolver.resolve_did_with_cache(did_cache, did, False)
      {
        Ok(doc) -> did_resolver.get_handle(doc)
        Error(_) -> None
      }
      Ok(SessionInfo(did: Some(did), handle: handle, authenticated: True))
    }
  }
}

/// Get the current session's access token for making authenticated requests
/// Returns the OAuth access token if the session is valid and authenticated
pub fn get_session_access_token(
  req: Request,
  db: Executor,
) -> Result(String, Nil) {
  use session_id <- result.try(get_session_id(req))
  use session_opt <- result.try(
    client_session_repo.get(db, session_id) |> result.replace_error(Nil),
  )
  use session <- result.try(case session_opt {
    Some(s) -> Ok(s)
    None -> Error(Nil)
  })

  // Must have an ATP session ID to get tokens
  use atp_session_id <- result.try(case session.atp_session_id {
    Some(id) -> Ok(id)
    None -> Error(Nil)
  })

  // Look up OAuth access token by session_id
  let access_token_opt =
    oauth_access_tokens.get_by_session_id(db, atp_session_id)
    |> result.unwrap(None)

  case access_token_opt {
    Some(token) -> Ok(token.token)
    None -> Error(Nil)
  }
}

/// Verify that a DPoP proof's JKT matches the session's stored JKT
pub fn verify_dpop_jkt(
  req: Request,
  db: Executor,
  dpop_jkt: String,
) -> Result(Nil, Nil) {
  use session_id <- result.try(get_session_id(req))
  use session_opt <- result.try(
    client_session_repo.get(db, session_id) |> result.replace_error(Nil),
  )
  use session <- result.try(case session_opt {
    Some(s) -> Ok(s)
    None -> Error(Nil)
  })

  case session.dpop_jkt == dpop_jkt {
    True -> Ok(Nil)
    False -> Error(Nil)
  }
}

/// Create a new client session
/// Called after OAuth callback to establish session with cookie
pub fn create_session(
  db: Executor,
  client_id: String,
  user_did: Option(String),
  atp_session_id: Option(String),
  dpop_jkt: String,
) -> Result(String, Nil) {
  let session_id = generate_session_id()
  case
    client_session_repo.insert(
      db,
      session_id,
      client_id,
      user_did,
      atp_session_id,
      dpop_jkt,
    )
  {
    Ok(_) -> Ok(session_id)
    Error(_) -> Error(Nil)
  }
}

/// Delete a client session (logout)
pub fn destroy_session(req: Request, db: Executor) -> Result(Nil, Nil) {
  use session_id <- result.try(get_session_id(req))
  client_session_repo.delete(db, session_id) |> result.replace_error(Nil)
}
