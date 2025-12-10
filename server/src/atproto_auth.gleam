import database/repositories/oauth_access_tokens
import database/repositories/oauth_atp_sessions
import gleam/erlang/process.{type Subject}
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import lib/oauth/atproto/bridge
import lib/oauth/atproto/did_resolver
import lib/oauth/did_cache
import lib/oauth/token_generator
import sqlight

/// UserInfo response from OAuth provider
pub type UserInfo {
  UserInfo(sub: String, did: String)
}

/// ATProto session data from AIP
pub type AtprotoSession {
  AtprotoSession(pds_endpoint: String, access_token: String, dpop_jwk: String)
}

/// Error type for authentication operations
pub type AuthError {
  MissingAuthHeader
  InvalidAuthHeader
  UnauthorizedToken
  TokenExpired
  SessionNotFound
  SessionNotReady
  RefreshFailed(String)
  DIDResolutionFailed(String)
  NetworkError
  ParseError
}

/// Extract bearer token from Authorization header
///
/// # Example
/// ```gleam
/// extract_bearer_token(request.headers)
/// // Ok("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...")
/// ```
pub fn extract_bearer_token(
  from headers: List(#(String, String)),
) -> Result(String, AuthError) {
  headers
  |> list_find(one_that: fn(header) {
    string.lowercase(header.0) == "authorization"
  })
  |> result.map(fn(header) { header.1 })
  |> result.replace_error(MissingAuthHeader)
  |> result.try(fn(auth_value) {
    case string.starts_with(auth_value, "Bearer ") {
      True -> {
        auth_value
        |> string.drop_start(7)
        |> Ok
      }
      False -> Error(InvalidAuthHeader)
    }
  })
}

/// Helper function to find in list
fn list_find(
  in list: List(a),
  one_that predicate: fn(a) -> Bool,
) -> Result(a, Nil) {
  case list {
    [] -> Error(Nil)
    [first, ..rest] ->
      case predicate(first) {
        True -> Ok(first)
        False -> list_find(rest, predicate)
      }
  }
}

/// Verify token from local database and return user info
pub fn verify_token(
  conn: sqlight.Connection,
  token: String,
) -> Result(UserInfo, AuthError) {
  // Look up token in database
  case oauth_access_tokens.get(conn, token) {
    Error(_) -> Error(UnauthorizedToken)
    Ok(None) -> Error(UnauthorizedToken)
    Ok(Some(access_token)) -> {
      // Check if revoked
      case access_token.revoked {
        True -> Error(UnauthorizedToken)
        False -> {
          // Check if expired
          let now = token_generator.current_timestamp()
          case access_token.expires_at < now {
            True -> Error(TokenExpired)
            False -> {
              // Check user_id is present
              case access_token.user_id {
                None -> Error(UnauthorizedToken)
                Some(did) -> Ok(UserInfo(sub: did, did: did))
              }
            }
          }
        }
      }
    }
  }
}

/// Get ATP session from local database, refreshing if needed
pub fn get_atp_session(
  conn: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
  token: String,
  signing_key: Option(String),
  atp_client_id: String,
) -> Result(AtprotoSession, AuthError) {
  // Look up access token to get session_id and iteration
  use access_token <- result.try(case oauth_access_tokens.get(conn, token) {
    Error(_) -> Error(UnauthorizedToken)
    Ok(None) -> Error(UnauthorizedToken)
    Ok(Some(t)) -> Ok(t)
  })

  // Get session_id and iteration
  use #(session_id, iteration) <- result.try(
    case access_token.session_id, access_token.session_iteration {
      Some(sid), Some(iter) -> Ok(#(sid, iter))
      _, _ -> Error(SessionNotFound)
    },
  )

  // Look up ATP session
  use atp_session <- result.try(
    case oauth_atp_sessions.get(conn, session_id, iteration) {
      Error(_) -> Error(SessionNotFound)
      Ok(None) -> Error(SessionNotFound)
      Ok(Some(s)) -> Ok(s)
    },
  )

  // Validate session is ready (exchanged, no error, has access token)
  use _ <- result.try(case atp_session.exchange_error {
    Some(_) -> Error(SessionNotReady)
    None -> Ok(Nil)
  })

  use _ <- result.try(case atp_session.session_exchanged_at {
    None -> Error(SessionNotReady)
    Some(_) -> Ok(Nil)
  })

  use atp_access_token <- result.try(case atp_session.access_token {
    None -> Error(SessionNotReady)
    Some(t) -> Ok(t)
  })

  // Check if ATP token is expired and refresh if needed
  let now = token_generator.current_timestamp()
  use current_session <- result.try(case atp_session.access_token_expires_at {
    Some(expires_at) if expires_at < now -> {
      // Token expired, try to refresh
      case
        bridge.refresh_tokens(
          conn,
          did_cache,
          atp_session,
          atp_client_id,
          signing_key,
        )
      {
        Ok(refreshed) -> {
          // Update the access token's session_iteration to point to the new ATP session
          let _ =
            oauth_access_tokens.update_session_iteration(
              conn,
              token,
              refreshed.iteration,
            )
          Ok(refreshed)
        }
        Error(err) -> Error(RefreshFailed(string.inspect(err)))
      }
    }
    _ -> Ok(atp_session)
  })

  // Get the (possibly refreshed) access token
  use final_access_token <- result.try(case current_session.access_token {
    None -> Error(SessionNotReady)
    Some(t) -> Ok(t)
  })

  // Get DID from session
  use did <- result.try(case current_session.did {
    None -> Error(SessionNotFound)
    Some(d) -> Ok(d)
  })

  // Resolve DID to get PDS endpoint
  use did_doc <- result.try(
    did_resolver.resolve_did_with_cache(did_cache, did, False)
    |> result.map_error(fn(err) { DIDResolutionFailed(string.inspect(err)) }),
  )

  use pds_endpoint <- result.try(case did_resolver.get_pds_endpoint(did_doc) {
    None -> Error(DIDResolutionFailed("No PDS endpoint in DID document"))
    Some(endpoint) -> Ok(endpoint)
  })

  // Suppress unused variable warning for atp_access_token
  let _ = atp_access_token
  let _ = final_access_token

  Ok(AtprotoSession(
    pds_endpoint: pds_endpoint,
    access_token: case current_session.access_token {
      Some(t) -> t
      None -> ""
    },
    dpop_jwk: current_session.dpop_key,
  ))
}
