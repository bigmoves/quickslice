/// Admin OAuth callback handler
/// GET /admin/oauth/callback - Handles ATP OAuth callback for admin login
import database/repositories/admin_session
import database/repositories/config as config_repo
import database/repositories/oauth_access_tokens
import database/repositories/oauth_atp_requests
import database/repositories/oauth_atp_sessions
import database/repositories/oauth_refresh_tokens
import database/types.{Bearer, OAuthAccessToken, OAuthRefreshToken}
import gleam/crypto
import gleam/erlang/process.{type Subject}
import gleam/http/cookie
import gleam/http/response
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/uri
import lib/oauth/atproto/bridge
import lib/oauth/did_cache
import lib/oauth/token_generator
import sqlight
import wisp

/// Handle GET /admin/oauth/callback
pub fn handle(
  req: wisp.Request,
  conn: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
  redirect_uri: String,
  client_id: String,
  signing_key: Option(String),
) -> wisp.Response {
  // Parse query parameters
  let query = wisp.get_query(req)

  // Check for OAuth error FIRST (user denied, etc.)
  case list.key_find(query, "error") {
    Ok(error) -> {
      let error_description =
        list.key_find(query, "error_description")
        |> result.unwrap("")

      // Redirect to / or /onboarding based on admin existence
      let redirect_path = case config_repo.has_admins(conn) {
        True -> "/"
        False -> "/onboarding"
      }

      let redirect_url =
        redirect_path
        <> "?error="
        <> uri.percent_encode(error)
        <> "&error_description="
        <> uri.percent_encode(error_description)

      wisp.redirect(redirect_url)
    }
    Error(_) -> {
      // Normal flow: check for code and state
      let code_result = list.key_find(query, "code")
      let state_result = list.key_find(query, "state")

      case code_result, state_result {
        Error(_), _ -> error_response(400, "Missing 'code' parameter")
        _, Error(_) -> error_response(400, "Missing 'state' parameter")
        Ok(code), Ok(state) -> {
          process_callback(
            req,
            conn,
            did_cache,
            code,
            state,
            redirect_uri,
            client_id,
            signing_key,
          )
        }
      }
    }
  }
}

fn process_callback(
  req: wisp.Request,
  conn: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
  code: String,
  state: String,
  redirect_uri: String,
  client_id: String,
  signing_key: Option(String),
) -> wisp.Response {
  // Retrieve ATP session by state
  case oauth_atp_sessions.get_by_state(conn, state) {
    Error(err) -> error_response(500, "Database error: " <> string.inspect(err))
    Ok(None) -> error_response(400, "Invalid or expired state parameter")
    Ok(Some(atp_session)) -> {
      // Retrieve ATP request to get PKCE verifier
      case oauth_atp_requests.get(conn, state) {
        Error(err) ->
          error_response(500, "Database error: " <> string.inspect(err))
        Ok(None) ->
          error_response(400, "OAuth request not found - PKCE verifier missing")
        Ok(Some(atp_request)) -> {
          let code_verifier = atp_request.pkce_verifier

          // Call bridge to exchange code for tokens
          case
            bridge.handle_callback(
              conn,
              did_cache,
              atp_session,
              code,
              code_verifier,
              redirect_uri,
              client_id,
              state,
              signing_key,
            )
          {
            Error(bridge_err) -> {
              error_response(
                500,
                "Token exchange failed: " <> bridge_error_to_string(bridge_err),
              )
            }
            Ok(updated_session) -> {
              // Clean up one-time-use oauth request
              let _ = oauth_atp_requests.delete(conn, state)

              // Generate admin session ID (for cookie)
              let admin_session_id = token_generator.generate_session_id()

              // Create admin session linking to ATP session
              case
                admin_session.insert(
                  conn,
                  admin_session_id,
                  updated_session.session_id,
                )
              {
                Error(err) ->
                  error_response(
                    500,
                    "Failed to create admin session: " <> string.inspect(err),
                  )
                Ok(_) -> {
                  // Get DID from ATP session
                  let did = case updated_session.did {
                    Some(d) -> d
                    None -> ""
                  }

                  // If no admins exist, register this user as the first admin
                  case config_repo.has_admins(conn) {
                    False -> {
                      let _ = config_repo.add_admin_did(conn, did)
                      wisp.log_info(
                        "[onboarding] First admin registered: " <> did,
                      )
                    }
                    True -> Nil
                  }

                  // Generate OAuth access token for GraphiQL/API use
                  let access_token_value =
                    token_generator.generate_access_token()
                  let refresh_token_value =
                    token_generator.generate_refresh_token()
                  let now = token_generator.current_timestamp()

                  let access_token =
                    OAuthAccessToken(
                      token: access_token_value,
                      token_type: Bearer,
                      client_id: "admin",
                      user_id: Some(did),
                      session_id: Some(updated_session.session_id),
                      session_iteration: Some(updated_session.iteration),
                      scope: None,
                      created_at: now,
                      expires_at: token_generator.expiration_timestamp(
                        3600 * 24 * 7,
                      ),
                      revoked: False,
                      dpop_jkt: None,
                    )

                  let refresh_token =
                    OAuthRefreshToken(
                      token: refresh_token_value,
                      access_token: access_token_value,
                      client_id: "admin",
                      user_id: did,
                      session_id: Some(updated_session.session_id),
                      session_iteration: Some(updated_session.iteration),
                      scope: None,
                      created_at: now,
                      expires_at: None,
                      revoked: False,
                    )

                  // Insert OAuth tokens
                  case oauth_access_tokens.insert(conn, access_token) {
                    Ok(_) ->
                      wisp.log_info(
                        "OAuth access token created for session: "
                        <> updated_session.session_id,
                      )
                    Error(err) ->
                      wisp.log_error(
                        "Failed to create OAuth access token: "
                        <> string.inspect(err),
                      )
                  }
                  case oauth_refresh_tokens.insert(conn, refresh_token) {
                    Ok(_) ->
                      wisp.log_info(
                        "OAuth refresh token created for session: "
                        <> updated_session.session_id,
                      )
                    Error(err) ->
                      wisp.log_error(
                        "Failed to create OAuth refresh token: "
                        <> string.inspect(err),
                      )
                  }

                  // Set session cookie and redirect to home
                  wisp.redirect("/")
                  |> set_session_cookie(req, admin_session_id)
                }
              }
            }
          }
        }
      }
    }
  }
}

fn error_response(status: Int, message: String) -> wisp.Response {
  wisp.log_error("Admin OAuth callback error: " <> message)
  let json_body =
    json.object([
      #("error", json.string("server_error")),
      #("error_description", json.string(message)),
    ])

  wisp.response(status)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text(json.to_string(json_body)))
}

fn bridge_error_to_string(err: bridge.BridgeError) -> String {
  case err {
    bridge.DIDResolutionError(_) -> "DID resolution failed"
    bridge.PDSNotFound(msg) -> "PDS not found: " <> msg
    bridge.TokenExchangeError(msg) -> "Token exchange failed: " <> msg
    bridge.HTTPError(msg) -> "HTTP error: " <> msg
    bridge.InvalidResponse(msg) -> "Invalid response: " <> msg
    bridge.StorageError(msg) -> "Storage error: " <> msg
    bridge.MetadataFetchError(msg) -> "Metadata fetch failed: " <> msg
    bridge.PARError(msg) -> "PAR error: " <> msg
  }
}

/// Set session cookie on response
fn set_session_cookie(
  resp: wisp.Response,
  req: wisp.Request,
  session_id: String,
) -> wisp.Response {
  // Sign the session ID the same way wisp does
  let signed_value = wisp.sign_message(req, <<session_id:utf8>>, crypto.Sha512)

  // Create cookie attributes
  let attributes =
    cookie.Attributes(
      max_age: option.Some(60 * 60 * 24 * 14),
      // 14 days
      domain: option.None,
      path: option.Some("/"),
      secure: False,
      // False for localhost HTTP
      http_only: True,
      same_site: option.None,
    )

  response.set_cookie(resp, "quickslice_session", signed_value, attributes)
}
