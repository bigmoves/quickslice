/// ATP OAuth callback endpoint
/// Handles OAuth callback from ATProtocol PDS after user authorization
import actor_validator
import backfill
import database/executor.{type Executor}
import database/repositories/config as config_repo
import database/repositories/oauth_atp_requests
import database/repositories/oauth_atp_sessions
import database/repositories/oauth_auth_requests
import database/repositories/oauth_authorization_code
import database/types.{OAuthAuthorizationCode, Plain, S256}
import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/uri
import lib/oauth/atproto/bridge
import lib/oauth/did_cache
import lib/oauth/token_generator
import logging
import wisp

/// Handle GET /oauth/atp/callback
pub fn handle(
  req: wisp.Request,
  conn: Executor,
  did_cache: process.Subject(did_cache.Message),
  redirect_uri: String,
  client_id: String,
  signing_key: Option(String),
) -> wisp.Response {
  // Parse query parameters from request path
  let query = wisp.get_query(req)

  // Check for OAuth error FIRST (user denied, etc.)
  case list.key_find(query, "error") {
    Ok(error) -> {
      handle_oauth_error(conn, query, error)
    }
    Error(_) -> {
      // Normal flow: check for code and state
      let code_result = list.key_find(query, "code")
      let state_result = list.key_find(query, "state")

      case code_result, state_result {
        Error(_), _ ->
          error_response(400, "missing_parameter", "Missing 'code' parameter")
        _, Error(_) ->
          error_response(400, "missing_parameter", "Missing 'state' parameter")
        Ok(code), Ok(state) -> {
          handle_callback(
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

fn handle_oauth_error(
  conn: Executor,
  query: List(#(String, String)),
  error: String,
) -> wisp.Response {
  let error_description =
    list.key_find(query, "error_description")
    |> result.unwrap("")
  let state =
    list.key_find(query, "state")
    |> result.unwrap("")

  // Look up client's redirect_uri via: state -> atp_session -> session_id -> auth_request
  case oauth_atp_sessions.get_by_state(conn, state) {
    Ok(Some(atp_session)) -> {
      case oauth_auth_requests.get(conn, atp_session.session_id) {
        Ok(Some(auth_request)) -> {
          // Build redirect URL with error params
          let separator = case string.contains(auth_request.redirect_uri, "?") {
            True -> "&"
            False -> "?"
          }

          let redirect_url =
            auth_request.redirect_uri
            <> separator
            <> "error="
            <> uri.percent_encode(error)
            <> "&error_description="
            <> uri.percent_encode(error_description)
            <> case auth_request.state {
              Some(client_state) ->
                "&state=" <> uri.percent_encode(client_state)
              None -> ""
            }

          wisp.redirect(redirect_url)
        }
        _ -> error_response(400, "missing_parameter", "OAuth session not found")
      }
    }
    _ -> error_response(400, "missing_parameter", "Invalid state parameter")
  }
}

fn handle_callback(
  conn: Executor,
  did_cache: process.Subject(did_cache.Message),
  code: String,
  state: String,
  redirect_uri: String,
  client_id: String,
  signing_key: Option(String),
) -> wisp.Response {
  // Retrieve ATP session by state
  case oauth_atp_sessions.get_by_state(conn, state) {
    Error(err) ->
      error_response(
        500,
        "server_error",
        "Database error: " <> string.inspect(err),
      )
    Ok(None) ->
      error_response(400, "invalid_state", "Invalid or expired state parameter")
    Ok(Some(session)) -> {
      // Retrieve ATP request to get PKCE verifier
      case oauth_atp_requests.get(conn, state) {
        Error(err) ->
          error_response(
            500,
            "server_error",
            "Database error: " <> string.inspect(err),
          )
        Ok(None) ->
          error_response(
            400,
            "invalid_request",
            "OAuth request not found - PKCE verifier missing",
          )
        Ok(Some(atp_request)) -> {
          let code_verifier = atp_request.pkce_verifier

          // Call bridge to exchange code for tokens
          case
            bridge.handle_callback(
              conn,
              did_cache,
              session,
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
                "token_exchange_failed",
                bridge_error_to_string(bridge_err),
              )
            }
            Ok(updated_session) -> {
              // Sync actor on first login (blocking)
              case updated_session.did {
                Some(did) -> {
                  let plc_url = config_repo.get_plc_directory_url(conn)
                  let #(collection_ids, external_collection_ids) =
                    backfill.get_collection_ids(conn)

                  case actor_validator.ensure_actor_exists(conn, did, plc_url) {
                    Ok(True) -> {
                      // New actor - backfill collections synchronously
                      logging.log(
                        logging.Info,
                        "[oauth] Syncing new actor: " <> did,
                      )
                      let _ =
                        backfill.backfill_collections_for_actor(
                          conn,
                          did,
                          collection_ids,
                          external_collection_ids,
                          plc_url,
                        )
                      Nil
                    }
                    Ok(False) -> Nil
                    // Existing actor, already synced
                    Error(e) -> {
                      logging.log(
                        logging.Warning,
                        "[oauth] Actor sync failed for "
                          <> did
                          <> ": "
                          <> string.inspect(e),
                      )
                      Nil
                    }
                  }
                }
                None -> Nil
              }

              // Clean up one-time-use oauth request
              let _ = oauth_atp_requests.delete(conn, state)

              // Get client authorization request using session_id
              case oauth_auth_requests.get(conn, updated_session.session_id) {
                Error(err) ->
                  error_response(
                    500,
                    "server_error",
                    "Database error: " <> string.inspect(err),
                  )
                Ok(None) ->
                  error_response(
                    400,
                    "invalid_session",
                    "Client authorization request not found",
                  )
                Ok(Some(client_req)) -> {
                  // Check if client request expired
                  case token_generator.is_expired(client_req.expires_at) {
                    True ->
                      error_response(
                        400,
                        "expired_request",
                        "Client authorization request has expired",
                      )
                    False -> {
                      // Generate authorization code for client
                      let authorization_code_value =
                        token_generator.generate_authorization_code()
                      let now = token_generator.current_timestamp()
                      let code_expires_at =
                        token_generator.expiration_timestamp(600)
                      // 10 minutes

                      // Get DID from session (user_id for authorization code)
                      let user_id = case updated_session.did {
                        Some(did) -> did
                        None -> "unknown"
                      }

                      // Create authorization code record
                      let auth_code =
                        OAuthAuthorizationCode(
                          code: authorization_code_value,
                          client_id: client_req.client_id,
                          user_id: user_id,
                          session_id: Some(updated_session.session_id),
                          session_iteration: Some(updated_session.iteration),
                          redirect_uri: client_req.redirect_uri,
                          scope: client_req.scope,
                          code_challenge: client_req.code_challenge,
                          code_challenge_method: case
                            client_req.code_challenge_method
                          {
                            Some("S256") -> Some(S256)
                            Some("plain") -> Some(Plain)
                            _ -> None
                          },
                          nonce: client_req.nonce,
                          created_at: now,
                          expires_at: code_expires_at,
                          used: False,
                        )

                      // Store authorization code
                      case oauth_authorization_code.insert(conn, auth_code) {
                        Error(err) ->
                          error_response(
                            500,
                            "server_error",
                            "Failed to store authorization code: "
                              <> string.inspect(err),
                          )
                        Ok(_) -> {
                          // Clean up client authorization request (one-time use)
                          let _ =
                            oauth_auth_requests.delete(
                              conn,
                              updated_session.session_id,
                            )

                          // Build redirect URL with code and state
                          let query_params = [
                            "code="
                              <> uri.percent_encode(authorization_code_value),
                            case client_req.state {
                              Some(s) -> "state=" <> uri.percent_encode(s)
                              None -> ""
                            },
                          ]

                          let query_string =
                            query_params
                            |> list.filter(fn(p) { p != "" })
                            |> string.join("&")

                          let redirect_url =
                            client_req.redirect_uri <> "?" <> query_string

                          // Redirect to client
                          wisp.redirect(redirect_url)
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

fn error_response(
  status: Int,
  error: String,
  description: String,
) -> wisp.Response {
  let json_body =
    json.object([
      #("error", json.string(error)),
      #("error_description", json.string(description)),
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
