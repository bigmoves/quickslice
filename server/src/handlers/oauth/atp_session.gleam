/// ATP Session API endpoint
/// GET /api/atp/sessions/:session_id
/// Returns ATP session data for authenticated clients
import database/repositories/oauth_access_tokens
import database/repositories/oauth_atp_sessions
import gleam/http/request
import gleam/json
import gleam/option.{None, Some}
import gleam/string
import lib/oauth/token_generator
import sqlight
import wisp

/// Handle GET /api/atp/sessions/:session_id
pub fn handle(
  req: wisp.Request,
  conn: sqlight.Connection,
  session_id: String,
) -> wisp.Response {
  // Extract Bearer token from Authorization header
  case request.get_header(req, "authorization") {
    Error(_) ->
      error_response(401, "unauthorized", "Missing Authorization header")
    Ok(auth_header) -> {
      case parse_bearer_token(auth_header) {
        None ->
          error_response(
            401,
            "unauthorized",
            "Invalid Authorization header format",
          )
        Some(token_value) -> {
          // Validate access token
          case oauth_access_tokens.get(conn, token_value) {
            Error(err) ->
              error_response(
                500,
                "server_error",
                "Database error: " <> string.inspect(err),
              )
            Ok(None) ->
              error_response(401, "unauthorized", "Invalid access token")
            Ok(Some(access_token)) -> {
              // Check if token is revoked
              case access_token.revoked {
                True ->
                  error_response(
                    401,
                    "unauthorized",
                    "Access token has been revoked",
                  )
                False -> {
                  // Check if token is expired
                  case token_generator.is_expired(access_token.expires_at) {
                    True ->
                      error_response(
                        401,
                        "unauthorized",
                        "Access token has expired",
                      )
                    False -> {
                      // Verify token's session_id matches requested session
                      case access_token.session_id {
                        None ->
                          error_response(
                            403,
                            "forbidden",
                            "Token has no associated session",
                          )
                        Some(token_session_id) -> {
                          case token_session_id == session_id {
                            False ->
                              error_response(
                                403,
                                "forbidden",
                                "Token not authorized for this session",
                              )
                            True -> {
                              // Get ATP session (latest iteration)
                              case
                                oauth_atp_sessions.get_latest(conn, session_id)
                              {
                                Error(err) ->
                                  error_response(
                                    500,
                                    "server_error",
                                    "Database error: " <> string.inspect(err),
                                  )
                                Ok(None) ->
                                  error_response(
                                    404,
                                    "not_found",
                                    "Session not found",
                                  )
                                Ok(Some(session)) -> {
                                  // Verify session has valid tokens
                                  case
                                    session.access_token,
                                    session.refresh_token,
                                    session.did
                                  {
                                    Some(at), Some(rt), Some(did) -> {
                                      // Parse scopes from space-separated string
                                      let scopes = case
                                        session.access_token_scopes
                                      {
                                        Some(scopes_str) ->
                                          string.split(scopes_str, " ")
                                        None -> []
                                      }

                                      // Return session data
                                      session_response(
                                        session.session_id,
                                        did,
                                        at,
                                        rt,
                                        session.dpop_key,
                                        session.access_token_expires_at
                                          |> option.unwrap(0),
                                        scopes,
                                        session.session_created_at,
                                      )
                                    }
                                    _, _, _ ->
                                      error_response(
                                        500,
                                        "server_error",
                                        "Session missing required data",
                                      )
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
        }
      }
    }
  }
}

/// Parse Bearer token from Authorization header
fn parse_bearer_token(auth_header: String) -> option.Option(String) {
  case string.starts_with(auth_header, "Bearer ") {
    True -> Some(string.drop_start(auth_header, 7))
    False -> None
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

fn session_response(
  session_id: String,
  did: String,
  access_token: String,
  refresh_token: String,
  dpop_key: String,
  access_token_expires_at: Int,
  access_token_scopes: List(String),
  session_created_at: Int,
) -> wisp.Response {
  let json_body =
    json.object([
      #("session_id", json.string(session_id)),
      #("did", json.string(did)),
      #("access_token", json.string(access_token)),
      #("refresh_token", json.string(refresh_token)),
      #("dpop_key", json.string(dpop_key)),
      #("access_token_expires_at", json.int(access_token_expires_at)),
      #("access_token_scopes", json.array(access_token_scopes, json.string)),
      #("session_created_at", json.int(session_created_at)),
    ])

  wisp.response(200)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text(json.to_string(json_body)))
}
