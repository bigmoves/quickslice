/// HTTP handlers for client session management
///
/// Provides REST endpoints for cookie-based session management:
/// - POST /api/client/session - Create session after OAuth callback
/// - GET /api/client/session - Get current session status
/// - DELETE /api/client/session - Logout (destroy session)
import database/executor.{type Executor}
import gleam/bit_array
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/http
import gleam/json
import gleam/option.{None, Some}
import gleam/result
import lib/client_session
import lib/oauth/did_cache
import wisp.{type Request, type Response}

/// Handle all client session requests based on method
pub fn handle(
  req: Request,
  db: Executor,
  did_cache: Subject(did_cache.Message),
) -> Response {
  case req.method {
    http.Post -> handle_create(req, db)
    http.Get -> handle_get(req, db, did_cache)
    http.Delete -> handle_delete(req, db)
    _ -> method_not_allowed()
  }
}

/// POST /api/client/session
/// Create a new session after OAuth callback
/// Body: { clientId, dpopJkt, userDid?, atpSessionId? }
fn handle_create(req: Request, db: Executor) -> Response {
  case wisp.read_body_bits(req) {
    Ok(body) -> {
      case bit_array.to_string(body) {
        Ok(body_string) -> {
          case parse_create_request(body_string) {
            Ok(create_req) -> {
              case
                client_session.create_session(
                  db,
                  create_req.client_id,
                  create_req.user_did,
                  create_req.atp_session_id,
                  create_req.dpop_jkt,
                )
              {
                Ok(session_id) -> {
                  // Build response with session info
                  let response_json =
                    json.object([
                      #(
                        "authenticated",
                        json.bool(option.is_some(create_req.user_did)),
                      ),
                      #("did", case create_req.user_did {
                        Some(did) -> json.string(did)
                        None -> json.null()
                      }),
                    ])

                  wisp.response(200)
                  |> wisp.set_header("content-type", "application/json")
                  |> wisp.set_body(wisp.Text(json.to_string(response_json)))
                  |> client_session.set_session_cookie(req, db, session_id)
                }
                Error(_) -> internal_error("Failed to create session")
              }
            }
            Error(err) -> bad_request(err)
          }
        }
        Error(_) -> bad_request("Request body must be valid UTF-8")
      }
    }
    Error(_) -> bad_request("Failed to read request body")
  }
}

/// GET /api/client/session
/// Get current session status
fn handle_get(
  req: Request,
  db: Executor,
  did_cache: Subject(did_cache.Message),
) -> Response {
  case client_session.get_session_info(req, db, did_cache) {
    Ok(info) -> {
      let response_json =
        json.object([
          #("authenticated", json.bool(info.authenticated)),
          #("did", case info.did {
            Some(did) -> json.string(did)
            None -> json.null()
          }),
          #("handle", case info.handle {
            Some(handle) -> json.string(handle)
            None -> json.null()
          }),
        ])

      wisp.response(200)
      |> wisp.set_header("content-type", "application/json")
      |> wisp.set_body(wisp.Text(json.to_string(response_json)))
    }
    Error(_) -> {
      // No valid session - return unauthenticated response
      let response_json =
        json.object([
          #("authenticated", json.bool(False)),
          #("did", json.null()),
          #("handle", json.null()),
        ])

      wisp.response(200)
      |> wisp.set_header("content-type", "application/json")
      |> wisp.set_body(wisp.Text(json.to_string(response_json)))
    }
  }
}

/// DELETE /api/client/session
/// Logout - destroy current session
fn handle_delete(req: Request, db: Executor) -> Response {
  let _ = client_session.destroy_session(req, db)

  wisp.response(200)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text("{\"success\": true}"))
  |> client_session.clear_session_cookie(req)
}

// Request parsing types

type CreateSessionRequest {
  CreateSessionRequest(
    client_id: String,
    dpop_jkt: String,
    user_did: option.Option(String),
    atp_session_id: option.Option(String),
  )
}

fn parse_create_request(
  json_str: String,
) -> Result(CreateSessionRequest, String) {
  let decoder = {
    use client_id <- decode.field("clientId", decode.string)
    use dpop_jkt <- decode.field("dpopJkt", decode.string)
    use user_did <- decode.optional_field(
      "userDid",
      None,
      decode.optional(decode.string),
    )
    use atp_session_id <- decode.optional_field(
      "atpSessionId",
      None,
      decode.optional(decode.string),
    )
    decode.success(CreateSessionRequest(
      client_id: client_id,
      dpop_jkt: dpop_jkt,
      user_did: user_did,
      atp_session_id: atp_session_id,
    ))
  }

  json.parse(json_str, decoder)
  |> result.map_error(fn(_) { "Invalid JSON or missing required fields" })
}

// Response helpers

fn bad_request(message: String) -> Response {
  wisp.response(400)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text(
    "{\"error\": \"BadRequest\", \"message\": \"" <> message <> "\"}",
  ))
}

fn internal_error(message: String) -> Response {
  wisp.response(500)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text(
    "{\"error\": \"InternalError\", \"message\": \"" <> message <> "\"}",
  ))
}

fn method_not_allowed() -> Response {
  wisp.response(405)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text(
    "{\"error\": \"MethodNotAllowed\", \"message\": \"Only POST, GET, DELETE are allowed\"}",
  ))
}
