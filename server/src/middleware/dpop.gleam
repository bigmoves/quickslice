/// DPoP validation middleware for protected resources
import database/executor.{type Executor}
import database/repositories/oauth_access_tokens
import database/repositories/oauth_dpop_jti
import gleam/http.{Delete, Get, Head, Options, Patch, Post, Put}
import gleam/http/request
import gleam/option.{None, Some}
import gleam/string
import lib/oauth/dpop/validator
import lib/oauth/token_generator
import wisp

/// Validate DPoP-bound access token
/// Returns the user_id if valid, or an error response
pub fn validate_dpop_access(
  req: wisp.Request,
  db: Executor,
  resource_url: String,
) -> Result(String, wisp.Response) {
  // Extract Authorization header
  case request.get_header(req, "authorization") {
    Error(_) -> Error(unauthorized("Missing Authorization header"))
    Ok(header) -> {
      // Parse "DPoP <token>" or "Bearer <token>"
      case string.split(header, " ") {
        ["DPoP", token] -> validate_dpop_token(req, db, token, resource_url)
        ["Bearer", token] -> validate_bearer_token(db, token)
        _ -> Error(unauthorized("Invalid Authorization header format"))
      }
    }
  }
}

fn validate_dpop_token(
  req: wisp.Request,
  db: Executor,
  token: String,
  resource_url: String,
) -> Result(String, wisp.Response) {
  // Get DPoP proof from header
  case validator.get_dpop_header(req.headers) {
    None -> Error(unauthorized("Missing DPoP proof for DPoP-bound token"))
    Some(dpop_proof) -> {
      // Verify the DPoP proof
      let method = method_to_string(req.method)
      case validator.verify_dpop_proof(dpop_proof, method, resource_url, 300) {
        Error(reason) -> Error(unauthorized("Invalid DPoP proof: " <> reason))
        Ok(dpop_result) -> {
          // Check JTI for replay
          case oauth_dpop_jti.use_jti(db, dpop_result.jti, dpop_result.iat) {
            Error(_) -> Error(server_error("Database error"))
            Ok(False) -> Error(unauthorized("DPoP proof replay detected"))
            Ok(True) -> {
              // Get the access token and verify JKT matches
              case oauth_access_tokens.get(db, token) {
                Error(_) -> Error(server_error("Database error"))
                Ok(None) -> Error(unauthorized("Invalid access token"))
                Ok(Some(access_token)) -> {
                  // Check if token is expired
                  case token_generator.is_expired(access_token.expires_at) {
                    True -> Error(unauthorized("Access token has expired"))
                    False -> {
                      // Check if token is revoked
                      case access_token.revoked {
                        True ->
                          Error(unauthorized("Access token has been revoked"))
                        False -> {
                          case access_token.dpop_jkt {
                            None ->
                              Error(unauthorized("Token is not DPoP-bound"))
                            Some(jkt) -> {
                              case jkt == dpop_result.jkt {
                                False ->
                                  Error(unauthorized("DPoP key mismatch"))
                                True -> {
                                  case access_token.user_id {
                                    None ->
                                      Error(unauthorized("Token has no user"))
                                    Some(user_id) -> Ok(user_id)
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

fn validate_bearer_token(
  db: Executor,
  token: String,
) -> Result(String, wisp.Response) {
  case oauth_access_tokens.get(db, token) {
    Error(_) -> Error(server_error("Database error"))
    Ok(None) -> Error(unauthorized("Invalid access token"))
    Ok(Some(access_token)) -> {
      // Check if token is expired
      case token_generator.is_expired(access_token.expires_at) {
        True -> Error(unauthorized("Access token has expired"))
        False -> {
          // Check if token is revoked
          case access_token.revoked {
            True -> Error(unauthorized("Access token has been revoked"))
            False -> {
              // DPoP-bound tokens MUST use DPoP authorization
              case access_token.dpop_jkt {
                Some(_) ->
                  Error(unauthorized(
                    "DPoP-bound token requires DPoP authorization",
                  ))
                None -> {
                  case access_token.user_id {
                    None -> Error(unauthorized("Token has no user"))
                    Some(user_id) -> Ok(user_id)
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

fn method_to_string(method: http.Method) -> String {
  case method {
    Get -> "GET"
    Post -> "POST"
    Put -> "PUT"
    Delete -> "DELETE"
    Patch -> "PATCH"
    Head -> "HEAD"
    Options -> "OPTIONS"
    _ -> "GET"
  }
}

fn unauthorized(message: String) -> wisp.Response {
  wisp.response(401)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text("{\"error\":\"" <> message <> "\"}"))
}

fn server_error(message: String) -> wisp.Response {
  wisp.response(500)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text("{\"error\":\"" <> message <> "\"}"))
}
