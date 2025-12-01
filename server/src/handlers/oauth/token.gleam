/// Token endpoint
/// POST /oauth/token
/// Exchanges authorization code or refresh token for access tokens
import database/repositories/oauth_access_tokens
import database/repositories/oauth_authorization_code
import database/repositories/oauth_clients
import database/repositories/oauth_refresh_tokens
import database/types.{
  type OAuthAuthorizationCode, type OAuthRefreshToken, Bearer, OAuthAccessToken,
  OAuthRefreshToken, Plain, S256,
}
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/uri
import lib/oauth/pkce
import lib/oauth/scopes/validator as scope_validator
import lib/oauth/token_generator
import lib/oauth/types/error
import sqlight
import wisp

/// Validate client authentication based on token_endpoint_auth_method
fn validate_client_authentication(
  client: types.OAuthClient,
  params: List(#(String, String)),
) -> Result(Nil, wisp.Response) {
  case client.token_endpoint_auth_method {
    types.AuthNone -> {
      // Public clients don't need authentication
      Ok(Nil)
    }
    types.ClientSecretPost -> {
      // Client secret must be in POST body
      case get_param(params, "client_secret") {
        None ->
          Error(error_response(
            401,
            "invalid_client",
            "client_secret is required for confidential clients",
          ))
        Some(provided_secret) -> {
          case client.client_secret {
            None ->
              Error(error_response(
                500,
                "server_error",
                "Client has no secret configured",
              ))
            Some(stored_secret) -> {
              case provided_secret == stored_secret {
                True -> Ok(Nil)
                False ->
                  Error(error_response(
                    401,
                    "invalid_client",
                    "Invalid client credentials",
                  ))
              }
            }
          }
        }
      }
    }
    types.ClientSecretBasic -> {
      // TODO: Implement Basic auth header parsing
      // For now, fall back to checking POST body
      case get_param(params, "client_secret") {
        None ->
          Error(error_response(
            401,
            "invalid_client",
            "client_secret is required (Basic auth not yet supported)",
          ))
        Some(provided_secret) -> {
          case client.client_secret {
            None ->
              Error(error_response(
                500,
                "server_error",
                "Client has no secret configured",
              ))
            Some(stored_secret) -> {
              case provided_secret == stored_secret {
                True -> Ok(Nil)
                False ->
                  Error(error_response(
                    401,
                    "invalid_client",
                    "Invalid client credentials",
                  ))
              }
            }
          }
        }
      }
    }
    types.PrivateKeyJwt -> {
      // TODO: Implement JWT client authentication
      Error(error_response(
        501,
        "unsupported_auth_method",
        "private_key_jwt authentication not yet implemented",
      ))
    }
  }
}

/// Handle POST /oauth/token
pub fn handle(req: wisp.Request, conn: sqlight.Connection) -> wisp.Response {
  // Read request body
  use body <- wisp.require_string_body(req)

  // Parse form data
  case uri.parse_query(body) {
    Error(_) ->
      error_response(400, "invalid_request", "Failed to parse form data")
    Ok(params) -> {
      // Extract grant_type
      case get_param(params, "grant_type") {
        None -> error_response(400, "invalid_request", "grant_type is required")
        Some(grant_type) -> {
          case grant_type {
            "authorization_code" -> handle_authorization_code(params, conn)
            "refresh_token" -> handle_refresh_token(params, conn)
            _ ->
              error_response(
                400,
                "unsupported_grant_type",
                "Unsupported grant_type: " <> grant_type,
              )
          }
        }
      }
    }
  }
}

/// Handle authorization_code grant
fn handle_authorization_code(
  params: List(#(String, String)),
  conn: sqlight.Connection,
) -> wisp.Response {
  // Extract required parameters
  let code_value = get_param(params, "code")
  let client_id = get_param(params, "client_id")
  let redirect_uri = get_param(params, "redirect_uri")
  let code_verifier = get_param(params, "code_verifier")

  case code_value, client_id, redirect_uri {
    None, _, _ -> error_response(400, "invalid_request", "code is required")
    _, None, _ ->
      error_response(400, "invalid_request", "client_id is required")
    _, _, None ->
      error_response(400, "invalid_request", "redirect_uri is required")
    Some(code_val), Some(cid), Some(ruri) -> {
      // Get client
      case oauth_clients.get(conn, cid) {
        Error(err) ->
          error_response(
            500,
            "server_error",
            "Database error: " <> string.inspect(err),
          )
        Ok(None) -> error_response(401, "invalid_client", "Client not found")
        Ok(Some(client)) -> {
          // Validate client authentication
          case validate_client_authentication(client, params) {
            Error(err) -> err
            Ok(_) -> {
              // Get authorization code
              case oauth_authorization_code.get(conn, code_val) {
                Error(err) ->
                  error_response(
                    500,
                    "server_error",
                    "Database error: " <> string.inspect(err),
                  )
                Ok(None) ->
                  error_response(
                    400,
                    "invalid_grant",
                    "Invalid authorization code",
                  )
                Ok(Some(code)) -> {
                  // Validate authorization code
                  case
                    validate_authorization_code(code, cid, ruri, code_verifier)
                  {
                    Error(err) -> err
                    Ok(_) -> {
                      // Mark code as used
                      case oauth_authorization_code.mark_used(conn, code_val) {
                        Error(err) ->
                          error_response(
                            500,
                            "server_error",
                            "Database error: " <> string.inspect(err),
                          )
                        Ok(_) -> {
                          // Generate tokens
                          let access_token_value =
                            token_generator.generate_access_token()
                          let refresh_token_value =
                            token_generator.generate_refresh_token()
                          let now = token_generator.current_timestamp()

                          let access_token =
                            OAuthAccessToken(
                              token: access_token_value,
                              token_type: Bearer,
                              client_id: cid,
                              user_id: Some(code.user_id),
                              session_id: code.session_id,
                              session_iteration: code.session_iteration,
                              scope: code.scope,
                              created_at: now,
                              expires_at: token_generator.expiration_timestamp(
                                client.access_token_expiration,
                              ),
                              revoked: False,
                              dpop_jkt: None,
                            )

                          let refresh_token =
                            OAuthRefreshToken(
                              token: refresh_token_value,
                              access_token: access_token_value,
                              client_id: cid,
                              user_id: code.user_id,
                              session_id: code.session_id,
                              session_iteration: code.session_iteration,
                              scope: code.scope,
                              created_at: now,
                              expires_at: case client.refresh_token_expiration {
                                0 -> None
                                exp ->
                                  Some(token_generator.expiration_timestamp(exp))
                              },
                              revoked: False,
                            )

                          // Store tokens
                          case oauth_access_tokens.insert(conn, access_token) {
                            Error(err) ->
                              error_response(
                                500,
                                "server_error",
                                "Failed to store access token: "
                                  <> string.inspect(err),
                              )
                            Ok(_) -> {
                              case
                                oauth_refresh_tokens.insert(conn, refresh_token)
                              {
                                Error(err) ->
                                  error_response(
                                    500,
                                    "server_error",
                                    "Failed to store refresh token: "
                                      <> string.inspect(err),
                                  )
                                Ok(_) -> {
                                  // Return token response
                                  token_response(
                                    access_token_value,
                                    "Bearer",
                                    client.access_token_expiration,
                                    Some(refresh_token_value),
                                    code.scope,
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

/// Handle refresh_token grant
fn handle_refresh_token(
  params: List(#(String, String)),
  conn: sqlight.Connection,
) -> wisp.Response {
  // Extract required parameters
  let refresh_token_value = get_param(params, "refresh_token")
  let client_id = get_param(params, "client_id")
  let requested_scope = get_param(params, "scope")

  // Validate scope format if provided
  case requested_scope {
    Some(scope_str) -> {
      case scope_validator.validate_scope_format(scope_str) {
        Error(e) ->
          Some(error_response(400, "invalid_scope", error.error_description(e)))
        Ok(_) -> None
      }
    }
    None -> None
  }
  |> fn(validation_error) {
    case validation_error {
      Some(response) -> response
      None -> {
        case refresh_token_value, client_id {
          None, _ ->
            error_response(400, "invalid_request", "refresh_token is required")
          _, None ->
            error_response(400, "invalid_request", "client_id is required")
          Some(rt_value), Some(cid) -> {
            // Get client
            case oauth_clients.get(conn, cid) {
              Error(err) ->
                error_response(
                  500,
                  "server_error",
                  "Database error: " <> string.inspect(err),
                )
              Ok(None) ->
                error_response(401, "invalid_client", "Client not found")
              Ok(Some(client)) -> {
                // Validate client authentication
                case validate_client_authentication(client, params) {
                  Error(err) -> err
                  Ok(_) -> {
                    // Get refresh token
                    case oauth_refresh_tokens.get(conn, rt_value) {
                      Error(err) ->
                        error_response(
                          500,
                          "server_error",
                          "Database error: " <> string.inspect(err),
                        )
                      Ok(None) ->
                        error_response(
                          400,
                          "invalid_grant",
                          "Invalid refresh token",
                        )
                      Ok(Some(old_refresh_token)) -> {
                        // Validate refresh token
                        case validate_refresh_token(old_refresh_token, cid) {
                          Error(err) -> err
                          Ok(_) -> {
                            // Revoke old refresh token
                            case oauth_refresh_tokens.revoke(conn, rt_value) {
                              Error(err) ->
                                error_response(
                                  500,
                                  "server_error",
                                  "Database error: " <> string.inspect(err),
                                )
                              Ok(_) -> {
                                // Generate new tokens
                                let new_access_token_value =
                                  token_generator.generate_access_token()
                                let new_refresh_token_value =
                                  token_generator.generate_refresh_token()
                                let now = token_generator.current_timestamp()

                                // Use requested scope or fall back to original
                                let scope = case requested_scope {
                                  Some(_) -> requested_scope
                                  None -> old_refresh_token.scope
                                }

                                let access_token =
                                  OAuthAccessToken(
                                    token: new_access_token_value,
                                    token_type: Bearer,
                                    client_id: cid,
                                    user_id: Some(old_refresh_token.user_id),
                                    session_id: old_refresh_token.session_id,
                                    session_iteration: old_refresh_token.session_iteration,
                                    scope: scope,
                                    created_at: now,
                                    expires_at: token_generator.expiration_timestamp(
                                      client.access_token_expiration,
                                    ),
                                    revoked: False,
                                    dpop_jkt: None,
                                  )

                                let refresh_token =
                                  OAuthRefreshToken(
                                    token: new_refresh_token_value,
                                    access_token: new_access_token_value,
                                    client_id: cid,
                                    user_id: old_refresh_token.user_id,
                                    session_id: old_refresh_token.session_id,
                                    session_iteration: old_refresh_token.session_iteration,
                                    scope: scope,
                                    created_at: now,
                                    expires_at: case
                                      client.refresh_token_expiration
                                    {
                                      0 -> None
                                      exp ->
                                        Some(
                                          token_generator.expiration_timestamp(
                                            exp,
                                          ),
                                        )
                                    },
                                    revoked: False,
                                  )

                                // Store new tokens
                                case
                                  oauth_access_tokens.insert(conn, access_token)
                                {
                                  Error(err) ->
                                    error_response(
                                      500,
                                      "server_error",
                                      "Failed to store access token: "
                                        <> string.inspect(err),
                                    )
                                  Ok(_) -> {
                                    case
                                      oauth_refresh_tokens.insert(
                                        conn,
                                        refresh_token,
                                      )
                                    {
                                      Error(err) ->
                                        error_response(
                                          500,
                                          "server_error",
                                          "Failed to store refresh token: "
                                            <> string.inspect(err),
                                        )
                                      Ok(_) -> {
                                        // Return token response
                                        token_response(
                                          new_access_token_value,
                                          "Bearer",
                                          client.access_token_expiration,
                                          Some(new_refresh_token_value),
                                          scope,
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
  }
}

/// Validate authorization code
fn validate_authorization_code(
  code: OAuthAuthorizationCode,
  client_id: String,
  redirect_uri: String,
  code_verifier: Option(String),
) -> Result(Nil, wisp.Response) {
  // Check if code is used
  case code.used {
    True ->
      Error(error_response(
        400,
        "invalid_grant",
        "Authorization code already used",
      ))
    False -> {
      // Check if expired
      case token_generator.is_expired(code.expires_at) {
        True ->
          Error(error_response(
            400,
            "invalid_grant",
            "Authorization code expired",
          ))
        False -> {
          // Check client_id matches
          case code.client_id == client_id {
            False ->
              Error(error_response(400, "invalid_grant", "Client ID mismatch"))
            True -> {
              // Check redirect_uri matches
              case code.redirect_uri == redirect_uri {
                False ->
                  Error(error_response(
                    400,
                    "invalid_grant",
                    "Redirect URI mismatch",
                  ))
                True -> {
                  // Verify PKCE if challenge was provided
                  case code.code_challenge, code_verifier {
                    Some(challenge), Some(verifier) -> {
                      let method = case code.code_challenge_method {
                        Some(S256) -> "S256"
                        Some(Plain) -> "plain"
                        None -> "S256"
                        // Default to S256
                      }
                      case
                        pkce.verify_code_challenge(verifier, challenge, method)
                      {
                        True -> Ok(Nil)
                        False ->
                          Error(error_response(
                            400,
                            "invalid_grant",
                            "Invalid code verifier",
                          ))
                      }
                    }
                    Some(_), None ->
                      Error(error_response(
                        400,
                        "invalid_request",
                        "code_verifier required for PKCE flow",
                      ))
                    None, Some(_) ->
                      Error(error_response(
                        400,
                        "invalid_request",
                        "code_verifier provided but no challenge in code",
                      ))
                    None, None -> Ok(Nil)
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

/// Validate refresh token
fn validate_refresh_token(
  refresh_token: OAuthRefreshToken,
  client_id: String,
) -> Result(Nil, wisp.Response) {
  // Check if revoked
  case refresh_token.revoked {
    True -> Error(error_response(400, "invalid_grant", "Refresh token revoked"))
    False -> {
      // Check if expired
      case refresh_token.expires_at {
        Some(exp) -> {
          case token_generator.is_expired(exp) {
            True ->
              Error(error_response(
                400,
                "invalid_grant",
                "Refresh token expired",
              ))
            False -> Ok(Nil)
          }
        }
        None -> Ok(Nil)
      }
      |> result.try(fn(_) {
        // Check client_id matches
        case refresh_token.client_id == client_id {
          False ->
            Error(error_response(400, "invalid_grant", "Client ID mismatch"))
          True -> Ok(Nil)
        }
      })
    }
  }
}

/// Helper to get parameter from list
fn get_param(params: List(#(String, String)), key: String) -> Option(String) {
  params
  |> list.find(fn(param) { param.0 == key })
  |> result.map(fn(param) { param.1 })
  |> option.from_result
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

fn token_response(
  access_token: String,
  token_type: String,
  expires_in: Int,
  refresh_token: Option(String),
  scope: Option(String),
) -> wisp.Response {
  let base_fields = [
    #("access_token", json.string(access_token)),
    #("token_type", json.string(token_type)),
    #("expires_in", json.int(expires_in)),
  ]

  let with_refresh = case refresh_token {
    Some(rt) -> list.append(base_fields, [#("refresh_token", json.string(rt))])
    None -> base_fields
  }

  let with_scope = case scope {
    Some(s) -> list.append(with_refresh, [#("scope", json.string(s))])
    None -> with_refresh
  }

  wisp.response(200)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_header("cache-control", "no-store")
  |> wisp.set_header("pragma", "no-cache")
  |> wisp.set_body(wisp.Text(json.to_string(json.object(with_scope))))
}
