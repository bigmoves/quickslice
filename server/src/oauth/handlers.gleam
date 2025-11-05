import gleam/bit_array
import gleam/dynamic/decode
import gleam/http.{Get, Post}
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/uri
import oauth/pkce
import oauth/session
import sqlight
import wisp.{type Request, type Response}

pub type OAuthConfig {
  OAuthConfig(
    client_id: String,
    client_secret: String,
    redirect_uri: String,
    auth_url: String,
  )
}

type TokenResponse {
  TokenResponse(
    access_token: String,
    refresh_token: Option(String),
    expires_in: Option(Int),
  )
}

type UserInfo {
  UserInfo(sub: String, did: String, handle: Option(String))
}

/// Handle POST /oauth/authorize - Initiate OAuth flow
pub fn handle_oauth_authorize(
  req: Request,
  db: sqlight.Connection,
  config: OAuthConfig,
) -> Response {
  use formdata <- wisp.require_form(req)

  // Get login hint from form
  let login_hint = case formdata.values {
    [#("loginHint", hint), ..] -> hint
    _ -> ""
  }

  wisp.log_info("OAuth: Authorization requested for: " <> login_hint)

  // Generate PKCE parameters
  let code_verifier = pkce.generate_code_verifier()
  let code_challenge = pkce.generate_code_challenge(code_verifier)
  let state = session.generate_session_id()

  // Store PKCE state
  let oauth_state =
    session.OAuthState(
      code_verifier: code_verifier,
      code_challenge: code_challenge,
      login_hint: login_hint,
    )
  let _ = session.save_oauth_state(db, state, oauth_state)

  // Build authorization URL
  let query_params = [
    #("response_type", "code"),
    #("client_id", config.client_id),
    #("redirect_uri", config.redirect_uri),
    #("state", state),
    #("code_challenge", code_challenge),
    #("code_challenge_method", "S256"),
    #("scope", "profile openid atproto transition:generic"),
    #("login_hint", login_hint),
  ]

  let full_auth_url = config.auth_url <> "/oauth/authorize"

  let auth_uri = case uri.parse(full_auth_url) {
    Ok(base_uri) -> {
      let query_string = list_to_query_string(query_params)

      uri.Uri(..base_uri, query: Some(query_string))
      |> uri.to_string
    }
    Error(_) -> full_auth_url
  }

  wisp.log_info("OAuth: Redirecting to: " <> auth_uri)
  wisp.redirect(auth_uri)
}

/// Handle GET /oauth/callback - OAuth provider redirects here
pub fn handle_oauth_callback(
  req: Request,
  db: sqlight.Connection,
  config: OAuthConfig,
) -> Response {
  // Get code from query params
  let code = case req.query {
    Some(query_string) -> {
      case uri.parse_query(query_string) {
        Ok(params) -> list.key_find(params, "code") |> result.unwrap("missing")
        Error(_) -> "missing"
      }
    }
    None -> "missing"
  }

  // Get state from query params
  let state = case req.query {
    Some(query_string) -> {
      case uri.parse_query(query_string) {
        Ok(params) -> list.key_find(params, "state") |> result.unwrap("missing")
        Error(_) -> "missing"
      }
    }
    None -> "missing"
  }

  // Validate we have both
  case code == "missing" || state == "missing" {
    True -> {
      wisp.log_error("OAuth: Missing code or state in callback")
      wisp.redirect("/?error=Missing+parameters")
    }
    False -> {
      wisp.log_info("OAuth: Callback received with code and state")

      // Retrieve PKCE code_verifier from state
      case session.get_oauth_state(db, state) {
        Ok(oauth_state) -> {
          // Clean up the OAuth state
          let _ = session.delete_oauth_state(db, state)

          wisp.log_info("OAuth: Exchanging code for tokens")

          let token_url = config.auth_url <> "/oauth/token"

          case
            exchange_code_for_tokens(
              token_url,
              code,
              oauth_state.code_verifier,
              config.client_id,
              config.client_secret,
              config.redirect_uri,
            )
          {
            Ok(token_response) -> {
              wisp.log_info("OAuth: Successfully exchanged code for tokens")

              // Fetch user info
              let userinfo_url = config.auth_url <> "/oauth/userinfo"
              case get_user_info(userinfo_url, token_response.access_token) {
                Ok(user_info) -> {
                  wisp.log_info("OAuth: Got user info")
                  wisp.log_info("  DID: " <> user_info.did)
                  wisp.log_info(
                    "  Handle: " <> option.unwrap(user_info.handle, "(none)"),
                  )

                  case
                    session.create_session(
                      db,
                      token_response.access_token,
                      token_response.refresh_token,
                      user_info.did,
                      option.unwrap(user_info.handle, ""),
                      token_response.expires_in,
                    )
                  {
                    Ok(session_id) -> {
                      wisp.redirect("/")
                      |> session.set_session_cookie(req, session_id)
                    }
                    Error(_err) -> {
                      wisp.log_error("OAuth: Failed to create session")
                      wisp.redirect("/?error=Session+creation+failed")
                    }
                  }
                }
                Error(err) -> {
                  wisp.log_error("OAuth: Failed to get user info: " <> err)
                  wisp.redirect("/?error=Failed+to+get+user+info")
                }
              }
            }
            Error(err) -> {
              wisp.log_error("OAuth: Token exchange failed: " <> err)
              wisp.redirect("/?error=Token+exchange+failed")
            }
          }
        }
        Error(_) -> {
          wisp.log_error("OAuth: Invalid or expired state")
          wisp.redirect("/?error=Invalid+state")
        }
      }
    }
  }
}

/// Handle POST /logout - Clear session and redirect
pub fn handle_logout(req: Request, db: sqlight.Connection) -> Response {
  // Get session ID and delete session
  case session.get_session_id(req) {
    Ok(session_id) -> {
      let _ = session.delete_session(db, session_id)
      wisp.log_info("User logged out")
    }
    Error(_) -> Nil
  }

  // Clear cookie and redirect
  wisp.redirect("/")
  |> session.clear_session_cookie(req)
}

// Public Helper Functions --------------------------------------------------------

/// Refresh an access token using a refresh token
pub fn refresh_access_token(
  config: OAuthConfig,
  refresh_token: String,
) -> Result(#(String, Option(String), Option(Int)), String) {
  let token_url = config.auth_url <> "/oauth/token"

  // Build form-encoded body for refresh grant
  let body_params = [
    #("grant_type", "refresh_token"),
    #("refresh_token", refresh_token),
    #("client_id", config.client_id),
  ]

  let body = list_to_query_string(body_params)

  // Create Basic Auth header
  let credentials = config.client_id <> ":" <> config.client_secret
  let credentials_bytes = bit_array.from_string(credentials)
  let basic_auth = "Basic " <> bit_array.base64_encode(credentials_bytes, True)

  // Create HTTP request
  case request.to(token_url) {
    Ok(req) -> {
      let req =
        req
        |> request.set_method(Post)
        |> request.set_header("authorization", basic_auth)
        |> request.set_header(
          "content-type",
          "application/x-www-form-urlencoded",
        )
        |> request.set_body(body)

      // Send request
      case httpc.send(req) {
        Ok(resp) -> {
          case resp.status {
            200 -> {
              // Parse JSON response
              case json.parse(resp.body, decode.dynamic) {
                Ok(parsed) -> {
                  // Extract fields from token response
                  let access_token = case
                    decode.run(
                      parsed,
                      decode.at(["access_token"], decode.string),
                    )
                  {
                    Ok(token) -> token
                    Error(_) -> ""
                  }

                  let new_refresh_token = case
                    decode.run(
                      parsed,
                      decode.at(["refresh_token"], decode.optional(decode.string)),
                    )
                  {
                    Ok(token) -> token
                    Error(_) -> option.None
                  }

                  let expires_in = case
                    decode.run(parsed, decode.at(["expires_in"], decode.optional(decode.int)))
                  {
                    Ok(exp) -> exp
                    Error(_) -> option.None
                  }

                  case access_token == "" {
                    True -> Error("Missing access_token in refresh response")
                    False -> Ok(#(access_token, new_refresh_token, expires_in))
                  }
                }
                Error(_) -> Error("Failed to parse refresh token response JSON")
              }
            }
            _ ->
              Error(
                "Token refresh failed with status: "
                <> string.inspect(resp.status),
              )
          }
        }
        Error(_) -> Error("Failed to send token refresh request")
      }
    }
    Error(_) -> Error("Invalid token URL")
  }
}

// Private Helper Functions -------------------------------------------------------

fn list_to_query_string(params: List(#(String, String))) -> String {
  params
  |> list.map(fn(pair) {
    let #(key, value) = pair
    uri.percent_encode(key) <> "=" <> uri.percent_encode(value)
  })
  |> string.join("&")
}

fn exchange_code_for_tokens(
  token_url: String,
  code: String,
  code_verifier: String,
  client_id: String,
  client_secret: String,
  redirect_uri: String,
) -> Result(TokenResponse, String) {
  // Build form-encoded body
  let body_params = [
    #("grant_type", "authorization_code"),
    #("code", code),
    #("redirect_uri", redirect_uri),
    #("client_id", client_id),
    #("code_verifier", code_verifier),
  ]

  let body = list_to_query_string(body_params)

  // Create Basic Auth header
  let credentials = client_id <> ":" <> client_secret
  let credentials_bytes = bit_array.from_string(credentials)
  let basic_auth = "Basic " <> bit_array.base64_encode(credentials_bytes, True)

  // Create HTTP request
  case request.to(token_url) {
    Ok(req) -> {
      let req =
        req
        |> request.set_method(Post)
        |> request.set_header("authorization", basic_auth)
        |> request.set_header(
          "content-type",
          "application/x-www-form-urlencoded",
        )
        |> request.set_body(body)

      // Send request
      case httpc.send(req) {
        Ok(resp) -> {
          case resp.status {
            200 -> {
              // Parse JSON response
              case json.parse(resp.body, decode.dynamic) {
                Ok(parsed) -> {
                  // Extract fields from token response
                  let access_token = case
                    decode.run(
                      parsed,
                      decode.at(["access_token"], decode.string),
                    )
                  {
                    Ok(token) -> token
                    Error(_) -> ""
                  }

                  let refresh_token = case
                    decode.run(
                      parsed,
                      decode.at(["refresh_token"], decode.optional(decode.string)),
                    )
                  {
                    Ok(token) -> token
                    Error(_) -> None
                  }

                  let expires_in = case
                    decode.run(
                      parsed,
                      decode.at(["expires_in"], decode.optional(decode.int)),
                    )
                  {
                    Ok(exp) -> exp
                    Error(_) -> None
                  }

                  case access_token == "" {
                    True -> Error("Missing access_token in token response")
                    False ->
                      Ok(TokenResponse(
                        access_token: access_token,
                        refresh_token: refresh_token,
                        expires_in: expires_in,
                      ))
                  }
                }
                Error(_) -> Error("Failed to parse token response JSON")
              }
            }
            _ ->
              Error(
                "Token request failed with status: "
                <> string.inspect(resp.status),
              )
          }
        }
        Error(_) -> Error("Failed to send token request")
      }
    }
    Error(_) -> Error("Invalid token URL")
  }
}

fn get_user_info(
  userinfo_url: String,
  access_token: String,
) -> Result(UserInfo, String) {
  case request.to(userinfo_url) {
    Ok(req) -> {
      let req =
        req
        |> request.set_method(Get)
        |> request.set_header("authorization", "Bearer " <> access_token)

      case httpc.send(req) {
        Ok(resp) -> {
          case resp.status {
            200 -> {
              case json.parse(resp.body, decode.dynamic) {
                Ok(parsed) -> {
                  let sub = case
                    decode.run(parsed, decode.at(["sub"], decode.string))
                  {
                    Ok(s) -> s
                    Error(_) -> ""
                  }

                  let did = case
                    decode.run(parsed, decode.at(["did"], decode.string))
                  {
                    Ok(d) -> d
                    Error(_) -> sub
                  }

                  let handle = case
                    decode.run(
                      parsed,
                      decode.at(["name"], decode.optional(decode.string)),
                    )
                  {
                    Ok(h) -> h
                    Error(_) -> None
                  }

                  case sub == "" {
                    True -> Error("Missing sub in userinfo response")
                    False -> Ok(UserInfo(sub: sub, did: did, handle: handle))
                  }
                }
                Error(_) -> Error("Failed to parse userinfo response JSON")
              }
            }
            _ ->
              Error(
                "Userinfo request failed with status: "
                <> string.inspect(resp.status),
              )
          }
        }
        Error(_) -> Error("Failed to send userinfo request")
      }
    }
    Error(_) -> Error("Invalid userinfo URL")
  }
}
