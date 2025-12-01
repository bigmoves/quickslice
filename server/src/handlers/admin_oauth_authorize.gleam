/// Admin OAuth authorize handler
/// POST /admin/oauth/authorize - Initiates ATProtocol OAuth for admin login
import database/repositories/oauth_atp_requests
import database/repositories/oauth_atp_sessions
import database/types.{OAuthAtpRequest, OAuthAtpSession}
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/http
import gleam/http/request as http_request
import gleam/httpc
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import gleam/uri
import lib/oauth/atproto/did_resolver
import lib/oauth/did_cache
import lib/oauth/dpop/keygen
import lib/oauth/pkce
import lib/oauth/token_generator
import sqlight
import wisp

/// Handle POST /admin/oauth/authorize
pub fn handle(
  req: wisp.Request,
  conn: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
  redirect_uri: String,
  client_id: String,
  signing_key: Option(String),
  oauth_supported_scopes: List(String),
) -> wisp.Response {
  case req.method {
    http.Post -> {
      use formdata <- wisp.require_form(req)

      // Get login_hint from form
      let login_hint = case list.key_find(formdata.values, "login_hint") {
        Ok(hint) -> hint
        Error(_) -> ""
      }

      case login_hint == "" {
        True -> error_response(400, "login_hint is required")
        False ->
          process_authorize(
            conn,
            did_cache,
            login_hint,
            redirect_uri,
            client_id,
            signing_key,
            oauth_supported_scopes,
          )
      }
    }
    _ -> wisp.method_not_allowed([http.Post])
  }
}

fn process_authorize(
  conn: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
  login_hint: String,
  redirect_uri: String,
  client_id: String,
  signing_key: Option(String),
  oauth_supported_scopes: List(String),
) -> wisp.Response {
  // Resolve handle to DID if needed
  let did_result = case string.starts_with(login_hint, "did:") {
    True -> Ok(login_hint)
    False -> did_resolver.resolve_handle_to_did(login_hint)
  }

  case did_result {
    Error(_) -> error_response(400, "Failed to resolve handle to DID")
    Ok(did) -> {
      // Generate session_id
      let session_id = token_generator.generate_session_id()
      let now = token_generator.current_timestamp()
      let expires_at = token_generator.expiration_timestamp(600)

      // Generate ATP OAuth state
      let atp_oauth_state = token_generator.generate_state()

      // Generate DPoP key pair
      let dpop_key = keygen.generate_dpop_jwk()

      // Generate signing key JKT
      let signing_key_jkt = case signing_key {
        Some(key) -> token_generator.compute_jkt(key)
        None -> token_generator.generate_state()
      }

      // Generate PKCE for ATP OAuth
      let pkce_verifier = pkce.generate_code_verifier()
      let code_challenge = pkce.generate_code_challenge(pkce_verifier)

      // Store OAuth request for callback
      let oauth_req =
        OAuthAtpRequest(
          oauth_state: atp_oauth_state,
          authorization_server: "https://unknown-pds.example.com",
          nonce: token_generator.generate_state(),
          pkce_verifier: pkce_verifier,
          signing_public_key: signing_key_jkt,
          dpop_private_key: dpop_key,
          created_at: now,
          expires_at: expires_at,
        )

      case oauth_atp_requests.insert(conn, oauth_req) {
        Error(_) -> error_response(500, "Failed to store OAuth request")
        Ok(_) -> {
          // Create ATP session
          let atp_session =
            OAuthAtpSession(
              session_id: session_id,
              iteration: 0,
              did: Some(did),
              session_created_at: now,
              atp_oauth_state: atp_oauth_state,
              signing_key_jkt: signing_key_jkt,
              dpop_key: dpop_key,
              access_token: None,
              refresh_token: None,
              access_token_created_at: None,
              access_token_expires_at: None,
              access_token_scopes: None,
              session_exchanged_at: None,
              exchange_error: None,
            )

          case oauth_atp_sessions.insert(conn, atp_session) {
            Error(_) -> error_response(500, "Failed to store ATP session")
            Ok(_) -> {
              // Resolve DID to get PDS endpoint
              case did_resolver.resolve_did_with_cache(did_cache, did, True) {
                Error(_) -> error_response(400, "Failed to resolve DID")
                Ok(did_doc) -> {
                  case did_resolver.get_pds_endpoint(did_doc) {
                    None ->
                      error_response(400, "No PDS endpoint in DID document")
                    Some(pds_endpoint) -> {
                      // Get authorization server metadata
                      case fetch_auth_server_metadata(pds_endpoint) {
                        Error(err) ->
                          error_response(
                            500,
                            "Failed to get auth server: " <> err,
                          )
                        Ok(auth_endpoint) -> {
                          // Build authorization URL
                          let auth_url =
                            auth_endpoint
                            <> "?client_id="
                            <> uri.percent_encode(client_id)
                            <> "&redirect_uri="
                            <> uri.percent_encode(redirect_uri)
                            <> "&response_type=code"
                            <> "&code_challenge="
                            <> uri.percent_encode(code_challenge)
                            <> "&code_challenge_method=S256"
                            <> "&state="
                            <> uri.percent_encode(atp_oauth_state)
                            <> "&scope="
                            <> uri.percent_encode(string.join(
                              oauth_supported_scopes,
                              " ",
                            ))
                            <> "&login_hint="
                            <> uri.percent_encode(did)

                          wisp.redirect(auth_url)
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

/// Fetch authorization server metadata from PDS
fn fetch_auth_server_metadata(pds_endpoint: String) -> Result(String, String) {
  let pr_url = pds_endpoint <> "/.well-known/oauth-protected-resource"

  case http_request.to(pr_url) {
    Error(_) -> Error("Invalid URL")
    Ok(pr_req) -> {
      case httpc.send(pr_req) {
        Error(_) -> Error("Request failed")
        Ok(pr_resp) -> {
          case pr_resp.status {
            200 -> {
              let decoder =
                decode.at(["authorization_servers"], decode.list(decode.string))
              case json.parse(pr_resp.body, decoder) {
                Ok([first, ..]) -> {
                  // Get authorization endpoint from auth server metadata
                  let as_url =
                    first <> "/.well-known/oauth-authorization-server"
                  case http_request.to(as_url) {
                    Error(_) -> Error("Invalid auth server URL")
                    Ok(as_req) -> {
                      case httpc.send(as_req) {
                        Error(_) -> Error("Auth server request failed")
                        Ok(as_resp) -> {
                          case as_resp.status {
                            200 -> {
                              let endpoint_decoder =
                                decode.at(
                                  ["authorization_endpoint"],
                                  decode.string,
                                )
                              case json.parse(as_resp.body, endpoint_decoder) {
                                Ok(endpoint) -> Ok(endpoint)
                                Error(_) ->
                                  Error("No authorization_endpoint in metadata")
                              }
                            }
                            _ -> Error("Auth server metadata request failed")
                          }
                        }
                      }
                    }
                  }
                }
                _ -> Error("No authorization servers found")
              }
            }
            _ -> Error("Protected resource metadata request failed")
          }
        }
      }
    }
  }
}

fn error_response(status: Int, message: String) -> wisp.Response {
  wisp.log_error("Admin OAuth error: " <> message)
  wisp.response(status)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text("{\"error\": \"" <> message <> "\"}"))
}
