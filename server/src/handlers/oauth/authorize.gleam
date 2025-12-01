/// OAuth authorization endpoint handler
/// GET /oauth/authorize
import database/repositories/oauth_atp_requests
import database/repositories/oauth_atp_sessions
import database/repositories/oauth_auth_requests
import database/repositories/oauth_clients
import database/types.{
  type OAuthClient, OAuthAtpRequest, OAuthAtpSession, OAuthAuthRequest,
}
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/http
import gleam/http/request as http_request
import gleam/httpc
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/uri
import lib/oauth/atproto/did_resolver
import lib/oauth/did_cache
import lib/oauth/dpop/keygen
import lib/oauth/pkce
import lib/oauth/scopes/validator as scope_validator
import lib/oauth/token_generator
import lib/oauth/types/error
import lib/oauth/types/request.{type AuthorizationRequest, AuthorizationRequest}
import lib/oauth/validator
import sqlight
import wisp

/// Authorization response type
pub type AuthorizeResponse {
  RedirectToATProtocol(authorization_url: String)
  RedirectWithError(
    redirect_uri: String,
    error: String,
    error_description: String,
    state: Option(String),
  )
}

/// Handle GET /oauth/authorize
pub fn handle(
  req: wisp.Request,
  conn: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
  redirect_uri: String,
  client_id: String,
  signing_key: Option(String),
) -> wisp.Response {
  case req.method {
    http.Get | http.Post -> {
      case req.query {
        Some(query) -> {
          case
            handle_authorize(
              query,
              conn,
              did_cache,
              redirect_uri,
              client_id,
              signing_key,
            )
          {
            Ok(response) -> build_redirect_response(response)
            Error(err) -> {
              wisp.log_error("Authorization error: " <> err)
              wisp.response(400)
              |> wisp.set_header("content-type", "application/json")
              |> wisp.set_body(wisp.Text("{\"error\": \"" <> err <> "\"}"))
            }
          }
        }
        None -> {
          wisp.response(400)
          |> wisp.set_header("content-type", "application/json")
          |> wisp.set_body(wisp.Text(
            "{\"error\": \"Missing query parameters\"}",
          ))
        }
      }
    }
    _ -> wisp.method_not_allowed([http.Get, http.Post])
  }
}

/// Main authorization handler
fn handle_authorize(
  query: String,
  conn: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
  server_redirect_uri: String,
  server_client_id: String,
  signing_key: Option(String),
) -> Result(AuthorizeResponse, String) {
  // Parse query parameters
  use params <- result.try(
    uri.parse_query(query)
    |> result.map_error(fn(_) { "Failed to parse query string" }),
  )

  // Check for PAR request_uri
  case get_param(params, "request_uri") {
    Some(_request_uri) -> {
      // PAR flow - not implemented yet
      Error("PAR flow not yet implemented")
    }
    None -> {
      // Standard flow
      handle_standard_flow(
        params,
        conn,
        did_cache,
        server_redirect_uri,
        server_client_id,
        signing_key,
      )
    }
  }
}

/// Handle standard authorization flow
fn handle_standard_flow(
  params: List(#(String, String)),
  conn: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
  server_redirect_uri: String,
  server_client_id: String,
  signing_key: Option(String),
) -> Result(AuthorizeResponse, String) {
  // Parse authorization request
  use auth_request <- result.try(parse_authorization_request(params))

  // Get client
  use client_opt <- result.try(
    oauth_clients.get(conn, auth_request.client_id)
    |> result.map_error(fn(_) { "Failed to retrieve client" }),
  )

  use client <- result.try(case client_opt {
    Some(c) -> Ok(c)
    None -> Error("Client not found")
  })

  // Validate request
  use _ <- result.try(validate_authorization_request(auth_request, client))

  // Process authorization
  process_authorization(
    auth_request,
    client,
    conn,
    did_cache,
    server_redirect_uri,
    server_client_id,
    signing_key,
  )
}

/// Parse authorization request from query parameters
fn parse_authorization_request(
  params: List(#(String, String)),
) -> Result(AuthorizationRequest, String) {
  use response_type <- result.try(case get_param(params, "response_type") {
    Some(rt) -> Ok(rt)
    None -> Error("response_type is required")
  })

  use client_id <- result.try(case get_param(params, "client_id") {
    Some(id) -> Ok(id)
    None -> Error("client_id is required")
  })

  use redirect_uri <- result.try(case get_param(params, "redirect_uri") {
    Some(uri) -> Ok(uri)
    None -> Error("redirect_uri is required")
  })

  Ok(AuthorizationRequest(
    response_type: response_type,
    client_id: client_id,
    redirect_uri: redirect_uri,
    scope: get_param(params, "scope"),
    state: get_param(params, "state"),
    code_challenge: get_param(params, "code_challenge"),
    code_challenge_method: get_param(params, "code_challenge_method"),
    nonce: get_param(params, "nonce"),
    login_hint: get_param(params, "login_hint"),
    request_uri: None,
  ))
}

/// Validate authorization request against client
fn validate_authorization_request(
  req: AuthorizationRequest,
  client: OAuthClient,
) -> Result(Nil, String) {
  // Validate response_type
  use _ <- result.try(case req.response_type {
    "code" -> Ok(Nil)
    _ -> Error("Unsupported response_type")
  })

  // Validate redirect_uri
  use _ <- result.try(
    validator.validate_redirect_uri(req.redirect_uri)
    |> result.map_error(fn(e) { error.error_description(e) }),
  )

  use _ <- result.try(
    validator.validate_redirect_uri_match(
      req.redirect_uri,
      client.redirect_uris,
      client.require_redirect_exact,
    )
    |> result.map_error(fn(e) { error.error_description(e) }),
  )

  // Validate PKCE if provided
  use _ <- result.try(case req.code_challenge, req.code_challenge_method {
    Some(_), Some(method) ->
      validator.validate_code_challenge_method(method)
      |> result.map_error(fn(e) { error.error_description(e) })
    Some(_), None -> Error("code_challenge_method required")
    None, Some(_) -> Error("code_challenge required")
    None, None -> Ok(Nil)
  })

  // Validate scope format if provided
  case req.scope {
    Some(scope_str) ->
      scope_validator.validate_scope_format(scope_str)
      |> result.map(fn(_) { Nil })
      |> result.map_error(fn(e) { error.error_description(e) })
    None -> Ok(Nil)
  }
}

/// Process the authorization - store request and redirect to ATP
fn process_authorization(
  req: AuthorizationRequest,
  client: OAuthClient,
  conn: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
  server_redirect_uri: String,
  server_client_id: String,
  signing_key: Option(String),
) -> Result(AuthorizeResponse, String) {
  // Extract DID from login_hint
  use did <- result.try(case req.login_hint {
    Some(hint) -> {
      case string.starts_with(hint, "did:") {
        True -> Ok(hint)
        False -> {
          // Resolve handle to DID
          did_resolver.resolve_handle_to_did(hint)
          |> result.map_error(fn(_) { "Failed to resolve handle" })
        }
      }
    }
    None -> Error("login_hint (DID or handle) is required")
  })

  // Generate session_id
  let session_id = token_generator.generate_session_id()
  let now = token_generator.current_timestamp()
  let expires_at = token_generator.expiration_timestamp(600)

  // Store client authorization request
  let auth_req =
    OAuthAuthRequest(
      session_id: session_id,
      client_id: req.client_id,
      redirect_uri: req.redirect_uri,
      scope: req.scope,
      state: req.state,
      code_challenge: req.code_challenge,
      code_challenge_method: req.code_challenge_method,
      response_type: req.response_type,
      nonce: req.nonce,
      login_hint: req.login_hint,
      created_at: now,
      expires_at: expires_at,
    )

  use _ <- result.try(
    oauth_auth_requests.insert(conn, auth_req)
    |> result.map_error(fn(_) { "Failed to store authorization request" }),
  )

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

  use _ <- result.try(
    oauth_atp_requests.insert(conn, oauth_req)
    |> result.map_error(fn(_) { "Failed to store OAuth request" }),
  )

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

  use _ <- result.try(
    oauth_atp_sessions.insert(conn, atp_session)
    |> result.map_error(fn(_) { "Failed to store ATP session" }),
  )

  // Resolve DID to get PDS endpoint
  use did_doc <- result.try(
    did_resolver.resolve_did_with_cache(did_cache, did, True)
    |> result.map_error(fn(_) { "Failed to resolve DID" }),
  )

  use pds_endpoint <- result.try(case did_resolver.get_pds_endpoint(did_doc) {
    Some(endpoint) -> Ok(endpoint)
    None -> Error("No PDS endpoint in DID document")
  })

  // Get authorization server metadata
  use auth_server <- result.try(
    fetch_authorization_server_metadata(pds_endpoint)
    |> result.map_error(fn(_) { "Failed to get authorization server metadata" }),
  )

  // Build authorization URL - use request scope, or fall back to client's configured scope
  let scope = case req.scope {
    Some(s) -> s
    None ->
      case client.scope {
        Some(s) -> s
        None -> "atproto"
      }
  }

  let auth_url =
    auth_server.authorization_endpoint
    <> "?client_id="
    <> uri.percent_encode(server_client_id)
    <> "&redirect_uri="
    <> uri.percent_encode(server_redirect_uri)
    <> "&response_type=code"
    <> "&code_challenge="
    <> uri.percent_encode(code_challenge)
    <> "&code_challenge_method=S256"
    <> "&state="
    <> uri.percent_encode(atp_oauth_state)
    <> "&scope="
    <> uri.percent_encode(scope)
    <> "&login_hint="
    <> uri.percent_encode(did)

  Ok(RedirectToATProtocol(authorization_url: auth_url))
}

/// Authorization server metadata
pub type AuthServerMetadata {
  AuthServerMetadata(
    issuer: String,
    authorization_endpoint: String,
    token_endpoint: String,
  )
}

/// Fetch authorization server metadata from PDS
fn fetch_authorization_server_metadata(
  pds_endpoint: String,
) -> Result(AuthServerMetadata, String) {
  // First get protected resource metadata
  let pr_url = pds_endpoint <> "/.well-known/oauth-protected-resource"

  use pr_req <- result.try(
    http_request.to(pr_url)
    |> result.map_error(fn(_) { "Invalid URL" }),
  )

  use pr_resp <- result.try(
    httpc.send(pr_req)
    |> result.map_error(fn(_) { "Request failed" }),
  )

  use auth_server_url <- result.try(case pr_resp.status {
    200 -> {
      let decoder =
        decode.at(["authorization_servers"], decode.list(decode.string))
      case json.parse(pr_resp.body, decoder) {
        Ok([first, ..]) -> Ok(first)
        _ -> Error("No authorization servers found")
      }
    }
    _ -> Error("Failed to get protected resource metadata")
  })

  // Now get authorization server metadata
  let as_url = auth_server_url <> "/.well-known/oauth-authorization-server"

  use as_req <- result.try(
    http_request.to(as_url)
    |> result.map_error(fn(_) { "Invalid URL" }),
  )

  use as_resp <- result.try(
    httpc.send(as_req)
    |> result.map_error(fn(_) { "Request failed" }),
  )

  case as_resp.status {
    200 -> {
      let decoder = {
        use issuer <- decode.field("issuer", decode.string)
        use auth_ep <- decode.field("authorization_endpoint", decode.string)
        use token_ep <- decode.field("token_endpoint", decode.string)
        decode.success(AuthServerMetadata(
          issuer: issuer,
          authorization_endpoint: auth_ep,
          token_endpoint: token_ep,
        ))
      }
      json.parse(as_resp.body, decoder)
      |> result.map_error(fn(_) { "Invalid metadata response" })
    }
    _ -> Error("Failed to get authorization server metadata")
  }
}

/// Build redirect response
fn build_redirect_response(response: AuthorizeResponse) -> wisp.Response {
  case response {
    RedirectToATProtocol(url) -> wisp.redirect(url)
    RedirectWithError(redirect_uri, err, description, state) -> {
      let query =
        "error="
        <> uri.percent_encode(err)
        <> "&error_description="
        <> uri.percent_encode(description)
        <> case state {
          Some(s) -> "&state=" <> uri.percent_encode(s)
          None -> ""
        }
      wisp.redirect(redirect_uri <> "?" <> query)
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
