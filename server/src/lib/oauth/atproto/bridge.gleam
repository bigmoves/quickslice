/// ATP OAuth Bridge
/// Bridges OAuth server with ATProtocol PDS via redirect flow
import database/repositories/oauth_atp_sessions
import database/types.{type OAuthAtpSession, OAuthAtpSession}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/uri
import lib/oauth/atproto/did_resolver
import lib/oauth/atproto/types as atp_types
import lib/oauth/crypto/jwt
import lib/oauth/did_cache
import lib/oauth/dpop/generator as dpop_generator
import lib/oauth/token_generator
import sqlight

/// Authorization server metadata from PDS
pub type AuthorizationServerMetadata {
  AuthorizationServerMetadata(
    issuer: String,
    authorization_endpoint: String,
    token_endpoint: String,
    pushed_authorization_request_endpoint: Option(String),
    dpop_signing_alg_values_supported: Option(List(String)),
  )
}

/// Protected resource metadata from PDS
pub type ProtectedResourceMetadata {
  ProtectedResourceMetadata(
    resource: String,
    authorization_servers: List(String),
  )
}

/// PAR (Pushed Authorization Request) response
pub type PARResponse {
  PARResponse(request_uri: String, expires_in: Int)
}

/// Token response from PDS
pub type TokenResponse {
  TokenResponse(
    access_token: String,
    token_type: String,
    expires_in: Int,
    refresh_token: String,
    scope: Option(String),
    sub: String,
  )
}

/// ATP bridge error
pub type BridgeError {
  DIDResolutionError(atp_types.ATProtoError)
  PDSNotFound(String)
  TokenExchangeError(String)
  HTTPError(String)
  InvalidResponse(String)
  StorageError(String)
  MetadataFetchError(String)
  PARError(String)
}

/// Handle ATP OAuth callback - exchange code for tokens
pub fn handle_callback(
  conn: sqlight.Connection,
  did_cache: process.Subject(did_cache.Message),
  session: OAuthAtpSession,
  authorization_code: String,
  code_verifier: String,
  redirect_uri: String,
  client_id: String,
  atp_oauth_state: String,
  signing_key: Option(String),
) -> Result(OAuthAtpSession, BridgeError) {
  // Verify state matches
  case session.atp_oauth_state == atp_oauth_state {
    False -> Error(TokenExchangeError("State mismatch"))
    True -> {
      // Get DID from session - must be set during initiation
      use did <- result.try(case session.did {
        Some(d) -> Ok(d)
        None -> Error(TokenExchangeError("Session has no DID"))
      })

      // Resolve DID to get PDS endpoint (use cache, no invalidation)
      use did_doc <- result.try(
        did_resolver.resolve_did_with_cache(did_cache, did, False)
        |> result.map_error(DIDResolutionError),
      )

      use pds_endpoint <- result.try(
        case did_resolver.get_pds_endpoint(did_doc) {
          Some(endpoint) -> Ok(endpoint)
          None -> Error(PDSNotFound("No PDS endpoint in DID document"))
        },
      )

      // Get authorization server metadata
      use protected_resource <- result.try(fetch_protected_resource_metadata(
        pds_endpoint,
      ))

      use auth_server_endpoint <- result.try(
        case protected_resource.authorization_servers {
          [first, ..] -> Ok(first)
          [] ->
            Error(MetadataFetchError(
              "No authorization servers in protected resource",
            ))
        },
      )

      use auth_server <- result.try(fetch_authorization_server_metadata(
        auth_server_endpoint,
      ))

      // Exchange code for tokens
      use token_response <- result.try(exchange_code_for_tokens(
        auth_server.token_endpoint,
        auth_server.issuer,
        authorization_code,
        code_verifier,
        redirect_uri,
        client_id,
        session.dpop_key,
        signing_key,
      ))

      // Update session with tokens
      update_session_with_tokens(
        conn,
        session,
        token_response.sub,
        token_response.access_token,
        token_response.refresh_token,
        token_response.expires_in,
        token_response.scope,
      )
    }
  }
}

/// Refresh ATP tokens (with session iteration)
pub fn refresh_tokens(
  conn: sqlight.Connection,
  did_cache: process.Subject(did_cache.Message),
  session: OAuthAtpSession,
  client_id: String,
  signing_key: Option(String),
) -> Result(OAuthAtpSession, BridgeError) {
  // Get refresh token from session
  use refresh_token <- result.try(case session.refresh_token {
    Some(rt) -> Ok(rt)
    None -> Error(TokenExchangeError("Session has no refresh token"))
  })

  // Get DID from session
  use did <- result.try(case session.did {
    Some(d) -> Ok(d)
    None -> Error(TokenExchangeError("Session has no DID"))
  })

  // Resolve DID to get PDS endpoint (use cache, no invalidation)
  use did_doc <- result.try(
    did_resolver.resolve_did_with_cache(did_cache, did, False)
    |> result.map_error(DIDResolutionError),
  )

  use pds_endpoint <- result.try(case did_resolver.get_pds_endpoint(did_doc) {
    Some(endpoint) -> Ok(endpoint)
    None -> Error(PDSNotFound("No PDS endpoint in DID document"))
  })

  // Get authorization server metadata
  use protected_resource <- result.try(fetch_protected_resource_metadata(
    pds_endpoint,
  ))

  use auth_server_endpoint <- result.try(
    case protected_resource.authorization_servers {
      [first, ..] -> Ok(first)
      [] ->
        Error(MetadataFetchError(
          "No authorization servers in protected resource",
        ))
    },
  )

  use auth_server <- result.try(fetch_authorization_server_metadata(
    auth_server_endpoint,
  ))

  // Build refresh request
  let body =
    "grant_type=refresh_token"
    <> "&refresh_token="
    <> uri.percent_encode(refresh_token)
    <> "&client_id="
    <> uri.percent_encode(client_id)

  // Make token request
  use token_response <- result.try(fetch_tokens(
    auth_server.token_endpoint,
    body,
    session.dpop_key,
    client_id,
    signing_key,
    auth_server.issuer,
  ))

  // Increment session iteration with new tokens
  increment_iteration(
    conn,
    session,
    token_response.access_token,
    token_response.refresh_token,
    token_response.expires_in,
  )
}

// ===== Helper Functions =====

/// Fetch protected resource metadata from PDS
pub fn fetch_protected_resource_metadata(
  pds_endpoint: String,
) -> Result(ProtectedResourceMetadata, BridgeError) {
  let url = pds_endpoint <> "/.well-known/oauth-protected-resource"

  use req <- result.try(
    request.to(url)
    |> result.map_error(fn(_) { HTTPError("Invalid metadata URL") }),
  )

  use resp <- result.try(
    httpc.send(req)
    |> result.map_error(fn(_) {
      HTTPError("Failed to fetch protected resource metadata")
    }),
  )

  case resp.status {
    200 -> parse_protected_resource_metadata(resp.body)
    status ->
      Error(MetadataFetchError(
        "Metadata request failed with status " <> int.to_string(status),
      ))
  }
}

/// Parse protected resource metadata
fn parse_protected_resource_metadata(
  json_str: String,
) -> Result(ProtectedResourceMetadata, BridgeError) {
  let decoder = {
    use resource <- decode.field("resource", decode.string)
    use authorization_servers <- decode.field(
      "authorization_servers",
      decode.list(decode.string),
    )

    decode.success(ProtectedResourceMetadata(
      resource: resource,
      authorization_servers: authorization_servers,
    ))
  }

  json.parse(json_str, decoder)
  |> result.map_error(fn(_) {
    InvalidResponse("Failed to parse protected resource metadata")
  })
}

/// Fetch authorization server metadata
pub fn fetch_authorization_server_metadata(
  auth_server_endpoint: String,
) -> Result(AuthorizationServerMetadata, BridgeError) {
  let url = auth_server_endpoint <> "/.well-known/oauth-authorization-server"

  use req <- result.try(
    request.to(url)
    |> result.map_error(fn(_) { HTTPError("Invalid metadata URL") }),
  )

  use resp <- result.try(
    httpc.send(req)
    |> result.map_error(fn(_) {
      HTTPError("Failed to fetch authorization server metadata")
    }),
  )

  case resp.status {
    200 -> parse_authorization_server_metadata(resp.body)
    status ->
      Error(MetadataFetchError(
        "Metadata request failed with status " <> int.to_string(status),
      ))
  }
}

/// Parse authorization server metadata
fn parse_authorization_server_metadata(
  json_str: String,
) -> Result(AuthorizationServerMetadata, BridgeError) {
  let decoder = {
    use issuer <- decode.field("issuer", decode.string)
    use authorization_endpoint <- decode.field(
      "authorization_endpoint",
      decode.string,
    )
    use token_endpoint <- decode.field("token_endpoint", decode.string)
    use par_endpoint <- decode.field(
      "pushed_authorization_request_endpoint",
      decode.optional(decode.string),
    )
    use dpop_algs <- decode.field(
      "dpop_signing_alg_values_supported",
      decode.optional(decode.list(decode.string)),
    )

    decode.success(AuthorizationServerMetadata(
      issuer: issuer,
      authorization_endpoint: authorization_endpoint,
      token_endpoint: token_endpoint,
      pushed_authorization_request_endpoint: par_endpoint,
      dpop_signing_alg_values_supported: dpop_algs,
    ))
  }

  json.parse(json_str, decoder)
  |> result.map_error(fn(_) {
    InvalidResponse("Failed to parse authorization server metadata")
  })
}

/// Exchange authorization code for tokens
fn exchange_code_for_tokens(
  token_endpoint: String,
  issuer: String,
  code: String,
  code_verifier: String,
  redirect_uri: String,
  client_id: String,
  dpop_key: String,
  signing_key: Option(String),
) -> Result(TokenResponse, BridgeError) {
  let body =
    "grant_type=authorization_code"
    <> "&code="
    <> uri.percent_encode(code)
    <> "&redirect_uri="
    <> uri.percent_encode(redirect_uri)
    <> "&client_id="
    <> uri.percent_encode(client_id)
    <> "&code_verifier="
    <> uri.percent_encode(code_verifier)

  fetch_tokens(token_endpoint, body, dpop_key, client_id, signing_key, issuer)
}

/// Fetch tokens from PDS
fn fetch_tokens(
  token_url: String,
  body: String,
  dpop_key: String,
  client_id: String,
  signing_key: Option(String),
  issuer: String,
) -> Result(TokenResponse, BridgeError) {
  // Generate client_assertion if signing key is provided
  let body_with_assertion = case signing_key {
    Some(key) -> {
      case jwt.create_client_assertion(client_id, issuer, key) {
        Ok(assertion) -> {
          body
          <> "&client_assertion="
          <> uri.percent_encode(assertion)
          <> "&client_assertion_type="
          <> uri.percent_encode(
            "urn:ietf:params:oauth:client-assertion-type:jwt-bearer",
          )
        }
        Error(_) -> body
      }
    }
    None -> body
  }

  use req <- result.try(
    request.to(token_url)
    |> result.map_error(fn(_) { HTTPError("Invalid token URL") }),
  )

  // Generate DPoP proof for the token request
  use dpop_proof <- result.try(
    dpop_generator.generate_dpop_proof_with_nonce(
      "POST",
      token_url,
      "",
      dpop_key,
      None,
    )
    |> result.map_error(fn(err) {
      TokenExchangeError("Failed to generate DPoP proof: " <> err)
    }),
  )

  let req =
    req
    |> request.set_method(http.Post)
    |> request.set_header("content-type", "application/x-www-form-urlencoded")
    |> request.set_header("dpop", dpop_proof)
    |> request.set_body(body_with_assertion)

  use resp <- result.try(
    httpc.send(req)
    |> result.map_error(fn(_) { HTTPError("Failed to send token request") }),
  )

  case resp.status {
    200 -> parse_token_response(resp.body)
    400 -> {
      // Check if server requires DPoP nonce
      case string.contains(resp.body, "use_dpop_nonce") {
        True -> {
          // Extract DPoP-Nonce header and retry
          case get_dpop_nonce_header(resp.headers) {
            Some(nonce) -> {
              fetch_tokens_with_nonce(
                token_url,
                body_with_assertion,
                dpop_key,
                nonce,
              )
            }
            None -> {
              Error(TokenExchangeError(
                "Server requires DPoP nonce but didn't provide one",
              ))
            }
          }
        }
        False -> {
          Error(TokenExchangeError("Token request failed: " <> resp.body))
        }
      }
    }
    status -> {
      Error(TokenExchangeError(
        "Token request failed with status "
        <> int.to_string(status)
        <> ": "
        <> resp.body,
      ))
    }
  }
}

/// Fetch tokens with a DPoP nonce (retry after initial request)
fn fetch_tokens_with_nonce(
  token_url: String,
  body: String,
  dpop_key: String,
  nonce: String,
) -> Result(TokenResponse, BridgeError) {
  use req <- result.try(
    request.to(token_url)
    |> result.map_error(fn(_) { HTTPError("Invalid token URL") }),
  )

  // Generate DPoP proof with nonce
  use dpop_proof <- result.try(
    dpop_generator.generate_dpop_proof_with_nonce(
      "POST",
      token_url,
      "",
      dpop_key,
      Some(nonce),
    )
    |> result.map_error(fn(err) {
      TokenExchangeError("Failed to generate DPoP proof with nonce: " <> err)
    }),
  )

  let req =
    req
    |> request.set_method(http.Post)
    |> request.set_header("content-type", "application/x-www-form-urlencoded")
    |> request.set_header("dpop", dpop_proof)
    |> request.set_body(body)

  use resp <- result.try(
    httpc.send(req)
    |> result.map_error(fn(_) { HTTPError("Failed to send token request") }),
  )

  case resp.status {
    200 -> parse_token_response(resp.body)
    status -> {
      Error(TokenExchangeError(
        "Token request with nonce failed with status "
        <> int.to_string(status)
        <> ": "
        <> resp.body,
      ))
    }
  }
}

/// Parse token response JSON
fn parse_token_response(json_str: String) -> Result(TokenResponse, BridgeError) {
  let decoder = {
    use access_token <- decode.field("access_token", decode.string)
    use token_type <- decode.field("token_type", decode.string)
    use expires_in <- decode.field("expires_in", decode.int)
    use refresh_token <- decode.field("refresh_token", decode.string)
    use scope <- decode.field("scope", decode.optional(decode.string))
    use sub <- decode.field("sub", decode.string)

    decode.success(TokenResponse(
      access_token: access_token,
      token_type: token_type,
      expires_in: expires_in,
      refresh_token: refresh_token,
      scope: scope,
      sub: sub,
    ))
  }

  json.parse(json_str, decoder)
  |> result.map_error(fn(_) {
    InvalidResponse("Failed to parse token response")
  })
}

/// Extract DPoP-Nonce header from response headers
fn get_dpop_nonce_header(headers: List(#(String, String))) -> Option(String) {
  case
    list.find(headers, fn(header: #(String, String)) {
      string.lowercase(header.0) == "dpop-nonce"
    })
  {
    Ok(#(_, nonce)) -> Some(nonce)
    Error(_) -> None
  }
}

/// Update ATP session with tokens after successful exchange
fn update_session_with_tokens(
  conn: sqlight.Connection,
  session: OAuthAtpSession,
  sub: String,
  access_token: String,
  refresh_token: String,
  expires_in: Int,
  scope: Option(String),
) -> Result(OAuthAtpSession, BridgeError) {
  let now = token_generator.current_timestamp()
  let expires_at = now + expires_in

  let updated_session =
    OAuthAtpSession(
      ..session,
      did: Some(sub),
      access_token: Some(access_token),
      refresh_token: Some(refresh_token),
      access_token_created_at: Some(now),
      access_token_expires_at: Some(expires_at),
      access_token_scopes: scope,
      session_exchanged_at: Some(now),
    )

  case oauth_atp_sessions.insert(conn, updated_session) {
    Ok(_) -> Ok(updated_session)
    Error(err) ->
      Error(StorageError("Failed to update session: " <> string.inspect(err)))
  }
}

/// Increment session iteration with new tokens (for refresh)
fn increment_iteration(
  conn: sqlight.Connection,
  session: OAuthAtpSession,
  access_token: String,
  refresh_token: String,
  expires_in: Int,
) -> Result(OAuthAtpSession, BridgeError) {
  let now = token_generator.current_timestamp()
  let expires_at = now + expires_in

  let new_session =
    OAuthAtpSession(
      ..session,
      iteration: session.iteration + 1,
      access_token: Some(access_token),
      refresh_token: Some(refresh_token),
      access_token_created_at: Some(now),
      access_token_expires_at: Some(expires_at),
    )

  case oauth_atp_sessions.insert(conn, new_session) {
    Ok(_) -> Ok(new_session)
    Error(err) ->
      Error(StorageError("Failed to increment session: " <> string.inspect(err)))
  }
}
