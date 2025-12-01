/// OAuth server metadata endpoint handler
/// GET /.well-known/oauth-authorization-server
import gleam/json
import wisp

pub type ServerMetadata {
  ServerMetadata(
    issuer: String,
    authorization_endpoint: String,
    token_endpoint: String,
    jwks_uri: String,
    registration_endpoint: String,
    scopes_supported: List(String),
    response_types_supported: List(String),
    grant_types_supported: List(String),
    token_endpoint_auth_methods_supported: List(String),
    code_challenge_methods_supported: List(String),
    pushed_authorization_request_endpoint: String,
    dpop_signing_alg_values_supported: List(String),
  )
}

/// Generate server metadata for the given base URL
pub fn generate_metadata(
  base_url: String,
  scopes_supported: List(String),
) -> ServerMetadata {
  ServerMetadata(
    issuer: base_url,
    authorization_endpoint: base_url <> "/oauth/authorize",
    token_endpoint: base_url <> "/oauth/token",
    jwks_uri: base_url <> "/.well-known/jwks.json",
    registration_endpoint: base_url <> "/oauth/register",
    scopes_supported: scopes_supported,
    response_types_supported: ["code"],
    grant_types_supported: ["authorization_code", "refresh_token"],
    token_endpoint_auth_methods_supported: [
      "client_secret_basic",
      "client_secret_post",
      "none",
    ],
    code_challenge_methods_supported: ["S256"],
    pushed_authorization_request_endpoint: base_url <> "/oauth/par",
    dpop_signing_alg_values_supported: ["ES256"],
  )
}

/// Encode metadata as JSON
pub fn encode_metadata(meta: ServerMetadata) -> json.Json {
  json.object([
    #("issuer", json.string(meta.issuer)),
    #("authorization_endpoint", json.string(meta.authorization_endpoint)),
    #("token_endpoint", json.string(meta.token_endpoint)),
    #("jwks_uri", json.string(meta.jwks_uri)),
    #("registration_endpoint", json.string(meta.registration_endpoint)),
    #("scopes_supported", json.array(meta.scopes_supported, json.string)),
    #(
      "response_types_supported",
      json.array(meta.response_types_supported, json.string),
    ),
    #(
      "grant_types_supported",
      json.array(meta.grant_types_supported, json.string),
    ),
    #(
      "token_endpoint_auth_methods_supported",
      json.array(meta.token_endpoint_auth_methods_supported, json.string),
    ),
    #(
      "code_challenge_methods_supported",
      json.array(meta.code_challenge_methods_supported, json.string),
    ),
    #(
      "pushed_authorization_request_endpoint",
      json.string(meta.pushed_authorization_request_endpoint),
    ),
    #(
      "dpop_signing_alg_values_supported",
      json.array(meta.dpop_signing_alg_values_supported, json.string),
    ),
  ])
}

/// Handle GET /.well-known/oauth-authorization-server
pub fn handle(base_url: String, scopes_supported: List(String)) -> wisp.Response {
  let meta = generate_metadata(base_url, scopes_supported)
  let json_response = encode_metadata(meta)

  wisp.response(200)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text(json.to_string(json_response)))
}
