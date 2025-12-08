/// Client metadata endpoint for ATProtocol OAuth
/// GET /oauth-client-metadata.json
import gleam/json
import gleam/option.{type Option, None, Some}
import wisp

/// Client metadata response
pub type ClientMetadataResponse {
  ClientMetadataResponse(
    client_id: String,
    client_name: String,
    redirect_uris: List(String),
    grant_types: List(String),
    response_types: List(String),
    scope: String,
    token_endpoint_auth_method: String,
    token_endpoint_auth_signing_alg: String,
    subject_type: String,
    application_type: String,
    dpop_bound_access_tokens: Bool,
    jwks: Option(json.Json),
    jwks_uri: Option(String),
  )
}

/// Generate client metadata
pub fn generate_metadata(
  client_id: String,
  client_name: String,
  redirect_uris: List(String),
  scope: String,
  jwks: Option(json.Json),
  jwks_uri: Option(String),
) -> ClientMetadataResponse {
  ClientMetadataResponse(
    client_id: client_id,
    client_name: client_name,
    redirect_uris: redirect_uris,
    grant_types: ["authorization_code", "refresh_token"],
    response_types: ["code"],
    scope: scope,
    token_endpoint_auth_method: "private_key_jwt",
    token_endpoint_auth_signing_alg: "ES256",
    subject_type: "public",
    application_type: "web",
    dpop_bound_access_tokens: True,
    jwks: jwks,
    jwks_uri: jwks_uri,
  )
}

/// Encode client metadata as JSON
pub fn encode_metadata(metadata: ClientMetadataResponse) -> json.Json {
  let base_fields = [
    #("client_id", json.string(metadata.client_id)),
    #("client_name", json.string(metadata.client_name)),
    #("redirect_uris", json.array(metadata.redirect_uris, json.string)),
    #("grant_types", json.array(metadata.grant_types, json.string)),
    #("response_types", json.array(metadata.response_types, json.string)),
    #("scope", json.string(metadata.scope)),
    #(
      "token_endpoint_auth_method",
      json.string(metadata.token_endpoint_auth_method),
    ),
    #(
      "token_endpoint_auth_signing_alg",
      json.string(metadata.token_endpoint_auth_signing_alg),
    ),
    #("subject_type", json.string(metadata.subject_type)),
    #("application_type", json.string(metadata.application_type)),
    #("dpop_bound_access_tokens", json.bool(metadata.dpop_bound_access_tokens)),
  ]

  let with_jwks = case metadata.jwks {
    Some(jwks) -> [#("jwks", jwks), ..base_fields]
    None -> base_fields
  }

  let with_jwks_uri = case metadata.jwks_uri {
    Some(uri) -> [#("jwks_uri", json.string(uri)), ..with_jwks]
    None -> with_jwks
  }

  json.object(with_jwks_uri)
}

/// Handle GET /oauth-client-metadata.json
pub fn handle(
  base_url: String,
  client_name: String,
  redirect_uris: List(String),
  scope: String,
  jwks: Option(json.Json),
  jwks_uri: Option(String),
) -> wisp.Response {
  let client_id = base_url <> "/oauth-client-metadata.json"
  let metadata =
    generate_metadata(
      client_id,
      client_name,
      redirect_uris,
      scope,
      jwks,
      jwks_uri,
    )
  let json_response = encode_metadata(metadata)

  wisp.response(200)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text(json.to_string(json_response)))
}
