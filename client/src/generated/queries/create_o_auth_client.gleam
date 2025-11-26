import gleam/dynamic/decode
import gleam/http/request.{type Request}
import gleam/json
import gleam/option.{type Option}
import squall

pub type OAuthClient {
  OAuthClient(
    client_id: String,
    client_secret: Option(String),
    client_name: String,
    client_type: String,
    redirect_uris: List(String),
    scope: Option(String),
    created_at: Int,
  )
}

pub fn o_auth_client_decoder() -> decode.Decoder(OAuthClient) {
  use client_id <- decode.field("clientId", decode.string)
  use client_secret <- decode.field(
    "clientSecret",
    decode.optional(decode.string),
  )
  use client_name <- decode.field("clientName", decode.string)
  use client_type <- decode.field("clientType", decode.string)
  use redirect_uris <- decode.field("redirectUris", decode.list(decode.string))
  use scope <- decode.field("scope", decode.optional(decode.string))
  use created_at <- decode.field("createdAt", decode.int)
  decode.success(OAuthClient(
    client_id: client_id,
    client_secret: client_secret,
    client_name: client_name,
    client_type: client_type,
    redirect_uris: redirect_uris,
    scope: scope,
    created_at: created_at,
  ))
}

pub fn o_auth_client_to_json(input: OAuthClient) -> json.Json {
  json.object([
    #("clientId", json.string(input.client_id)),
    #("clientSecret", json.nullable(input.client_secret, json.string)),
    #("clientName", json.string(input.client_name)),
    #("clientType", json.string(input.client_type)),
    #("redirectUris", json.array(from: input.redirect_uris, of: json.string)),
    #("scope", json.nullable(input.scope, json.string)),
    #("createdAt", json.int(input.created_at)),
  ])
}

pub type CreateOAuthClientResponse {
  CreateOAuthClientResponse(create_o_auth_client: OAuthClient)
}

pub fn create_o_auth_client_response_decoder() -> decode.Decoder(
  CreateOAuthClientResponse,
) {
  use create_o_auth_client <- decode.field(
    "createOAuthClient",
    o_auth_client_decoder(),
  )
  decode.success(CreateOAuthClientResponse(
    create_o_auth_client: create_o_auth_client,
  ))
}

pub fn create_o_auth_client_response_to_json(
  input: CreateOAuthClientResponse,
) -> json.Json {
  json.object([
    #("createOAuthClient", o_auth_client_to_json(input.create_o_auth_client)),
  ])
}

pub fn create_o_auth_client(
  client: squall.Client,
  client_name: String,
  client_type: String,
  redirect_uris: List(String),
  scope: String,
) -> Result(Request(String), String) {
  squall.prepare_request(
    client,
    "mutation CreateOAuthClient($clientName: String!, $clientType: String!, $redirectUris: [String!]!, $scope: String!) {\n  createOAuthClient(clientName: $clientName, clientType: $clientType, redirectUris: $redirectUris, scope: $scope) {\n    clientId\n    clientSecret\n    clientName\n    clientType\n    redirectUris\n    scope\n    createdAt\n  }\n}",
    json.object([
      #("clientName", json.string(client_name)),
      #("clientType", json.string(client_type)),
      #("redirectUris", json.array(from: redirect_uris, of: json.string)),
      #("scope", json.string(scope)),
    ]),
  )
}

pub fn parse_create_o_auth_client_response(
  body: String,
) -> Result(CreateOAuthClientResponse, String) {
  squall.parse_response(body, create_o_auth_client_response_decoder())
}
