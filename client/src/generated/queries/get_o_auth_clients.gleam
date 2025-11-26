import gleam/dynamic/decode
import gleam/http/request.{type Request}
import gleam/json
import squall
import gleam/option.{type Option}

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
  use client_secret <- decode.field("clientSecret", decode.optional(decode.string))
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
  json.object(
    [
      #("clientId", json.string(input.client_id)),
      #("clientSecret", json.nullable(input.client_secret, json.string)),
      #("clientName", json.string(input.client_name)),
      #("clientType", json.string(input.client_type)),
      #("redirectUris", json.array(from: input.redirect_uris, of: json.string)),
      #("scope", json.nullable(input.scope, json.string)),
      #("createdAt", json.int(input.created_at)),
    ],
  )
}

pub type GetOAuthClientsResponse {
  GetOAuthClientsResponse(oauth_clients: List(OAuthClient))
}

pub fn get_o_auth_clients_response_decoder() -> decode.Decoder(GetOAuthClientsResponse) {
  use oauth_clients <- decode.field("oauthClients", decode.list(o_auth_client_decoder()))
  decode.success(GetOAuthClientsResponse(oauth_clients: oauth_clients))
}

pub fn get_o_auth_clients_response_to_json(input: GetOAuthClientsResponse) -> json.Json {
  json.object(
    [
      #("oauthClients", json.array(
        from: input.oauth_clients,
        of: o_auth_client_to_json,
      )),
    ],
  )
}

pub fn get_o_auth_clients(client: squall.Client) -> Result(Request(String), String) {
  squall.prepare_request(
    client,
    "query GetOAuthClients {\n  oauthClients {\n    clientId\n    clientSecret\n    clientName\n    clientType\n    redirectUris\n    scope\n    createdAt\n  }\n}",
    json.object([]),
  )
}

pub fn parse_get_o_auth_clients_response(body: String) -> Result(GetOAuthClientsResponse, String) {
  squall.parse_response(body, get_o_auth_clients_response_decoder())
}
