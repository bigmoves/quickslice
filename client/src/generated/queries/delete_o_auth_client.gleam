import gleam/dynamic/decode
import gleam/http/request.{type Request}
import gleam/json
import squall

pub type DeleteOAuthClientResponse {
  DeleteOAuthClientResponse(delete_o_auth_client: Bool)
}

pub fn delete_o_auth_client_response_decoder() -> decode.Decoder(
  DeleteOAuthClientResponse,
) {
  use delete_o_auth_client <- decode.field("deleteOAuthClient", decode.bool)
  decode.success(DeleteOAuthClientResponse(
    delete_o_auth_client: delete_o_auth_client,
  ))
}

pub fn delete_o_auth_client_response_to_json(
  input: DeleteOAuthClientResponse,
) -> json.Json {
  json.object([#("deleteOAuthClient", json.bool(input.delete_o_auth_client))])
}

pub fn delete_o_auth_client(
  client: squall.Client,
  client_id: String,
) -> Result(Request(String), String) {
  squall.prepare_request(
    client,
    "mutation DeleteOAuthClient($clientId: String!) {\n  deleteOAuthClient(clientId: $clientId)\n}",
    json.object([#("clientId", json.string(client_id))]),
  )
}

pub fn parse_delete_o_auth_client_response(
  body: String,
) -> Result(DeleteOAuthClientResponse, String) {
  squall.parse_response(body, delete_o_auth_client_response_decoder())
}
