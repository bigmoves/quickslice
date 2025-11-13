import gleam/dynamic/decode
import gleam/http/request.{type Request}
import gleam/json
import squall

pub type ResetAllResponse {
  ResetAllResponse(reset_all: Bool)
}

pub fn reset_all_response_decoder() -> decode.Decoder(ResetAllResponse) {
  use reset_all <- decode.field("resetAll", decode.bool)
  decode.success(ResetAllResponse(reset_all: reset_all))
}

pub fn reset_all_response_to_json(input: ResetAllResponse) -> json.Json {
  json.object([#("resetAll", json.bool(input.reset_all))])
}

pub fn reset_all(client: squall.Client, confirm: String) -> Result(Request(String), String) {
  squall.prepare_request(
    client,
    "mutation ResetAll($confirm: String!) {\n  resetAll(confirm: $confirm)\n}",
    json.object([#("confirm", json.string(confirm))]),
  )
}

pub fn parse_reset_all_response(body: String) -> Result(ResetAllResponse, String) {
  squall.parse_response(body, reset_all_response_decoder())
}
