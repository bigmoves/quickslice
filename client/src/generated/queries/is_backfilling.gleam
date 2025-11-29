import gleam/dynamic/decode
import gleam/http/request.{type Request}
import gleam/json
import squall

pub type IsBackfillingResponse {
  IsBackfillingResponse(is_backfilling: Bool)
}

pub fn is_backfilling_response_decoder() -> decode.Decoder(
  IsBackfillingResponse,
) {
  use is_backfilling <- decode.field("isBackfilling", decode.bool)
  decode.success(IsBackfillingResponse(is_backfilling: is_backfilling))
}

pub fn is_backfilling_response_to_json(
  input: IsBackfillingResponse,
) -> json.Json {
  json.object([#("isBackfilling", json.bool(input.is_backfilling))])
}

pub fn is_backfilling(client: squall.Client) -> Result(Request(String), String) {
  squall.prepare_request(
    client,
    "query IsBackfilling {\n  isBackfilling\n}",
    json.object([]),
  )
}

pub fn parse_is_backfilling_response(
  body: String,
) -> Result(IsBackfillingResponse, String) {
  squall.parse_response(body, is_backfilling_response_decoder())
}
