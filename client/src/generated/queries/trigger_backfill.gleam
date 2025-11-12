import gleam/dynamic/decode
import gleam/http/request.{type Request}
import gleam/json
import squall

pub type TriggerBackfillResponse {
  TriggerBackfillResponse(trigger_backfill: Bool)
}

pub fn trigger_backfill_response_decoder() -> decode.Decoder(TriggerBackfillResponse) {
  use trigger_backfill <- decode.field("triggerBackfill", decode.bool)
  decode.success(TriggerBackfillResponse(trigger_backfill: trigger_backfill))
}

pub fn trigger_backfill_response_to_json(input: TriggerBackfillResponse) -> json.Json {
  json.object([#("triggerBackfill", json.bool(input.trigger_backfill))])
}

pub fn trigger_backfill(client: squall.Client) -> Result(Request(String), String) {
  squall.prepare_request(
    client,
    "mutation TriggerBackfill {\n  triggerBackfill\n}",
    json.object([]),
  )
}

pub fn parse_trigger_backfill_response(body: String) -> Result(TriggerBackfillResponse, String) {
  squall.parse_response(body, trigger_backfill_response_decoder())
}
