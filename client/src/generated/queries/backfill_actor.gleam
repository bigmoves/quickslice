import gleam/dynamic/decode
import gleam/http/request.{type Request}
import gleam/json
import squall

pub type BackfillActorResponse {
  BackfillActorResponse(backfill_actor: Bool)
}

pub fn backfill_actor_response_decoder() -> decode.Decoder(
  BackfillActorResponse,
) {
  use backfill_actor <- decode.field("backfillActor", decode.bool)
  decode.success(BackfillActorResponse(backfill_actor: backfill_actor))
}

pub fn backfill_actor_response_to_json(
  input: BackfillActorResponse,
) -> json.Json {
  json.object([#("backfillActor", json.bool(input.backfill_actor))])
}

pub fn backfill_actor(
  client: squall.Client,
  did: String,
) -> Result(Request(String), String) {
  squall.prepare_request(
    client,
    "mutation BackfillActor($did: String!) {\n  backfillActor(did: $did)\n}",
    json.object([#("did", json.string(did))]),
  )
}

pub fn parse_backfill_actor_response(
  body: String,
) -> Result(BackfillActorResponse, String) {
  squall.parse_response(body, backfill_actor_response_decoder())
}
