import gleam/dynamic/decode
import gleam/http/request.{type Request}
import gleam/json
import squall

pub type Statistics {
  Statistics(record_count: Int, actor_count: Int, lexicon_count: Int)
}

pub fn statistics_decoder() -> decode.Decoder(Statistics) {
  use record_count <- decode.field("recordCount", decode.int)
  use actor_count <- decode.field("actorCount", decode.int)
  use lexicon_count <- decode.field("lexiconCount", decode.int)
  decode.success(Statistics(
    record_count: record_count,
    actor_count: actor_count,
    lexicon_count: lexicon_count,
  ))
}

pub fn statistics_to_json(input: Statistics) -> json.Json {
  json.object(
    [
      #("recordCount", json.int(input.record_count)),
      #("actorCount", json.int(input.actor_count)),
      #("lexiconCount", json.int(input.lexicon_count)),
    ],
  )
}

pub type GetStatisticsResponse {
  GetStatisticsResponse(statistics: Statistics)
}

pub fn get_statistics_response_decoder() -> decode.Decoder(GetStatisticsResponse) {
  use statistics <- decode.field("statistics", statistics_decoder())
  decode.success(GetStatisticsResponse(statistics: statistics))
}

pub fn get_statistics_response_to_json(input: GetStatisticsResponse) -> json.Json {
  json.object([#("statistics", statistics_to_json(input.statistics))])
}

pub fn get_statistics(client: squall.Client) -> Result(Request(String), String) {
  squall.prepare_request(
    client,
    "query GetStatistics {\n  statistics {\n    recordCount\n    actorCount\n    lexiconCount\n  }\n}",
    json.object([]),
  )
}

pub fn parse_get_statistics_response(body: String) -> Result(GetStatisticsResponse, String) {
  squall.parse_response(body, get_statistics_response_decoder())
}
