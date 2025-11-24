import gleam/dynamic/decode
import gleam/http/request.{type Request}
import gleam/json
import squall

pub type Lexicon {
  Lexicon(id: String, json: String, created_at: String)
}

pub fn lexicon_decoder() -> decode.Decoder(Lexicon) {
  use id <- decode.field("id", decode.string)
  use json <- decode.field("json", decode.string)
  use created_at <- decode.field("createdAt", decode.string)
  decode.success(Lexicon(id: id, json: json, created_at: created_at))
}

pub fn lexicon_to_json(input: Lexicon) -> json.Json {
  json.object([
    #("id", json.string(input.id)),
    #("json", json.string(input.json)),
    #("createdAt", json.string(input.created_at)),
  ])
}

pub type GetLexiconsResponse {
  GetLexiconsResponse(lexicons: List(Lexicon))
}

pub fn get_lexicons_response_decoder() -> decode.Decoder(GetLexiconsResponse) {
  use lexicons <- decode.field("lexicons", decode.list(lexicon_decoder()))
  decode.success(GetLexiconsResponse(lexicons: lexicons))
}

pub fn get_lexicons_response_to_json(input: GetLexiconsResponse) -> json.Json {
  json.object([
    #("lexicons", json.array(from: input.lexicons, of: lexicon_to_json)),
  ])
}

pub fn get_lexicons(client: squall.Client) -> Result(Request(String), String) {
  squall.prepare_request(
    client,
    "query GetLexicons {\n  lexicons {\n    id\n    json\n    createdAt\n  }\n}",
    json.object([]),
  )
}

pub fn parse_get_lexicons_response(
  body: String,
) -> Result(GetLexiconsResponse, String) {
  squall.parse_response(body, get_lexicons_response_decoder())
}
