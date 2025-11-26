import gleam/dynamic/decode
import gleam/http/request.{type Request}
import gleam/json
import squall

pub type Settings {
  Settings(id: String, domain_authority: String)
}

pub fn settings_decoder() -> decode.Decoder(Settings) {
  use id <- decode.field("id", decode.string)
  use domain_authority <- decode.field("domainAuthority", decode.string)
  decode.success(Settings(id: id, domain_authority: domain_authority))
}

pub fn settings_to_json(input: Settings) -> json.Json {
  json.object([
    #("id", json.string(input.id)),
    #("domainAuthority", json.string(input.domain_authority)),
  ])
}

pub type GetSettingsResponse {
  GetSettingsResponse(settings: Settings)
}

pub fn get_settings_response_decoder() -> decode.Decoder(GetSettingsResponse) {
  use settings <- decode.field("settings", settings_decoder())
  decode.success(GetSettingsResponse(settings: settings))
}

pub fn get_settings_response_to_json(input: GetSettingsResponse) -> json.Json {
  json.object([#("settings", settings_to_json(input.settings))])
}

pub fn get_settings(client: squall.Client) -> Result(Request(String), String) {
  squall.prepare_request(
    client,
    "query GetSettings {\n  settings {\n    id\n    domainAuthority\n  }\n}",
    json.object([]),
  )
}

pub fn parse_get_settings_response(
  body: String,
) -> Result(GetSettingsResponse, String) {
  squall.parse_response(body, get_settings_response_decoder())
}
