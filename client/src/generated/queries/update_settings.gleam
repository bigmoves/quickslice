import gleam/dynamic/decode
import gleam/http/request.{type Request}
import gleam/json
import squall

pub type Settings {
  Settings(id: String, domain_authority: String, admin_dids: List(String))
}

pub fn settings_decoder() -> decode.Decoder(Settings) {
  use id <- decode.field("id", decode.string)
  use domain_authority <- decode.field("domainAuthority", decode.string)
  use admin_dids <- decode.field("adminDids", decode.list(decode.string))
  decode.success(Settings(
    id: id,
    domain_authority: domain_authority,
    admin_dids: admin_dids,
  ))
}

pub fn settings_to_json(input: Settings) -> json.Json {
  json.object([
    #("id", json.string(input.id)),
    #("domainAuthority", json.string(input.domain_authority)),
    #("adminDids", json.array(from: input.admin_dids, of: json.string)),
  ])
}

pub type UpdateSettingsResponse {
  UpdateSettingsResponse(update_settings: Settings)
}

pub fn update_settings_response_decoder() -> decode.Decoder(
  UpdateSettingsResponse,
) {
  use update_settings <- decode.field("updateSettings", settings_decoder())
  decode.success(UpdateSettingsResponse(update_settings: update_settings))
}

pub fn update_settings_response_to_json(
  input: UpdateSettingsResponse,
) -> json.Json {
  json.object([#("updateSettings", settings_to_json(input.update_settings))])
}

pub fn update_settings(
  client: squall.Client,
  domain_authority: String,
  admin_dids: List(String),
) -> Result(Request(String), String) {
  squall.prepare_request(
    client,
    "mutation UpdateSettings($domainAuthority: String, $adminDids: [String!]) {\n  updateSettings(domainAuthority: $domainAuthority, adminDids: $adminDids) {\n    id\n    domainAuthority\n    adminDids\n  }\n}",
    json.object([
      #("domainAuthority", json.string(domain_authority)),
      #("adminDids", json.array(from: admin_dids, of: json.string)),
    ]),
  )
}

pub fn parse_update_settings_response(
  body: String,
) -> Result(UpdateSettingsResponse, String) {
  squall.parse_response(body, update_settings_response_decoder())
}
