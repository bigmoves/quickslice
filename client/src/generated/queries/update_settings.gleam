import gleam/dynamic/decode
import gleam/http/request.{type Request}
import gleam/json
import squall

pub type Settings {
  Settings(
    id: String,
    domain_authority: String,
    admin_dids: List(String),
    relay_url: String,
    plc_directory_url: String,
    jetstream_url: String,
    oauth_supported_scopes: String,
  )
}

pub fn settings_decoder() -> decode.Decoder(Settings) {
  use id <- decode.field("id", decode.string)
  use domain_authority <- decode.field("domainAuthority", decode.string)
  use admin_dids <- decode.field("adminDids", decode.list(decode.string))
  use relay_url <- decode.field("relayUrl", decode.string)
  use plc_directory_url <- decode.field("plcDirectoryUrl", decode.string)
  use jetstream_url <- decode.field("jetstreamUrl", decode.string)
  use oauth_supported_scopes <- decode.field(
    "oauthSupportedScopes",
    decode.string,
  )
  decode.success(Settings(
    id: id,
    domain_authority: domain_authority,
    admin_dids: admin_dids,
    relay_url: relay_url,
    plc_directory_url: plc_directory_url,
    jetstream_url: jetstream_url,
    oauth_supported_scopes: oauth_supported_scopes,
  ))
}

pub fn settings_to_json(input: Settings) -> json.Json {
  json.object([
    #("id", json.string(input.id)),
    #("domainAuthority", json.string(input.domain_authority)),
    #("adminDids", json.array(from: input.admin_dids, of: json.string)),
    #("relayUrl", json.string(input.relay_url)),
    #("plcDirectoryUrl", json.string(input.plc_directory_url)),
    #("jetstreamUrl", json.string(input.jetstream_url)),
    #("oauthSupportedScopes", json.string(input.oauth_supported_scopes)),
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
  relay_url: String,
  plc_directory_url: String,
  jetstream_url: String,
  oauth_supported_scopes: String,
) -> Result(Request(String), String) {
  squall.prepare_request(
    client,
    "mutation UpdateSettings($domainAuthority: String, $adminDids: [String!], $relayUrl: String, $plcDirectoryUrl: String, $jetstreamUrl: String, $oauthSupportedScopes: String) {\n  updateSettings(domainAuthority: $domainAuthority, adminDids: $adminDids, relayUrl: $relayUrl, plcDirectoryUrl: $plcDirectoryUrl, jetstreamUrl: $jetstreamUrl, oauthSupportedScopes: $oauthSupportedScopes) {\n    id\n    domainAuthority\n    adminDids\n    relayUrl\n    plcDirectoryUrl\n    jetstreamUrl\n    oauthSupportedScopes\n  }\n}",
    json.object([
      #("domainAuthority", json.string(domain_authority)),
      #("adminDids", json.array(from: admin_dids, of: json.string)),
      #("relayUrl", json.string(relay_url)),
      #("plcDirectoryUrl", json.string(plc_directory_url)),
      #("jetstreamUrl", json.string(jetstream_url)),
      #("oauthSupportedScopes", json.string(oauth_supported_scopes)),
    ]),
  )
}

pub fn parse_update_settings_response(
  body: String,
) -> Result(UpdateSettingsResponse, String) {
  squall.parse_response(body, update_settings_response_decoder())
}
