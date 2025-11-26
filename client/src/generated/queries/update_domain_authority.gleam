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
  json.object(
    [
      #("id", json.string(input.id)),
      #("domainAuthority", json.string(input.domain_authority)),
    ],
  )
}

pub type UpdateDomainAuthorityResponse {
  UpdateDomainAuthorityResponse(update_domain_authority: Settings)
}

pub fn update_domain_authority_response_decoder() -> decode.Decoder(UpdateDomainAuthorityResponse) {
  use update_domain_authority <- decode.field("updateDomainAuthority", settings_decoder())
  decode.success(UpdateDomainAuthorityResponse(
    update_domain_authority: update_domain_authority,
  ))
}

pub fn update_domain_authority_response_to_json(input: UpdateDomainAuthorityResponse) -> json.Json {
  json.object(
    [
      #("updateDomainAuthority", settings_to_json(input.update_domain_authority)),
    ],
  )
}

pub fn update_domain_authority(client: squall.Client, domain_authority: String) -> Result(Request(String), String) {
  squall.prepare_request(
    client,
    "mutation UpdateDomainAuthority($domainAuthority: String!) {\n  updateDomainAuthority(domainAuthority: $domainAuthority) {\n    id\n    domainAuthority\n  }\n}",
    json.object([#("domainAuthority", json.string(domain_authority))]),
  )
}

pub fn parse_update_domain_authority_response(body: String) -> Result(UpdateDomainAuthorityResponse, String) {
  squall.parse_response(body, update_domain_authority_response_decoder())
}
