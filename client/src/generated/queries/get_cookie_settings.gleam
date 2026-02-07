import gleam/dynamic/decode
import gleam/http/request.{type Request}
import gleam/json
import gleam/option.{type Option}
import squall

pub type CookieSettings {
  CookieSettings(same_site: String, secure: String, domain: Option(String))
}

pub fn cookie_settings_decoder() -> decode.Decoder(CookieSettings) {
  use same_site <- decode.field("sameSite", decode.string)
  use secure <- decode.field("secure", decode.string)
  use domain <- decode.field("domain", decode.optional(decode.string))
  decode.success(CookieSettings(
    same_site: same_site,
    secure: secure,
    domain: domain,
  ))
}

pub fn cookie_settings_to_json(input: CookieSettings) -> json.Json {
  json.object([
    #("sameSite", json.string(input.same_site)),
    #("secure", json.string(input.secure)),
    #("domain", json.nullable(input.domain, json.string)),
  ])
}

pub type GetCookieSettingsResponse {
  GetCookieSettingsResponse(cookie_settings: CookieSettings)
}

pub fn get_cookie_settings_response_decoder() -> decode.Decoder(
  GetCookieSettingsResponse,
) {
  use cookie_settings <- decode.field(
    "cookieSettings",
    cookie_settings_decoder(),
  )
  decode.success(GetCookieSettingsResponse(cookie_settings: cookie_settings))
}

pub fn get_cookie_settings_response_to_json(
  input: GetCookieSettingsResponse,
) -> json.Json {
  json.object([
    #("cookieSettings", cookie_settings_to_json(input.cookie_settings)),
  ])
}

pub fn get_cookie_settings(
  client: squall.Client,
) -> Result(Request(String), String) {
  squall.prepare_request(
    client,
    "query GetCookieSettings {\n  cookieSettings {\n    __typename\n    sameSite\n    secure\n    domain\n  }\n}",
    json.object([]),
  )
}

pub fn parse_get_cookie_settings_response(
  body: String,
) -> Result(GetCookieSettingsResponse, String) {
  squall.parse_response(body, get_cookie_settings_response_decoder())
}
