import generated/queries/get_cookie_settings.{
  type CookieSettings, cookie_settings_decoder, cookie_settings_to_json,
}
import gleam/dynamic/decode
import gleam/json
import squall

pub type UpdateCookieSettingsResponse {
  UpdateCookieSettingsResponse(update_cookie_settings: CookieSettings)
}

pub fn update_cookie_settings_response_decoder() -> decode.Decoder(
  UpdateCookieSettingsResponse,
) {
  use update_cookie_settings <- decode.field(
    "updateCookieSettings",
    cookie_settings_decoder(),
  )
  decode.success(UpdateCookieSettingsResponse(
    update_cookie_settings: update_cookie_settings,
  ))
}

pub fn update_cookie_settings_response_to_json(
  input: UpdateCookieSettingsResponse,
) -> json.Json {
  json.object([
    #(
      "updateCookieSettings",
      cookie_settings_to_json(input.update_cookie_settings),
    ),
  ])
}

pub fn parse_update_cookie_settings_response(
  body: String,
) -> Result(UpdateCookieSettingsResponse, String) {
  squall.parse_response(body, update_cookie_settings_response_decoder())
}
