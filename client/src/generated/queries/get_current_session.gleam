import gleam/dynamic/decode
import gleam/http/request.{type Request}
import gleam/json
import gleam/option.{type Option}
import squall

pub type CurrentSession {
  CurrentSession(did: String, handle: String, is_admin: Bool)
}

pub fn current_session_decoder() -> decode.Decoder(CurrentSession) {
  use did <- decode.field("did", decode.string)
  use handle <- decode.field("handle", decode.string)
  use is_admin <- decode.field("isAdmin", decode.bool)
  decode.success(CurrentSession(did: did, handle: handle, is_admin: is_admin))
}

pub fn current_session_to_json(input: CurrentSession) -> json.Json {
  json.object([
    #("did", json.string(input.did)),
    #("handle", json.string(input.handle)),
    #("isAdmin", json.bool(input.is_admin)),
  ])
}

pub type GetCurrentSessionResponse {
  GetCurrentSessionResponse(current_session: Option(CurrentSession))
}

pub fn get_current_session_response_decoder() -> decode.Decoder(
  GetCurrentSessionResponse,
) {
  use current_session <- decode.field(
    "currentSession",
    decode.optional(current_session_decoder()),
  )
  decode.success(GetCurrentSessionResponse(current_session: current_session))
}

pub fn get_current_session_response_to_json(
  input: GetCurrentSessionResponse,
) -> json.Json {
  json.object([
    #(
      "currentSession",
      json.nullable(input.current_session, current_session_to_json),
    ),
  ])
}

pub fn get_current_session(
  client: squall.Client,
) -> Result(Request(String), String) {
  squall.prepare_request(
    client,
    "query GetCurrentSession {\n  currentSession {\n    did\n    handle\n    isAdmin\n  }\n}",
    json.object([]),
  )
}

pub fn parse_get_current_session_response(
  body: String,
) -> Result(GetCurrentSessionResponse, String) {
  squall.parse_response(body, get_current_session_response_decoder())
}
