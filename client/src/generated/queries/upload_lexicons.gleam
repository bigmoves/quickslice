import gleam/dynamic/decode
import gleam/http/request.{type Request}
import gleam/json
import squall

pub type UploadLexiconsResponse {
  UploadLexiconsResponse(upload_lexicons: Bool)
}

pub fn upload_lexicons_response_decoder() -> decode.Decoder(UploadLexiconsResponse) {
  use upload_lexicons <- decode.field("uploadLexicons", decode.bool)
  decode.success(UploadLexiconsResponse(upload_lexicons: upload_lexicons))
}

pub fn upload_lexicons_response_to_json(input: UploadLexiconsResponse) -> json.Json {
  json.object([#("uploadLexicons", json.bool(input.upload_lexicons))])
}

pub fn upload_lexicons(client: squall.Client, zip_base64: String) -> Result(Request(String), String) {
  squall.prepare_request(
    client,
    "mutation UploadLexicons($zipBase64: String!) {\n  uploadLexicons(zipBase64: $zipBase64)\n}",
    json.object([#("zipBase64", json.string(zip_base64))]),
  )
}

pub fn parse_upload_lexicons_response(body: String) -> Result(UploadLexiconsResponse, String) {
  squall.parse_response(body, upload_lexicons_response_decoder())
}
