import gleam/dynamic/decode
import gleam/json
import gleam/option.{type Option, None}
import gleam/result

/// JSON-RPC request ID can be string or int
pub type RequestId {
  StringId(String)
  IntId(Int)
  NullId
}

/// Decoded MCP request
pub type McpRequest {
  McpRequest(
    jsonrpc: String,
    method: String,
    params: Option(json.Json),
    id: RequestId,
  )
}

/// MCP response (success case)
pub type McpResponse {
  McpResponse(jsonrpc: String, result: json.Json, id: RequestId)
}

/// MCP error response
pub type McpErrorResponse {
  McpErrorResponse(jsonrpc: String, error: McpError, id: RequestId)
}

/// JSON-RPC error object
pub type McpError {
  McpError(code: Int, message: String)
}

/// Standard JSON-RPC error codes
pub const parse_error = -32_700

pub const invalid_request = -32_600

pub const method_not_found = -32_601

pub const invalid_params = -32_602

pub const internal_error = -32_603

/// Decode a JSON-RPC request from string
pub fn decode_request(json_str: String) -> Result(McpRequest, String) {
  // ID can be int or string - try both
  let id_decoder =
    decode.one_of(decode.int |> decode.map(IntId), [
      decode.string |> decode.map(StringId),
    ])

  let decoder = {
    use jsonrpc <- decode.field("jsonrpc", decode.string)
    use method <- decode.field("method", decode.string)
    use id <- decode.optional_field("id", NullId, id_decoder)
    decode.success(McpRequest(jsonrpc:, method:, params: None, id:))
  }

  json.parse(json_str, decoder)
  |> result.map_error(fn(_) { "Failed to parse JSON-RPC request" })
}

/// Encode a success response
pub fn encode_response(result: json.Json, id: RequestId) -> String {
  let id_json = case id {
    IntId(i) -> json.int(i)
    StringId(s) -> json.string(s)
    NullId -> json.null()
  }

  json.object([
    #("jsonrpc", json.string("2.0")),
    #("result", result),
    #("id", id_json),
  ])
  |> json.to_string
}

/// Encode an error response
pub fn encode_error(code: Int, message: String, id: RequestId) -> String {
  let id_json = case id {
    IntId(i) -> json.int(i)
    StringId(s) -> json.string(s)
    NullId -> json.null()
  }

  json.object([
    #("jsonrpc", json.string("2.0")),
    #(
      "error",
      json.object([
        #("code", json.int(code)),
        #("message", json.string(message)),
      ]),
    ),
    #("id", id_json),
  ])
  |> json.to_string
}
