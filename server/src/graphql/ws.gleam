/// GraphQL-WS Protocol Implementation
///
/// Implements the graphql-ws WebSocket subprotocol for GraphQL subscriptions
/// Spec: https://github.com/enisdenjo/graphql-ws/blob/master/PROTOCOL.md
import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// Client-to-server message types
/// Server-to-client message types
pub type Message {
  // Client messages
  ConnectionInit(payload: Dict(String, String))
  Subscribe(id: String, query: String, variables: Option(String))
  Complete(id: String)
  Ping
  Pong

  // Server messages
  ConnectionAck
  Next(id: String, data: String)
  ErrorMessage(id: String, message: String)
}

/// Parse a JSON string into a GraphQL-WS message
pub fn parse_message(json_str: String) -> Result(Message, String) {
  //  First parse to extract the type field
  let type_decoder = {
    use message_type <- decode.field("type", decode.string)
    decode.success(message_type)
  }

  use message_type <- result.try(
    json.parse(json_str, type_decoder)
    |> result.map_error(fn(_) { "Missing or invalid 'type' field" }),
  )

  case message_type {
    "connection_init" -> {
      // Try to extract payload, but it's optional
      let payload =
        extract_string_payload(json_str) |> option.unwrap(dict.new())
      Ok(ConnectionInit(payload))
    }

    "subscribe" -> {
      let subscribe_decoder = {
        use id <- decode.field("id", decode.string)
        use query <- decode.subfield(["payload", "query"], decode.string)
        decode.success(#(id, query))
      }

      use #(id, query) <- result.try(
        json.parse(json_str, subscribe_decoder)
        |> result.map_error(fn(_) {
          "Subscribe message missing required fields"
        }),
      )

      // Variables are optional - try to extract them
      let vars = extract_variables_from_json(json_str)

      Ok(Subscribe(id, query, vars))
    }

    "complete" -> {
      let complete_decoder = {
        use id <- decode.field("id", decode.string)
        decode.success(id)
      }

      use id <- result.try(
        json.parse(json_str, complete_decoder)
        |> result.map_error(fn(_) { "Complete message missing 'id' field" }),
      )

      Ok(Complete(id))
    }

    "ping" -> Ok(Ping)

    "pong" -> Ok(Pong)

    _ -> {
      let err_msg = "Unknown message type: " <> message_type
      Error(err_msg)
    }
  }
}

/// Format a GraphQL-WS message as JSON string
pub fn format_message(message: Message) -> String {
  case message {
    ConnectionAck -> "{\"type\":\"connection_ack\"}"

    Next(id, data) ->
      // data is already a JSON string containing the GraphQL response
      "{\"id\":\"" <> id <> "\",\"type\":\"next\",\"payload\":" <> data <> "}"

    ErrorMessage(id, msg) -> {
      let escaped_msg = escape_json_string(msg)
      "{\"id\":\""
      <> id
      <> "\",\"type\":\"error\",\"payload\":[{\"message\":\""
      <> escaped_msg
      <> "\"}]}"
    }

    Complete(id) -> "{\"id\":\"" <> id <> "\",\"type\":\"complete\"}"

    Pong -> "{\"type\":\"pong\"}"

    Ping -> "{\"type\":\"ping\"}"

    // These are client messages, shouldn't normally be formatted by server
    ConnectionInit(_) -> "{\"type\":\"connection_init\"}"

    Subscribe(id, _, _) -> "{\"id\":\"" <> id <> "\",\"type\":\"subscribe\"}"
  }
}

// Helper to escape JSON strings
fn escape_json_string(str: String) -> String {
  str
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\r", "\\r")
  |> string.replace("\t", "\\t")
}

// Helper to extract payload field as dict of strings
fn extract_string_payload(json_str: String) -> Option(Dict(String, String)) {
  let decoder = {
    use payload <- decode.field(
      "payload",
      decode.dict(decode.string, decode.string),
    )
    decode.success(payload)
  }

  json.parse(json_str, decoder)
  |> result.map(Some)
  |> result.unwrap(None)
}

// Helper to extract variables from subscribe message
fn extract_variables_from_json(json_str: String) -> Option(String) {
  let vars_decoder = {
    use vars <- decode.subfield(["payload", "variables"], decode.string)
    decode.success(vars)
  }

  json.parse(json_str, vars_decoder)
  |> result.map(Some)
  |> result.unwrap(None)
}
