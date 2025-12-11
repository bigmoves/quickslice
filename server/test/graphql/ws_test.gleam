/// Tests for GraphQL-WS protocol implementation
///
/// Tests the GraphQL-WS (WebSocket) protocol message handling
import gleam/dict
import gleam/option.{Some}
import gleeunit
import gleeunit/should
import graphql/ws

pub fn main() {
  gleeunit.main()
}

// Test: Parse connection_init message
pub fn parse_connection_init_test() {
  let json_str =
    "{\"type\":\"connection_init\",\"payload\":{\"Authorization\":\"Bearer token123\"}}"

  case ws.parse_message(json_str) {
    Ok(ws.ConnectionInit(payload)) -> {
      case dict.get(payload, "Authorization") {
        Ok("Bearer token123") -> should.be_true(True)
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }
}

// Test: Parse connection_init without payload
pub fn parse_connection_init_no_payload_test() {
  let json_str = "{\"type\":\"connection_init\"}"

  case ws.parse_message(json_str) {
    Ok(ws.ConnectionInit(payload)) -> {
      dict.size(payload)
      |> should.equal(0)
    }
    _ -> should.fail()
  }
}

// Test: Parse subscribe message
pub fn parse_subscribe_message_test() {
  let json_str =
    "{\"id\":\"1\",\"type\":\"subscribe\",\"payload\":{\"query\":\"subscription { postCreated { text } }\"}}"

  case ws.parse_message(json_str) {
    Ok(ws.Subscribe(id, query, _vars)) -> {
      id |> should.equal("1")
      query |> should.equal("subscription { postCreated { text } }")
    }
    _ -> should.fail()
  }
}

// Test: Parse subscribe message with variables
pub fn parse_subscribe_with_variables_test() {
  let json_str =
    "{\"id\":\"1\",\"type\":\"subscribe\",\"payload\":{\"query\":\"subscription { postCreated { text } }\",\"variables\":\"{}\"}}"

  case ws.parse_message(json_str) {
    Ok(ws.Subscribe(id, query, vars)) -> {
      id |> should.equal("1")
      query |> should.equal("subscription { postCreated { text } }")
      vars |> should.equal(Some("{}"))
    }
    _ -> should.fail()
  }
}

// Test: Parse ping message
pub fn parse_ping_message_test() {
  let json_str = "{\"type\":\"ping\"}"

  case ws.parse_message(json_str) {
    Ok(ws.Ping) -> should.be_true(True)
    _ -> should.fail()
  }
}

// Test: Parse pong message
pub fn parse_pong_message_test() {
  let json_str = "{\"type\":\"pong\"}"

  case ws.parse_message(json_str) {
    Ok(ws.Pong) -> should.be_true(True)
    _ -> should.fail()
  }
}

// Test: Parse complete message
pub fn parse_complete_message_test() {
  let json_str = "{\"id\":\"1\",\"type\":\"complete\"}"

  case ws.parse_message(json_str) {
    Ok(ws.Complete(id)) -> {
      id |> should.equal("1")
    }
    _ -> should.fail()
  }
}

// Test: Format connection_ack message
pub fn format_connection_ack_test() {
  let message = ws.ConnectionAck

  let json_str = ws.format_message(message)

  // Should produce valid JSON with type "connection_ack"
  json_str |> should.equal("{\"type\":\"connection_ack\"}")
}

// Test: Format next message with data
pub fn format_next_message_test() {
  let data_json = "{\"data\":{\"postCreated\":{\"text\":\"Hello\"}}}"

  let message = ws.Next("1", data_json)

  let json_str = ws.format_message(message)

  // Should contain id, type "next", and payload
  json_str
  |> should.equal(
    "{\"id\":\"1\",\"type\":\"next\",\"payload\":" <> data_json <> "}",
  )
}

// Test: Format error message
pub fn format_error_message_test() {
  let message = ws.ErrorMessage("1", "Syntax error")

  let json_str = ws.format_message(message)

  // Should contain id, type "error", and payload with message
  json_str
  |> should.equal(
    "{\"id\":\"1\",\"type\":\"error\",\"payload\":[{\"message\":\"Syntax error\"}]}",
  )
}

// Test: Format error message with quotes
pub fn format_error_message_with_quotes_test() {
  let message = ws.ErrorMessage("1", "Field \"text\" not found")

  let json_str = ws.format_message(message)

  // Should escape quotes in error message
  json_str
  |> should.equal(
    "{\"id\":\"1\",\"type\":\"error\",\"payload\":[{\"message\":\"Field \\\"text\\\" not found\"}]}",
  )
}

// Test: Format complete message
pub fn format_complete_message_test() {
  let message = ws.Complete("1")

  let json_str = ws.format_message(message)

  json_str |> should.equal("{\"id\":\"1\",\"type\":\"complete\"}")
}

// Test: Format pong message
pub fn format_pong_message_test() {
  let message = ws.Pong

  let json_str = ws.format_message(message)

  json_str |> should.equal("{\"type\":\"pong\"}")
}

// Test: Invalid JSON should return error
pub fn parse_invalid_json_test() {
  let json_str = "not valid json"

  case ws.parse_message(json_str) {
    Error(_) -> should.be_true(True)
    Ok(_) -> should.fail()
  }
}

// Test: Unknown message type should return error
pub fn parse_unknown_type_test() {
  let json_str = "{\"type\":\"unknown_message_type\"}"

  case ws.parse_message(json_str) {
    Error(_) -> should.be_true(True)
    Ok(_) -> should.fail()
  }
}

// Test: Missing required fields should return error
pub fn parse_subscribe_missing_id_test() {
  let json_str = "{\"type\":\"subscribe\",\"payload\":{}}"

  case ws.parse_message(json_str) {
    Error(_) -> should.be_true(True)
    Ok(_) -> should.fail()
  }
}
