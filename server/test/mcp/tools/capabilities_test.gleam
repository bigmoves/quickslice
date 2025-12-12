import database/sqlite/connection as db_connection
import gleam/json
import gleam/string
import gleeunit/should
import lib/mcp/tools/capabilities

pub fn get_server_capabilities_returns_version_test() {
  let assert Ok(exec) = db_connection.connect("sqlite::memory:")

  let result = capabilities.get_server_capabilities(exec)

  result |> should.be_ok
  let assert Ok(json_result) = result
  let json_str = json.to_string(json_result)

  // Should contain version
  string.contains(json_str, "version") |> should.be_true
}

pub fn get_server_capabilities_returns_features_test() {
  let assert Ok(exec) = db_connection.connect("sqlite::memory:")

  let result = capabilities.get_server_capabilities(exec)

  result |> should.be_ok
  let assert Ok(json_result) = result
  let json_str = json.to_string(json_result)

  // Should contain features
  string.contains(json_str, "graphql") |> should.be_true
  string.contains(json_str, "subscriptions") |> should.be_true
}
