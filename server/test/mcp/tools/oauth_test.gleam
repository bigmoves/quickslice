import gleam/json
import gleam/string
import gleeunit/should
import lib/mcp/tools/oauth as oauth_tools

pub fn get_oauth_info_returns_flows_test() {
  let result =
    oauth_tools.get_oauth_info("https://example.com", [
      "atproto",
      "transition:generic",
    ])

  result |> should.be_ok
  let assert Ok(json_result) = result
  let json_str = json.to_string(json_result)

  // Should contain authorization_code flow
  string.contains(json_str, "authorization_code") |> should.be_true
}

pub fn get_oauth_info_returns_endpoints_test() {
  let result =
    oauth_tools.get_oauth_info("https://example.com", [
      "atproto",
      "transition:generic",
    ])

  result |> should.be_ok
  let assert Ok(json_result) = result
  let json_str = json.to_string(json_result)

  // Should contain endpoints
  string.contains(json_str, "/oauth/authorize") |> should.be_true
  string.contains(json_str, "/oauth/token") |> should.be_true
}

pub fn get_oauth_info_returns_scopes_test() {
  let result =
    oauth_tools.get_oauth_info("https://example.com", [
      "atproto",
      "custom:scope",
    ])

  result |> should.be_ok
  let assert Ok(json_result) = result
  let json_str = json.to_string(json_result)

  // Should contain the scopes
  string.contains(json_str, "atproto") |> should.be_true
  string.contains(json_str, "custom:scope") |> should.be_true
}
