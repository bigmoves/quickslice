import database/schema/migrations
import gleam/json
import gleam/string
import gleeunit/should
import lib/mcp/tools/capabilities
import sqlight

pub fn get_server_capabilities_returns_version_test() {
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = migrations.run_migrations(db)

  let result = capabilities.get_server_capabilities(db)

  result |> should.be_ok
  let assert Ok(json_result) = result
  let json_str = json.to_string(json_result)

  // Should contain version
  string.contains(json_str, "version") |> should.be_true
}

pub fn get_server_capabilities_returns_features_test() {
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = migrations.run_migrations(db)

  let result = capabilities.get_server_capabilities(db)

  result |> should.be_ok
  let assert Ok(json_result) = result
  let json_str = json.to_string(json_result)

  // Should contain features
  string.contains(json_str, "graphql") |> should.be_true
  string.contains(json_str, "subscriptions") |> should.be_true
}
