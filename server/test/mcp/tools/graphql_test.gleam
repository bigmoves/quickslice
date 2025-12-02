import database/repositories/lexicons
import database/schema/migrations
import gleam/json
import gleam/option
import gleam/string
import gleeunit/should
import lib/mcp/tools/graphql as graphql_tools
import lib/oauth/did_cache
import sqlight

fn setup_test_db() -> sqlight.Connection {
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = migrations.run_migrations(db)

  // Insert a test lexicon so schema builds
  let lexicon_json =
    "{\"lexicon\":1,\"id\":\"test.example.status\",\"defs\":{\"main\":{\"type\":\"record\",\"key\":\"tid\",\"record\":{\"type\":\"object\",\"properties\":{\"text\":{\"type\":\"string\"}}}}}}"
  let assert Ok(_) = lexicons.insert(db, "test.example.status", lexicon_json)

  db
}

pub fn execute_query_runs_introspection_test() {
  let db = setup_test_db()
  let assert Ok(did_cache) = did_cache.start()

  let result =
    graphql_tools.execute_query(
      db,
      "{ __typename }",
      "{}",
      did_cache,
      option.None,
      "https://plc.directory",
    )

  result |> should.be_ok
  let assert Ok(json_result) = result
  let json_str = json.to_string(json_result)

  // Should contain Query typename
  string.contains(json_str, "Query") |> should.be_true
}

pub fn introspect_schema_returns_schema_test() {
  let db = setup_test_db()
  let assert Ok(did_cache) = did_cache.start()

  let result =
    graphql_tools.introspect_schema(
      db,
      did_cache,
      option.None,
      "https://plc.directory",
    )

  result |> should.be_ok
  let assert Ok(json_result) = result
  let json_str = json.to_string(json_result)

  // Should contain schema info
  string.contains(json_str, "__schema") |> should.be_true
}
