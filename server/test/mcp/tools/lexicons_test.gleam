import database/repositories/lexicons
import database/schema/migrations
import gleam/json
import gleam/string
import gleeunit/should
import lib/mcp/tools/lexicons as lexicon_tools
import sqlight

pub fn list_lexicons_returns_array_test() {
  // Set up in-memory database
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = migrations.run_migrations(db)

  // Insert a test lexicon
  let lexicon_json =
    "{\"lexicon\":1,\"id\":\"test.example.record\",\"defs\":{\"main\":{\"type\":\"record\"}}}"
  let assert Ok(_) = lexicons.insert(db, "test.example.record", lexicon_json)

  // Call the tool
  let result = lexicon_tools.list_lexicons(db)

  result |> should.be_ok
  let assert Ok(json_result) = result
  let json_str = json.to_string(json_result)

  // Should contain the lexicon NSID
  string.contains(json_str, "test.example.record") |> should.be_true
}

pub fn get_lexicon_returns_full_definition_test() {
  // Set up in-memory database
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = migrations.run_migrations(db)

  // Insert a test lexicon
  let lexicon_json =
    "{\"lexicon\":1,\"id\":\"test.example.record\",\"defs\":{\"main\":{\"type\":\"record\"}}}"
  let assert Ok(_) = lexicons.insert(db, "test.example.record", lexicon_json)

  // Call the tool
  let result = lexicon_tools.get_lexicon(db, "test.example.record")

  result |> should.be_ok
  let assert Ok(json_result) = result
  let json_str = json.to_string(json_result)

  // Should contain the full lexicon definition (escaped since it's inside a JSON string)
  string.contains(json_str, "record") |> should.be_true
}

pub fn get_lexicon_not_found_test() {
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = migrations.run_migrations(db)

  let result = lexicon_tools.get_lexicon(db, "nonexistent.lexicon")

  result |> should.be_error
}
