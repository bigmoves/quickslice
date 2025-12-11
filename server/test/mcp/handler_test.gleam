import database/executor.{type Executor}
import database/repositories/lexicons
import gleam/http
import gleam/option
import gleam/string
import gleeunit/should
import handlers/mcp
import lib/oauth/did_cache
import test_helpers
import wisp
import wisp/simulate

fn setup_test_ctx() -> #(Executor, mcp.McpContext) {
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_lexicon_table(exec)
  let assert Ok(_) = test_helpers.create_record_table(exec)
  let assert Ok(did_cache) = did_cache.start()

  let ctx =
    mcp.McpContext(
      db: exec,
      external_base_url: "https://example.com",
      did_cache: did_cache,
      signing_key: option.None,
      plc_url: "https://plc.directory",
      supported_scopes: ["atproto", "transition:generic"],
    )

  #(exec, ctx)
}

pub fn handle_initialize_test() {
  let #(_db, ctx) = setup_test_ctx()

  let body =
    "{\"jsonrpc\":\"2.0\",\"method\":\"initialize\",\"params\":{},\"id\":1}"
  let req =
    simulate.request(http.Post, "/mcp")
    |> simulate.header("content-type", "application/json")
    |> simulate.string_body(body)

  let response = mcp.handle(req, ctx)

  response.status |> should.equal(200)
  let assert wisp.Text(body_result) = response.body
  string.contains(body_result, "quickslice") |> should.be_true
}

pub fn handle_tools_list_test() {
  let #(_db, ctx) = setup_test_ctx()

  let body = "{\"jsonrpc\":\"2.0\",\"method\":\"tools/list\",\"id\":2}"
  let req =
    simulate.request(http.Post, "/mcp")
    |> simulate.header("content-type", "application/json")
    |> simulate.string_body(body)

  let response = mcp.handle(req, ctx)

  response.status |> should.equal(200)
  let assert wisp.Text(body_result) = response.body
  string.contains(body_result, "list_lexicons") |> should.be_true
}

pub fn handle_method_not_allowed_test() {
  let #(_db, ctx) = setup_test_ctx()

  let req = simulate.request(http.Get, "/mcp")

  let response = mcp.handle(req, ctx)

  response.status |> should.equal(405)
}

pub fn handle_tools_call_get_lexicon_test() {
  let #(exec, ctx) = setup_test_ctx()

  // Insert a test lexicon
  let lexicon_json =
    "{\"lexicon\":1,\"id\":\"test.example.status\",\"defs\":{\"main\":{\"type\":\"record\"}}}"
  let assert Ok(_) = lexicons.insert(exec, "test.example.status", lexicon_json)

  let body =
    "{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"params\":{\"name\":\"get_lexicon\",\"arguments\":{\"nsid\":\"test.example.status\"}},\"id\":3}"
  let req =
    simulate.request(http.Post, "/mcp")
    |> simulate.header("content-type", "application/json")
    |> simulate.string_body(body)

  let response = mcp.handle(req, ctx)

  response.status |> should.equal(200)
  let assert wisp.Text(body_result) = response.body
  string.contains(body_result, "test.example.status") |> should.be_true
}

pub fn handle_tools_call_execute_query_test() {
  let #(exec, ctx) = setup_test_ctx()

  // Insert a test lexicon so schema builds
  let lexicon_json =
    "{\"lexicon\":1,\"id\":\"test.example.status\",\"defs\":{\"main\":{\"type\":\"record\",\"key\":\"tid\",\"record\":{\"type\":\"object\",\"properties\":{\"text\":{\"type\":\"string\"}}}}}}"
  let assert Ok(_) = lexicons.insert(exec, "test.example.status", lexicon_json)

  let body =
    "{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"params\":{\"name\":\"execute_query\",\"arguments\":{\"query\":\"{ __typename }\"}},\"id\":4}"
  let req =
    simulate.request(http.Post, "/mcp")
    |> simulate.header("content-type", "application/json")
    |> simulate.string_body(body)

  let response = mcp.handle(req, ctx)

  response.status |> should.equal(200)
  let assert wisp.Text(body_result) = response.body
  string.contains(body_result, "Query") |> should.be_true
}
