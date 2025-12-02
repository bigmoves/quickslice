import database/repositories/lexicons
import database/schema/migrations
import gleam/http
import gleam/option
import gleam/string
import gleeunit/should
import handlers/mcp
import lib/oauth/did_cache
import sqlight
import wisp
import wisp/simulate

fn setup_full_ctx() -> #(sqlight.Connection, mcp.McpContext) {
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = migrations.run_migrations(db)
  let assert Ok(did_cache) = did_cache.start()

  // Insert test lexicons
  let lexicon1 =
    "{\"lexicon\":1,\"id\":\"app.example.post\",\"defs\":{\"main\":{\"type\":\"record\",\"key\":\"tid\",\"record\":{\"type\":\"object\",\"properties\":{\"text\":{\"type\":\"string\"}}}}}}"
  let lexicon2 =
    "{\"lexicon\":1,\"id\":\"app.example.like\",\"defs\":{\"main\":{\"type\":\"record\",\"key\":\"tid\",\"record\":{\"type\":\"object\",\"properties\":{\"subject\":{\"type\":\"string\"}}}}}}"
  let assert Ok(_) = lexicons.insert(db, "app.example.post", lexicon1)
  let assert Ok(_) = lexicons.insert(db, "app.example.like", lexicon2)

  let ctx =
    mcp.McpContext(
      db: db,
      external_base_url: "https://example.com",
      did_cache: did_cache,
      signing_key: option.None,
      plc_url: "https://plc.directory",
      supported_scopes: ["atproto", "transition:generic"],
    )

  #(db, ctx)
}

pub fn full_mcp_flow_test() {
  let #(_db, ctx) = setup_full_ctx()

  // 1. Initialize
  let init_body =
    "{\"jsonrpc\":\"2.0\",\"method\":\"initialize\",\"params\":{},\"id\":1}"
  let init_req =
    simulate.request(http.Post, "/mcp")
    |> simulate.header("content-type", "application/json")
    |> simulate.string_body(init_body)
  let init_response = mcp.handle(init_req, ctx)
  init_response.status |> should.equal(200)

  // 2. List tools
  let tools_body = "{\"jsonrpc\":\"2.0\",\"method\":\"tools/list\",\"id\":2}"
  let tools_req =
    simulate.request(http.Post, "/mcp")
    |> simulate.header("content-type", "application/json")
    |> simulate.string_body(tools_body)
  let tools_response = mcp.handle(tools_req, ctx)
  tools_response.status |> should.equal(200)
  let assert wisp.Text(tools_text) = tools_response.body
  string.contains(tools_text, "list_lexicons") |> should.be_true
  string.contains(tools_text, "execute_query") |> should.be_true

  // 3. List lexicons
  let lex_body =
    "{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"params\":{\"name\":\"list_lexicons\",\"arguments\":{}},\"id\":3}"
  let lex_req =
    simulate.request(http.Post, "/mcp")
    |> simulate.header("content-type", "application/json")
    |> simulate.string_body(lex_body)
  let lex_response = mcp.handle(lex_req, ctx)
  lex_response.status |> should.equal(200)
  let assert wisp.Text(lex_text) = lex_response.body
  string.contains(lex_text, "app.example.post") |> should.be_true
  string.contains(lex_text, "app.example.like") |> should.be_true

  // 4. Get OAuth info
  let oauth_body =
    "{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"params\":{\"name\":\"get_oauth_info\",\"arguments\":{}},\"id\":4}"
  let oauth_req =
    simulate.request(http.Post, "/mcp")
    |> simulate.header("content-type", "application/json")
    |> simulate.string_body(oauth_body)
  let oauth_response = mcp.handle(oauth_req, ctx)
  oauth_response.status |> should.equal(200)
  let assert wisp.Text(oauth_text) = oauth_response.body
  string.contains(oauth_text, "authorization_code") |> should.be_true

  // 5. Get capabilities
  let cap_body =
    "{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"params\":{\"name\":\"get_server_capabilities\",\"arguments\":{}},\"id\":5}"
  let cap_req =
    simulate.request(http.Post, "/mcp")
    |> simulate.header("content-type", "application/json")
    |> simulate.string_body(cap_body)
  let cap_response = mcp.handle(cap_req, ctx)
  cap_response.status |> should.equal(200)
  let assert wisp.Text(cap_text) = cap_response.body
  string.contains(cap_text, "quickslice") |> should.be_true
  string.contains(cap_text, "graphql") |> should.be_true
}
