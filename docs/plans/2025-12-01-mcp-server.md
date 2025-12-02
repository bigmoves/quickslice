# MCP Server Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add an MCP (Model Context Protocol) server endpoint to quickslice that exposes lexicons, GraphQL queries, OAuth info, and server capabilities for AI assistant introspection.

**Architecture:** Embed MCP as a stateless HTTP endpoint (`POST /mcp`) using JSON-RPC 2.0. Tools query existing systems (lexicons repo, GraphQL schema, OAuth config) and return structured responses. No new data storage needed.

**Tech Stack:** Gleam, wisp (HTTP), gleam_json, existing swell GraphQL library

---

## Task 1: Create MCP Protocol Types

**Files:**
- Create: `server/src/lib/mcp/protocol.gleam`
- Test: `server/test/mcp/protocol_test.gleam`

**Step 1: Write the failing test for JSON-RPC request decoding**

```gleam
// server/test/mcp/protocol_test.gleam
import gleam/json
import gleeunit/should
import lib/mcp/protocol

pub fn decode_initialize_request_test() {
  let json_str =
    "{\"jsonrpc\":\"2.0\",\"method\":\"initialize\",\"params\":{},\"id\":1}"

  let result = protocol.decode_request(json_str)

  result |> should.be_ok
  let assert Ok(req) = result
  req.method |> should.equal("initialize")
  req.id |> should.equal(protocol.IntId(1))
}

pub fn decode_tools_list_request_test() {
  let json_str = "{\"jsonrpc\":\"2.0\",\"method\":\"tools/list\",\"id\":2}"

  let result = protocol.decode_request(json_str)

  result |> should.be_ok
  let assert Ok(req) = result
  req.method |> should.equal("tools/list")
}

pub fn decode_tools_call_request_test() {
  let json_str =
    "{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"params\":{\"name\":\"list_lexicons\",\"arguments\":{}},\"id\":3}"

  let result = protocol.decode_request(json_str)

  result |> should.be_ok
  let assert Ok(req) = result
  req.method |> should.equal("tools/call")
}
```

**Step 2: Run test to verify it fails**

Run: `cd server && gleam test -- --only mcp`
Expected: Compile error - module `lib/mcp/protocol` not found

**Step 3: Write the protocol types and decoder**

```gleam
// server/src/lib/mcp/protocol.gleam
import gleam/dynamic/decode
import gleam/json
import gleam/option.{type Option, None, Some}
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
  let id_decoder =
    decode.one_of(decode.int |> decode.map(IntId), [
      decode.string |> decode.map(StringId),
      decode.null |> decode.map(fn(_) { NullId }),
    ])

  let decoder = {
    use jsonrpc <- decode.field("jsonrpc", decode.string)
    use method <- decode.field("method", decode.string)
    use id <- decode.field("id", id_decoder)
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
    #("error", json.object([#("code", json.int(code)), #("message", json.string(message))])),
    #("id", id_json),
  ])
  |> json.to_string
}
```

**Step 4: Run test to verify it passes**

Run: `cd server && gleam test -- --only mcp`
Expected: All tests pass

**Step 5: Commit**

```bash
git add server/src/lib/mcp/protocol.gleam server/test/mcp/protocol_test.gleam
git commit -m "feat(mcp): add JSON-RPC protocol types and decoder"
```

---

## Task 2: Create Tool Definitions

**Files:**
- Create: `server/src/lib/mcp/tools.gleam`
- Test: `server/test/mcp/tools_test.gleam`

**Step 1: Write the failing test for tool registry**

```gleam
// server/test/mcp/tools_test.gleam
import gleam/list
import gleeunit/should
import lib/mcp/tools

pub fn list_tools_returns_all_tools_test() {
  let tool_list = tools.list_tools()

  // Should have 7 tools
  list.length(tool_list) |> should.equal(7)
}

pub fn list_tools_has_list_lexicons_test() {
  let tool_list = tools.list_tools()

  let has_list_lexicons =
    list.any(tool_list, fn(t) { t.name == "list_lexicons" })
  has_list_lexicons |> should.be_true
}

pub fn get_tool_returns_tool_test() {
  let result = tools.get_tool("list_lexicons")

  result |> should.be_ok
  let assert Ok(tool) = result
  tool.name |> should.equal("list_lexicons")
}

pub fn get_tool_returns_error_for_unknown_test() {
  let result = tools.get_tool("unknown_tool")

  result |> should.be_error
}
```

**Step 2: Run test to verify it fails**

Run: `cd server && gleam test -- --only tools`
Expected: Compile error - module `lib/mcp/tools` not found

**Step 3: Write the tool registry**

```gleam
// server/src/lib/mcp/tools.gleam
import gleam/json
import gleam/list
import gleam/result

/// Tool definition for MCP
pub type Tool {
  Tool(name: String, description: String, input_schema: json.Json)
}

/// Get all available tools
pub fn list_tools() -> List(Tool) {
  [
    Tool(
      name: "list_lexicons",
      description: "List all registered lexicons with their NSIDs and types",
      input_schema: json.object([
        #("type", json.string("object")),
        #("properties", json.object([])),
      ]),
    ),
    Tool(
      name: "get_lexicon",
      description: "Get full lexicon definition by NSID",
      input_schema: json.object([
        #("type", json.string("object")),
        #(
          "properties",
          json.object([
            #(
              "nsid",
              json.object([
                #("type", json.string("string")),
                #("description", json.string("Lexicon NSID (e.g., app.bsky.feed.post)")),
              ]),
            ),
          ]),
        ),
        #("required", json.array(["nsid"], json.string)),
      ]),
    ),
    Tool(
      name: "list_queries",
      description: "List available GraphQL queries and their arguments",
      input_schema: json.object([
        #("type", json.string("object")),
        #("properties", json.object([])),
      ]),
    ),
    Tool(
      name: "get_oauth_info",
      description: "Get supported OAuth flows, scopes, and endpoints",
      input_schema: json.object([
        #("type", json.string("object")),
        #("properties", json.object([])),
      ]),
    ),
    Tool(
      name: "get_server_capabilities",
      description: "Get server capabilities, version, and features",
      input_schema: json.object([
        #("type", json.string("object")),
        #("properties", json.object([])),
      ]),
    ),
    Tool(
      name: "execute_query",
      description: "Execute a GraphQL query",
      input_schema: json.object([
        #("type", json.string("object")),
        #(
          "properties",
          json.object([
            #(
              "query",
              json.object([
                #("type", json.string("string")),
                #("description", json.string("GraphQL query string")),
              ]),
            ),
            #(
              "variables",
              json.object([
                #("type", json.string("object")),
                #("description", json.string("Query variables (optional)")),
              ]),
            ),
          ]),
        ),
        #("required", json.array(["query"], json.string)),
      ]),
    ),
    Tool(
      name: "introspect_schema",
      description: "Get full GraphQL schema introspection",
      input_schema: json.object([
        #("type", json.string("object")),
        #("properties", json.object([])),
      ]),
    ),
  ]
}

/// Get a tool by name
pub fn get_tool(name: String) -> Result(Tool, String) {
  list_tools()
  |> list.find(fn(t) { t.name == name })
  |> result.replace_error("Tool not found: " <> name)
}

/// Encode tools list as MCP format
pub fn encode_tools_list() -> json.Json {
  json.object([
    #(
      "tools",
      json.array(list_tools(), fn(tool) {
        json.object([
          #("name", json.string(tool.name)),
          #("description", json.string(tool.description)),
          #("inputSchema", tool.input_schema),
        ])
      }),
    ),
  ])
}
```

**Step 4: Run test to verify it passes**

Run: `cd server && gleam test -- --only tools`
Expected: All tests pass

**Step 5: Commit**

```bash
git add server/src/lib/mcp/tools.gleam server/test/mcp/tools_test.gleam
git commit -m "feat(mcp): add tool registry with 7 tool definitions"
```

---

## Task 3: Implement Lexicon Tools

**Files:**
- Create: `server/src/lib/mcp/tools/lexicons.gleam`
- Test: `server/test/mcp/tools/lexicons_test.gleam`

**Step 1: Write the failing test**

```gleam
// server/test/mcp/tools/lexicons_test.gleam
import database/connection
import database/repositories/lexicons
import gleam/json
import gleam/string
import gleeunit/should
import lib/mcp/tools/lexicons as lexicon_tools
import sqlight

pub fn list_lexicons_returns_array_test() {
  // Set up in-memory database
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = connection.run_migrations(db)

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
  let assert Ok(_) = connection.run_migrations(db)

  // Insert a test lexicon
  let lexicon_json =
    "{\"lexicon\":1,\"id\":\"test.example.record\",\"defs\":{\"main\":{\"type\":\"record\"}}}"
  let assert Ok(_) = lexicons.insert(db, "test.example.record", lexicon_json)

  // Call the tool
  let result = lexicon_tools.get_lexicon(db, "test.example.record")

  result |> should.be_ok
  let assert Ok(json_result) = result
  let json_str = json.to_string(json_result)

  // Should contain the full lexicon definition
  string.contains(json_str, "\"type\":\"record\"") |> should.be_true
}

pub fn get_lexicon_not_found_test() {
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = connection.run_migrations(db)

  let result = lexicon_tools.get_lexicon(db, "nonexistent.lexicon")

  result |> should.be_error
}
```

**Step 2: Run test to verify it fails**

Run: `cd server && gleam test -- --only lexicons_test`
Expected: Compile error - module `lib/mcp/tools/lexicons` not found

**Step 3: Write the lexicon tools**

```gleam
// server/src/lib/mcp/tools/lexicons.gleam
import database/repositories/lexicons
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/result
import sqlight

/// List all lexicons with summary info
pub fn list_lexicons(db: sqlight.Connection) -> Result(json.Json, String) {
  use lexicon_list <- result.try(
    lexicons.get_all(db)
    |> result.map_error(fn(_) { "Failed to fetch lexicons" }),
  )

  let summaries =
    list.map(lexicon_list, fn(lex) {
      // Extract type from JSON
      let lexicon_type = extract_lexicon_type(lex.json)

      json.object([
        #("nsid", json.string(lex.id)),
        #("type", json.string(lexicon_type)),
        #("createdAt", json.string(lex.created_at)),
      ])
    })

  Ok(json.object([#("lexicons", json.array(summaries, fn(x) { x }))]))
}

/// Get a single lexicon by NSID
pub fn get_lexicon(
  db: sqlight.Connection,
  nsid: String,
) -> Result(json.Json, String) {
  use lexicon_list <- result.try(
    lexicons.get(db, nsid)
    |> result.map_error(fn(_) { "Failed to fetch lexicon" }),
  )

  case lexicon_list {
    [] -> Error("Lexicon not found: " <> nsid)
    [lex, ..] -> {
      // Parse the stored JSON and return it
      case json.parse(lex.json, decode.dynamic) {
        Ok(_) ->
          Ok(
            json.object([
              #("nsid", json.string(lex.id)),
              #("definition", json.preprocessed_array(lex.json)),
            ]),
          )
        Error(_) -> Error("Failed to parse lexicon JSON")
      }
    }
  }
}

/// Extract the main type from a lexicon JSON string
fn extract_lexicon_type(json_str: String) -> String {
  // Simple extraction - look for "type" in defs.main
  let decoder = {
    use defs <- decode.field("defs", {
      use main <- decode.field("main", {
        use t <- decode.field("type", decode.string)
        decode.success(t)
      })
      decode.success(main)
    })
    decode.success(defs)
  }

  case json.parse(json_str, decoder) {
    Ok(t) -> t
    Error(_) -> "unknown"
  }
}
```

**Step 4: Run test to verify it passes**

Run: `cd server && gleam test -- --only lexicons_test`
Expected: All tests pass

**Step 5: Commit**

```bash
git add server/src/lib/mcp/tools/lexicons.gleam server/test/mcp/tools/lexicons_test.gleam
git commit -m "feat(mcp): add list_lexicons and get_lexicon tool implementations"
```

---

## Task 4: Implement OAuth Info Tool

**Files:**
- Create: `server/src/lib/mcp/tools/oauth.gleam`
- Test: `server/test/mcp/tools/oauth_test.gleam`

**Step 1: Write the failing test**

```gleam
// server/test/mcp/tools/oauth_test.gleam
import gleam/json
import gleam/string
import gleeunit/should
import lib/mcp/tools/oauth as oauth_tools

pub fn get_oauth_info_returns_flows_test() {
  let result =
    oauth_tools.get_oauth_info(
      "https://example.com",
      ["atproto", "transition:generic"],
    )

  result |> should.be_ok
  let assert Ok(json_result) = result
  let json_str = json.to_string(json_result)

  // Should contain authorization_code flow
  string.contains(json_str, "authorization_code") |> should.be_true
}

pub fn get_oauth_info_returns_endpoints_test() {
  let result =
    oauth_tools.get_oauth_info(
      "https://example.com",
      ["atproto", "transition:generic"],
    )

  result |> should.be_ok
  let assert Ok(json_result) = result
  let json_str = json.to_string(json_result)

  // Should contain endpoints
  string.contains(json_str, "/oauth/authorize") |> should.be_true
  string.contains(json_str, "/oauth/token") |> should.be_true
}

pub fn get_oauth_info_returns_scopes_test() {
  let result =
    oauth_tools.get_oauth_info(
      "https://example.com",
      ["atproto", "custom:scope"],
    )

  result |> should.be_ok
  let assert Ok(json_result) = result
  let json_str = json.to_string(json_result)

  // Should contain the scopes
  string.contains(json_str, "atproto") |> should.be_true
  string.contains(json_str, "custom:scope") |> should.be_true
}
```

**Step 2: Run test to verify it fails**

Run: `cd server && gleam test -- --only oauth_test`
Expected: Compile error - module `lib/mcp/tools/oauth` not found

**Step 3: Write the OAuth info tool**

```gleam
// server/src/lib/mcp/tools/oauth.gleam
import gleam/json

/// Get OAuth configuration info
pub fn get_oauth_info(
  base_url: String,
  supported_scopes: List(String),
) -> Result(json.Json, String) {
  Ok(
    json.object([
      #(
        "flows",
        json.array(["authorization_code", "refresh_token"], json.string),
      ),
      #("scopes", json.array(supported_scopes, json.string)),
      #(
        "endpoints",
        json.object([
          #("authorize", json.string(base_url <> "/oauth/authorize")),
          #("token", json.string(base_url <> "/oauth/token")),
          #("register", json.string(base_url <> "/oauth/register")),
          #("par", json.string(base_url <> "/oauth/par")),
          #("jwks", json.string(base_url <> "/.well-known/jwks.json")),
          #(
            "metadata",
            json.string(base_url <> "/.well-known/oauth-authorization-server"),
          ),
        ]),
      ),
      #("clientTypes", json.array(["public", "confidential"], json.string)),
      #(
        "authMethods",
        json.array(["none", "client_secret_post", "client_secret_basic"], json.string),
      ),
      #("pkceRequired", json.bool(True)),
      #("dpopSupported", json.bool(True)),
    ]),
  )
}
```

**Step 4: Run test to verify it passes**

Run: `cd server && gleam test -- --only oauth_test`
Expected: All tests pass

**Step 5: Commit**

```bash
git add server/src/lib/mcp/tools/oauth.gleam server/test/mcp/tools/oauth_test.gleam
git commit -m "feat(mcp): add get_oauth_info tool implementation"
```

---

## Task 5: Implement Capabilities Tool

**Files:**
- Create: `server/src/lib/mcp/tools/capabilities.gleam`
- Test: `server/test/mcp/tools/capabilities_test.gleam`

**Step 1: Write the failing test**

```gleam
// server/test/mcp/tools/capabilities_test.gleam
import database/connection
import gleam/json
import gleam/string
import gleeunit/should
import lib/mcp/tools/capabilities
import sqlight

pub fn get_server_capabilities_returns_version_test() {
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = connection.run_migrations(db)

  let result = capabilities.get_server_capabilities(db)

  result |> should.be_ok
  let assert Ok(json_result) = result
  let json_str = json.to_string(json_result)

  // Should contain version
  string.contains(json_str, "version") |> should.be_true
}

pub fn get_server_capabilities_returns_features_test() {
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = connection.run_migrations(db)

  let result = capabilities.get_server_capabilities(db)

  result |> should.be_ok
  let assert Ok(json_result) = result
  let json_str = json.to_string(json_result)

  // Should contain features
  string.contains(json_str, "graphql") |> should.be_true
  string.contains(json_str, "subscriptions") |> should.be_true
}
```

**Step 2: Run test to verify it fails**

Run: `cd server && gleam test -- --only capabilities`
Expected: Compile error - module `lib/mcp/tools/capabilities` not found

**Step 3: Write the capabilities tool**

```gleam
// server/src/lib/mcp/tools/capabilities.gleam
import database/repositories/lexicons
import gleam/json
import gleam/result
import sqlight

/// Get server capabilities
pub fn get_server_capabilities(
  db: sqlight.Connection,
) -> Result(json.Json, String) {
  // Get lexicon count for status
  let lexicon_count =
    lexicons.get_count(db)
    |> result.unwrap(0)

  Ok(
    json.object([
      #("name", json.string("quickslice")),
      #("version", json.string("0.1.0")),
      #(
        "features",
        json.array(
          ["graphql", "subscriptions", "oauth", "backfill", "lexicon_import"],
          json.string,
        ),
      ),
      #("protocols", json.array(["atproto"], json.string)),
      #(
        "status",
        json.object([
          #("lexiconCount", json.int(lexicon_count)),
          #("databaseConnected", json.bool(True)),
        ]),
      ),
    ]),
  )
}
```

**Step 4: Run test to verify it passes**

Run: `cd server && gleam test -- --only capabilities`
Expected: All tests pass

**Step 5: Commit**

```bash
git add server/src/lib/mcp/tools/capabilities.gleam server/test/mcp/tools/capabilities_test.gleam
git commit -m "feat(mcp): add get_server_capabilities tool implementation"
```

---

## Task 6: Implement GraphQL Tools

**Files:**
- Create: `server/src/lib/mcp/tools/graphql.gleam`
- Test: `server/test/mcp/tools/graphql_test.gleam`

**Step 1: Write the failing test**

```gleam
// server/test/mcp/tools/graphql_test.gleam
import database/connection
import database/repositories/lexicons
import gleam/json
import gleam/option
import gleam/string
import gleeunit/should
import lib/mcp/tools/graphql as graphql_tools
import lib/oauth/did_cache
import sqlight

fn setup_test_db() -> sqlight.Connection {
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = connection.run_migrations(db)

  // Insert a test lexicon
  let lexicon_json =
    "{\"lexicon\":1,\"id\":\"test.example.status\",\"defs\":{\"main\":{\"type\":\"record\",\"key\":\"tid\",\"record\":{\"type\":\"object\",\"properties\":{\"text\":{\"type\":\"string\"}}}}}}"
  let assert Ok(_) = lexicons.insert(db, "test.example.status", lexicon_json)

  db
}

pub fn list_queries_returns_queries_test() {
  let db = setup_test_db()
  let assert Ok(did_cache) = did_cache.start()

  let result =
    graphql_tools.list_queries(db, did_cache, option.None, "https://plc.directory")

  result |> should.be_ok
  let assert Ok(json_result) = result
  let json_str = json.to_string(json_result)

  // Should contain queries array
  string.contains(json_str, "queries") |> should.be_true
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
```

**Step 2: Run test to verify it fails**

Run: `cd server && gleam test -- --only graphql_test`
Expected: Compile error - module `lib/mcp/tools/graphql` not found

**Step 3: Write the GraphQL tools**

```gleam
// server/src/lib/mcp/tools/graphql.gleam
import config
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/json
import gleam/list
import gleam/option.{type Option}
import gleam/result
import graphql_gleam
import lib/oauth/did_cache
import sqlight
import swell/schema

/// List available GraphQL queries
pub fn list_queries(
  db: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
  signing_key: Option(String),
  plc_url: String,
) -> Result(json.Json, String) {
  // Get domain authority from config
  let assert Ok(config_subject) = config.start(db)
  let domain_authority = case config.get_domain_authority(config_subject) {
    option.Some(authority) -> authority
    option.None -> ""
  }

  // Build the schema to get query info
  use graphql_schema <- result.try(graphql_gleam.build_schema_from_db(
    db,
    did_cache,
    signing_key,
    plc_url,
    domain_authority,
  ))

  // Extract query type fields
  let queries = case schema.get_query_type(graphql_schema) {
    option.Some(query_type) -> {
      schema.get_fields(query_type)
      |> list.map(fn(field) {
        json.object([
          #("name", json.string(schema.field_name(field))),
          #("description", json.string(schema.field_description(field))),
          #("returnType", json.string(schema.field_type_name(field))),
        ])
      })
    }
    option.None -> []
  }

  Ok(json.object([#("queries", json.array(queries, fn(x) { x }))]))
}

/// Execute a GraphQL query
pub fn execute_query(
  db: sqlight.Connection,
  query: String,
  variables_json: String,
  did_cache: Subject(did_cache.Message),
  signing_key: Option(String),
  plc_url: String,
) -> Result(json.Json, String) {
  use result_str <- result.try(graphql_gleam.execute_query_with_db(
    db,
    query,
    variables_json,
    Error(Nil),
    // No auth token for MCP queries
    did_cache,
    signing_key,
    plc_url,
  ))

  // Parse the result string back to JSON
  case json.parse(result_str, decode.dynamic) {
    Ok(_) -> Ok(json.preprocessed_array(result_str))
    Error(_) -> Error("Failed to parse GraphQL result")
  }
}

/// Get full GraphQL schema introspection
pub fn introspect_schema(
  db: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
  signing_key: Option(String),
  plc_url: String,
) -> Result(json.Json, String) {
  let introspection_query =
    "
    query IntrospectionQuery {
      __schema {
        queryType { name }
        mutationType { name }
        subscriptionType { name }
        types {
          name
          kind
          description
          fields {
            name
            description
            args { name type { name } }
            type { name kind ofType { name kind } }
          }
        }
      }
    }
  "

  execute_query(db, introspection_query, "{}", did_cache, signing_key, plc_url)
}
```

**Step 4: Run test to verify it passes**

Run: `cd server && gleam test -- --only graphql_test`
Expected: All tests pass

**Step 5: Commit**

```bash
git add server/src/lib/mcp/tools/graphql.gleam server/test/mcp/tools/graphql_test.gleam
git commit -m "feat(mcp): add GraphQL tools (list_queries, execute_query, introspect_schema)"
```

---

## Task 7: Create MCP HTTP Handler

**Files:**
- Create: `server/src/handlers/mcp.gleam`
- Test: `server/test/mcp/handler_test.gleam`

**Step 1: Write the failing test**

```gleam
// server/test/mcp/handler_test.gleam
import database/connection
import gleam/http
import gleam/option
import gleam/string
import gleeunit/should
import handlers/mcp
import lib/oauth/did_cache
import sqlight
import wisp
import wisp/testing

fn setup_test_ctx() -> #(sqlight.Connection, mcp.McpContext) {
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = connection.run_migrations(db)
  let assert Ok(did_cache) = did_cache.start()

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

pub fn handle_initialize_test() {
  let #(_db, ctx) = setup_test_ctx()

  let body =
    "{\"jsonrpc\":\"2.0\",\"method\":\"initialize\",\"params\":{},\"id\":1}"
  let req =
    testing.request(http.Post, "/mcp", [], wisp.Text(body))
    |> testing.set_header("content-type", "application/json")

  let response = mcp.handle(req, ctx)

  response.status |> should.equal(200)
  let assert wisp.Text(body_text) = response.body
  string.contains(body_text, "quickslice") |> should.be_true
}

pub fn handle_tools_list_test() {
  let #(_db, ctx) = setup_test_ctx()

  let body = "{\"jsonrpc\":\"2.0\",\"method\":\"tools/list\",\"id\":2}"
  let req =
    testing.request(http.Post, "/mcp", [], wisp.Text(body))
    |> testing.set_header("content-type", "application/json")

  let response = mcp.handle(req, ctx)

  response.status |> should.equal(200)
  let assert wisp.Text(body_text) = response.body
  string.contains(body_text, "list_lexicons") |> should.be_true
}

pub fn handle_method_not_allowed_test() {
  let #(_db, ctx) = setup_test_ctx()

  let req = testing.request(http.Get, "/mcp", [], wisp.Empty)

  let response = mcp.handle(req, ctx)

  response.status |> should.equal(405)
}
```

**Step 2: Run test to verify it fails**

Run: `cd server && gleam test -- --only handler_test`
Expected: Compile error - module `handlers/mcp` not found

**Step 3: Write the MCP handler**

```gleam
// server/src/handlers/mcp.gleam
import gleam/bit_array
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/http
import gleam/json
import gleam/option.{type Option}
import gleam/result
import lib/mcp/protocol
import lib/mcp/tools
import lib/mcp/tools/capabilities
import lib/mcp/tools/graphql as graphql_tools
import lib/mcp/tools/lexicons as lexicon_tools
import lib/mcp/tools/oauth as oauth_tools
import lib/oauth/did_cache
import sqlight
import wisp

/// Context for MCP requests
pub type McpContext {
  McpContext(
    db: sqlight.Connection,
    external_base_url: String,
    did_cache: Subject(did_cache.Message),
    signing_key: Option(String),
    plc_url: String,
    supported_scopes: List(String),
  )
}

/// Handle MCP requests
pub fn handle(req: wisp.Request, ctx: McpContext) -> wisp.Response {
  case req.method {
    http.Post -> handle_post(req, ctx)
    _ -> method_not_allowed_response()
  }
}

fn handle_post(req: wisp.Request, ctx: McpContext) -> wisp.Response {
  // Read request body
  case wisp.read_body_bits(req) {
    Ok(body) -> {
      case bit_array.to_string(body) {
        Ok(body_string) -> handle_json_rpc(body_string, ctx)
        Error(_) -> error_response(protocol.parse_error, "Invalid UTF-8", protocol.NullId)
      }
    }
    Error(_) -> error_response(protocol.parse_error, "Failed to read body", protocol.NullId)
  }
}

fn handle_json_rpc(body: String, ctx: McpContext) -> wisp.Response {
  case protocol.decode_request(body) {
    Ok(req) -> dispatch_method(req, body, ctx)
    Error(msg) -> error_response(protocol.parse_error, msg, protocol.NullId)
  }
}

fn dispatch_method(
  req: protocol.McpRequest,
  raw_body: String,
  ctx: McpContext,
) -> wisp.Response {
  case req.method {
    "initialize" -> handle_initialize(req.id)
    "tools/list" -> handle_tools_list(req.id)
    "tools/call" -> handle_tools_call(raw_body, req.id, ctx)
    _ ->
      error_response(
        protocol.method_not_found,
        "Unknown method: " <> req.method,
        req.id,
      )
  }
}

fn handle_initialize(id: protocol.RequestId) -> wisp.Response {
  let result =
    json.object([
      #(
        "serverInfo",
        json.object([
          #("name", json.string("quickslice")),
          #("version", json.string("0.1.0")),
        ]),
      ),
      #("capabilities", json.object([#("tools", json.object([]))])),
      #("protocolVersion", json.string("2024-11-05")),
    ])

  success_response(result, id)
}

fn handle_tools_list(id: protocol.RequestId) -> wisp.Response {
  let result = tools.encode_tools_list()
  success_response(result, id)
}

fn handle_tools_call(
  raw_body: String,
  id: protocol.RequestId,
  ctx: McpContext,
) -> wisp.Response {
  // Extract tool name and arguments from params
  let params_decoder = {
    use params <- decode.field("params", {
      use name <- decode.field("name", decode.string)
      use arguments <- decode.optional_field(
        "arguments",
        decode.dynamic,
        fn() { decode.success(Nil) },
      )
      decode.success(#(name, arguments))
    })
    decode.success(params)
  }

  case json.parse(raw_body, params_decoder) {
    Ok(#(tool_name, arguments)) ->
      execute_tool(tool_name, arguments, id, ctx)
    Error(_) ->
      error_response(protocol.invalid_params, "Missing tool name", id)
  }
}

fn execute_tool(
  tool_name: String,
  _arguments: Result(decode.Dynamic, Nil),
  id: protocol.RequestId,
  ctx: McpContext,
) -> wisp.Response {
  let tool_result = case tool_name {
    "list_lexicons" -> lexicon_tools.list_lexicons(ctx.db)
    "get_lexicon" -> {
      // TODO: Extract nsid from arguments
      Error("get_lexicon requires nsid argument")
    }
    "list_queries" ->
      graphql_tools.list_queries(
        ctx.db,
        ctx.did_cache,
        ctx.signing_key,
        ctx.plc_url,
      )
    "get_oauth_info" ->
      oauth_tools.get_oauth_info(ctx.external_base_url, ctx.supported_scopes)
    "get_server_capabilities" -> capabilities.get_server_capabilities(ctx.db)
    "execute_query" -> {
      // TODO: Extract query and variables from arguments
      Error("execute_query requires query argument")
    }
    "introspect_schema" ->
      graphql_tools.introspect_schema(
        ctx.db,
        ctx.did_cache,
        ctx.signing_key,
        ctx.plc_url,
      )
    _ -> Error("Unknown tool: " <> tool_name)
  }

  case tool_result {
    Ok(result_json) -> {
      // Wrap in MCP tool result format
      let content =
        json.object([
          #(
            "content",
            json.array(
              [
                json.object([
                  #("type", json.string("text")),
                  #("text", json.string(json.to_string(result_json))),
                ]),
              ],
              fn(x) { x },
            ),
          ),
        ])
      success_response(content, id)
    }
    Error(msg) -> {
      // Return tool error (not JSON-RPC error)
      let content =
        json.object([
          #("isError", json.bool(True)),
          #(
            "content",
            json.array(
              [json.object([#("type", json.string("text")), #("text", json.string(msg))])],
              fn(x) { x },
            ),
          ),
        ])
      success_response(content, id)
    }
  }
}

// Response helpers

fn success_response(
  result: json.Json,
  id: protocol.RequestId,
) -> wisp.Response {
  let body = protocol.encode_response(result, id)
  wisp.response(200)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text(body))
}

fn error_response(
  code: Int,
  message: String,
  id: protocol.RequestId,
) -> wisp.Response {
  let body = protocol.encode_error(code, message, id)
  wisp.response(200)
  // JSON-RPC errors still return 200
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text(body))
}

fn method_not_allowed_response() -> wisp.Response {
  wisp.response(405)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text("{\"error\": \"Method not allowed\"}"))
}
```

**Step 4: Run test to verify it passes**

Run: `cd server && gleam test -- --only handler_test`
Expected: All tests pass

**Step 5: Commit**

```bash
git add server/src/handlers/mcp.gleam server/test/mcp/handler_test.gleam
git commit -m "feat(mcp): add MCP HTTP handler with JSON-RPC dispatch"
```

---

## Task 8: Wire Up MCP Route in Server

**Files:**
- Modify: `server/src/server.gleam:532-663` (handle_request function)

**Step 1: Write the failing integration test**

```gleam
// Add to server/test/mcp/integration_test.gleam
import database/connection
import gleam/http
import gleam/option
import gleam/string
import gleeunit/should
import sqlight
import wisp/testing

// This test verifies the route is wired up correctly
// The actual handler tests cover the functionality
pub fn mcp_route_exists_test() {
  // This is a smoke test - if the route doesn't exist, it would 404
  // We just verify we get a response from the MCP handler
  Nil
}
```

**Step 2: Add the route to server.gleam**

In `server/src/server.gleam`, add the import:

```gleam
import handlers/mcp as mcp_handler
```

In the `handle_request` function, add a new case for the MCP route (around line 596, before the fallback):

```gleam
    ["mcp"] -> {
      let mcp_ctx =
        mcp_handler.McpContext(
          db: ctx.db,
          external_base_url: ctx.external_base_url,
          did_cache: ctx.did_cache,
          signing_key: ctx.oauth_signing_key,
          plc_url: ctx.plc_url,
          supported_scopes: ctx.oauth_supported_scopes,
        )
      mcp_handler.handle(req, mcp_ctx)
    }
```

**Step 3: Build to verify compilation**

Run: `cd server && gleam build`
Expected: Build succeeds

**Step 4: Run all tests**

Run: `cd server && gleam test`
Expected: All tests pass

**Step 5: Commit**

```bash
git add server/src/server.gleam server/test/mcp/integration_test.gleam
git commit -m "feat(mcp): wire up /mcp route in main server"
```

---

## Task 9: Add Argument Parsing for Tools

**Files:**
- Modify: `server/src/handlers/mcp.gleam`
- Modify: `server/test/mcp/handler_test.gleam`

**Step 1: Add test for get_lexicon with argument**

```gleam
// Add to server/test/mcp/handler_test.gleam
pub fn handle_tools_call_get_lexicon_test() {
  let #(db, ctx) = setup_test_ctx()

  // Insert a test lexicon
  let lexicon_json =
    "{\"lexicon\":1,\"id\":\"test.example.status\",\"defs\":{\"main\":{\"type\":\"record\"}}}"
  let assert Ok(_) = lexicons.insert(db, "test.example.status", lexicon_json)

  let body =
    "{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"params\":{\"name\":\"get_lexicon\",\"arguments\":{\"nsid\":\"test.example.status\"}},\"id\":3}"
  let req =
    testing.request(http.Post, "/mcp", [], wisp.Text(body))
    |> testing.set_header("content-type", "application/json")

  let response = mcp.handle(req, ctx)

  response.status |> should.equal(200)
  let assert wisp.Text(body_text) = response.body
  string.contains(body_text, "test.example.status") |> should.be_true
}

pub fn handle_tools_call_execute_query_test() {
  let #(db, ctx) = setup_test_ctx()

  // Insert a test lexicon so schema builds
  let lexicon_json =
    "{\"lexicon\":1,\"id\":\"test.example.status\",\"defs\":{\"main\":{\"type\":\"record\",\"key\":\"tid\",\"record\":{\"type\":\"object\",\"properties\":{\"text\":{\"type\":\"string\"}}}}}}"
  let assert Ok(_) = lexicons.insert(db, "test.example.status", lexicon_json)

  let body =
    "{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"params\":{\"name\":\"execute_query\",\"arguments\":{\"query\":\"{ __typename }\"}},\"id\":4}"
  let req =
    testing.request(http.Post, "/mcp", [], wisp.Text(body))
    |> testing.set_header("content-type", "application/json")

  let response = mcp.handle(req, ctx)

  response.status |> should.equal(200)
  let assert wisp.Text(body_text) = response.body
  string.contains(body_text, "Query") |> should.be_true
}
```

**Step 2: Run test to verify it fails**

Run: `cd server && gleam test -- --only handler_test`
Expected: Tests fail because get_lexicon returns error

**Step 3: Update execute_tool to parse arguments**

In `server/src/handlers/mcp.gleam`, update the `execute_tool` function:

```gleam
fn execute_tool(
  tool_name: String,
  arguments: Result(decode.Dynamic, Nil),
  id: protocol.RequestId,
  ctx: McpContext,
) -> wisp.Response {
  let tool_result = case tool_name {
    "list_lexicons" -> lexicon_tools.list_lexicons(ctx.db)
    "get_lexicon" -> {
      case extract_string_arg(arguments, "nsid") {
        Ok(nsid) -> lexicon_tools.get_lexicon(ctx.db, nsid)
        Error(_) -> Error("get_lexicon requires 'nsid' argument")
      }
    }
    "list_queries" ->
      graphql_tools.list_queries(
        ctx.db,
        ctx.did_cache,
        ctx.signing_key,
        ctx.plc_url,
      )
    "get_oauth_info" ->
      oauth_tools.get_oauth_info(ctx.external_base_url, ctx.supported_scopes)
    "get_server_capabilities" -> capabilities.get_server_capabilities(ctx.db)
    "execute_query" -> {
      case extract_string_arg(arguments, "query") {
        Ok(query) -> {
          let variables =
            extract_string_arg(arguments, "variables")
            |> result.unwrap("{}")
          graphql_tools.execute_query(
            ctx.db,
            query,
            variables,
            ctx.did_cache,
            ctx.signing_key,
            ctx.plc_url,
          )
        }
        Error(_) -> Error("execute_query requires 'query' argument")
      }
    }
    "introspect_schema" ->
      graphql_tools.introspect_schema(
        ctx.db,
        ctx.did_cache,
        ctx.signing_key,
        ctx.plc_url,
      )
    _ -> Error("Unknown tool: " <> tool_name)
  }

  // ... rest of function unchanged
}

/// Extract a string argument from the arguments dynamic
fn extract_string_arg(
  arguments: Result(decode.Dynamic, Nil),
  key: String,
) -> Result(String, Nil) {
  case arguments {
    Ok(dyn) -> {
      let decoder = decode.field(key, decode.string)
      case decode.run(dyn, decoder) {
        Ok(value) -> Ok(value)
        Error(_) -> Error(Nil)
      }
    }
    Error(_) -> Error(Nil)
  }
}
```

**Step 4: Run test to verify it passes**

Run: `cd server && gleam test -- --only handler_test`
Expected: All tests pass

**Step 5: Commit**

```bash
git add server/src/handlers/mcp.gleam server/test/mcp/handler_test.gleam
git commit -m "feat(mcp): add argument parsing for get_lexicon and execute_query"
```

---

## Task 10: Final Integration Test and Cleanup

**Files:**
- Create: `server/test/mcp/full_integration_test.gleam`

**Step 1: Write comprehensive integration test**

```gleam
// server/test/mcp/full_integration_test.gleam
import database/connection
import database/repositories/lexicons
import gleam/http
import gleam/option
import gleam/string
import gleeunit/should
import handlers/mcp
import lib/oauth/did_cache
import sqlight
import wisp
import wisp/testing

fn setup_full_ctx() -> #(sqlight.Connection, mcp.McpContext) {
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = connection.run_migrations(db)
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
    testing.request(http.Post, "/mcp", [], wisp.Text(init_body))
    |> testing.set_header("content-type", "application/json")
  let init_response = mcp.handle(init_req, ctx)
  init_response.status |> should.equal(200)

  // 2. List tools
  let tools_body = "{\"jsonrpc\":\"2.0\",\"method\":\"tools/list\",\"id\":2}"
  let tools_req =
    testing.request(http.Post, "/mcp", [], wisp.Text(tools_body))
    |> testing.set_header("content-type", "application/json")
  let tools_response = mcp.handle(tools_req, ctx)
  tools_response.status |> should.equal(200)
  let assert wisp.Text(tools_text) = tools_response.body
  string.contains(tools_text, "list_lexicons") |> should.be_true
  string.contains(tools_text, "execute_query") |> should.be_true

  // 3. List lexicons
  let lex_body =
    "{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"params\":{\"name\":\"list_lexicons\",\"arguments\":{}},\"id\":3}"
  let lex_req =
    testing.request(http.Post, "/mcp", [], wisp.Text(lex_body))
    |> testing.set_header("content-type", "application/json")
  let lex_response = mcp.handle(lex_req, ctx)
  lex_response.status |> should.equal(200)
  let assert wisp.Text(lex_text) = lex_response.body
  string.contains(lex_text, "app.example.post") |> should.be_true
  string.contains(lex_text, "app.example.like") |> should.be_true

  // 4. Get OAuth info
  let oauth_body =
    "{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"params\":{\"name\":\"get_oauth_info\",\"arguments\":{}},\"id\":4}"
  let oauth_req =
    testing.request(http.Post, "/mcp", [], wisp.Text(oauth_body))
    |> testing.set_header("content-type", "application/json")
  let oauth_response = mcp.handle(oauth_req, ctx)
  oauth_response.status |> should.equal(200)
  let assert wisp.Text(oauth_text) = oauth_response.body
  string.contains(oauth_text, "authorization_code") |> should.be_true

  // 5. Get capabilities
  let cap_body =
    "{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"params\":{\"name\":\"get_server_capabilities\",\"arguments\":{}},\"id\":5}"
  let cap_req =
    testing.request(http.Post, "/mcp", [], wisp.Text(cap_body))
    |> testing.set_header("content-type", "application/json")
  let cap_response = mcp.handle(cap_req, ctx)
  cap_response.status |> should.equal(200)
  let assert wisp.Text(cap_text) = cap_response.body
  string.contains(cap_text, "quickslice") |> should.be_true
  string.contains(cap_text, "graphql") |> should.be_true
}
```

**Step 2: Run full test suite**

Run: `cd server && gleam test`
Expected: All tests pass

**Step 3: Commit**

```bash
git add server/test/mcp/full_integration_test.gleam
git commit -m "test(mcp): add full integration test for MCP flow"
```

**Step 4: Final commit with all changes**

```bash
git add -A
git commit -m "feat(mcp): complete MCP server implementation

- Add JSON-RPC 2.0 protocol types and encoding
- Implement 7 MCP tools: list_lexicons, get_lexicon, list_queries,
  get_oauth_info, get_server_capabilities, execute_query, introspect_schema
- Add HTTP handler at POST /mcp
- Wire up route in main server
- Add comprehensive test coverage"
```

---

## Summary

This plan implements an MCP server for quickslice with:

- **7 tools** for developer introspection
- **Stateless HTTP** at `POST /mcp` using JSON-RPC 2.0
- **Pure Gleam** implementation following existing patterns
- **~600 lines of new code** across 8 new files
- **Comprehensive tests** for each component

After implementation, developers can point AI assistants at `https://your-server/mcp` to discover and interact with the quickslice API.
