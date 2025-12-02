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
                #(
                  "description",
                  json.string("Lexicon NSID (e.g., app.bsky.feed.post)"),
                ),
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
      description: "Execute a GraphQL query. IMPORTANT: Use introspect_schema first to discover exact field names and enum values. Sort fields use camelCase (e.g., createdAt, not CREATED_AT). Example: sortBy: [{ field: createdAt, direction: DESC }]",
      input_schema: json.object([
        #("type", json.string("object")),
        #(
          "properties",
          json.object([
            #(
              "query",
              json.object([
                #("type", json.string("string")),
                #(
                  "description",
                  json.string(
                    "GraphQL query string. Use introspect_schema to discover available types, fields, and enum values before querying.",
                  ),
                ),
              ]),
            ),
            #(
              "variables",
              json.object([
                #("type", json.string("object")),
                #(
                  "description",
                  json.string("Query variables as JSON (optional)"),
                ),
              ]),
            ),
          ]),
        ),
        #("required", json.array(["query"], json.string)),
      ]),
    ),
    Tool(
      name: "introspect_schema",
      description: "Get full GraphQL schema introspection. ALWAYS call this before execute_query to discover: available types, field names (camelCase), sort field values (camelCase like createdAt), and query arguments.",
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
