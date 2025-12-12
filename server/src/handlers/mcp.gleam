import database/executor.{type Executor}
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
import wisp

/// Context for MCP requests
pub type McpContext {
  McpContext(
    db: Executor,
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
        Error(_) ->
          error_response(protocol.parse_error, "Invalid UTF-8", protocol.NullId)
      }
    }
    Error(_) ->
      error_response(
        protocol.parse_error,
        "Failed to read body",
        protocol.NullId,
      )
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
  // Extract tool name from params
  let name_decoder = {
    use params <- decode.field("params", {
      use name <- decode.field("name", decode.string)
      decode.success(name)
    })
    decode.success(params)
  }

  case json.parse(raw_body, name_decoder) {
    Ok(tool_name) -> execute_tool(tool_name, raw_body, id, ctx)
    Error(_) -> error_response(protocol.invalid_params, "Missing tool name", id)
  }
}

fn execute_tool(
  tool_name: String,
  raw_body: String,
  id: protocol.RequestId,
  ctx: McpContext,
) -> wisp.Response {
  let tool_result = case tool_name {
    "list_lexicons" -> lexicon_tools.list_lexicons(ctx.db)
    "get_lexicon" -> {
      case extract_string_arg(raw_body, "nsid") {
        Ok(nsid) -> lexicon_tools.get_lexicon(ctx.db, nsid)
        Error(_) -> Error("get_lexicon requires 'nsid' argument")
      }
    }
    "list_queries" -> {
      // Return a simple description for now
      Ok(
        json.object([
          #(
            "description",
            json.string(
              "Use introspect_schema to see available queries, or execute_query to run GraphQL queries",
            ),
          ),
        ]),
      )
    }
    "get_oauth_info" ->
      oauth_tools.get_oauth_info(ctx.external_base_url, ctx.supported_scopes)
    "get_server_capabilities" -> capabilities.get_server_capabilities(ctx.db)
    "execute_query" -> {
      case extract_string_arg(raw_body, "query") {
        Ok(query) -> {
          let variables =
            extract_string_arg(raw_body, "variables")
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
              [
                json.object([
                  #("type", json.string("text")),
                  #("text", json.string(msg)),
                ]),
              ],
              fn(x) { x },
            ),
          ),
        ])
      success_response(content, id)
    }
  }
}

// Response helpers

fn success_response(result: json.Json, id: protocol.RequestId) -> wisp.Response {
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

/// Extract a string argument from the raw JSON body
/// Looks for params.arguments.{key}
fn extract_string_arg(raw_body: String, key: String) -> Result(String, Nil) {
  let decoder = {
    use params <- decode.field("params", {
      use arguments <- decode.field("arguments", {
        use value <- decode.field(key, decode.string)
        decode.success(value)
      })
      decode.success(arguments)
    })
    decode.success(params)
  }

  case json.parse(raw_body, decoder) {
    Ok(value) -> Ok(value)
    Error(_) -> Error(Nil)
  }
}
