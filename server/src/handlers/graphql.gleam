/// GraphQL HTTP request handler
///
/// Handles POST requests to /graphql endpoint, builds schemas from lexicons,
/// and executes GraphQL queries.
///
/// Supports two authentication methods:
/// 1. Cookie + DPoP (primary for JS SDK v2+): Session cookie with DPoP proof
/// 2. Authorization header (fallback): For backward compatibility
import database/executor.{type Executor}
import gleam/bit_array
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/http
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import graphql/lexicon/schema as lexicon_schema
import lib/client_session
import lib/oauth/did_cache
import wisp

/// Handle GraphQL HTTP requests
///
/// Expects POST requests with JSON body containing:
/// - query: GraphQL query string
///
/// Returns GraphQL query results as JSON
pub fn handle_graphql_request(
  req: wisp.Request,
  db: Executor,
  did_cache: Subject(did_cache.Message),
  signing_key: option.Option(String),
  atp_client_id: String,
  plc_url: String,
) -> wisp.Response {
  case req.method {
    http.Post ->
      handle_graphql_post(
        req,
        db,
        did_cache,
        signing_key,
        atp_client_id,
        plc_url,
      )
    http.Get ->
      handle_graphql_get(
        req,
        db,
        did_cache,
        signing_key,
        atp_client_id,
        plc_url,
      )
    _ -> method_not_allowed_response()
  }
}

fn handle_graphql_post(
  req: wisp.Request,
  db: Executor,
  did_cache: Subject(did_cache.Message),
  signing_key: option.Option(String),
  atp_client_id: String,
  plc_url: String,
) -> wisp.Response {
  // Try to get auth token, checking cookie-based auth first, then Authorization header
  let auth_token = get_auth_token(req, db)

  // Read request body
  case wisp.read_body_bits(req) {
    Ok(body) -> {
      case bit_array.to_string(body) {
        Ok(body_string) -> {
          // Parse JSON to extract query and variables
          case extract_request_from_json(body_string) {
            Ok(#(query, variables)) -> {
              execute_graphql_query(
                db,
                query,
                variables,
                auth_token,
                did_cache,
                signing_key,
                atp_client_id,
                plc_url,
              )
            }
            Error(err) -> bad_request_response("Invalid JSON: " <> err)
          }
        }
        Error(_) -> bad_request_response("Request body must be valid UTF-8")
      }
    }
    Error(_) -> bad_request_response("Failed to read request body")
  }
}

fn handle_graphql_get(
  req: wisp.Request,
  db: Executor,
  did_cache: Subject(did_cache.Message),
  signing_key: option.Option(String),
  atp_client_id: String,
  plc_url: String,
) -> wisp.Response {
  // Try to get auth token, checking cookie-based auth first, then Authorization header
  let auth_token = get_auth_token(req, db)

  // Support GET requests with query parameter (no variables for GET)
  let query_params = wisp.get_query(req)
  case list.key_find(query_params, "query") {
    Ok(query) ->
      execute_graphql_query(
        db,
        query,
        "{}",
        auth_token,
        did_cache,
        signing_key,
        atp_client_id,
        plc_url,
      )
    Error(_) -> bad_request_response("Missing 'query' parameter")
  }
}

fn execute_graphql_query(
  db: Executor,
  query: String,
  variables_json_str: String,
  auth_token: Result(String, Nil),
  did_cache: Subject(did_cache.Message),
  signing_key: option.Option(String),
  atp_client_id: String,
  plc_url: String,
) -> wisp.Response {
  // Use the new pure Gleam GraphQL implementation
  case
    lexicon_schema.execute_query_with_db(
      db,
      query,
      variables_json_str,
      auth_token,
      did_cache,
      signing_key,
      atp_client_id,
      plc_url,
    )
  {
    Ok(result_json) -> success_response(result_json)
    Error(err) -> internal_error_response(err)
  }
}

fn extract_request_from_json(
  json_str: String,
) -> Result(#(String, String), String) {
  // Extract just the query for now - variables will be parsed from the original JSON
  let decoder = {
    use query <- decode.field("query", decode.string)
    decode.success(query)
  }

  use query <- result.try(
    json.parse(json_str, decoder)
    |> result.map_error(fn(_) { "Invalid JSON or missing 'query' field" }),
  )

  // Pass the original JSON string so the executor can extract variables
  Ok(#(query, json_str))
}

/// Get auth token from request, trying cookie-based auth first, then Authorization header
///
/// Auth methods checked in order:
/// 1. Cookie (quickslice_client_session) - for JS SDK v2+ with cookie auth
/// 2. Authorization header (DPoP or Bearer) - for backward compatibility
fn get_auth_token(req: wisp.Request, db: Executor) -> Result(String, Nil) {
  // First, try cookie-based authentication
  case client_session.get_session_access_token(req, db) {
    Ok(token) -> Ok(token)
    Error(_) -> {
      // Fall back to Authorization header
      list.key_find(req.headers, "authorization")
      |> result.map(strip_auth_prefix)
    }
  }
}

/// Strip "Bearer " or "DPoP " prefix from Authorization header value
fn strip_auth_prefix(auth_header: String) -> String {
  case string.starts_with(auth_header, "Bearer ") {
    True -> string.drop_start(auth_header, 7)
    False ->
      case string.starts_with(auth_header, "DPoP ") {
        True -> string.drop_start(auth_header, 5)
        False -> auth_header
      }
  }
}

// Response helpers

fn success_response(data: String) -> wisp.Response {
  wisp.response(200)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text(data))
}

fn bad_request_response(message: String) -> wisp.Response {
  wisp.response(400)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text(
    "{\"error\": \"BadRequest\", \"message\": \"" <> message <> "\"}",
  ))
}

fn internal_error_response(message: String) -> wisp.Response {
  wisp.response(500)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text(
    "{\"error\": \"InternalError\", \"message\": \"" <> message <> "\"}",
  ))
}

fn method_not_allowed_response() -> wisp.Response {
  wisp.response(405)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text(
    "{\"error\": \"MethodNotAllowed\", \"message\": \"Only POST and GET are allowed\"}",
  ))
}
