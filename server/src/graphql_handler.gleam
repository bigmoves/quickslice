/// GraphQL HTTP request handler
///
/// Handles POST requests to /graphql endpoint, builds schemas from lexicons,
/// and executes GraphQL queries.
import gleam/bit_array
import gleam/dynamic/decode
import gleam/http
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import graphql_gleam
import sqlight
import wisp

/// Handle GraphQL HTTP requests
///
/// Expects POST requests with JSON body containing:
/// - query: GraphQL query string
///
/// Returns GraphQL query results as JSON
pub fn handle_graphql_request(
  req: wisp.Request,
  db: sqlight.Connection,
  auth_base_url: String,
) -> wisp.Response {
  case req.method {
    http.Post -> handle_graphql_post(req, db, auth_base_url)
    http.Get -> handle_graphql_get(req, db, auth_base_url)
    _ -> method_not_allowed_response()
  }
}

fn handle_graphql_post(
  req: wisp.Request,
  db: sqlight.Connection,
  auth_base_url: String,
) -> wisp.Response {
  // Extract Authorization header (optional for queries, required for mutations)
  // Strip "Bearer " prefix if present
  let auth_token =
    list.key_find(req.headers, "authorization")
    |> result.map(strip_bearer_prefix)

  // Read request body
  case wisp.read_body_bits(req) {
    Ok(body) -> {
      case bit_array.to_string(body) {
        Ok(body_string) -> {
          // Parse JSON to extract query and variables
          case extract_request_from_json(body_string) {
            Ok(#(query, variables)) -> {
              execute_graphql_query(db, query, variables, auth_token, auth_base_url)
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
  db: sqlight.Connection,
  auth_base_url: String,
) -> wisp.Response {
  // Extract Authorization header (optional for queries, required for mutations)
  // Strip "Bearer " prefix if present
  let auth_token =
    list.key_find(req.headers, "authorization")
    |> result.map(strip_bearer_prefix)

  // Support GET requests with query parameter (no variables for GET)
  let query_params = wisp.get_query(req)
  case list.key_find(query_params, "query") {
    Ok(query) ->
      execute_graphql_query(db, query, "{}", auth_token, auth_base_url)
    Error(_) -> bad_request_response("Missing 'query' parameter")
  }
}

fn execute_graphql_query(
  db: sqlight.Connection,
  query: String,
  variables_json_str: String,
  auth_token: Result(String, Nil),
  auth_base_url: String,
) -> wisp.Response {
  // Use the new pure Gleam GraphQL implementation
  case
    graphql_gleam.execute_query_with_db(
      db,
      query,
      variables_json_str,
      auth_token,
      auth_base_url,
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

/// Strip "Bearer " prefix from Authorization header value
fn strip_bearer_prefix(auth_header: String) -> String {
  case string.starts_with(auth_header, "Bearer ") {
    True -> string.drop_start(auth_header, 7)
    False -> auth_header
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
