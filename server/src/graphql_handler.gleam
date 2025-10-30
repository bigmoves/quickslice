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
  let auth_token = list.key_find(req.headers, "authorization")

  // Read request body
  case wisp.read_body_bits(req) {
    Ok(body) -> {
      case bit_array.to_string(body) {
        Ok(body_string) -> {
          // Parse JSON to extract query
          case extract_query_from_json(body_string) {
            Ok(query) -> execute_graphql_query(db, query, auth_token, auth_base_url)
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
  let auth_token = list.key_find(req.headers, "authorization")

  // Support GET requests with query parameter
  let query_params = wisp.get_query(req)
  case list.key_find(query_params, "query") {
    Ok(query) -> execute_graphql_query(db, query, auth_token, auth_base_url)
    Error(_) -> bad_request_response("Missing 'query' parameter")
  }
}

fn execute_graphql_query(
  db: sqlight.Connection,
  query: String,
  auth_token: Result(String, Nil),
  auth_base_url: String,
) -> wisp.Response {
  // Use the new pure Gleam GraphQL implementation
  case graphql_gleam.execute_query_with_db(db, query, auth_token, auth_base_url) {
    Ok(result_json) -> success_response(result_json)
    Error(err) -> internal_error_response(err)
  }
}

fn extract_query_from_json(json_str: String) -> Result(String, String) {
  // Use proper JSON decoder with gleam/json and gleam/dynamic/decode
  let decoder = {
    use query <- decode.field("query", decode.string)
    decode.success(query)
  }

  json.parse(json_str, decoder)
  |> result.map_error(fn(_) { "Invalid JSON or missing 'query' field" })
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
