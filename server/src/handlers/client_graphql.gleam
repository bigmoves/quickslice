/// GraphQL HTTP handler for client statistics and activity API
///
/// This handler serves the /admin/graphql endpoint which provides
/// stats and activity data to the client SPA using a separate schema
import backfill_state
import client_schema
import gleam/bit_array
import gleam/dict
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/http
import gleam/json
import gleam/list
import gleam/option
import jetstream_consumer
import lib/oauth/did_cache
import sqlight
import swell/executor
import swell/schema as swell_schema
import swell/value
import wisp

/// Handle GraphQL HTTP requests for client API
pub fn handle_client_graphql_request(
  req: wisp.Request,
  db: sqlight.Connection,
  jetstream_subject: option.Option(Subject(jetstream_consumer.ManagerMessage)),
  did_cache: Subject(did_cache.Message),
  oauth_supported_scopes: List(String),
  backfill_state_subject: Subject(backfill_state.Message),
) -> wisp.Response {
  case req.method {
    http.Post ->
      handle_post(
        req,
        db,
        jetstream_subject,
        did_cache,
        oauth_supported_scopes,
        backfill_state_subject,
      )
    http.Get ->
      handle_get(
        req,
        db,
        jetstream_subject,
        did_cache,
        oauth_supported_scopes,
        backfill_state_subject,
      )
    _ -> method_not_allowed_response()
  }
}

fn handle_post(
  req: wisp.Request,
  db: sqlight.Connection,
  jetstream_subject: option.Option(Subject(jetstream_consumer.ManagerMessage)),
  did_cache: Subject(did_cache.Message),
  oauth_supported_scopes: List(String),
  backfill_state_subject: Subject(backfill_state.Message),
) -> wisp.Response {
  case wisp.read_body_bits(req) {
    Ok(body) -> {
      case bit_array.to_string(body) {
        Ok(body_string) -> {
          case extract_query_and_variables_from_json(body_string) {
            Ok(#(query, variables)) ->
              execute_query(
                req,
                db,
                jetstream_subject,
                did_cache,
                oauth_supported_scopes,
                backfill_state_subject,
                query,
                variables,
              )
            Error(err) -> bad_request_response("Invalid JSON: " <> err)
          }
        }
        Error(_) -> bad_request_response("Request body must be valid UTF-8")
      }
    }
    Error(_) -> bad_request_response("Failed to read request body")
  }
}

fn handle_get(
  req: wisp.Request,
  db: sqlight.Connection,
  jetstream_subject: option.Option(Subject(jetstream_consumer.ManagerMessage)),
  did_cache: Subject(did_cache.Message),
  oauth_supported_scopes: List(String),
  backfill_state_subject: Subject(backfill_state.Message),
) -> wisp.Response {
  let query_params = wisp.get_query(req)
  case list.key_find(query_params, "query") {
    Ok(query) ->
      execute_query(
        req,
        db,
        jetstream_subject,
        did_cache,
        oauth_supported_scopes,
        backfill_state_subject,
        query,
        option.None,
      )
    Error(_) -> bad_request_response("Missing 'query' parameter")
  }
}

fn execute_query(
  req: wisp.Request,
  db: sqlight.Connection,
  jetstream_subject: option.Option(Subject(jetstream_consumer.ManagerMessage)),
  did_cache: Subject(did_cache.Message),
  oauth_supported_scopes: List(String),
  backfill_state_subject: Subject(backfill_state.Message),
  query: String,
  variables: option.Option(value.Value),
) -> wisp.Response {
  // Build the schema
  let graphql_schema =
    client_schema.build_schema(
      db,
      req,
      jetstream_subject,
      did_cache,
      oauth_supported_scopes,
      backfill_state_subject,
    )

  // Create context with variables
  let ctx = case variables {
    option.Some(value.Object(fields)) -> {
      // Convert list of tuples to dict
      let vars_dict = dict.from_list(fields)
      swell_schema.context_with_variables(option.None, vars_dict)
    }
    _ -> swell_schema.context(option.None)
  }

  // Execute the query
  case executor.execute(query, graphql_schema, ctx) {
    Ok(result) -> {
      // Convert executor response to JSON
      let response_json = case result.errors {
        [] -> {
          // Success with no errors
          json.object([#("data", value_to_json(result.data))])
        }
        errors -> {
          // Convert GraphQLError records to JSON
          let errors_json =
            json.array(errors, fn(err) {
              json.object([
                #("message", json.string(err.message)),
                #("path", json.array(err.path, json.string)),
              ])
            })

          // Partial success or errors
          json.object([
            #("data", value_to_json(result.data)),
            #("errors", errors_json),
          ])
        }
      }

      let json_string = json.to_string(response_json)
      success_response(json_string)
    }
    Error(err) -> internal_error_response(err)
  }
}

/// Convert a GraphQL Value to JSON
fn value_to_json(val: value.Value) -> json.Json {
  case val {
    value.Null -> json.null()
    value.Int(i) -> json.int(i)
    value.Float(f) -> json.float(f)
    value.String(s) -> json.string(s)
    value.Boolean(b) -> json.bool(b)
    value.Enum(e) -> json.string(e)
    value.List(items) -> json.array(items, value_to_json)
    value.Object(fields) ->
      json.object(
        list.map(fields, fn(field) { #(field.0, value_to_json(field.1)) }),
      )
  }
}

fn extract_query_and_variables_from_json(
  json_str: String,
) -> Result(#(String, option.Option(value.Value)), String) {
  // First just get the query
  let query_decoder = {
    use query <- decode.field("query", decode.string)
    decode.success(query)
  }

  case json.parse(json_str, query_decoder) {
    Ok(query) -> {
      // Try to parse variables separately (they're optional)
      let variables_decoder = {
        use vars <- decode.field("variables", decode.dynamic)
        decode.success(option.Some(vars))
      }

      let variables_value = case json.parse(json_str, variables_decoder) {
        Ok(option.Some(vars)) -> option.Some(dynamic_to_value(vars))
        _ -> option.None
      }

      Ok(#(query, variables_value))
    }
    Error(_) -> Error("Invalid JSON or missing 'query' field")
  }
}

/// Convert a Dynamic value to a GraphQL Value
/// For strings, we treat them as Enum values since GraphQL enums are sent as strings in JSON
fn dynamic_to_value(dyn: decode.Dynamic) -> value.Value {
  // Try to decode as different types
  case decode.run(dyn, decode.dict(decode.string, decode.dynamic)) {
    Ok(dict_value) -> {
      // It's an object
      let fields =
        dict_value
        |> dict.to_list
        |> list.map(fn(pair) {
          let #(key, val) = pair
          #(key, dynamic_to_value(val))
        })
      value.Object(fields)
    }
    Error(_) ->
      case decode.run(dyn, decode.list(decode.dynamic)) {
        Ok(list_value) -> {
          let items = list.map(list_value, dynamic_to_value)
          value.List(items)
        }
        Error(_) ->
          case decode.run(dyn, decode.int) {
            Ok(i) -> value.Int(i)
            Error(_) ->
              case decode.run(dyn, decode.float) {
                Ok(f) -> value.Float(f)
                Error(_) ->
                  case decode.run(dyn, decode.bool) {
                    Ok(b) -> value.Boolean(b)
                    Error(_) ->
                      case decode.run(dyn, decode.string) {
                        Ok(str) -> value.String(str)
                        Error(_) -> value.Null
                      }
                  }
              }
          }
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
