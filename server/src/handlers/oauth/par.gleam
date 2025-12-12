/// Pushed Authorization Request (PAR) endpoint
/// POST /oauth/par
import database/executor.{type Executor}
import database/repositories/oauth_clients
import database/repositories/oauth_par_requests
import database/types.{OAuthParRequest}
import gleam/bit_array
import gleam/crypto
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/uri
import wisp

/// PAR response
pub type PARResponse {
  PARResponse(request_uri: String, expires_in: Int)
}

/// Handle POST /oauth/par
pub fn handle(req: wisp.Request, conn: Executor) -> wisp.Response {
  use body <- wisp.require_string_body(req)

  case process_par_request(body, conn) {
    Ok(response) -> {
      let json_response =
        json.object([
          #("request_uri", json.string(response.request_uri)),
          #("expires_in", json.int(response.expires_in)),
        ])

      wisp.response(201)
      |> wisp.set_header("content-type", "application/json")
      |> wisp.set_body(wisp.Text(json.to_string(json_response)))
    }
    Error(#(status, error, description)) -> {
      let json_response =
        json.object([
          #("error", json.string(error)),
          #("error_description", json.string(description)),
        ])

      wisp.response(status)
      |> wisp.set_header("content-type", "application/json")
      |> wisp.set_body(wisp.Text(json.to_string(json_response)))
    }
  }
}

fn process_par_request(
  body: String,
  conn: Executor,
) -> Result(PARResponse, #(Int, String, String)) {
  // Parse form data from POST body
  use params <- result.try(
    uri.parse_query(body)
    |> result.map_error(fn(_) {
      #(400, "invalid_request", "Failed to parse form data")
    }),
  )

  // Extract client_id (required)
  use client_id <- result.try(case get_param(params, "client_id") {
    Some(id) -> Ok(id)
    None -> Error(#(400, "invalid_request", "client_id is required"))
  })

  // Get client from storage
  use client_opt <- result.try(
    oauth_clients.get(conn, client_id)
    |> result.map_error(fn(_) {
      #(401, "invalid_client", "Failed to retrieve client")
    }),
  )

  use _client <- result.try(case client_opt {
    Some(c) -> Ok(c)
    None -> Error(#(401, "invalid_client", "Client not found"))
  })

  // Extract required parameters
  use response_type <- result.try(case get_param(params, "response_type") {
    Some(rt) -> Ok(rt)
    None -> Error(#(400, "invalid_request", "response_type is required"))
  })

  use redirect_uri <- result.try(case get_param(params, "redirect_uri") {
    Some(uri) -> Ok(uri)
    None -> Error(#(400, "invalid_request", "redirect_uri is required"))
  })

  // Build authorization request JSON
  let auth_request_json =
    json.to_string(
      json.object([
        #("response_type", json.string(response_type)),
        #("client_id", json.string(client_id)),
        #("redirect_uri", json.string(redirect_uri)),
        #("scope", case get_param(params, "scope") {
          Some(s) -> json.string(s)
          None -> json.null()
        }),
        #("state", case get_param(params, "state") {
          Some(s) -> json.string(s)
          None -> json.null()
        }),
        #("code_challenge", case get_param(params, "code_challenge") {
          Some(c) -> json.string(c)
          None -> json.null()
        }),
        #(
          "code_challenge_method",
          case get_param(params, "code_challenge_method") {
            Some(m) -> json.string(m)
            None -> json.null()
          },
        ),
        #("login_hint", case get_param(params, "login_hint") {
          Some(h) -> json.string(h)
          None -> json.null()
        }),
      ]),
    )

  // Generate PAR request
  let request_uri = generate_par_request_uri()
  let now = current_timestamp()
  let expires_in = 60
  let expires_at = now + expires_in

  let par =
    OAuthParRequest(
      request_uri: request_uri,
      authorization_request: auth_request_json,
      client_id: client_id,
      created_at: now,
      expires_at: expires_at,
      subject: None,
      metadata: "{}",
    )

  // Store PAR request
  use _ <- result.try(
    oauth_par_requests.insert(conn, par)
    |> result.map_error(fn(_) {
      #(500, "server_error", "Failed to store PAR request")
    }),
  )

  Ok(PARResponse(request_uri: request_uri, expires_in: expires_in))
}

fn get_param(params: List(#(String, String)), key: String) -> Option(String) {
  params
  |> list.find(fn(param) { param.0 == key })
  |> result.map(fn(param) { param.1 })
  |> option.from_result
}

fn generate_par_request_uri() -> String {
  "urn:ietf:params:oauth:request_uri:" <> random_string(32)
}

fn random_string(bytes: Int) -> String {
  crypto.strong_random_bytes(bytes)
  |> bit_array.base64_url_encode(False)
}

fn current_timestamp() -> Int {
  get_system_time_native() / 1_000_000_000
}

@external(erlang, "os", "system_time")
fn get_system_time_native() -> Int
