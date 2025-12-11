/// DPoP nonce endpoint handler
/// GET /oauth/dpop/nonce
import database/executor.{type Executor}
import database/repositories/oauth_dpop_nonces
import database/types.{OAuthDpopNonce}
import gleam/bit_array
import gleam/crypto
import gleam/json
import wisp

/// Nonce response
pub type NonceResponse {
  NonceResponse(nonce: String, expires_in: Int)
}

/// Handle GET /oauth/dpop/nonce
/// Returns a fresh DPoP nonce for use in subsequent requests
pub fn handle(conn: Executor) -> wisp.Response {
  // Generate a fresh nonce
  let nonce = generate_nonce()

  // Calculate expiration (5 minutes from now)
  let expires_in = 300
  let now = current_timestamp()
  let expires_at = now + expires_in

  // Store the nonce
  let nonce_record = OAuthDpopNonce(nonce: nonce, expires_at: expires_at)

  case oauth_dpop_nonces.insert(conn, nonce_record) {
    Ok(_) -> {
      let json_response =
        json.object([
          #("nonce", json.string(nonce)),
          #("expires_in", json.int(expires_in)),
        ])

      wisp.response(200)
      |> wisp.set_header("content-type", "application/json")
      |> wisp.set_header("cache-control", "no-store")
      |> wisp.set_body(wisp.Text(json.to_string(json_response)))
    }
    Error(_) -> {
      let error_response =
        json.object([
          #("error", json.string("server_error")),
          #("error_description", json.string("Failed to generate nonce")),
        ])

      wisp.response(500)
      |> wisp.set_header("content-type", "application/json")
      |> wisp.set_body(wisp.Text(json.to_string(error_response)))
    }
  }
}

/// Generate a cryptographically secure random nonce
fn generate_nonce() -> String {
  crypto.strong_random_bytes(32)
  |> bit_array.base64_url_encode(False)
}

/// Get current timestamp in seconds
fn current_timestamp() -> Int {
  get_system_time_native() / 1_000_000_000
}

@external(erlang, "os", "system_time")
fn get_system_time_native() -> Int
