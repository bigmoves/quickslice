/// Secure token generation utilities
import gleam/bit_array
import gleam/crypto

/// Generate a secure random authorization code
pub fn generate_authorization_code() -> String {
  let random_bytes = crypto.strong_random_bytes(32)
  bit_array.base64_url_encode(random_bytes, False)
}

/// Generate a secure random access token
pub fn generate_access_token() -> String {
  let random_bytes = crypto.strong_random_bytes(32)
  bit_array.base64_url_encode(random_bytes, False)
}

/// Generate a secure random refresh token
pub fn generate_refresh_token() -> String {
  let random_bytes = crypto.strong_random_bytes(32)
  bit_array.base64_url_encode(random_bytes, False)
}

/// Generate a secure random client ID
pub fn generate_client_id() -> String {
  let random_bytes = crypto.strong_random_bytes(16)
  let encoded = bit_array.base64_url_encode(random_bytes, False)
  "client_" <> encoded
}

/// Generate a secure random client secret
pub fn generate_client_secret() -> String {
  let random_bytes = crypto.strong_random_bytes(32)
  bit_array.base64_url_encode(random_bytes, False)
}

/// Generate a PAR request URI
pub fn generate_par_request_uri() -> String {
  let random_bytes = crypto.strong_random_bytes(16)
  let encoded = bit_array.base64_url_encode(random_bytes, False)
  "urn:ietf:params:oauth:request_uri:" <> encoded
}

/// Generate a DPoP nonce
pub fn generate_dpop_nonce() -> String {
  let random_bytes = crypto.strong_random_bytes(16)
  bit_array.base64_url_encode(random_bytes, False)
}

/// Generate a session ID
pub fn generate_session_id() -> String {
  let random_bytes = crypto.strong_random_bytes(16)
  bit_array.base64_url_encode(random_bytes, False)
}

/// Generate an OAuth state parameter
pub fn generate_state() -> String {
  let random_bytes = crypto.strong_random_bytes(16)
  bit_array.base64_url_encode(random_bytes, False)
}

/// Compute JWK thumbprint (JKT) from a signing key
pub fn compute_jkt(key: String) -> String {
  let key_bytes = bit_array.from_string(key)
  let hash = crypto.hash(crypto.Sha256, key_bytes)
  bit_array.base64_url_encode(hash, False)
}

/// Get current timestamp (seconds since epoch)
pub fn current_timestamp() -> Int {
  get_system_time_native() / 1_000_000_000
}

/// Calculate expiration timestamp
pub fn expiration_timestamp(lifetime_seconds: Int) -> Int {
  current_timestamp() + lifetime_seconds
}

/// Check if a timestamp is expired
pub fn is_expired(expires_at: Int) -> Bool {
  current_timestamp() >= expires_at
}

@external(erlang, "os", "system_time")
fn get_system_time_native() -> Int
