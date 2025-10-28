/// Gleam wrapper for JOSE (JSON Object Signing and Encryption) library
/// Provides functions for DPoP proof generation using Erlang's jose library
import gleam/option.{type Option}

/// Generate a DPoP proof JWT token with optional nonce
///
/// Creates a signed JWT token for DPoP (Demonstrating Proof-of-Possession) authentication.
/// The token includes:
/// - jti: Unique nonce for this proof
/// - htm: HTTP method (GET, POST, etc.)
/// - htu: HTTP URI being accessed
/// - iat: Timestamp when proof was created
/// - ath: Base64url-encoded SHA-256 hash of the access token
/// - nonce: Optional DPoP nonce from server (if provided)
///
/// # Arguments
/// * `method` - HTTP method (e.g., "POST", "GET")
/// * `url` - Full URL being accessed
/// * `access_token` - OAuth access token
/// * `jwk_json` - JSON Web Key as a JSON string
/// * `nonce` - Optional nonce from server's DPoP-Nonce header
///
/// # Returns
/// * `Ok(String)` - The DPoP proof token (compact JWT format)
/// * `Error(String)` - Error message if generation fails
pub fn generate_dpop_proof_with_nonce(
  method: String,
  url: String,
  access_token: String,
  jwk_json: String,
  nonce: Option(String),
) -> Result(String, String) {
  case nonce {
    option.Some(n) ->
      generate_dpop_proof_internal(method, url, access_token, jwk_json, n)
    option.None ->
      generate_dpop_proof_internal(method, url, access_token, jwk_json, "")
  }
}

@external(erlang, "jose_ffi", "generate_dpop_proof")
fn generate_dpop_proof_internal(
  method: String,
  url: String,
  access_token: String,
  jwk_json: String,
  nonce: String,
) -> Result(String, String)

/// Hash a string using SHA-256
///
/// # Arguments
/// * `data` - The string to hash
///
/// # Returns
/// Base64-encoded SHA-256 hash
@external(erlang, "jose_ffi", "sha256_hash")
pub fn sha256_hash(data: String) -> String
