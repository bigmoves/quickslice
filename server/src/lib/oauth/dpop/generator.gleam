/// DPoP proof generation
import gleam/option.{type Option}

/// Generate a DPoP proof JWT token with optional nonce
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
@external(erlang, "jose_ffi", "sha256_hash")
pub fn sha256_hash(data: String) -> String
