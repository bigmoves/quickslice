/// DPoP key generation for ES256 (P-256) keys
import gleam/dynamic
import gleam/io

/// Generate a complete P-256 EC JWK for DPoP use (including private key)
/// Returns JWK as JSON string with kty, crv, x, y, d fields
pub fn generate_dpop_jwk() -> String {
  io.println("[keygen] Calling erlang_secp256r1_params()")
  let key_params = erlang_secp256r1_params()

  io.println("[keygen] Calling erlang_generate_key() with params")
  let ec_private_key = erlang_generate_key(key_params)

  io.println("[keygen] Calling erlang_ec_key_to_jwk_json()")
  let jwk_json = erlang_ec_key_to_jwk_json(ec_private_key)
  io.println("[keygen] JWK JSON generated successfully")
  jwk_json
}

@external(erlang, "public_key", "generate_key")
fn erlang_generate_key(params: dynamic.Dynamic) -> dynamic.Dynamic

@external(erlang, "dpop_keygen_ffi", "secp256r1_params")
fn erlang_secp256r1_params() -> dynamic.Dynamic

@external(erlang, "dpop_keygen_ffi", "ec_key_to_jwk_json")
fn erlang_ec_key_to_jwk_json(ec_key: dynamic.Dynamic) -> String
