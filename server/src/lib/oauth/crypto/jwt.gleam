/// JWT signing utilities
import gleam/json
import gleam/result
import lib/oauth/token_generator

/// Create a client assertion JWT for token endpoint authentication
pub fn create_client_assertion(
  client_id: String,
  audience: String,
  signing_key: String,
) -> Result(String, String) {
  // Derive the kid (did:key) from the signing key to match JWKS
  use kid <- result.try(derive_kid_from_key(signing_key))

  let now = token_generator.current_timestamp()
  let exp = now + 300
  // 5 minutes

  let claims =
    json.object([
      #("iss", json.string(client_id)),
      #("sub", json.string(client_id)),
      #("aud", json.string(audience)),
      #("iat", json.int(now)),
      #("exp", json.int(exp)),
      #("jti", json.string(token_generator.generate_state())),
    ])

  let claims_json = json.to_string(claims)

  sign_jwt_internal(claims_json, kid, signing_key)
}

/// Derive the kid (did:key) from a multibase private key
@external(erlang, "jwt_ffi", "derive_public_did_key")
fn derive_kid_from_key(private_key: String) -> Result(String, String)

@external(erlang, "jwt_ffi", "sign_jwt")
fn sign_jwt_internal(
  claims_json: String,
  kid: String,
  private_key: String,
) -> Result(String, String)
