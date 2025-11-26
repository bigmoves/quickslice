/// PKCE (Proof Key for Code Exchange) implementation
/// Reference: RFC 7636
import gleam/bit_array
import gleam/crypto

/// Generate a cryptographically random code verifier
/// Returns a 43-128 character URL-safe string
pub fn generate_code_verifier() -> String {
  let random_bytes = crypto.strong_random_bytes(32)
  bit_array.base64_url_encode(random_bytes, False)
}

/// Generate a code challenge from a code verifier using S256 method
/// SHA-256 hash of verifier, base64url encoded
pub fn generate_code_challenge(verifier: String) -> String {
  let verifier_bytes = bit_array.from_string(verifier)
  let hash = crypto.hash(crypto.Sha256, verifier_bytes)
  bit_array.base64_url_encode(hash, False)
}

/// Verify a code verifier against a code challenge
/// Returns True if the verifier matches the challenge using S256
pub fn verify_code_challenge(
  verifier: String,
  challenge: String,
  method: String,
) -> Bool {
  case method {
    "S256" -> {
      let computed_challenge = generate_code_challenge(verifier)
      computed_challenge == challenge
    }
    "plain" -> verifier == challenge
    _ -> False
  }
}
