import gleam/bit_array
import gleam/crypto

/// PKCE (Proof Key for Code Exchange) helpers for OAuth 2.0
/// Reference: RFC 7636 https://tools.ietf.org/html/rfc7636
/// Generate a random code verifier (43-128 characters)
/// Uses URL-safe characters: A-Z, a-z, 0-9, -, ., _, ~
pub fn generate_code_verifier() -> String {
  // Generate 32 random bytes (will produce 43 characters when base64url encoded)
  let random_bytes = crypto.strong_random_bytes(32)

  // Base64URL encode without padding to get a valid code verifier
  bit_array.base64_url_encode(random_bytes, False)
}

/// Generate a code challenge from a code verifier using S256 method
/// S256: code_challenge = BASE64URL(SHA256(ASCII(code_verifier)))
pub fn generate_code_challenge(code_verifier: String) -> String {
  // Convert string to bit array
  let verifier_bits = bit_array.from_string(code_verifier)

  // Hash with SHA256
  let hash = crypto.hash(crypto.Sha256, verifier_bits)

  // Base64URL encode without padding
  bit_array.base64_url_encode(hash, False)
}

/// Generate a random state parameter for OAuth flow
/// Used to prevent CSRF attacks
pub fn generate_state() -> String {
  // Generate 32 random bytes for the state
  let random_bytes = crypto.strong_random_bytes(32)

  // Base64URL encode without padding
  bit_array.base64_url_encode(random_bytes, False)
}

/// Verify that a code verifier matches a code challenge
/// This is used during token exchange to verify PKCE
pub fn verify_code_challenge(
  code_verifier: String,
  code_challenge: String,
) -> Bool {
  let computed_challenge = generate_code_challenge(code_verifier)
  computed_challenge == code_challenge
}
