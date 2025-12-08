/// DPoP proof validation
/// Validates DPoP proofs according to RFC 9449
import gleam/option.{type Option, None, Some}

/// Result of successful DPoP validation
pub type DPoPValidationResult {
  DPoPValidationResult(
    /// JWK thumbprint (SHA-256) of the public key
    jkt: String,
    /// Unique identifier for replay protection
    jti: String,
    /// Issued-at timestamp
    iat: Int,
  )
}

/// Verify a DPoP proof JWT
///
/// # Arguments
/// * `dpop_proof` - The DPoP proof JWT from the DPoP header
/// * `method` - Expected HTTP method (e.g., "POST")
/// * `url` - Expected URL being accessed
/// * `max_age_seconds` - Maximum allowed age of the proof (typically 300 = 5 minutes)
///
/// # Returns
/// * `Ok(DPoPValidationResult)` - Validation succeeded
/// * `Error(String)` - Validation failed with reason
@external(erlang, "dpop_validator_ffi", "verify_dpop_proof")
pub fn verify_dpop_proof(
  dpop_proof: String,
  method: String,
  url: String,
  max_age_seconds: Int,
) -> Result(DPoPValidationResult, String)

/// Extract DPoP header from request headers
pub fn get_dpop_header(headers: List(#(String, String))) -> Option(String) {
  case headers {
    [] -> None
    [#(name, value), ..rest] -> {
      case name {
        "dpop" -> Some(value)
        "DPoP" -> Some(value)
        _ -> get_dpop_header(rest)
      }
    }
  }
}
