/// DPoP (Demonstrating Proof-of-Possession) types
/// DPoP proof claims
pub type DPoPClaims {
  DPoPClaims(
    jti: String,
    htm: String,
    htu: String,
    iat: Int,
    ath: String,
    nonce: String,
  )
}
