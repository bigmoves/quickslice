/// OAuth request types
import gleam/option.{type Option}

/// Authorization request parameters
pub type AuthorizationRequest {
  AuthorizationRequest(
    response_type: String,
    client_id: String,
    redirect_uri: String,
    scope: Option(String),
    state: Option(String),
    code_challenge: Option(String),
    code_challenge_method: Option(String),
    nonce: Option(String),
    login_hint: Option(String),
    request_uri: Option(String),
  )
}

/// OAuth request stored for ATP callback (PKCE verifier, DPoP key, etc.)
pub type OAuthRequest {
  OAuthRequest(
    oauth_state: String,
    authorization_server: String,
    nonce: String,
    pkce_verifier: String,
    signing_public_key: String,
    dpop_private_key: String,
    created_at: Int,
    expires_at: Int,
  )
}

/// Token request parameters
pub type TokenRequest {
  TokenRequest(
    grant_type: String,
    code: Option(String),
    redirect_uri: Option(String),
    client_id: Option(String),
    code_verifier: Option(String),
    refresh_token: Option(String),
    scope: Option(String),
  )
}

/// PAR (Pushed Authorization Request) response
pub type PARResponse {
  PARResponse(request_uri: String, expires_in: Int)
}
