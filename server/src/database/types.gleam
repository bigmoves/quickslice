// Shared database type definitions

import gleam/option.{type Option}

/// A record stored in the database
pub type Record {
  Record(
    uri: String,
    cid: String,
    did: String,
    collection: String,
    json: String,
    indexed_at: String,
    rkey: String,
  )
}

/// An actor (user) stored in the database
pub type Actor {
  Actor(did: String, handle: String, indexed_at: String)
}

/// A lexicon schema definition
pub type Lexicon {
  Lexicon(id: String, json: String, created_at: String)
}

/// Collection statistics
pub type CollectionStat {
  CollectionStat(collection: String, count: Int)
}

/// Result of inserting a record
pub type InsertResult {
  /// Record was newly inserted or updated
  Inserted
  /// Record was skipped (duplicate CID or unchanged)
  Skipped
}

/// Date interval for date truncation in aggregations
pub type DateInterval {
  Hour
  Day
  Week
  Month
}

/// A field to group by with optional date truncation
pub type GroupByField {
  SimpleField(field: String)
  TruncatedField(field: String, interval: DateInterval)
}

/// A jetstream activity log entry
pub type ActivityEntry {
  ActivityEntry(
    id: Int,
    timestamp: String,
    operation: String,
    collection: String,
    did: String,
    status: String,
    error_message: Option(String),
    event_json: String,
  )
}

/// Activity bucket for aggregated data
pub type ActivityBucket {
  ActivityBucket(
    timestamp: String,
    create_count: Int,
    update_count: Int,
    delete_count: Int,
  )
}

// ===== OAuth Enum Types =====

/// OAuth 2.0 grant types
pub type GrantType {
  AuthorizationCode
  RefreshToken
  ClientCredentials
  DeviceCode
}

/// OAuth 2.0 response types
pub type ResponseType {
  Code
}

/// Client authentication methods at token endpoint
pub type ClientAuthMethod {
  ClientSecretBasic
  ClientSecretPost
  PrivateKeyJwt
  AuthNone
}

/// Client type classification
pub type ClientType {
  Public
  Confidential
}

/// Token type
pub type TokenType {
  Bearer
  DPoP
}

/// PKCE code challenge method
pub type CodeChallengeMethod {
  S256
  Plain
}

// ===== OAuth Enum Conversion Functions =====

pub fn grant_type_to_string(gt: GrantType) -> String {
  case gt {
    AuthorizationCode -> "authorization_code"
    RefreshToken -> "refresh_token"
    ClientCredentials -> "client_credentials"
    DeviceCode -> "urn:ietf:params:oauth:grant-type:device_code"
  }
}

pub fn grant_type_from_string(s: String) -> Option(GrantType) {
  case s {
    "authorization_code" -> option.Some(AuthorizationCode)
    "refresh_token" -> option.Some(RefreshToken)
    "client_credentials" -> option.Some(ClientCredentials)
    "urn:ietf:params:oauth:grant-type:device_code" -> option.Some(DeviceCode)
    _ -> option.None
  }
}

pub fn response_type_to_string(rt: ResponseType) -> String {
  case rt {
    Code -> "code"
  }
}

pub fn response_type_from_string(s: String) -> Option(ResponseType) {
  case s {
    "code" -> option.Some(Code)
    _ -> option.None
  }
}

pub fn client_auth_method_to_string(method: ClientAuthMethod) -> String {
  case method {
    ClientSecretBasic -> "client_secret_basic"
    ClientSecretPost -> "client_secret_post"
    PrivateKeyJwt -> "private_key_jwt"
    AuthNone -> "none"
  }
}

pub fn client_auth_method_from_string(s: String) -> Option(ClientAuthMethod) {
  case s {
    "client_secret_basic" -> option.Some(ClientSecretBasic)
    "client_secret_post" -> option.Some(ClientSecretPost)
    "private_key_jwt" -> option.Some(PrivateKeyJwt)
    "none" -> option.Some(AuthNone)
    _ -> option.None
  }
}

pub fn client_type_to_string(ct: ClientType) -> String {
  case ct {
    Public -> "public"
    Confidential -> "confidential"
  }
}

pub fn client_type_from_string(s: String) -> Option(ClientType) {
  case s {
    "public" -> option.Some(Public)
    "confidential" -> option.Some(Confidential)
    _ -> option.None
  }
}

pub fn token_type_to_string(tt: TokenType) -> String {
  case tt {
    Bearer -> "Bearer"
    DPoP -> "DPoP"
  }
}

pub fn token_type_from_string(s: String) -> Option(TokenType) {
  case s {
    "Bearer" -> option.Some(Bearer)
    "DPoP" -> option.Some(DPoP)
    _ -> option.None
  }
}

pub fn code_challenge_method_to_string(method: CodeChallengeMethod) -> String {
  case method {
    S256 -> "S256"
    Plain -> "plain"
  }
}

pub fn code_challenge_method_from_string(
  s: String,
) -> Option(CodeChallengeMethod) {
  case s {
    "S256" -> option.Some(S256)
    "plain" -> option.Some(Plain)
    _ -> option.None
  }
}

// ===== OAuth Record Types =====

/// OAuth client registration
pub type OAuthClient {
  OAuthClient(
    client_id: String,
    client_secret: Option(String),
    client_name: String,
    redirect_uris: List(String),
    grant_types: List(GrantType),
    response_types: List(ResponseType),
    scope: Option(String),
    token_endpoint_auth_method: ClientAuthMethod,
    client_type: ClientType,
    created_at: Int,
    updated_at: Int,
    metadata: String,
    access_token_expiration: Int,
    refresh_token_expiration: Int,
    require_redirect_exact: Bool,
    registration_access_token: Option(String),
    jwks: Option(String),
  )
}

/// OAuth access token
pub type OAuthAccessToken {
  OAuthAccessToken(
    token: String,
    token_type: TokenType,
    client_id: String,
    user_id: Option(String),
    session_id: Option(String),
    session_iteration: Option(Int),
    scope: Option(String),
    created_at: Int,
    expires_at: Int,
    revoked: Bool,
    dpop_jkt: Option(String),
  )
}

/// OAuth refresh token
pub type OAuthRefreshToken {
  OAuthRefreshToken(
    token: String,
    access_token: String,
    client_id: String,
    user_id: String,
    session_id: Option(String),
    session_iteration: Option(Int),
    scope: Option(String),
    created_at: Int,
    expires_at: Option(Int),
    revoked: Bool,
  )
}

/// Pushed Authorization Request
pub type OAuthParRequest {
  OAuthParRequest(
    request_uri: String,
    authorization_request: String,
    client_id: String,
    created_at: Int,
    expires_at: Int,
    subject: Option(String),
    metadata: String,
  )
}

/// DPoP nonce
pub type OAuthDpopNonce {
  OAuthDpopNonce(nonce: String, expires_at: Int)
}

/// Client authorization request during bridge flow
pub type OAuthAuthRequest {
  OAuthAuthRequest(
    session_id: String,
    client_id: String,
    redirect_uri: String,
    scope: Option(String),
    state: Option(String),
    code_challenge: Option(String),
    code_challenge_method: Option(String),
    response_type: String,
    nonce: Option(String),
    login_hint: Option(String),
    created_at: Int,
    expires_at: Int,
  )
}

/// ATP bridge session state
pub type OAuthAtpSession {
  OAuthAtpSession(
    session_id: String,
    iteration: Int,
    did: Option(String),
    session_created_at: Int,
    atp_oauth_state: String,
    signing_key_jkt: String,
    dpop_key: String,
    access_token: Option(String),
    refresh_token: Option(String),
    access_token_created_at: Option(Int),
    access_token_expires_at: Option(Int),
    access_token_scopes: Option(String),
    session_exchanged_at: Option(Int),
    exchange_error: Option(String),
  )
}

/// Outbound OAuth request to ATP
pub type OAuthAtpRequest {
  OAuthAtpRequest(
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

/// OAuth authorization code
pub type OAuthAuthorizationCode {
  OAuthAuthorizationCode(
    code: String,
    client_id: String,
    user_id: String,
    session_id: Option(String),
    session_iteration: Option(Int),
    redirect_uri: String,
    scope: Option(String),
    code_challenge: Option(String),
    code_challenge_method: Option(CodeChallengeMethod),
    nonce: Option(String),
    created_at: Int,
    expires_at: Int,
    used: Bool,
  )
}
