/// OAuth error types
/// Reference: RFC 6749 Section 5.2
pub type OAuthError {
  /// Invalid request parameters
  InvalidRequest(String)
  /// Client authentication failed
  InvalidClient(String)
  /// Invalid or expired authorization grant
  InvalidGrant(String)
  /// Client not authorized to use this grant type
  UnauthorizedClient(String)
  /// Grant type not supported
  UnsupportedGrantType(String)
  /// Requested scope not allowed
  InvalidScope(String)
  /// Response type not supported
  UnsupportedResponseType(String)
  /// Server error
  ServerError(String)
  /// Service temporarily unavailable
  TemporarilyUnavailable(String)
  /// Access denied by resource owner
  AccessDenied(String)
}

pub type StorageError {
  DatabaseError(String)
  NotFound(String)
  DuplicateKey(String)
}

/// Convert OAuth error to standard error code
pub fn error_code(error: OAuthError) -> String {
  case error {
    InvalidRequest(_) -> "invalid_request"
    InvalidClient(_) -> "invalid_client"
    InvalidGrant(_) -> "invalid_grant"
    UnauthorizedClient(_) -> "unauthorized_client"
    UnsupportedGrantType(_) -> "unsupported_grant_type"
    InvalidScope(_) -> "invalid_scope"
    UnsupportedResponseType(_) -> "unsupported_response_type"
    ServerError(_) -> "server_error"
    TemporarilyUnavailable(_) -> "temporarily_unavailable"
    AccessDenied(_) -> "access_denied"
  }
}

/// Get error description
pub fn error_description(error: OAuthError) -> String {
  case error {
    InvalidRequest(msg) -> msg
    InvalidClient(msg) -> msg
    InvalidGrant(msg) -> msg
    UnauthorizedClient(msg) -> msg
    UnsupportedGrantType(msg) -> msg
    InvalidScope(msg) -> msg
    UnsupportedResponseType(msg) -> msg
    ServerError(msg) -> msg
    TemporarilyUnavailable(msg) -> msg
    AccessDenied(msg) -> msg
  }
}
