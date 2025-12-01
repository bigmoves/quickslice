/// Parse error types for OAuth scope parsing
pub type ParseError {
  InvalidScopeFormat(scope: String, reason: String)
  InvalidAction(action: String)
  InvalidAttribute(attribute: String)
  InvalidMimeType(mime: String)
  InvalidRpcScope(reason: String)
}

/// Convert parse error to user-facing string
pub fn to_string(error: ParseError) -> String {
  case error {
    InvalidScopeFormat(scope, reason) ->
      "Invalid scope format: '" <> scope <> "' - " <> reason
    InvalidAction(action) ->
      "Unknown action '"
      <> action
      <> "', expected: create, update, delete, read, manage"
    InvalidAttribute(attr) -> "Unknown attribute '" <> attr <> "'"
    InvalidMimeType(mime) ->
      "Invalid MIME type '" <> mime <> "', expected format: type/subtype"
    InvalidRpcScope(reason) -> "Invalid RPC scope: " <> reason
  }
}
