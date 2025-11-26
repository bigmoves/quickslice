/// ATProto-specific types
/// ATProto error types
pub type ATProtoError {
  DIDResolutionFailed(String)
  PDSNotFound(String)
  InvalidDIDDocument(String)
  HTTPError(String)
}
