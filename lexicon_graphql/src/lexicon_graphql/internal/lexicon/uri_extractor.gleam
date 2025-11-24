/// URI Extraction Utility
///
/// Extracts AT Protocol URIs from both strongRef objects and plain at-uri strings.
/// This is used at runtime to resolve forward joins by extracting the target URI
/// from a record's field value.
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/option.{type Option, None, Some}
import gleam/string

/// Extract a URI from a dynamic value
///
/// Handles two cases:
/// 1. strongRef object: { "$type": "com.atproto.repo.strongRef", "uri": "at://...", "cid": "..." }
/// 2. Plain at-uri string: "at://did:plc:abc123/collection/rkey"
///
/// Returns None if the value is not a valid URI or strongRef
pub fn extract_uri(value: Dynamic) -> Option(String) {
  // Try to decode as a string first (at-uri)
  case decode.run(value, decode.string) {
    Ok(uri_str) -> {
      case is_valid_at_uri(uri_str) {
        True -> Some(uri_str)
        False -> None
      }
    }
    Error(_) -> {
      // Not a string, try as strongRef object
      extract_from_strong_ref(value)
    }
  }
}

/// Extract URI from a strongRef object
/// strongRef format: { "$type": "com.atproto.repo.strongRef", "uri": "at://...", "cid": "..." }
fn extract_from_strong_ref(value: Dynamic) -> Option(String) {
  // Try to decode as an object with uri field
  let uri_decoder = {
    use uri <- decode.field("uri", decode.string)
    decode.success(uri)
  }

  case decode.run(value, uri_decoder) {
    Ok(uri) -> {
      case is_valid_at_uri(uri) {
        True -> Some(uri)
        False -> None
      }
    }
    Error(_) -> None
  }
}

/// Check if a string is a valid AT Protocol URI
/// AT URIs have the format: at://did/collection/rkey
fn is_valid_at_uri(uri: String) -> Bool {
  string.starts_with(uri, "at://")
}

/// Check if a value is a strongRef object
pub fn is_strong_ref(value: Dynamic) -> Bool {
  let type_decoder = {
    use type_str <- decode.field("$type", decode.string)
    decode.success(type_str)
  }

  case decode.run(value, type_decoder) {
    Ok(type_str) -> type_str == "com.atproto.repo.strongRef"
    Error(_) -> False
  }
}

/// Check if a value is a plain at-uri string
pub fn is_at_uri_string(value: Dynamic) -> Bool {
  case decode.run(value, decode.string) {
    Ok(uri_str) -> is_valid_at_uri(uri_str)
    Error(_) -> False
  }
}
