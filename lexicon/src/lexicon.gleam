/// Lexicon validation library using Rust's slices-lexicon via NIF
///
/// This module provides type-safe wrappers around the Rust NIF implementation
/// for validating AT Protocol Lexicon schemas and data records.
/// Result type for validation errors
pub type ValidationError {
  ParseError(message: String)
  ValidationError(message: String)
}

/// Validates multiple lexicon schema documents
///
/// Takes a list of JSON strings representing lexicon schemas and validates them.
/// Returns Ok(Nil) if all schemas are valid, or Error with validation details.
///
/// ## Example
///
/// ```gleam
/// let schemas = [schema_json1, schema_json2]
/// case validate_schemas(schemas) {
///   Ok(Nil) -> io.println("All schemas valid!")
///   Error(err) -> io.println("Validation failed: " <> describe_error(err))
/// }
/// ```
pub fn validate_schemas(
  json_strings: List(String),
) -> Result(Nil, ValidationError) {
  case do_validate_schemas(json_strings) {
    Ok(_) -> Ok(Nil)
    Error(msg) -> Error(ValidationError(msg))
  }
}

/// Validates a single data record against lexicon schemas
///
/// Takes lexicon schemas, a collection name, and the record JSON to validate.
///
/// ## Example
///
/// ```gleam
/// let schemas = [schema_json]
/// case validate_record(schemas, "app.bsky.feed.post", record_json) {
///   Ok(Nil) -> io.println("Record is valid!")
///   Error(err) -> io.println("Invalid record: " <> describe_error(err))
/// }
/// ```
pub fn validate_record(
  lexicon_jsons: List(String),
  collection: String,
  record_json: String,
) -> Result(Nil, ValidationError) {
  case do_validate_record(lexicon_jsons, collection, record_json) {
    Ok(_) -> Ok(Nil)
    Error(msg) -> Error(ValidationError(msg))
  }
}

/// Checks if a string is a valid NSID (Namespaced Identifier)
///
/// NSIDs are used throughout the AT Protocol to identify lexicons, collections,
/// and other namespaced resources.
///
/// ## Example
///
/// ```gleam
/// is_valid_nsid("com.atproto.repo.createRecord")  // True
/// is_valid_nsid("invalid nsid")                    // False
/// ```
pub fn is_valid_nsid(nsid: String) -> Bool {
  do_is_valid_nsid(nsid)
}

/// Converts a ValidationError to a human-readable string
pub fn describe_error(error: ValidationError) -> String {
  case error {
    ParseError(msg) -> "Parse error: " <> msg
    ValidationError(msg) -> "Validation error: " <> msg
  }
}

// External NIF function declarations

@external(erlang, "lexicon_nif", "validate_schemas")
fn do_validate_schemas(json_strings: List(String)) -> Result(String, String)

@external(erlang, "lexicon_nif", "validate_record")
fn do_validate_record(
  lexicon_jsons: List(String),
  collection: String,
  record_json: String,
) -> Result(String, String)

@external(erlang, "lexicon_nif", "is_valid_nsid")
fn do_is_valid_nsid(nsid: String) -> Bool
