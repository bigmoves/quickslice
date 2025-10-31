/// Lexicon Type Mapper
///
/// Maps AT Protocol lexicon types to GraphQL types.
/// Simplified MVP version - handles basic types only.
///
/// Based on the Elixir implementation but adapted for the pure Gleam GraphQL library.
import graphql/schema
import lexicon_graphql/blob_type

/// Maps a lexicon type string to a GraphQL output Type.
///
/// ## Examples
///
/// ```gleam
/// map_type("string")   // schema.string_type()
/// map_type("integer")  // schema.int_type()
/// map_type("boolean")  // schema.boolean_type()
/// ```
pub fn map_type(lexicon_type: String) -> schema.Type {
  case lexicon_type {
    // Primitive types
    "string" -> schema.string_type()
    "integer" -> schema.int_type()
    "boolean" -> schema.boolean_type()
    "number" -> schema.float_type()

    // Binary/blob types
    "blob" -> blob_type.create_blob_type()
    "bytes" -> schema.string_type()
    "cid-link" -> schema.string_type()

    // Complex types - simplified to String for MVP
    "unknown" -> schema.string_type()
    "ref" -> schema.string_type()
    "union" -> schema.string_type()

    // Default fallback for any unknown type
    _ -> schema.string_type()
  }
}

/// Maps a lexicon type string to a GraphQL input Type.
/// For most types this is the same as output types, but for complex types
/// like blob, we need to return input object types instead.
///
/// ## Examples
///
/// ```gleam
/// map_input_type("string")   // schema.string_type()
/// map_input_type("blob")     // BlobInput input object type
/// ```
pub fn map_input_type(lexicon_type: String) -> schema.Type {
  case lexicon_type {
    // Primitive types (same as output)
    "string" -> schema.string_type()
    "integer" -> schema.int_type()
    "boolean" -> schema.boolean_type()
    "number" -> schema.float_type()

    // Binary/blob types - use input type for blob
    "blob" -> blob_type.create_blob_input_type()
    "bytes" -> schema.string_type()
    "cid-link" -> schema.string_type()

    // Complex types - simplified to String for MVP
    "unknown" -> schema.string_type()
    "ref" -> schema.string_type()
    "union" -> schema.string_type()

    // Default fallback for any unknown type
    _ -> schema.string_type()
  }
}

/// Get the Blob output type (for mutations and queries)
pub fn get_blob_type() -> schema.Type {
  blob_type.create_blob_type()
}
