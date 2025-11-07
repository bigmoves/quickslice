/// Tests for Lexicon Type Mapper
///
/// Maps AT Protocol lexicon types to GraphQL types
import gleeunit/should
import lexicon_graphql/type_mapper
import swell/schema

pub fn map_string_type_test() {
  type_mapper.map_type("string")
  |> should.equal(schema.string_type())
}

pub fn map_integer_type_test() {
  type_mapper.map_type("integer")
  |> should.equal(schema.int_type())
}

pub fn map_boolean_type_test() {
  type_mapper.map_type("boolean")
  |> should.equal(schema.boolean_type())
}

pub fn map_number_type_test() {
  // Lexicon "number" maps to GraphQL Float
  type_mapper.map_type("number")
  |> should.equal(schema.float_type())
}

pub fn map_unknown_type_test() {
  // Unknown types map to String (JSON-serialized)
  type_mapper.map_type("unknown")
  |> should.equal(schema.string_type())
}

pub fn map_blob_type_test() {
  // Blob types map to Blob object type with ref, mimeType, size, and url fields
  let blob_type = type_mapper.map_type("blob")

  schema.type_name(blob_type)
  |> should.equal("Blob")
}

pub fn map_bytes_type_test() {
  // Bytes map to String (base64 encoded)
  type_mapper.map_type("bytes")
  |> should.equal(schema.string_type())
}

pub fn map_cid_link_type_test() {
  // CID links map to String
  type_mapper.map_type("cid-link")
  |> should.equal(schema.string_type())
}

pub fn map_ref_type_test() {
  // Ref types map to String for now (URI reference)
  type_mapper.map_type("ref")
  |> should.equal(schema.string_type())
}

pub fn map_union_type_test() {
  // Union types map to String for now
  type_mapper.map_type("union")
  |> should.equal(schema.string_type())
}

pub fn map_default_fallback_test() {
  // Any unknown type falls back to String
  type_mapper.map_type("somethingWeird")
  |> should.equal(schema.string_type())
}
