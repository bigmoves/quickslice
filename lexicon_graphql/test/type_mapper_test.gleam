/// Tests for Lexicon Type Mapper
///
/// Maps AT Protocol lexicon types to GraphQL types
import gleam/dict
import gleam/option
import gleeunit/should
import lexicon_graphql/internal/graphql/type_mapper
import lexicon_graphql/types
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

// Array type mapping tests

pub fn map_array_of_strings_test() {
  let items =
    types.ArrayItems(type_: "string", ref: option.None, refs: option.None)
  let result =
    type_mapper.map_array_type(
      option.Some(items),
      dict.new(),
      "TestType",
      "testField",
    )

  // Should be [String!] - list of non-null strings
  schema.type_name(result)
  |> should.equal("[String!]")
}

pub fn map_array_of_integers_test() {
  let items =
    types.ArrayItems(type_: "integer", ref: option.None, refs: option.None)
  let result =
    type_mapper.map_array_type(
      option.Some(items),
      dict.new(),
      "TestType",
      "testField",
    )

  schema.type_name(result)
  |> should.equal("[Int!]")
}

pub fn map_array_without_items_test() {
  let result =
    type_mapper.map_array_type(option.None, dict.new(), "TestType", "testField")

  // Should fallback to [String!]
  schema.type_name(result)
  |> should.equal("[String!]")
}

pub fn ref_to_type_name_test() {
  type_mapper.ref_to_type_name("fm.teal.alpha.feed.defs#artist")
  |> should.equal("FmTealAlphaFeedDefsArtist")
}

pub fn ref_to_type_name_simple_test() {
  type_mapper.ref_to_type_name("app.bsky.feed.post")
  |> should.equal("AppBskyFeedPost")
}

// map_property_type tests

pub fn map_property_type_array_test() {
  let items =
    types.ArrayItems(type_: "string", ref: option.None, refs: option.None)
  let property =
    types.Property(
      type_: "array",
      required: True,
      format: option.None,
      ref: option.None,
      refs: option.None,
      items: option.Some(items),
    )

  let result = type_mapper.map_property_type(property, dict.new())

  schema.type_name(result)
  |> should.equal("[String!]")
}

pub fn map_property_type_string_test() {
  let property =
    types.Property(
      type_: "string",
      required: True,
      format: option.None,
      ref: option.None,
      refs: option.None,
      items: option.None,
    )

  let result = type_mapper.map_property_type(property, dict.new())

  result
  |> should.equal(schema.string_type())
}

pub fn map_property_union_type_test() {
  // Create object types that the union will reference
  let images_type = schema.object_type("AppBskyEmbedImages", "Images embed", [])
  let video_type = schema.object_type("AppBskyEmbedVideo", "Video embed", [])

  let object_types =
    dict.new()
    |> dict.insert("app.bsky.embed.images", images_type)
    |> dict.insert("app.bsky.embed.video", video_type)

  let property =
    types.Property(
      type_: "union",
      required: False,
      format: option.None,
      ref: option.None,
      refs: option.Some([
        "app.bsky.embed.images",
        "app.bsky.embed.video",
      ]),
      items: option.None,
    )

  let result =
    type_mapper.map_property_type_with_context(
      property,
      object_types,
      "AppBskyFeedPost",
      "embed",
      "app.bsky.feed.post",
    )

  // Should be a union type, not String
  schema.type_name(result)
  |> should.equal("AppBskyFeedPostEmbed")
}
