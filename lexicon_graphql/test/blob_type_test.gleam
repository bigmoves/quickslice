/// Tests for Blob Type
///
/// Tests the Blob GraphQL object type with ref, mimeType, size, and url fields
import gleam/dict
import gleam/option.{Some}
import gleeunit/should
import graphql/schema
import graphql/value
import lexicon_graphql/blob_type

pub fn create_blob_type_test() {
  let blob_type = blob_type.create_blob_type()

  // Verify it's an object type named "Blob"
  schema.type_name(blob_type)
  |> should.equal("Blob")
}

pub fn blob_type_has_ref_field_test() {
  let blob_type = blob_type.create_blob_type()

  // Verify the type has a "ref" field
  blob_type
  |> blob_type.has_field("ref")
  |> should.be_true()
}

pub fn blob_type_has_mime_type_field_test() {
  let blob_type = blob_type.create_blob_type()

  // Verify the type has a "mimeType" field
  blob_type
  |> blob_type.has_field("mimeType")
  |> should.be_true()
}

pub fn blob_type_has_size_field_test() {
  let blob_type = blob_type.create_blob_type()

  // Verify the type has a "size" field
  blob_type
  |> blob_type.has_field("size")
  |> should.be_true()
}

pub fn blob_type_has_url_field_test() {
  let blob_type = blob_type.create_blob_type()

  // Verify the type has a "url" field
  blob_type
  |> blob_type.has_field("url")
  |> should.be_true()
}

pub fn blob_ref_field_resolver_test() {
  // Test that the ref field resolver returns the correct value
  let blob_data =
    value.Object([
      #("ref", value.String("bafyreiabc123")),
      #("mime_type", value.String("image/jpeg")),
      #("size", value.Int(12_345)),
      #("did", value.String("did:plc:test123")),
    ])

  let ctx = schema.Context(Some(blob_data), dict.new(), dict.new())
  let result = blob_type.resolve_ref(ctx)

  result
  |> should.be_ok()
  |> should.equal(value.String("bafyreiabc123"))
}

pub fn blob_mime_type_field_resolver_test() {
  // Test that the mimeType field resolver returns the correct value
  let blob_data =
    value.Object([
      #("ref", value.String("bafyreiabc123")),
      #("mime_type", value.String("image/png")),
      #("size", value.Int(12_345)),
      #("did", value.String("did:plc:test123")),
    ])

  let ctx = schema.Context(Some(blob_data), dict.new(), dict.new())
  let result = blob_type.resolve_mime_type(ctx)

  result
  |> should.be_ok()
  |> should.equal(value.String("image/png"))
}

pub fn blob_size_field_resolver_test() {
  // Test that the size field resolver returns the correct value
  let blob_data =
    value.Object([
      #("ref", value.String("bafyreiabc123")),
      #("mime_type", value.String("image/jpeg")),
      #("size", value.Int(54_321)),
      #("did", value.String("did:plc:test123")),
    ])

  let ctx = schema.Context(Some(blob_data), dict.new(), dict.new())
  let result = blob_type.resolve_size(ctx)

  result
  |> should.be_ok()
  |> should.equal(value.Int(54_321))
}

pub fn blob_url_field_resolver_with_default_preset_test() {
  // Test URL generation with default preset
  let blob_data =
    value.Object([
      #("ref", value.String("bafyreiabc123")),
      #("mime_type", value.String("image/jpeg")),
      #("size", value.Int(12_345)),
      #("did", value.String("did:plc:test123")),
    ])

  let ctx = schema.Context(Some(blob_data), dict.new(), dict.new())
  let result = blob_type.resolve_url(ctx)

  let expected_url =
    "https://cdn.bsky.app/img/feed_fullsize/plain/did:plc:test123/bafyreiabc123@jpeg"

  result
  |> should.be_ok()
  |> should.equal(value.String(expected_url))
}

pub fn blob_url_field_resolver_with_avatar_preset_test() {
  // Test URL generation with avatar preset
  let blob_data =
    value.Object([
      #("ref", value.String("bafyreiabc456")),
      #("mime_type", value.String("image/jpeg")),
      #("size", value.Int(12_345)),
      #("did", value.String("did:plc:user789")),
    ])

  let args = dict.from_list([#("preset", value.String("avatar"))])
  let ctx = schema.Context(Some(blob_data), args, dict.new())
  let result = blob_type.resolve_url(ctx)

  let expected_url =
    "https://cdn.bsky.app/img/avatar/plain/did:plc:user789/bafyreiabc456@jpeg"

  result
  |> should.be_ok()
  |> should.equal(value.String(expected_url))
}

pub fn blob_url_field_resolver_with_banner_preset_test() {
  // Test URL generation with banner preset
  let blob_data =
    value.Object([
      #("ref", value.String("bafyreiabc789")),
      #("mime_type", value.String("image/png")),
      #("size", value.Int(98_765)),
      #("did", value.String("did:plc:banner123")),
    ])

  let args = dict.from_list([#("preset", value.String("banner"))])
  let ctx = schema.Context(Some(blob_data), args, dict.new())
  let result = blob_type.resolve_url(ctx)

  let expected_url =
    "https://cdn.bsky.app/img/banner/plain/did:plc:banner123/bafyreiabc789@jpeg"

  result
  |> should.be_ok()
  |> should.equal(value.String(expected_url))
}

pub fn blob_url_field_resolver_with_feed_thumbnail_preset_test() {
  // Test URL generation with feed_thumbnail preset
  let blob_data =
    value.Object([
      #("ref", value.String("bafyreiathumbnail")),
      #("mime_type", value.String("image/jpeg")),
      #("size", value.Int(5000)),
      #("did", value.String("did:plc:thumb456")),
    ])

  let args = dict.from_list([#("preset", value.String("feed_thumbnail"))])
  let ctx = schema.Context(Some(blob_data), args, dict.new())
  let result = blob_type.resolve_url(ctx)

  let expected_url =
    "https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:thumb456/bafyreiathumbnail@jpeg"

  result
  |> should.be_ok()
  |> should.equal(value.String(expected_url))
}
