/// Tests for Collection Metadata Extraction
///
/// Tests that we correctly identify forward and reverse join fields from lexicons
import gleam/dict
import gleam/option.{None, Some}
import gleeunit/should
import lexicon_graphql/collection_meta
import lexicon_graphql/types

// Test extracting metadata from a lexicon with strongRef fields
pub fn extract_metadata_with_strong_ref_test() {
  let lexicon =
    types.Lexicon(
      id: "app.bsky.actor.profile",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "displayName",
              types.Property(
                type_: "string",
                required: True,
                format: None,
                ref: None,
              ),
            ),
            #(
              "pinnedPost",
              types.Property(
                type_: "ref",
                required: False,
                format: None,
                ref: Some("com.atproto.repo.strongRef"),
              ),
            ),
            #(
              "joinedViaStarterPack",
              types.Property(
                type_: "ref",
                required: False,
                format: None,
                ref: Some("com.atproto.repo.strongRef"),
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let meta = collection_meta.extract_metadata(lexicon)

  // Check basic metadata
  meta.nsid
  |> should.equal("app.bsky.actor.profile")

  meta.type_name
  |> should.equal("AppBskyActorProfile")

  // Check forward join fields
  collection_meta.get_forward_join_field_names(meta)
  |> should.equal(["joinedViaStarterPack", "pinnedPost"])

  // Check that they're identified as strongRef fields
  collection_meta.is_strong_ref_field(meta, "pinnedPost")
  |> should.be_true

  collection_meta.is_strong_ref_field(meta, "joinedViaStarterPack")
  |> should.be_true

  // Check reverse join fields (none in this case)
  meta.reverse_join_fields
  |> should.equal([])
}

// Test extracting metadata from a lexicon with at-uri fields
pub fn extract_metadata_with_at_uri_test() {
  let lexicon =
    types.Lexicon(
      id: "app.bsky.feed.like",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "subject",
              types.Property(
                type_: "string",
                required: True,
                format: Some("at-uri"),
                ref: None,
              ),
            ),
            #(
              "createdAt",
              types.Property(
                type_: "string",
                required: True,
                format: Some("datetime"),
                ref: None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let meta = collection_meta.extract_metadata(lexicon)

  // Check basic metadata
  meta.nsid
  |> should.equal("app.bsky.feed.like")

  meta.type_name
  |> should.equal("AppBskyFeedLike")

  // Check forward join fields (at-uri string)
  collection_meta.get_forward_join_field_names(meta)
  |> should.equal(["subject"])

  // Check that it's identified as at-uri field
  collection_meta.is_at_uri_field(meta, "subject")
  |> should.be_true

  collection_meta.is_strong_ref_field(meta, "subject")
  |> should.be_false

  // Check reverse join fields (at-uri fields can also be used for reverse joins)
  meta.reverse_join_fields
  |> should.equal(["subject"])
}

// Test extracting metadata from a lexicon with both types
pub fn extract_metadata_with_mixed_fields_test() {
  let lexicon =
    types.Lexicon(
      id: "app.bsky.feed.post",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "text",
              types.Property(
                type_: "string",
                required: True,
                format: None,
                ref: None,
              ),
            ),
            #(
              "reply",
              types.Property(
                type_: "ref",
                required: False,
                format: None,
                ref: Some("com.atproto.repo.strongRef"),
              ),
            ),
            #(
              "via",
              types.Property(
                type_: "string",
                required: False,
                format: Some("at-uri"),
                ref: None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let meta = collection_meta.extract_metadata(lexicon)

  // Check forward join fields (should have both strongRef and at-uri)
  let forward_field_names = collection_meta.get_forward_join_field_names(meta)

  // Should contain both fields (order doesn't matter for this test)
  forward_field_names
  |> should.equal(["via", "reply"])

  // Check types
  collection_meta.is_strong_ref_field(meta, "reply")
  |> should.be_true

  collection_meta.is_at_uri_field(meta, "via")
  |> should.be_true

  // Check reverse join fields (only at-uri)
  meta.reverse_join_fields
  |> should.equal(["via"])
}

// Test extracting metadata from a lexicon with no join fields
pub fn extract_metadata_with_no_join_fields_test() {
  let lexicon =
    types.Lexicon(
      id: "xyz.statusphere.status",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "status",
              types.Property(
                type_: "string",
                required: True,
                format: None,
                ref: None,
              ),
            ),
            #(
              "createdAt",
              types.Property(
                type_: "string",
                required: True,
                format: Some("datetime"),
                ref: None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let meta = collection_meta.extract_metadata(lexicon)

  // Check that no join fields are identified
  collection_meta.get_forward_join_field_names(meta)
  |> should.equal([])

  meta.reverse_join_fields
  |> should.equal([])
}

// Test helper function: is_strong_ref_field with non-existent field
pub fn is_strong_ref_field_non_existent_test() {
  let lexicon =
    types.Lexicon(
      id: "test.collection",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "text",
              types.Property(
                type_: "string",
                required: True,
                format: None,
                ref: None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let meta = collection_meta.extract_metadata(lexicon)

  collection_meta.is_strong_ref_field(meta, "nonexistent")
  |> should.be_false
}

// Test helper function: is_at_uri_field with non-existent field
pub fn is_at_uri_field_non_existent_test() {
  let lexicon =
    types.Lexicon(
      id: "test.collection",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "text",
              types.Property(
                type_: "string",
                required: True,
                format: None,
                ref: None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let meta = collection_meta.extract_metadata(lexicon)

  collection_meta.is_at_uri_field(meta, "nonexistent")
  |> should.be_false
}
