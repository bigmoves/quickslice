/// Tests for forward join field generation
///
/// Verifies that forward join fields are added to GraphQL schemas based on lexicon metadata
import gleam/dict
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import graphql/introspection
import graphql/schema
import graphql/sdl
import lexicon_graphql/db_schema_builder
import lexicon_graphql/types

// Helper to create a test schema with a mock fetcher
fn create_test_schema_from_lexicons(
  lexicons: List(types.Lexicon),
) -> schema.Schema {
  // Mock fetcher that returns empty results (we're only testing schema generation)
  let fetcher = fn(_collection, _params) {
    Ok(#([], option.None, False, False, option.None))
  }

  case
    db_schema_builder.build_schema_with_fetcher(
      lexicons,
      fetcher,
      option.None,
      option.None,
      option.None,
      option.None,
      option.None,
      option.None,
    )
  {
    Ok(s) -> s
    Error(_) -> panic as "Failed to build test schema"
  }
}

// Test that strongRef fields generate forward join fields
pub fn strong_ref_generates_forward_join_field_test() {
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
          ]),
        ),
        others: dict.new(),
      ),
    )

  let test_schema = create_test_schema_from_lexicons([lexicon])

  // Get all types and serialize to SDL
  let all_types = introspection.get_all_schema_types(test_schema)
  let serialized = sdl.print_types(all_types)

  // Verify that pinnedPostResolved field appears in the schema
  string.contains(serialized, "pinnedPostResolved")
  |> should.be_true
}

// Test that at-uri fields generate forward join fields
pub fn at_uri_generates_forward_join_field_test() {
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

  let test_schema = create_test_schema_from_lexicons([lexicon])

  let all_types = introspection.get_all_schema_types(test_schema)
  let serialized = sdl.print_types(all_types)

  // Verify that subjectResolved field appears in the schema
  string.contains(serialized, "subjectResolved")
  |> should.be_true
}

// Test that multiple forward join fields are all generated
pub fn multiple_forward_join_fields_test() {
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

  let test_schema = create_test_schema_from_lexicons([lexicon])

  let all_types = introspection.get_all_schema_types(test_schema)
  let serialized = sdl.print_types(all_types)

  // Check both forward join fields exist
  string.contains(serialized, "replyResolved")
  |> should.be_true

  string.contains(serialized, "viaResolved")
  |> should.be_true
}

// Test that collections without join fields don't generate extra fields
pub fn no_join_fields_test() {
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

  let test_schema = create_test_schema_from_lexicons([lexicon])

  let all_types = introspection.get_all_schema_types(test_schema)
  let serialized = sdl.print_types(all_types)

  // Should not have any "Resolved" fields for non-join fields
  let has_status_resolved = string.contains(serialized, "statusResolved")
  let has_created_at_resolved = string.contains(serialized, "createdAtResolved")

  has_status_resolved
  |> should.be_false

  has_created_at_resolved
  |> should.be_false
}

// Test that Record union is generated for forward joins
pub fn record_union_type_exists_test() {
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
          ]),
        ),
        others: dict.new(),
      ),
    )

  let test_schema = create_test_schema_from_lexicons([lexicon])

  let all_types = introspection.get_all_schema_types(test_schema)
  let serialized = sdl.print_types(all_types)

  // Verify that Record union exists
  string.contains(serialized, "union Record")
  |> should.be_true
}

// Test that forward join fields have Record union type
pub fn forward_join_field_has_union_type_test() {
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
          ]),
        ),
        others: dict.new(),
      ),
    )

  let test_schema = create_test_schema_from_lexicons([lexicon])

  let all_types = introspection.get_all_schema_types(test_schema)
  let serialized = sdl.print_types(all_types)

  // Verify that replyResolved field has Record type
  string.contains(serialized, "replyResolved: Record")
  |> should.be_true
}
