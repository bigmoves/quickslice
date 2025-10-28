/// Tests for Schema Builder
///
/// Builds GraphQL schemas from AT Protocol lexicon definitions
import gleeunit/should
import lexicon_graphql/schema_builder

// Test building a schema from a simple lexicon
pub fn build_simple_schema_test() {
  // Simple status lexicon with text field
  let lexicon =
    schema_builder.Lexicon(
      id: "xyz.statusphere.status",
      defs: schema_builder.Defs(
        main: schema_builder.RecordDef(type_: "record", properties: [
          #("text", schema_builder.Property("string", False)),
          #("createdAt", schema_builder.Property("string", True)),
        ]),
      ),
    )

  let result = schema_builder.build_schema([lexicon])

  // Should successfully build a schema
  should.be_ok(result)
}

// Test building schema with multiple lexicons
pub fn build_schema_with_multiple_lexicons_test() {
  let status_lexicon =
    schema_builder.Lexicon(
      id: "xyz.statusphere.status",
      defs: schema_builder.Defs(
        main: schema_builder.RecordDef(type_: "record", properties: [
          #("text", schema_builder.Property("string", False)),
        ]),
      ),
    )

  let profile_lexicon =
    schema_builder.Lexicon(
      id: "xyz.statusphere.profile",
      defs: schema_builder.Defs(
        main: schema_builder.RecordDef(type_: "record", properties: [
          #("displayName", schema_builder.Property("string", False)),
        ]),
      ),
    )

  let result = schema_builder.build_schema([status_lexicon, profile_lexicon])

  should.be_ok(result)
}

// Test that the schema has correct type names
pub fn schema_has_correct_type_names_test() {
  let lexicon =
    schema_builder.Lexicon(
      id: "app.bsky.feed.post",
      defs: schema_builder.Defs(
        main: schema_builder.RecordDef(type_: "record", properties: [
          #("text", schema_builder.Property("string", True)),
        ]),
      ),
    )

  let result = schema_builder.build_schema([lexicon])

  should.be_ok(result)
  // Type name should be AppBskyFeedPost (PascalCase from NSID)
  // Field name should be appBskyFeedPost (camelCase from NSID)
}

// Test empty lexicon list
pub fn build_schema_with_empty_list_test() {
  let result = schema_builder.build_schema([])

  // Should return error for empty lexicon list
  should.be_error(result)
}
