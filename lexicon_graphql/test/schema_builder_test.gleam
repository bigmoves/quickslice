/// Snapshot tests for Schema Builder
///
/// Tests GraphQL schema generation from AT Protocol lexicon definitions
/// Uses birdie to capture and verify the generated schemas

import birdie
import gleeunit/should
import graphql/introspection
import graphql/schema
import graphql/sdl
import lexicon_graphql/schema_builder
import lexicon_graphql/types

// Test building a schema from a simple lexicon
pub fn simple_schema_snapshot_test() {
  // Simple status lexicon with text field
  let lexicon =
    types.Lexicon(
      id: "xyz.statusphere.status",
      defs: types.Defs(
        main: types.RecordDef(type_: "record", properties: [
          #("text", types.Property("string", False)),
          #("createdAt", types.Property("string", True)),
        ]),
      ),
    )

  case schema_builder.build_schema([lexicon]) {
    Ok(s) -> {
      let query_type = schema.query_type(s)
      let serialized = sdl.print_type(query_type)
      birdie.snap(
        title: "Simple status record schema",
        content: serialized,
      )
    }
    Error(_) -> should.fail()
  }
}

// Test building schema with multiple lexicons
pub fn multiple_lexicons_snapshot_test() {
  let status_lexicon =
    types.Lexicon(
      id: "xyz.statusphere.status",
      defs: types.Defs(
        main: types.RecordDef(type_: "record", properties: [
          #("text", types.Property("string", False)),
        ]),
      ),
    )

  let profile_lexicon =
    types.Lexicon(
      id: "xyz.statusphere.profile",
      defs: types.Defs(
        main: types.RecordDef(type_: "record", properties: [
          #("displayName", types.Property("string", False)),
        ]),
      ),
    )

  case schema_builder.build_schema([status_lexicon, profile_lexicon]) {
    Ok(s) -> {
      let query_type = schema.query_type(s)
      let serialized = sdl.print_type(query_type)
      birdie.snap(
        title: "Schema with multiple record types",
        content: serialized,
      )
    }
    Error(_) -> should.fail()
  }
}

// Test that the schema has correct type names from NSID
pub fn correct_type_names_snapshot_test() {
  let lexicon =
    types.Lexicon(
      id: "app.bsky.feed.post",
      defs: types.Defs(
        main: types.RecordDef(type_: "record", properties: [
          #("text", types.Property("string", True)),
          #("replyCount", types.Property("integer", False)),
        ]),
      ),
    )

  case schema_builder.build_schema([lexicon]) {
    Ok(s) -> {
      let query_type = schema.query_type(s)
      let serialized = sdl.print_type(query_type)
      birdie.snap(
        title: "Schema showing PascalCase type name and camelCase field name from NSID",
        content: serialized,
      )
    }
    Error(_) -> should.fail()
  }
}

// Test empty lexicon list (keep as unit test)
pub fn build_schema_with_empty_list_test() {
  let result = schema_builder.build_schema([])

  // Should return error for empty lexicon list
  should.be_error(result)
}

// Comprehensive test showing ALL generated types
pub fn simple_schema_all_types_snapshot_test() {
  let lexicon =
    types.Lexicon(
      id: "xyz.statusphere.status",
      defs: types.Defs(
        main: types.RecordDef(type_: "record", properties: [
          #("text", types.Property("string", False)),
          #("createdAt", types.Property("string", True)),
        ]),
      ),
    )

  case schema_builder.build_schema([lexicon]) {
    Ok(s) -> {
      // Use introspection to get ALL types in the schema
      let all_types = introspection.get_all_schema_types(s)
      let serialized = sdl.print_types(all_types)
      birdie.snap(
        title: "All types generated for simple status record",
        content: serialized,
      )
    }
    Error(_) -> should.fail()
  }
}
