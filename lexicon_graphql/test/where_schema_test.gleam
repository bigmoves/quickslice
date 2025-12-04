/// Snapshot tests for WhereInput schema generation
///
/// Uses birdie to capture and verify the GraphQL schema types
/// generated for where input filtering
import birdie
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should
import lexicon_graphql/input/connection
import lexicon_graphql/schema/database as db_schema_builder
import lexicon_graphql/types
import swell/introspection
import swell/schema
import swell/sdl

pub fn main() {
  gleeunit.main()
}

// Helper to create a test schema with a mock fetcher
fn create_test_schema_from_lexicons(
  lexicons: List(types.Lexicon),
) -> schema.Schema {
  // Mock fetcher that returns empty results (we're only testing schema generation)
  let fetcher = fn(_collection, _params) {
    Ok(#([], option.None, False, False, option.None))
  }

  // Mock aggregate fetcher for aggregation queries
  let aggregate_fetcher = fn(
    _collection: String,
    _params: db_schema_builder.AggregateParams,
  ) {
    Ok([])
  }

  case
    db_schema_builder.build_schema_with_subscriptions(
      lexicons,
      fetcher,
      option.None,
      option.None,
      option.None,
      option.None,
      option.None,
      option.None,
      option.Some(aggregate_fetcher),
      option.None,
    )
  {
    Ok(s) -> s
    Error(_) -> panic as "Failed to build test schema"
  }
}

// ===== Simple Record Type =====

pub fn simple_post_where_input_snapshot_test() {
  // Simulate a simple post lexicon with basic fields
  let field_names = ["uri", "cid", "text", "createdAt"]

  let where_input_type =
    connection.build_where_input_type("AppBskyFeedPost", field_names)

  let serialized = sdl.print_type(where_input_type)

  birdie.snap(
    title: "Simple Post WhereInput with basic fields",
    content: serialized,
  )
}

// ===== Complex Record Type =====

pub fn complex_post_where_input_snapshot_test() {
  // Simulate a complex post with many fields
  let field_names = [
    "uri", "cid", "text", "createdAt", "indexedAt", "likeCount", "replyCount",
    "repostCount", "author",
  ]

  let where_input_type =
    connection.build_where_input_type("AppBskyFeedPost", field_names)

  let serialized = sdl.print_type(where_input_type)

  birdie.snap(
    title: "Complex Post WhereInput with many fields",
    content: serialized,
  )
}

// ===== Profile Record Type =====

pub fn profile_where_input_snapshot_test() {
  // Simulate actor profile lexicon
  let field_names = ["did", "handle", "displayName", "description", "avatar"]

  let where_input_type =
    connection.build_where_input_type("AppBskyActorProfile", field_names)

  let serialized = sdl.print_type(where_input_type)

  birdie.snap(
    title: "Profile WhereInput with actor fields",
    content: serialized,
  )
}

// ===== Empty Record (Edge Case) =====

pub fn empty_where_input_snapshot_test() {
  // Edge case: record with no filterable fields
  let field_names = []

  let where_input_type =
    connection.build_where_input_type("EmptyRecord", field_names)

  let serialized = sdl.print_type(where_input_type)

  birdie.snap(
    title: "Empty WhereInput with only AND/OR fields",
    content: serialized,
  )
}

// ===== Field Condition Type =====

pub fn field_condition_snapshot_test() {
  // Test the FieldCondition type that defines operators
  let condition_type =
    connection.build_where_condition_input_type(
      "AppBskyFeedPost",
      schema.string_type(),
    )

  let serialized = sdl.print_type(condition_type)

  birdie.snap(
    title: "Field condition type with all operators",
    content: serialized,
  )
}

// ===== Related Types Together =====

pub fn related_types_snapshot_test() {
  // Test serializing both the WhereInput and its FieldCondition together
  let field_names = ["text", "createdAt", "likes"]

  let where_input_type =
    connection.build_where_input_type("TestRecord", field_names)

  let condition_type =
    connection.build_where_condition_input_type(
      "TestRecord",
      schema.string_type(),
    )

  let serialized = sdl.print_types([where_input_type, condition_type])

  birdie.snap(
    title: "WhereInput and FieldCondition types together",
    content: serialized,
  )
}

// ===== Different Record Types Have Different Names =====

pub fn multiple_record_types_snapshot_test() {
  // Verify different record types generate different WhereInput names
  let post_where =
    connection.build_where_input_type("AppBskyFeedPost", [
      "text",
      "createdAt",
    ])

  let profile_where =
    connection.build_where_input_type("AppBskyActorProfile", [
      "displayName",
      "description",
    ])

  let serialized = sdl.print_types([post_where, profile_where])

  birdie.snap(
    title: "Multiple record types with different WhereInput names",
    content: serialized,
  )
}

// ===== Verify Recursive AND/OR Fields =====

pub fn recursive_and_or_fields_snapshot_test() {
  // This test specifically highlights the recursive AND/OR structure
  let field_names = ["name", "status"]

  let where_input_type =
    connection.build_where_input_type("RecursiveTest", field_names)

  let serialized = sdl.print_type(where_input_type)

  birdie.snap(
    title: "WhereInput showing recursive AND/OR fields",
    content: serialized,
  )
}

// ===== Integration Tests with db_schema_builder =====

// Test: WHERE input only includes primitive types (string, integer, boolean, number)
pub fn where_input_excludes_blob_and_ref_types_test() {
  let lexicon =
    types.Lexicon(
      "app.bsky.test.record",
      types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "stringField",
              types.Property(
                type_: "string",
                required: False,
                format: None,
                ref: None,
                refs: None,
                items: None,
              ),
            ),
            #(
              "intField",
              types.Property(
                type_: "integer",
                required: False,
                format: None,
                ref: None,
                refs: None,
                items: None,
              ),
            ),
            #(
              "boolField",
              types.Property(
                type_: "boolean",
                required: False,
                format: None,
                ref: None,
                refs: None,
                items: None,
              ),
            ),
            #(
              "numberField",
              types.Property(
                type_: "number",
                required: False,
                format: None,
                ref: None,
                refs: None,
                items: None,
              ),
            ),
            #(
              "uriField",
              types.Property(
                type_: "string",
                required: False,
                format: Some("at-uri"),
                ref: None,
                refs: None,
                items: None,
              ),
            ),
            // Non-sortable types that should be excluded
            #(
              "blobField",
              types.Property(
                type_: "blob",
                required: False,
                format: None,
                ref: None,
                refs: None,
                items: None,
              ),
            ),
            #(
              "refField",
              types.Property(
                type_: "ref",
                required: False,
                format: None,
                ref: Some("app.bsky.test.object"),
                refs: None,
                items: None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let test_schema = create_test_schema_from_lexicons([lexicon])
  let all_types = introspection.get_all_schema_types(test_schema)

  // Find the WhereInput type
  let where_input =
    list.find(all_types, fn(t) {
      schema.type_name(t) == "AppBskyTestRecordWhereInput"
    })

  case where_input {
    Ok(input_type) -> {
      let input_fields = schema.get_input_fields(input_type)
      let field_names = list.map(input_fields, schema.input_field_name)

      // Should include primitive fields
      should.be_true(list.contains(field_names, "stringField"))
      should.be_true(list.contains(field_names, "intField"))
      should.be_true(list.contains(field_names, "boolField"))
      should.be_true(list.contains(field_names, "numberField"))
      should.be_true(list.contains(field_names, "uriField"))

      // Should include standard fields
      should.be_true(list.contains(field_names, "uri"))
      should.be_true(list.contains(field_names, "cid"))
      should.be_true(list.contains(field_names, "did"))
      should.be_true(list.contains(field_names, "collection"))
      should.be_true(list.contains(field_names, "indexedAt"))
      should.be_true(list.contains(field_names, "actorHandle"))

      // Should include AND/OR fields
      should.be_true(list.contains(field_names, "and"))
      should.be_true(list.contains(field_names, "or"))

      // Should NOT include blob or ref fields
      should.be_false(list.contains(field_names, "blobField"))
      should.be_false(list.contains(field_names, "refField"))
    }
    Error(_) -> should.fail()
  }
}

// Snapshot test: WHERE input with mixed field types
pub fn where_input_with_mixed_field_types_snapshot_test() {
  let lexicon =
    types.Lexicon(
      "app.bsky.test.record",
      types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            // Sortable primitive types
            #(
              "stringField",
              types.Property(
                type_: "string",
                required: False,
                format: None,
                ref: None,
                refs: None,
                items: None,
              ),
            ),
            #(
              "intField",
              types.Property(
                type_: "integer",
                required: False,
                format: None,
                ref: None,
                refs: None,
                items: None,
              ),
            ),
            #(
              "boolField",
              types.Property(
                type_: "boolean",
                required: False,
                format: None,
                ref: None,
                refs: None,
                items: None,
              ),
            ),
            #(
              "numberField",
              types.Property(
                type_: "number",
                required: False,
                format: None,
                ref: None,
                refs: None,
                items: None,
              ),
            ),
            #(
              "datetimeField",
              types.Property(
                type_: "string",
                required: False,
                format: Some("datetime"),
                ref: None,
                refs: None,
                items: None,
              ),
            ),
            #(
              "uriField",
              types.Property(
                type_: "string",
                required: False,
                format: Some("at-uri"),
                ref: None,
                refs: None,
                items: None,
              ),
            ),
            // Non-sortable types
            #(
              "blobField",
              types.Property(
                type_: "blob",
                required: False,
                format: None,
                ref: None,
                refs: None,
                items: None,
              ),
            ),
            #(
              "refField",
              types.Property(
                type_: "ref",
                required: False,
                format: None,
                ref: Some("com.atproto.repo.strongRef"),
                refs: None,
                items: None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let test_schema = create_test_schema_from_lexicons([lexicon])
  let all_types = introspection.get_all_schema_types(test_schema)

  // Find and print the WhereInput type
  let where_input =
    list.find(all_types, fn(t) {
      schema.type_name(t) == "AppBskyTestRecordWhereInput"
    })

  case where_input {
    Ok(input_type) -> {
      let serialized = sdl.print_type(input_type)
      birdie.snap(
        title: "WhereInput with mixed types - only includes primitives",
        content: serialized,
      )
    }
    Error(_) -> should.fail()
  }
}
