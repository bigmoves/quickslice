/// Snapshot tests for sortBy schema generation
///
/// Tests verify that the GraphQL schema is generated correctly with:
/// - Custom SortFieldEnum for each record type
/// - SortFieldInput InputObject type
/// - sortBy argument on connection fields
/// - Pagination arguments (first, after, last, before)
///
/// Uses birdie to capture and verify the generated schemas
import birdie
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import lexicon_graphql/db_schema_builder
import lexicon_graphql/schema_builder
import lexicon_graphql/types
import swell/introspection
import swell/schema
import swell/sdl

// Helper to create a test schema with a mock fetcher
fn create_test_schema_from_lexicons(
  lexicons: List(schema_builder.Lexicon),
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

// Test: Single lexicon creates connection field with sortBy
pub fn single_lexicon_with_sorting_snapshot_test() {
  let lexicon =
    types.Lexicon(
      "xyz.statusphere.status",
      types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "status",
              types.Property(
                type_: "string",
                required: False,
                format: None,
                ref: None,
              ),
            ),
            #(
              "createdAt",
              types.Property(
                type_: "string",
                required: False,
                format: None,
                ref: None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let test_schema = create_test_schema_from_lexicons([lexicon])
  let query_type = schema.query_type(test_schema)

  let serialized = sdl.print_type(query_type)

  birdie.snap(
    title: "Query type with connection field and sortBy argument",
    content: serialized,
  )
}

// Test: Multiple lexicons create distinct fields with separate sort enums
pub fn multiple_lexicons_with_distinct_sort_enums_snapshot_test() {
  let lexicon1 =
    types.Lexicon(
      "xyz.statusphere.status",
      types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "status",
              types.Property(
                type_: "string",
                required: False,
                format: None,
                ref: None,
              ),
            ),
            #(
              "createdAt",
              types.Property(
                type_: "string",
                required: False,
                format: None,
                ref: None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let lexicon2 =
    types.Lexicon(
      "app.bsky.feed.post",
      types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "text",
              types.Property(
                type_: "string",
                required: False,
                format: None,
                ref: None,
              ),
            ),
            #(
              "likeCount",
              types.Property(
                type_: "integer",
                required: False,
                format: None,
                ref: None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let test_schema = create_test_schema_from_lexicons([lexicon1, lexicon2])
  let query_type = schema.query_type(test_schema)

  let serialized = sdl.print_type(query_type)

  birdie.snap(
    title: "Query type with multiple connection fields and distinct sort enums",
    content: serialized,
  )
}

// Unit test: Verify sortBy argument is a list type
pub fn sortby_argument_is_list_type_test() {
  let lexicon =
    types.Lexicon(
      "xyz.statusphere.status",
      types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "status",
              types.Property(
                type_: "string",
                required: False,
                format: None,
                ref: None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let test_schema = create_test_schema_from_lexicons([lexicon])
  let query_type = schema.query_type(test_schema)

  case schema.get_field(query_type, "xyzStatusphereStatus") {
    Some(field) -> {
      let args = schema.field_arguments(field)
      let sortby_arg =
        list.find(args, fn(arg) { schema.argument_name(arg) == "sortBy" })

      case sortby_arg {
        Ok(arg) -> {
          let arg_type = schema.argument_type(arg)
          should.be_true(schema.is_list(arg_type))
        }
        Error(_) -> should.fail()
      }
    }
    option.None -> should.fail()
  }
}

// Unit test: Verify connection has all pagination arguments
pub fn connection_has_all_pagination_arguments_test() {
  let lexicon =
    types.Lexicon(
      "xyz.statusphere.status",
      types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "status",
              types.Property(
                type_: "string",
                required: False,
                format: None,
                ref: None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let test_schema = create_test_schema_from_lexicons([lexicon])
  let query_type = schema.query_type(test_schema)

  case schema.get_field(query_type, "xyzStatusphereStatus") {
    Some(field) -> {
      let args = schema.field_arguments(field)
      let arg_names = list.map(args, schema.argument_name)

      // Verify we have all pagination arguments
      should.be_true(list.contains(arg_names, "first"))
      should.be_true(list.contains(arg_names, "after"))
      should.be_true(list.contains(arg_names, "last"))
      should.be_true(list.contains(arg_names, "before"))
      should.be_true(list.contains(arg_names, "sortBy"))
    }
    option.None -> should.fail()
  }
}

// Comprehensive test showing ALL generated types for db_schema_builder
pub fn db_schema_all_types_snapshot_test() {
  let lexicon =
    types.Lexicon(
      "xyz.statusphere.status",
      types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "text",
              types.Property(
                type_: "string",
                required: False,
                format: None,
                ref: None,
              ),
            ),
            #(
              "createdAt",
              types.Property(
                type_: "string",
                required: False,
                format: None,
                ref: None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let test_schema = create_test_schema_from_lexicons([lexicon])

  // Use introspection to get ALL types in the schema
  let all_types = introspection.get_all_schema_types(test_schema)
  let serialized = sdl.print_types(all_types)

  birdie.snap(
    title: "All types generated by db_schema_builder including Connection, Edge, PageInfo, SortField enum, WhereInput, etc.",
    content: serialized,
  )
}

// Test: Sort enum only includes primitive types (string, integer, boolean, number)
pub fn sort_enum_excludes_blob_and_ref_types_test() {
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
              ),
            ),
            #(
              "intField",
              types.Property(
                type_: "integer",
                required: False,
                format: None,
                ref: None,
              ),
            ),
            #(
              "boolField",
              types.Property(
                type_: "boolean",
                required: False,
                format: None,
                ref: None,
              ),
            ),
            #(
              "numberField",
              types.Property(
                type_: "number",
                required: False,
                format: None,
                ref: None,
              ),
            ),
            #(
              "uriField",
              types.Property(
                type_: "string",
                required: False,
                format: Some("at-uri"),
                ref: None,
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
              ),
            ),
            #(
              "refField",
              types.Property(
                type_: "ref",
                required: False,
                format: None,
                ref: Some("app.bsky.test.object"),
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let test_schema = create_test_schema_from_lexicons([lexicon])
  let all_types = introspection.get_all_schema_types(test_schema)

  // Find the SortField enum
  let sort_enum =
    list.find(all_types, fn(t) {
      schema.type_name(t) == "AppBskyTestRecordSortField"
    })

  case sort_enum {
    Ok(enum_type) -> {
      let enum_values = schema.get_enum_values(enum_type)
      let value_names = list.map(enum_values, schema.enum_value_name)

      // Should include primitive fields
      should.be_true(list.contains(value_names, "stringField"))
      should.be_true(list.contains(value_names, "intField"))
      should.be_true(list.contains(value_names, "boolField"))
      should.be_true(list.contains(value_names, "numberField"))
      should.be_true(list.contains(value_names, "uriField"))

      // Should include standard fields
      should.be_true(list.contains(value_names, "uri"))
      should.be_true(list.contains(value_names, "cid"))
      should.be_true(list.contains(value_names, "did"))
      should.be_true(list.contains(value_names, "collection"))
      should.be_true(list.contains(value_names, "indexedAt"))

      // Should NOT include blob or ref fields
      should.be_false(list.contains(value_names, "blobField"))
      should.be_false(list.contains(value_names, "refField"))

      // Should NOT include actorHandle (it's a computed field, not sortable)
      should.be_false(list.contains(value_names, "actorHandle"))
    }
    Error(_) -> should.fail()
  }
}

// Snapshot test: Sort enum with mixed field types
pub fn sort_enum_with_mixed_field_types_snapshot_test() {
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
              ),
            ),
            #(
              "intField",
              types.Property(
                type_: "integer",
                required: False,
                format: None,
                ref: None,
              ),
            ),
            #(
              "boolField",
              types.Property(
                type_: "boolean",
                required: False,
                format: None,
                ref: None,
              ),
            ),
            #(
              "numberField",
              types.Property(
                type_: "number",
                required: False,
                format: None,
                ref: None,
              ),
            ),
            #(
              "datetimeField",
              types.Property(
                type_: "string",
                required: False,
                format: Some("datetime"),
                ref: None,
              ),
            ),
            #(
              "uriField",
              types.Property(
                type_: "string",
                required: False,
                format: Some("at-uri"),
                ref: None,
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
              ),
            ),
            #(
              "refField",
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

  // Find and print the SortField enum
  let sort_enum =
    list.find(all_types, fn(t) {
      schema.type_name(t) == "AppBskyTestRecordSortField"
    })

  case sort_enum {
    Ok(enum_type) -> {
      let serialized = sdl.print_type(enum_type)
      birdie.snap(
        title: "SortField enum with mixed types - only includes primitives",
        content: serialized,
      )
    }
    Error(_) -> should.fail()
  }
}
