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
import gleam/list
import gleam/option.{Some}
import gleeunit/should
import graphql/introspection
import graphql/schema
import graphql/sdl
import lexicon_graphql/db_schema_builder
import lexicon_graphql/schema_builder

// Helper to create a test schema with a mock fetcher
fn create_test_schema_from_lexicons(
  lexicons: List(schema_builder.Lexicon),
) -> schema.Schema {
  // Mock fetcher that returns empty results (we're only testing schema generation)
  let fetcher = fn(_collection, _params) { Ok(#([], option.None, False, False)) }

  case db_schema_builder.build_schema_with_fetcher(lexicons, fetcher) {
    Ok(s) -> s
    Error(_) -> panic as "Failed to build test schema"
  }
}

// Test: Single lexicon creates connection field with sortBy
pub fn single_lexicon_with_sorting_snapshot_test() {
  let lexicon =
    schema_builder.Lexicon(
      "xyz.statusphere.status",
      schema_builder.Defs(
        schema_builder.RecordDef("record", [
          #("status", schema_builder.Property("string", False)),
          #("createdAt", schema_builder.Property("string", False)),
        ]),
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
    schema_builder.Lexicon(
      "xyz.statusphere.status",
      schema_builder.Defs(
        schema_builder.RecordDef("record", [
          #("status", schema_builder.Property("string", False)),
          #("createdAt", schema_builder.Property("string", False)),
        ]),
      ),
    )

  let lexicon2 =
    schema_builder.Lexicon(
      "app.bsky.feed.post",
      schema_builder.Defs(
        schema_builder.RecordDef("record", [
          #("text", schema_builder.Property("string", False)),
          #("likeCount", schema_builder.Property("integer", False)),
        ]),
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
    schema_builder.Lexicon(
      "xyz.statusphere.status",
      schema_builder.Defs(
        schema_builder.RecordDef("record", [
          #("status", schema_builder.Property("string", False)),
        ]),
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
    schema_builder.Lexicon(
      "xyz.statusphere.status",
      schema_builder.Defs(
        schema_builder.RecordDef("record", [
          #("status", schema_builder.Property("string", False)),
        ]),
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
    schema_builder.Lexicon(
      "xyz.statusphere.status",
      schema_builder.Defs(
        schema_builder.RecordDef("record", [
          #("text", schema_builder.Property("string", False)),
          #("createdAt", schema_builder.Property("string", False)),
        ]),
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
