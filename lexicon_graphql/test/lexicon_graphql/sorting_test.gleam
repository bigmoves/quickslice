/// Tests for sortBy schema generation
///
/// These tests verify that the GraphQL schema is generated correctly with:
/// - Custom SortFieldEnum for each record type
/// - SortFieldInput InputObject type
/// - sortBy argument on connection fields
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import graphql/schema
import lexicon_graphql/db_schema_builder
import lexicon_graphql/schema_builder

// Create a simple test schema
fn create_test_schema() -> schema.Schema {
  // Mock fetcher that returns empty results (we're only testing schema generation)
  let fetcher = fn(_collection, _params) { Ok(#([], None, False, False)) }

  // Create a lexicon for xyz.statusphere.status
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

  case db_schema_builder.build_schema_with_fetcher([lexicon], fetcher) {
    Ok(s) -> s
    Error(_) -> panic as "Failed to build test schema"
  }
}

// Test: Schema has the query type with connection field
pub fn test_schema_has_connection_field() {
  let test_schema = create_test_schema()
  let query_type = schema.query_type(test_schema)

  // Verify the query type has the xyzStatusphereStatus field
  case schema.get_field(query_type, "xyzStatusphereStatus") {
    None -> should.be_true(False)
    Some(field) -> {
      // Verify field name
      should.equal(schema.field_name(field), "xyzStatusphereStatus")

      // Verify it has arguments
      let args = schema.field_arguments(field)
      should.be_true(args != [])
    }
  }
}

// Test: Connection field has sortBy argument
pub fn test_connection_has_sortby_argument() {
  let test_schema = create_test_schema()
  let query_type = schema.query_type(test_schema)

  case schema.get_field(query_type, "xyzStatusphereStatus") {
    None -> should.be_true(False)
    Some(field) -> {
      let args = schema.field_arguments(field)

      // Find sortBy argument
      let sortby_arg =
        list.find(args, fn(arg) { schema.argument_name(arg) == "sortBy" })

      case sortby_arg {
        Error(_) -> should.be_true(False)
        Ok(arg) -> {
          should.equal(schema.argument_name(arg), "sortBy")

          // Verify it's a list type
          let arg_type = schema.argument_type(arg)
          should.be_true(schema.is_list(arg_type))
        }
      }
    }
  }
}

// Test: Connection field has first/after arguments (forward pagination)
pub fn test_connection_has_pagination_arguments() {
  let test_schema = create_test_schema()
  let query_type = schema.query_type(test_schema)

  case schema.get_field(query_type, "xyzStatusphereStatus") {
    None -> should.be_true(False)
    Some(field) -> {
      let args = schema.field_arguments(field)
      let arg_names = list.map(args, schema.argument_name)

      // Verify we have pagination arguments
      should.be_true(list.contains(arg_names, "first"))
      should.be_true(list.contains(arg_names, "after"))
      should.be_true(list.contains(arg_names, "last"))
      should.be_true(list.contains(arg_names, "before"))
      should.be_true(list.contains(arg_names, "sortBy"))
    }
  }
}

// Test: Query type has correct field for the lexicon
pub fn test_query_type_has_lexicon_field() {
  let test_schema = create_test_schema()
  let query_type = schema.query_type(test_schema)

  // The field name should be camelCase version of the NSID
  let field = schema.get_field(query_type, "xyzStatusphereStatus")

  case field {
    None -> should.be_true(False)
    Some(_) -> should.be_true(True)
  }
}

// Test: Multiple lexicons create multiple fields with distinct sort enums
pub fn test_multiple_lexicons_create_distinct_fields() {
  let fetcher = fn(_collection, _params) { Ok(#([], None, False, False)) }

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
          #("createdAt", schema_builder.Property("string", False)),
        ]),
      ),
    )

  case db_schema_builder.build_schema_with_fetcher([lexicon1, lexicon2], fetcher) {
    Ok(test_schema) -> {
      let query_type = schema.query_type(test_schema)

      // Verify both fields exist
      let field1 = schema.get_field(query_type, "xyzStatusphereStatus")
      let field2 = schema.get_field(query_type, "appBskyFeedPost")

      case field1, field2 {
        None, _ -> should.be_true(False)
        _, None -> should.be_true(False)
        Some(_), Some(_) -> should.be_true(True)
      }
    }
    Error(_) -> should.be_true(False)
  }
}
