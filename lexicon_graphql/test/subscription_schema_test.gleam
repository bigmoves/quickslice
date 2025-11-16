/// Tests for GraphQL Subscription Schema Generation
///
/// Tests that subscription fields are auto-generated for record type lexicons
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit
import gleeunit/should
import lexicon_graphql/db_schema_builder
import lexicon_graphql/types
import swell/schema

pub fn main() {
  gleeunit.main()
}

// Helper to create a simple test fetcher (not used for subscriptions but required)
fn test_fetcher(_collection: String, _params) {
  Ok(#([], None, False, False, None))
}

// Test: Subscription type is generated for record lexicons
pub fn subscription_type_generated_test() {
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
          ]),
        ),
        others: dict.new(),
      ),
    )

  // Build schema with subscriptions
  case
    db_schema_builder.build_schema_with_subscriptions(
      [lexicon],
      test_fetcher,
      None,
      None,
      None,
      None,
      None,
      None,
      None,  // aggregate_fetcher
    )
  {
    Ok(s) -> {
      // Schema should have subscription type
      let subscription_type = schema.get_subscription_type(s)
      subscription_type
      |> should.not_equal(None)
    }
    Error(_err) -> {
      should.fail()
    }
  }
}

// Test: Subscription fields for Created, Updated, Deleted
pub fn subscription_fields_generated_test() {
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
          ]),
        ),
        others: dict.new(),
      ),
    )

  case
    db_schema_builder.build_schema_with_subscriptions(
      [lexicon],
      test_fetcher,
      None,
      None,
      None,
      None,
      None,
      None,
      None,  // aggregate_fetcher
    )
  {
    Ok(s) -> {
      case schema.get_subscription_type(s) {
        Some(sub_type) -> {
          let fields = schema.get_fields(sub_type)

          // Should have 3 subscription fields: Created, Updated, Deleted
          list.length(fields)
          |> should.equal(3)

          // Check that field names contain expected patterns
          let field_names = list.map(fields, schema.field_name)

          // Should have a Created field
          let has_created =
            list.any(field_names, fn(name) { string.ends_with(name, "Created") })
          has_created |> should.be_true()

          // Should have an Updated field
          let has_updated =
            list.any(field_names, fn(name) { string.ends_with(name, "Updated") })
          has_updated |> should.be_true()

          // Should have a Deleted field
          let has_deleted =
            list.any(field_names, fn(name) { string.ends_with(name, "Deleted") })
          has_deleted |> should.be_true()
        }
        None -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// Test: Deleted subscription returns String (just URI)
pub fn deleted_subscription_returns_string_test() {
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
                format: None,
                ref: None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  case
    db_schema_builder.build_schema_with_subscriptions(
      [lexicon],
      test_fetcher,
      None,
      None,
      None,
      None,
      None,
      None,
      None,  // aggregate_fetcher
    )
  {
    Ok(s) -> {
      case schema.get_subscription_type(s) {
        Some(sub_type) -> {
          let fields = schema.get_fields(sub_type)
          // Find the Deleted field (ends with "Deleted")
          let deleted_fields =
            list.filter(fields, fn(f) {
              string.ends_with(schema.field_name(f), "Deleted")
            })

          case deleted_fields {
            [field, ..] -> {
              let field_type = schema.field_type(field)
              // Should be NonNull(String)
              schema.is_non_null(field_type)
              |> should.be_true()
            }
            [] -> should.fail()
          }
        }
        None -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// Test: Created/Updated subscriptions return the record type
pub fn created_updated_return_record_type_test() {
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
          ]),
        ),
        others: dict.new(),
      ),
    )

  case
    db_schema_builder.build_schema_with_subscriptions(
      [lexicon],
      test_fetcher,
      None,
      None,
      None,
      None,
      None,
      None,
      None,  // aggregate_fetcher
    )
  {
    Ok(s) -> {
      case schema.get_subscription_type(s) {
        Some(sub_type) -> {
          let fields = schema.get_fields(sub_type)

          // Find Created field
          let created_fields =
            list.filter(fields, fn(f) {
              string.ends_with(schema.field_name(f), "Created")
            })

          case created_fields {
            [field, ..] -> {
              // Should be NonNull type
              let field_type = schema.field_type(field)
              schema.is_non_null(field_type)
              |> should.be_true()
            }
            [] -> should.fail()
          }

          // Find Updated field
          let updated_fields =
            list.filter(fields, fn(f) {
              string.ends_with(schema.field_name(f), "Updated")
            })

          case updated_fields {
            [field, ..] -> {
              // Should be NonNull type
              let field_type = schema.field_type(field)
              schema.is_non_null(field_type)
              |> should.be_true()
            }
            [] -> should.fail()
          }
        }
        None -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// Test: Multiple record types generate multiple subscription fields
pub fn multiple_records_generate_subscriptions_test() {
  let post_lexicon =
    types.Lexicon(
      id: "app.bsky.feed.post",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #("text", types.Property("string", True, None, None)),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let like_lexicon =
    types.Lexicon(
      id: "app.bsky.feed.like",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #("subject", types.Property("string", True, None, None)),
          ]),
        ),
        others: dict.new(),
      ),
    )

  case
    db_schema_builder.build_schema_with_subscriptions(
      [post_lexicon, like_lexicon],
      test_fetcher,
      None,
      None,
      None,
      None,
      None,
      None,
      None,  // aggregate_fetcher
    )
  {
    Ok(s) -> {
      case schema.get_subscription_type(s) {
        Some(sub_type) -> {
          let fields = schema.get_fields(sub_type)

          // Should have 6 subscription fields: 3 for post, 3 for like
          list.length(fields)
          |> should.equal(6)
        }
        None -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}
