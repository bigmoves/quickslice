/// Tests for GraphQL Introspection
///
/// Comprehensive tests for introspection queries
import gleam/list
import gleam/option.{None}
import gleeunit/should
import graphql/executor
import graphql/schema
import graphql/value

// Helper to create a simple test schema
fn test_schema() -> schema.Schema {
  let query_type =
    schema.object_type("Query", "Root query type", [
      schema.field("hello", schema.string_type(), "Hello field", fn(_ctx) {
        Ok(value.String("world"))
      }),
      schema.field("number", schema.int_type(), "Number field", fn(_ctx) {
        Ok(value.Int(42))
      }),
    ])

  schema.schema(query_type, None)
}

/// Test: Multiple scalar fields on __schema
/// This test verifies that all requested fields on __schema are returned
pub fn schema_multiple_fields_test() {
  let schema = test_schema()
  let query =
    "{ __schema { queryType { name } mutationType { name } subscriptionType { name } } }"

  let result = executor.execute(query, schema, schema.Context(None))

  should.be_ok(result)
  |> fn(response) {
    case response {
      executor.Response(data: value.Object(fields), errors: []) -> {
        // Check that we have __schema field
        case list.key_find(fields, "__schema") {
          Ok(value.Object(schema_fields)) -> {
            // Check for all three fields
            let has_query_type = case
              list.key_find(schema_fields, "queryType")
            {
              Ok(value.Object(_)) -> True
              _ -> False
            }
            let has_mutation_type = case
              list.key_find(schema_fields, "mutationType")
            {
              Ok(value.Null) -> True
              // Should be null
              _ -> False
            }
            let has_subscription_type = case
              list.key_find(schema_fields, "subscriptionType")
            {
              Ok(value.Null) -> True
              // Should be null
              _ -> False
            }
            has_query_type && has_mutation_type && has_subscription_type
          }
          _ -> False
        }
      }
      _ -> False
    }
  }
  |> should.be_true
}

/// Test: types field with other fields
/// Verifies that the types array is returned along with other fields
pub fn schema_types_with_other_fields_test() {
  let schema = test_schema()
  let query = "{ __schema { queryType { name } types { name } } }"

  let result = executor.execute(query, schema, schema.Context(None))

  should.be_ok(result)
  |> fn(response) {
    case response {
      executor.Response(data: value.Object(fields), errors: []) -> {
        case list.key_find(fields, "__schema") {
          Ok(value.Object(schema_fields)) -> {
            // Check for both fields
            let has_query_type = case
              list.key_find(schema_fields, "queryType")
            {
              Ok(value.Object(qt_fields)) -> {
                case list.key_find(qt_fields, "name") {
                  Ok(value.String("Query")) -> True
                  _ -> False
                }
              }
              _ -> False
            }
            let has_types = case list.key_find(schema_fields, "types") {
              Ok(value.List(types)) -> {
                // Should have 6 types: Query + 5 scalars
                list.length(types) == 6
              }
              _ -> False
            }
            has_query_type && has_types
          }
          _ -> False
        }
      }
      _ -> False
    }
  }
  |> should.be_true
}

/// Test: All __schema top-level fields
/// Verifies that a query with all possible __schema fields returns all of them
pub fn schema_all_fields_test() {
  let schema = test_schema()
  let query =
    "{ __schema { queryType { name } mutationType { name } subscriptionType { name } types { name } directives { name } } }"

  let result = executor.execute(query, schema, schema.Context(None))

  should.be_ok(result)
  |> fn(response) {
    case response {
      executor.Response(data: value.Object(fields), errors: []) -> {
        case list.key_find(fields, "__schema") {
          Ok(value.Object(schema_fields)) -> {
            // Check for all five fields
            let field_count = list.length(schema_fields)
            // Should have exactly 5 fields
            field_count == 5
          }
          _ -> False
        }
      }
      _ -> False
    }
  }
  |> should.be_true
}

/// Test: Field order doesn't matter
/// Verifies that field order in the query doesn't affect results
pub fn schema_field_order_test() {
  let schema = test_schema()
  let query1 = "{ __schema { types { name } queryType { name } } }"
  let query2 = "{ __schema { queryType { name } types { name } } }"

  let result1 = executor.execute(query1, schema, schema.Context(None))
  let result2 = executor.execute(query2, schema, schema.Context(None))

  // Both should succeed
  should.be_ok(result1)
  should.be_ok(result2)

  // Both should have the same fields
  case result1, result2 {
    Ok(executor.Response(data: value.Object(fields1), errors: [])),
      Ok(executor.Response(data: value.Object(fields2), errors: []))
    -> {
      case
        list.key_find(fields1, "__schema"),
        list.key_find(fields2, "__schema")
      {
        Ok(value.Object(schema_fields1)), Ok(value.Object(schema_fields2)) -> {
          let count1 = list.length(schema_fields1)
          let count2 = list.length(schema_fields2)
          // Both should have 2 fields
          count1 == 2 && count2 == 2
        }
        _, _ -> False
      }
    }
    _, _ -> False
  }
  |> should.be_true
}

/// Test: Nested introspection on types
/// Verifies that nested field selections work correctly
pub fn schema_types_nested_fields_test() {
  let schema = test_schema()
  let query = "{ __schema { types { name kind fields { name } } } }"

  let result = executor.execute(query, schema, schema.Context(None))

  should.be_ok(result)
  |> fn(response) {
    case response {
      executor.Response(data: value.Object(fields), errors: []) -> {
        case list.key_find(fields, "__schema") {
          Ok(value.Object(schema_fields)) -> {
            case list.key_find(schema_fields, "types") {
              Ok(value.List(types)) -> {
                // Check that each type has name, kind, and fields
                list.all(types, fn(type_val) {
                  case type_val {
                    value.Object(type_fields) -> {
                      let has_name = case list.key_find(type_fields, "name") {
                        Ok(_) -> True
                        _ -> False
                      }
                      let has_kind = case list.key_find(type_fields, "kind") {
                        Ok(_) -> True
                        _ -> False
                      }
                      let has_fields = case
                        list.key_find(type_fields, "fields")
                      {
                        Ok(_) -> True
                        // Can be null or list
                        _ -> False
                      }
                      has_name && has_kind && has_fields
                    }
                    _ -> False
                  }
                })
              }
              _ -> False
            }
          }
          _ -> False
        }
      }
      _ -> False
    }
  }
  |> should.be_true
}

/// Test: Empty nested selections on null fields
/// Verifies that querying nested fields on null values doesn't cause errors
pub fn schema_null_field_with_deep_nesting_test() {
  let schema = test_schema()
  let query = "{ __schema { mutationType { name fields { name } } } }"

  let result = executor.execute(query, schema, schema.Context(None))

  should.be_ok(result)
  |> fn(response) {
    case response {
      executor.Response(data: value.Object(fields), errors: []) -> {
        case list.key_find(fields, "__schema") {
          Ok(value.Object(schema_fields)) -> {
            case list.key_find(schema_fields, "mutationType") {
              Ok(value.Null) -> True
              // Should be null, not error
              _ -> False
            }
          }
          _ -> False
        }
      }
      _ -> False
    }
  }
  |> should.be_true
}

/// Test: Inline fragments in introspection
/// Verifies that inline fragments work correctly in introspection queries (like GraphiQL uses)
pub fn schema_inline_fragment_test() {
  let schema = test_schema()
  let query = "{ __schema { types { ... on __Type { kind name } } } }"

  let result = executor.execute(query, schema, schema.Context(None))

  should.be_ok(result)
  |> fn(response) {
    case response {
      executor.Response(data: value.Object(fields), errors: []) -> {
        case list.key_find(fields, "__schema") {
          Ok(value.Object(schema_fields)) -> {
            case list.key_find(schema_fields, "types") {
              Ok(value.List(types)) -> {
                // Should have 6 types with kind and name fields
                list.length(types) == 6
                && list.all(types, fn(type_val) {
                  case type_val {
                    value.Object(type_fields) -> {
                      let has_kind = case list.key_find(type_fields, "kind") {
                        Ok(value.String(_)) -> True
                        _ -> False
                      }
                      let has_name = case list.key_find(type_fields, "name") {
                        Ok(value.String(_)) -> True
                        _ -> False
                      }
                      has_kind && has_name
                    }
                    _ -> False
                  }
                })
              }
              _ -> False
            }
          }
          _ -> False
        }
      }
      _ -> False
    }
  }
  |> should.be_true
}
