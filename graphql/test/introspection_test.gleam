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

  let result = executor.execute(query, schema, schema.context(None))

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

  let result = executor.execute(query, schema, schema.context(None))

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

  let result = executor.execute(query, schema, schema.context(None))

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

  let result1 = executor.execute(query1, schema, schema.context(None))
  let result2 = executor.execute(query2, schema, schema.context(None))

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

  let result = executor.execute(query, schema, schema.context(None))

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

  let result = executor.execute(query, schema, schema.context(None))

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

  let result = executor.execute(query, schema, schema.context(None))

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

/// Test: Basic __type query
/// Verifies that __type(name: "TypeName") returns the correct type
pub fn type_basic_query_test() {
  let schema = test_schema()
  let query = "{ __type(name: \"Query\") { name kind } }"

  let result = executor.execute(query, schema, schema.context(None))

  should.be_ok(result)
  |> fn(response) {
    case response {
      executor.Response(data: value.Object(fields), errors: []) -> {
        case list.key_find(fields, "__type") {
          Ok(value.Object(type_fields)) -> {
            // Check name and kind
            let has_correct_name = case list.key_find(type_fields, "name") {
              Ok(value.String("Query")) -> True
              _ -> False
            }
            let has_correct_kind = case list.key_find(type_fields, "kind") {
              Ok(value.String("OBJECT")) -> True
              _ -> False
            }
            has_correct_name && has_correct_kind
          }
          _ -> False
        }
      }
      _ -> False
    }
  }
  |> should.be_true
}

/// Test: __type query with nested fields
/// Verifies that nested selections work correctly on __type
pub fn type_nested_fields_test() {
  let schema = test_schema()
  let query = "{ __type(name: \"Query\") { name kind fields { name type { name kind } } } }"

  let result = executor.execute(query, schema, schema.context(None))

  should.be_ok(result)
  |> fn(response) {
    case response {
      executor.Response(data: value.Object(fields), errors: []) -> {
        case list.key_find(fields, "__type") {
          Ok(value.Object(type_fields)) -> {
            // Check that fields exists and is a list
            case list.key_find(type_fields, "fields") {
              Ok(value.List(field_list)) -> {
                // Should have 2 fields (hello and number)
                list.length(field_list) == 2
                && list.all(field_list, fn(field_val) {
                  case field_val {
                    value.Object(field_fields) -> {
                      let has_name = case list.key_find(field_fields, "name") {
                        Ok(value.String(_)) -> True
                        _ -> False
                      }
                      let has_type = case list.key_find(field_fields, "type") {
                        Ok(value.Object(_)) -> True
                        _ -> False
                      }
                      has_name && has_type
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

/// Test: __type query for scalar types
/// Verifies that __type works for built-in scalar types
pub fn type_scalar_query_test() {
  let schema = test_schema()
  let query = "{ __type(name: \"String\") { name kind } }"

  let result = executor.execute(query, schema, schema.context(None))

  should.be_ok(result)
  |> fn(response) {
    case response {
      executor.Response(data: value.Object(fields), errors: []) -> {
        case list.key_find(fields, "__type") {
          Ok(value.Object(type_fields)) -> {
            // Check name and kind
            let has_correct_name = case list.key_find(type_fields, "name") {
              Ok(value.String("String")) -> True
              _ -> False
            }
            let has_correct_kind = case list.key_find(type_fields, "kind") {
              Ok(value.String("SCALAR")) -> True
              _ -> False
            }
            has_correct_name && has_correct_kind
          }
          _ -> False
        }
      }
      _ -> False
    }
  }
  |> should.be_true
}

/// Test: __type query for non-existent type
/// Verifies that __type returns null for types that don't exist
pub fn type_not_found_test() {
  let schema = test_schema()
  let query = "{ __type(name: \"NonExistentType\") { name kind } }"

  let result = executor.execute(query, schema, schema.context(None))

  should.be_ok(result)
  |> fn(response) {
    case response {
      executor.Response(data: value.Object(fields), errors: []) -> {
        case list.key_find(fields, "__type") {
          Ok(value.Null) -> True
          _ -> False
        }
      }
      _ -> False
    }
  }
  |> should.be_true
}

/// Test: __type query without name argument
/// Verifies that __type returns an error when name argument is missing
pub fn type_missing_argument_test() {
  let schema = test_schema()
  let query = "{ __type { name kind } }"

  let result = executor.execute(query, schema, schema.context(None))

  should.be_ok(result)
  |> fn(response) {
    case response {
      executor.Response(data: value.Object(fields), errors: errors) -> {
        // Should have __type field as null
        let has_null_type = case list.key_find(fields, "__type") {
          Ok(value.Null) -> True
          _ -> False
        }
        // Should have an error
        let has_error = errors != []
        has_null_type && has_error
      }
      _ -> False
    }
  }
  |> should.be_true
}

/// Test: Combined __type and __schema query
/// Verifies that __type and __schema can be queried together
pub fn type_and_schema_combined_test() {
  let schema = test_schema()
  let query = "{ __schema { queryType { name } } __type(name: \"String\") { name kind } }"

  let result = executor.execute(query, schema, schema.context(None))

  should.be_ok(result)
  |> fn(response) {
    case response {
      executor.Response(data: value.Object(fields), errors: []) -> {
        let has_schema = case list.key_find(fields, "__schema") {
          Ok(value.Object(_)) -> True
          _ -> False
        }
        let has_type = case list.key_find(fields, "__type") {
          Ok(value.Object(_)) -> True
          _ -> False
        }
        has_schema && has_type
      }
      _ -> False
    }
  }
  |> should.be_true
}

/// Test: Deep introspection queries complete without hanging
/// This test verifies that the cycle detection prevents infinite loops
/// by successfully completing a deeply nested introspection query
pub fn deep_introspection_test() {
  let schema = test_schema()

  // Query with deep nesting including ofType chains
  // Without cycle detection, this could cause infinite loops
  let query =
    "{ __schema { types { name kind fields { name type { name kind ofType { name kind ofType { name } } } } } } }"

  let result = executor.execute(query, schema, schema.context(None))

  // The key test: should complete without hanging
  should.be_ok(result)
  |> fn(response) {
    case response {
      executor.Response(data: value.Object(fields), errors: _errors) -> {
        // Should have __schema field with types
        case list.key_find(fields, "__schema") {
          Ok(value.Object(schema_fields)) -> {
            case list.key_find(schema_fields, "types") {
              Ok(value.List(types)) -> types != []
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

/// Test: Fragment spreads work in introspection queries
/// Verifies that fragment spreads like those used by GraphiQL work correctly
pub fn introspection_fragment_spread_test() {
  // Create a schema with an ENUM type
  let sort_enum =
    schema.enum_type("SortDirection", "Sort direction", [
      schema.enum_value("ASC", "Ascending"),
      schema.enum_value("DESC", "Descending"),
    ])

  let query_type =
    schema.object_type("Query", "Root query", [
      schema.field("items", schema.list_type(schema.string_type()), "", fn(_) {
        Ok(value.List([value.String("a"), value.String("b")]))
      }),
      schema.field("sort", sort_enum, "", fn(_) { Ok(value.String("ASC")) }),
    ])

  let test_schema = schema.schema(query_type, None)

  // Use a fragment spread like GraphiQL does
  let query =
    "
    query IntrospectionQuery {
      __schema {
        types {
          ...FullType
        }
      }
    }

    fragment FullType on __Type {
      kind
      name
      enumValues(includeDeprecated: true) {
        name
        description
      }
    }
    "

  let result = executor.execute(query, test_schema, schema.context(None))

  should.be_ok(result)
  |> fn(response) {
    case response {
      executor.Response(data: value.Object(fields), errors: _) -> {
        case list.key_find(fields, "__schema") {
          Ok(value.Object(schema_fields)) -> {
            case list.key_find(schema_fields, "types") {
              Ok(value.List(types)) -> {
                // Find the SortDirection enum
                let enum_type =
                  list.find(types, fn(t) {
                    case t {
                      value.Object(type_fields) -> {
                        case list.key_find(type_fields, "name") {
                          Ok(value.String("SortDirection")) -> True
                          _ -> False
                        }
                      }
                      _ -> False
                    }
                  })

                case enum_type {
                  Ok(value.Object(type_fields)) -> {
                    // Should have kind field from fragment
                    let has_kind = case list.key_find(type_fields, "kind") {
                      Ok(value.String("ENUM")) -> True
                      _ -> False
                    }

                    // Should have enumValues field from fragment
                    let has_enum_values = case
                      list.key_find(type_fields, "enumValues")
                    {
                      Ok(value.List(values)) -> list.length(values) == 2
                      _ -> False
                    }

                    has_kind && has_enum_values
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
      _ -> False
    }
  }
  |> should.be_true
}

/// Test: Simple fragment on __type
pub fn simple_type_fragment_test() {
  let schema = test_schema()

  let query = "{ __type(name: \"Query\") { ...TypeFrag } } fragment TypeFrag on __Type { name kind }"

  let result = executor.execute(query, schema, schema.context(None))

  should.be_ok(result)
  |> fn(response) {
    case response {
      executor.Response(data: value.Object(fields), errors: _) -> {
        case list.key_find(fields, "__type") {
          Ok(value.Object(type_fields)) -> {
            // Check if we got an error about fragment not found
            case list.key_find(type_fields, "__FRAGMENT_ERROR") {
              Ok(value.String(msg)) -> {
                // Fragment wasn't found
                panic as msg
              }
              _ -> {
                // No error, check if we have actual fields
                type_fields != []
              }
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
