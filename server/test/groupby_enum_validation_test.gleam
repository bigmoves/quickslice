/// Integration test for groupBy field enum validation
///
/// Verifies that aggregation queries use collection-specific GroupByField enums
/// instead of plain strings, providing type safety and autocomplete.
import gleam/dict
import gleam/list
import gleam/option
import gleam/result
import gleeunit/should
import lexicon_graphql
import lexicon_graphql/output/aggregate
import lexicon_graphql/query/dataloader
import lexicon_graphql/schema/database
import lexicon_graphql/types
import swell/executor
import swell/schema
import swell/value

pub fn groupby_field_enum_exists_test() {
  // Test: Each collection should have its own GroupByField enum
  // Using introspection query to verify AppBskyFeedPostGroupByField exists

  let lexicons = load_test_lexicons()

  // Create a stub fetcher
  let stub_fetcher = fn(_uri: String, _params: dataloader.PaginationParams) -> Result(
    #(
      List(#(value.Value, String)),
      option.Option(String),
      Bool,
      Bool,
      option.Option(Int),
    ),
    String,
  ) {
    Error("Not implemented for test")
  }

  // Create a stub aggregate fetcher
  let stub_aggregate_fetcher = fn(
    _uri: String,
    _params: database.AggregateParams,
  ) -> Result(List(aggregate.AggregateResult), String) {
    Error("Not implemented for test")
  }

  let assert Ok(graphql_schema) =
    database.build_schema_with_subscriptions(
      lexicons,
      stub_fetcher,
      option.None,
      option.None,
      option.None,
      option.None,
      option.None,
      option.None,
      option.Some(stub_aggregate_fetcher),
      option.None,
      option.None,
      option.None,
      option.None,
      option.None,
      option.None,
    )

  // Introspection query to check if AppBskyFeedPostGroupByField enum exists
  let query =
    "
    {
      __type(name: \"AppBskyFeedPostGroupByField\") {
        name
        kind
        enumValues {
          name
          description
        }
      }
    }
  "

  let ctx = schema.context(option.None)
  let result = executor.execute(query, graphql_schema, ctx)

  // Should successfully execute
  result
  |> should.be_ok()

  // Verify the enum exists and has the expected values
  case result {
    Ok(executor.Response(data, errors)) -> {
      // Should have no errors
      errors
      |> should.equal([])

      case data {
        value.Object(fields) -> {
          case list.key_find(fields, "__type") {
            Ok(value.Object(type_fields)) -> {
              // Verify type name
              case list.key_find(type_fields, "name") {
                Ok(value.String(name)) -> {
                  name
                  |> should.equal("AppBskyFeedPostGroupByField")
                }
                _ -> should.fail()
              }

              // Verify it's an ENUM
              case list.key_find(type_fields, "kind") {
                Ok(value.String(kind)) -> {
                  kind
                  |> should.equal("ENUM")
                }
                _ -> should.fail()
              }

              // Verify it has enum values including standard fields
              case list.key_find(type_fields, "enumValues") {
                Ok(value.List(enum_values)) -> {
                  // Convert to list of names
                  let value_names =
                    list.filter_map(enum_values, fn(v) {
                      case v {
                        value.Object(enum_data) -> {
                          case list.key_find(enum_data, "name") {
                            Ok(value.String(n)) -> Ok(n)
                            _ -> Error(Nil)
                          }
                        }
                        _ -> Error(Nil)
                      }
                    })

                  // Should have standard fields
                  value_names
                  |> list.contains("uri")
                  |> should.equal(True)

                  value_names
                  |> list.contains("did")
                  |> should.equal(True)

                  value_names
                  |> list.contains("indexedAt")
                  |> should.equal(True)

                  // Should have actorHandle (computed field)
                  value_names
                  |> list.contains("actorHandle")
                  |> should.equal(True)

                  // Should have custom property fields (lang, author from test lexicon)
                  value_names
                  |> list.contains("lang")
                  |> should.equal(True)

                  value_names
                  |> list.contains("author")
                  |> should.equal(True)
                }
                _ -> should.fail()
              }
            }
            _ -> should.fail()
          }
        }
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }
}

pub fn groupby_input_uses_field_enum_test() {
  // Test: GroupByFieldInput should use the collection-specific enum for the field parameter
  // Verify AppBskyFeedPostGroupByFieldInput.field uses AppBskyFeedPostGroupByField

  let lexicons = load_test_lexicons()

  let stub_fetcher = fn(_uri: String, _params: dataloader.PaginationParams) -> Result(
    #(
      List(#(value.Value, String)),
      option.Option(String),
      Bool,
      Bool,
      option.Option(Int),
    ),
    String,
  ) {
    Error("Not implemented for test")
  }

  // Create a stub aggregate fetcher
  let stub_aggregate_fetcher = fn(
    _uri: String,
    _params: database.AggregateParams,
  ) -> Result(List(aggregate.AggregateResult), String) {
    Error("Not implemented for test")
  }

  let assert Ok(graphql_schema) =
    database.build_schema_with_subscriptions(
      lexicons,
      stub_fetcher,
      option.None,
      option.None,
      option.None,
      option.None,
      option.None,
      option.None,
      option.Some(stub_aggregate_fetcher),
      option.None,
      option.None,
      option.None,
      option.None,
      option.None,
      option.None,
    )

  // Introspection query to check AppBskyFeedPostGroupByFieldInput
  let query =
    "
    {
      __type(name: \"AppBskyFeedPostGroupByFieldInput\") {
        name
        kind
        inputFields {
          name
          type {
            kind
            ofType {
              name
              kind
            }
          }
        }
      }
    }
  "

  let ctx = schema.context(option.None)
  let result = executor.execute(query, graphql_schema, ctx)

  result
  |> should.be_ok()

  // Verify the input type uses the correct enum
  case result {
    Ok(executor.Response(data, errors)) -> {
      errors
      |> should.equal([])

      case data {
        value.Object(fields) -> {
          case list.key_find(fields, "__type") {
            Ok(value.Object(type_fields)) -> {
              // Verify it's an INPUT_OBJECT
              case list.key_find(type_fields, "kind") {
                Ok(value.String(kind)) -> {
                  kind
                  |> should.equal("INPUT_OBJECT")
                }
                _ -> should.fail()
              }

              // Find the "field" input field
              case list.key_find(type_fields, "inputFields") {
                Ok(value.List(input_fields)) -> {
                  let field_input =
                    list.find(input_fields, fn(f) {
                      case f {
                        value.Object(field_data) -> {
                          case list.key_find(field_data, "name") {
                            Ok(value.String("field")) -> True
                            _ -> False
                          }
                        }
                        _ -> False
                      }
                    })

                  case field_input {
                    Ok(value.Object(field_data)) -> {
                      // Check the type is AppBskyFeedPostGroupByField (wrapped in NON_NULL)
                      case list.key_find(field_data, "type") {
                        Ok(value.Object(type_data)) -> {
                          // Should be NON_NULL
                          case list.key_find(type_data, "kind") {
                            Ok(value.String("NON_NULL")) -> {
                              // Get the inner type
                              case list.key_find(type_data, "ofType") {
                                Ok(value.Object(inner_type)) -> {
                                  case list.key_find(inner_type, "name") {
                                    Ok(value.String(enum_name)) -> {
                                      enum_name
                                      |> should.equal(
                                        "AppBskyFeedPostGroupByField",
                                      )
                                    }
                                    _ -> should.fail()
                                  }

                                  // Verify it's an ENUM
                                  case list.key_find(inner_type, "kind") {
                                    Ok(value.String(kind)) -> {
                                      kind
                                      |> should.equal("ENUM")
                                    }
                                    _ -> should.fail()
                                  }
                                }
                                _ -> should.fail()
                              }
                            }
                            _ -> should.fail()
                          }
                        }
                        _ -> should.fail()
                      }
                    }
                    _ -> should.fail()
                  }
                }
                _ -> should.fail()
              }
            }
            _ -> should.fail()
          }
        }
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }
}

// Helper to load app.bsky.feed.post lexicon for testing
fn load_test_lexicons() -> List(types.Lexicon) {
  let post_json =
    "{
    \"lexicon\": 1,
    \"id\": \"app.bsky.feed.post\",
    \"defs\": {
      \"main\": {
        \"type\": \"record\",
        \"key\": \"tid\",
        \"record\": {
          \"type\": \"object\",
          \"required\": [\"text\"],
          \"properties\": {
            \"text\": {\"type\": \"string\"},
            \"lang\": {\"type\": \"string\"},
            \"author\": {\"type\": \"string\"},
            \"likes\": {\"type\": \"integer\"}
          }
        }
      }
    }
  }"

  [
    lexicon_graphql.parse_lexicon(post_json) |> result.unwrap(empty_lexicon()),
  ]
}

fn empty_lexicon() -> types.Lexicon {
  types.Lexicon(
    id: "empty",
    defs: types.Defs(main: option.None, others: dict.new()),
  )
}
