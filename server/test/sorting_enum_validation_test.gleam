/// Integration test for sorting enum validation
///
/// Verifies that each Connection field uses the correct collection-specific
/// SortFieldInput type with the appropriate enum, fixing the bug where all
/// fields were sharing a single global enum.
import gleam/dict
import gleam/list
import gleam/option
import gleam/result
import gleeunit/should
import lexicon_graphql
import lexicon_graphql/query/dataloader
import lexicon_graphql/schema/database
import lexicon_graphql/types
import swell/executor
import swell/schema
import swell/value

pub fn sorting_enum_input_types_are_unique_per_collection_test() {
  // Test: Each collection should have its own SortFieldInput type
  // Using introspection query to verify SocialGrainGalleryItemSortFieldInput exists

  let lexicons = load_social_grain_lexicons()

  // Create a stub fetcher that won't actually be called
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

  let assert Ok(graphql_schema) =
    database.build_schema_with_fetcher(
      lexicons,
      stub_fetcher,
      option.None,
      option.None,
      option.None,
      option.None,
      option.None,
      option.None,
    )

  // Introspection query to check if SocialGrainGalleryItemSortFieldInput exists
  let query =
    "
    {
      __type(name: \"SocialGrainGalleryItemSortFieldInput\") {
        name
        kind
        inputFields {
          name
          type {
            name
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

  // Should successfully execute
  result
  |> should.be_ok()

  // Verify the type exists and has the correct structure
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
                  |> should.equal("SocialGrainGalleryItemSortFieldInput")
                }
                _ -> should.fail()
              }

              // Verify it's an INPUT_OBJECT
              case list.key_find(type_fields, "kind") {
                Ok(value.String(kind)) -> {
                  kind
                  |> should.equal("INPUT_OBJECT")
                }
                _ -> should.fail()
              }

              // Verify it has a "field" input field that uses the correct enum
              case list.key_find(type_fields, "inputFields") {
                Ok(value.List(input_fields)) -> {
                  // Find the "field" input field
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
                      // Check the type is SocialGrainGalleryItemSortField (wrapped in NON_NULL)
                      case list.key_find(field_data, "type") {
                        Ok(value.Object(type_data)) -> {
                          case list.key_find(type_data, "ofType") {
                            Ok(value.Object(inner_type)) -> {
                              case list.key_find(inner_type, "name") {
                                Ok(value.String(enum_name)) -> {
                                  enum_name
                                  |> should.equal(
                                    "SocialGrainGalleryItemSortField",
                                  )
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

pub fn did_join_uses_correct_sort_enum_test() {
  // Test: DID join fields should use the SOURCE collection's sort enum
  // Query SocialGrainGallery.socialGrainGalleryItemByDid's sortBy argument

  let lexicons = load_social_grain_lexicons()

  // Create a stub fetcher that won't actually be called
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

  let assert Ok(graphql_schema) =
    database.build_schema_with_fetcher(
      lexicons,
      stub_fetcher,
      option.None,
      option.None,
      option.None,
      option.None,
      option.None,
      option.None,
    )

  // Introspection query to check socialGrainGalleryItemByDid's sortBy argument
  let query =
    "
    {
      __type(name: \"SocialGrainGallery\") {
        fields {
          name
          args {
            name
            type {
              kind
              ofType {
                kind
                ofType {
                  name
                }
              }
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

  // Find the socialGrainGalleryItemViaGallery field and verify its sortBy arg
  case result {
    Ok(executor.Response(data, errors)) -> {
      errors
      |> should.equal([])

      case data {
        value.Object(response_fields) -> {
          case list.key_find(response_fields, "__type") {
            Ok(value.Object(type_fields)) -> {
              case list.key_find(type_fields, "fields") {
                Ok(value.List(fields)) -> {
                  // Find socialGrainGalleryItemByDid field
                  let did_join_field =
                    list.find(fields, fn(field) {
                      case field {
                        value.Object(field_data) -> {
                          case list.key_find(field_data, "name") {
                            Ok(value.String("socialGrainGalleryItemByDid")) ->
                              True
                            _ -> False
                          }
                        }
                        _ -> False
                      }
                    })

                  case did_join_field {
                    Ok(value.Object(field_data)) -> {
                      case list.key_find(field_data, "args") {
                        Ok(value.List(args)) -> {
                          // Find sortBy argument
                          let sortby_arg =
                            list.find(args, fn(arg) {
                              case arg {
                                value.Object(arg_data) -> {
                                  case list.key_find(arg_data, "name") {
                                    Ok(value.String("sortBy")) -> True
                                    _ -> False
                                  }
                                }
                                _ -> False
                              }
                            })

                          case sortby_arg {
                            Ok(value.Object(arg_data)) -> {
                              // Get the input type name: [SocialGrainGalleryItemSortFieldInput!]
                              case list.key_find(arg_data, "type") {
                                Ok(value.Object(type_data)) -> {
                                  case list.key_find(type_data, "ofType") {
                                    Ok(value.Object(non_null_data)) -> {
                                      case
                                        list.key_find(non_null_data, "ofType")
                                      {
                                        Ok(value.Object(input_type_data)) -> {
                                          case
                                            list.key_find(
                                              input_type_data,
                                              "name",
                                            )
                                          {
                                            Ok(value.String(input_type_name)) -> {
                                              // Should use GalleryItem's input type, NOT Gallery's or Favorite's
                                              input_type_name
                                              |> should.equal(
                                                "SocialGrainGalleryItemSortFieldInput",
                                              )
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
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }
}

// Helper to load social.grain lexicons for testing
fn load_social_grain_lexicons() -> List(types.Lexicon) {
  let gallery_json =
    "{
    \"lexicon\": 1,
    \"id\": \"social.grain.gallery\",
    \"defs\": {
      \"main\": {
        \"type\": \"record\",
        \"key\": \"tid\",
        \"record\": {
          \"type\": \"object\",
          \"required\": [\"title\"],
          \"properties\": {
            \"title\": {\"type\": \"string\"},
            \"description\": {\"type\": \"string\"}
          }
        }
      }
    }
  }"

  let gallery_item_json =
    "{
    \"lexicon\": 1,
    \"id\": \"social.grain.gallery.item\",
    \"defs\": {
      \"main\": {
        \"type\": \"record\",
        \"key\": \"tid\",
        \"record\": {
          \"type\": \"object\",
          \"required\": [\"gallery\", \"item\", \"position\"],
          \"properties\": {
            \"gallery\": {\"type\": \"string\"},
            \"item\": {\"type\": \"string\"},
            \"position\": {\"type\": \"integer\"}
          }
        }
      }
    }
  }"

  let favorite_json =
    "{
    \"lexicon\": 1,
    \"id\": \"social.grain.favorite\",
    \"defs\": {
      \"main\": {
        \"type\": \"record\",
        \"key\": \"tid\",
        \"record\": {
          \"type\": \"object\",
          \"required\": [\"subject\"],
          \"properties\": {
            \"subject\": {\"type\": \"string\"},
            \"createdAt\": {\"type\": \"string\"}
          }
        }
      }
    }
  }"

  [
    lexicon_graphql.parse_lexicon(gallery_json)
      |> result.unwrap(empty_lexicon()),
    lexicon_graphql.parse_lexicon(gallery_item_json)
      |> result.unwrap(empty_lexicon()),
    lexicon_graphql.parse_lexicon(favorite_json)
      |> result.unwrap(empty_lexicon()),
  ]
}

fn empty_lexicon() -> types.Lexicon {
  types.Lexicon(
    id: "empty",
    defs: types.Defs(main: option.None, others: dict.new()),
  )
}
