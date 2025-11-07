/// Integration tests for mutation resolvers
///
/// Tests mutation execution with mock resolver factories to verify:
/// - Authentication extraction from context
/// - Argument parsing and validation
/// - Error handling
/// - Response formatting
import database
import gleam/json
import gleam/list
import gleam/option
import gleeunit/should
import lexicon_graphql/db_schema_builder
import lexicon_graphql/lexicon_parser
import sqlight
import swell/executor
import swell/schema
import swell/value

// Helper to create a status lexicon JSON
fn create_status_lexicon() -> String {
  json.object([
    #("lexicon", json.int(1)),
    #("id", json.string("xyz.statusphere.status")),
    #(
      "defs",
      json.object([
        #(
          "main",
          json.object([
            #("type", json.string("record")),
            #("key", json.string("tid")),
            #(
              "record",
              json.object([
                #("type", json.string("object")),
                #(
                  "required",
                  json.array(
                    [json.string("status"), json.string("createdAt")],
                    of: fn(x) { x },
                  ),
                ),
                #(
                  "properties",
                  json.object([
                    #(
                      "status",
                      json.object([
                        #("type", json.string("string")),
                        #("maxLength", json.int(300)),
                      ]),
                    ),
                    #(
                      "createdAt",
                      json.object([
                        #("type", json.string("string")),
                        #("format", json.string("datetime")),
                      ]),
                    ),
                  ]),
                ),
              ]),
            ),
          ]),
        ),
      ]),
    ),
  ])
  |> json.to_string
}

// Mock resolver factory that returns success without making AT Protocol calls
fn mock_create_resolver_factory(_collection: String) -> schema.Resolver {
  fn(ctx: schema.Context) -> Result(value.Value, String) {
    // Verify auth token is present
    case ctx.data {
      option.Some(value.Object(fields)) -> {
        case list.key_find(fields, "auth_token") {
          Ok(value.String(_token)) -> {
            // Verify input argument exists
            case schema.get_argument(ctx, "input") {
              option.Some(_input) -> {
                // Return mock success response
                Ok(
                  value.Object([
                    #(
                      "uri",
                      value.String(
                        "at://did:plc:test/xyz.statusphere.status/test123",
                      ),
                    ),
                    #("cid", value.String("bafyreimock")),
                    #("did", value.String("did:plc:test")),
                    #("collection", value.String("xyz.statusphere.status")),
                    #("indexedAt", value.String("2024-01-01T00:00:00Z")),
                  ]),
                )
              }
              option.None -> Error("Missing required argument: input")
            }
          }
          Ok(_) -> Error("auth_token must be a string")
          Error(_) ->
            Error(
              "Authentication required. Please provide Authorization header.",
            )
        }
      }
      _ ->
        Error("Authentication required. Please provide Authorization header.")
    }
  }
}

// Mock resolver factory that returns success for update
fn mock_update_resolver_factory(_collection: String) -> schema.Resolver {
  fn(ctx: schema.Context) -> Result(value.Value, String) {
    // Verify auth token is present
    case ctx.data {
      option.Some(value.Object(fields)) -> {
        case list.key_find(fields, "auth_token") {
          Ok(value.String(_token)) -> {
            // Verify rkey and input arguments exist
            case schema.get_argument(ctx, "rkey") {
              option.Some(value.String(rkey)) -> {
                case schema.get_argument(ctx, "input") {
                  option.Some(_input) -> {
                    // Return mock success response
                    Ok(
                      value.Object([
                        #(
                          "uri",
                          value.String(
                            "at://did:plc:test/xyz.statusphere.status/" <> rkey,
                          ),
                        ),
                        #("cid", value.String("bafyreiupdated")),
                        #("did", value.String("did:plc:test")),
                        #("collection", value.String("xyz.statusphere.status")),
                        #("indexedAt", value.String("2024-01-01T00:00:00Z")),
                      ]),
                    )
                  }
                  option.None -> Error("Missing required argument: input")
                }
              }
              option.Some(_) -> Error("rkey must be a string")
              option.None -> Error("Missing required argument: rkey")
            }
          }
          Ok(_) -> Error("auth_token must be a string")
          Error(_) ->
            Error(
              "Authentication required. Please provide Authorization header.",
            )
        }
      }
      _ ->
        Error("Authentication required. Please provide Authorization header.")
    }
  }
}

// Mock resolver factory that returns success for delete
fn mock_delete_resolver_factory(_collection: String) -> schema.Resolver {
  fn(ctx: schema.Context) -> Result(value.Value, String) {
    // Verify auth token is present
    case ctx.data {
      option.Some(value.Object(fields)) -> {
        case list.key_find(fields, "auth_token") {
          Ok(value.String(_token)) -> {
            // Verify rkey argument exists
            case schema.get_argument(ctx, "rkey") {
              option.Some(value.String(rkey)) -> {
                // Return mock success response
                Ok(
                  value.Object([
                    #(
                      "uri",
                      value.String(
                        "at://did:plc:test/xyz.statusphere.status/" <> rkey,
                      ),
                    ),
                    #("cid", value.String("")),
                    #("did", value.String("did:plc:test")),
                    #("collection", value.String("xyz.statusphere.status")),
                    #("indexedAt", value.String("")),
                  ]),
                )
              }
              option.Some(_) -> Error("rkey must be a string")
              option.None -> Error("Missing required argument: rkey")
            }
          }
          Ok(_) -> Error("auth_token must be a string")
          Error(_) ->
            Error(
              "Authentication required. Please provide Authorization header.",
            )
        }
      }
      _ ->
        Error("Authentication required. Please provide Authorization header.")
    }
  }
}

// Test: Create mutation without authentication should fail
pub fn create_mutation_without_auth_fails_test() {
  // Setup: Create in-memory database with test lexicons
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = database.create_lexicon_table(db)

  let status_lexicon = create_status_lexicon()
  let assert Ok(_) =
    database.insert_lexicon(db, "xyz.statusphere.status", status_lexicon)

  let assert Ok(lexicon_records) = database.get_all_lexicons(db)
  let parsed_lexicons =
    lexicon_records
    |> list.filter_map(fn(lex) { lexicon_parser.parse_lexicon(lex.json) })

  let empty_fetcher = fn(_collection, _params) {
    Ok(#([], option.None, False, False, option.None))
  }

  let create_factory = option.Some(mock_create_resolver_factory)

  let assert Ok(built_schema) =
    db_schema_builder.build_schema_with_fetcher(
      parsed_lexicons,
      empty_fetcher,
      option.None,
      option.None,
      create_factory,
      option.None,
      option.None,
      option.None,
    )

  // Execute mutation WITHOUT auth token (using shorthand syntax)
  let mutation =
    "mutation { createXyzStatusphereStatus(input: { status: \"test\", createdAt: \"2024-01-01T00:00:00Z\" }) { uri cid } }"

  let ctx = schema.context(option.None)
  let assert Ok(response) = executor.execute(mutation, built_schema, ctx)

  // Should have errors
  response.errors
  |> list.length
  |> should.equal(1)

  // Error message should mention authentication
  let assert [error, ..] = response.errors
  error.message
  |> should.equal(
    "Authentication required. Please provide Authorization header.",
  )
}

// Test: Create mutation with authentication should succeed
pub fn create_mutation_with_auth_succeeds_test() {
  // Setup
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = database.create_lexicon_table(db)

  let status_lexicon = create_status_lexicon()
  let assert Ok(_) =
    database.insert_lexicon(db, "xyz.statusphere.status", status_lexicon)

  let assert Ok(lexicon_records) = database.get_all_lexicons(db)
  let parsed_lexicons =
    lexicon_records
    |> list.filter_map(fn(lex) { lexicon_parser.parse_lexicon(lex.json) })

  let empty_fetcher = fn(_collection, _params) {
    Ok(#([], option.None, False, False, option.None))
  }

  let create_factory = option.Some(mock_create_resolver_factory)

  let assert Ok(built_schema) =
    db_schema_builder.build_schema_with_fetcher(
      parsed_lexicons,
      empty_fetcher,
      option.None,
      option.None,
      create_factory,
      option.None,
      option.None,
      option.None,
    )

  // Execute mutation WITH auth token in context
  let mutation =
    "mutation { createXyzStatusphereStatus(input: { status: \"test\", createdAt: \"2024-01-01T00:00:00Z\" }) { uri cid did } }"

  let ctx_data = value.Object([#("auth_token", value.String("mock_token_123"))])
  let ctx = schema.context(option.Some(ctx_data))
  let assert Ok(response) = executor.execute(mutation, built_schema, ctx)

  // Should have no errors
  response.errors
  |> list.length
  |> should.equal(0)

  // Should return data
  case response.data {
    value.Object(fields) -> {
      case list.key_find(fields, "createXyzStatusphereStatus") {
        Ok(value.Object(record_fields)) -> {
          // Verify URI
          case list.key_find(record_fields, "uri") {
            Ok(value.String(uri)) -> {
              uri
              |> should.equal(
                "at://did:plc:test/xyz.statusphere.status/test123",
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

// Test: Update mutation with authentication should succeed
pub fn update_mutation_with_auth_succeeds_test() {
  // Setup
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = database.create_lexicon_table(db)

  let status_lexicon = create_status_lexicon()
  let assert Ok(_) =
    database.insert_lexicon(db, "xyz.statusphere.status", status_lexicon)

  let assert Ok(lexicon_records) = database.get_all_lexicons(db)
  let parsed_lexicons =
    lexicon_records
    |> list.filter_map(fn(lex) { lexicon_parser.parse_lexicon(lex.json) })

  let empty_fetcher = fn(_collection, _params) {
    Ok(#([], option.None, False, False, option.None))
  }

  let update_factory = option.Some(mock_update_resolver_factory)

  let assert Ok(built_schema) =
    db_schema_builder.build_schema_with_fetcher(
      parsed_lexicons,
      empty_fetcher,
      option.None,
      option.None,
      option.None,
      update_factory,
      option.None,
      option.None,
    )

  // Execute update mutation WITH auth token and rkey
  let mutation =
    "mutation { updateXyzStatusphereStatus(rkey: \"existing123\", input: { status: \"updated\", createdAt: \"2024-01-01T00:00:00Z\" }) { uri cid } }"

  let ctx_data = value.Object([#("auth_token", value.String("mock_token_123"))])
  let ctx = schema.context(option.Some(ctx_data))
  let assert Ok(response) = executor.execute(mutation, built_schema, ctx)

  // Should have no errors
  response.errors
  |> list.length
  |> should.equal(0)

  // Verify URI contains the rkey
  case response.data {
    value.Object(fields) -> {
      case list.key_find(fields, "updateXyzStatusphereStatus") {
        Ok(value.Object(record_fields)) -> {
          case list.key_find(record_fields, "uri") {
            Ok(value.String(uri)) -> {
              uri
              |> should.equal(
                "at://did:plc:test/xyz.statusphere.status/existing123",
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

// Test: Delete mutation with authentication should succeed
pub fn delete_mutation_with_auth_succeeds_test() {
  // Setup
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = database.create_lexicon_table(db)

  let status_lexicon = create_status_lexicon()
  let assert Ok(_) =
    database.insert_lexicon(db, "xyz.statusphere.status", status_lexicon)

  let assert Ok(lexicon_records) = database.get_all_lexicons(db)
  let parsed_lexicons =
    lexicon_records
    |> list.filter_map(fn(lex) { lexicon_parser.parse_lexicon(lex.json) })

  let empty_fetcher = fn(_collection, _params) {
    Ok(#([], option.None, False, False, option.None))
  }

  let delete_factory = option.Some(mock_delete_resolver_factory)

  let assert Ok(built_schema) =
    db_schema_builder.build_schema_with_fetcher(
      parsed_lexicons,
      empty_fetcher,
      option.None,
      option.None,
      option.None,
      option.None,
      delete_factory,
      option.None,
    )

  // Execute delete mutation WITH auth token and rkey
  let mutation =
    "mutation { deleteXyzStatusphereStatus(rkey: \"todelete123\") { uri } }"

  let ctx_data = value.Object([#("auth_token", value.String("mock_token_123"))])
  let ctx = schema.context(option.Some(ctx_data))
  let assert Ok(response) = executor.execute(mutation, built_schema, ctx)

  // Should have no errors
  response.errors
  |> list.length
  |> should.equal(0)

  // Verify URI contains the rkey
  case response.data {
    value.Object(fields) -> {
      case list.key_find(fields, "deleteXyzStatusphereStatus") {
        Ok(value.Object(record_fields)) -> {
          case list.key_find(record_fields, "uri") {
            Ok(value.String(uri)) -> {
              uri
              |> should.equal(
                "at://did:plc:test/xyz.statusphere.status/todelete123",
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

// Test: Update mutation without rkey should fail
pub fn update_mutation_without_rkey_fails_test() {
  // Setup
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = database.create_lexicon_table(db)

  let status_lexicon = create_status_lexicon()
  let assert Ok(_) =
    database.insert_lexicon(db, "xyz.statusphere.status", status_lexicon)

  let assert Ok(lexicon_records) = database.get_all_lexicons(db)
  let parsed_lexicons =
    lexicon_records
    |> list.filter_map(fn(lex) { lexicon_parser.parse_lexicon(lex.json) })

  let empty_fetcher = fn(_collection, _params) {
    Ok(#([], option.None, False, False, option.None))
  }

  let update_factory = option.Some(mock_update_resolver_factory)

  let assert Ok(built_schema) =
    db_schema_builder.build_schema_with_fetcher(
      parsed_lexicons,
      empty_fetcher,
      option.None,
      option.None,
      update_factory,
      option.None,
      option.None,
      option.None,
    )

  // Execute update mutation WITHOUT rkey (should fail at GraphQL validation level)
  let mutation =
    "mutation { updateXyzStatusphereStatus(input: { status: \"updated\", createdAt: \"2024-01-01T00:00:00Z\" }) { uri } }"

  let ctx_data = value.Object([#("auth_token", value.String("mock_token_123"))])
  let ctx = schema.context(option.Some(ctx_data))
  let assert Ok(response) = executor.execute(mutation, built_schema, ctx)

  // Should have errors (validation error for missing required argument)
  let error_count = list.length(response.errors)
  case error_count > 0 {
    True -> Nil
    False -> should.fail()
  }
}

// Test: Delete mutation without rkey should fail
pub fn delete_mutation_without_rkey_fails_test() {
  // Setup
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = database.create_lexicon_table(db)

  let status_lexicon = create_status_lexicon()
  let assert Ok(_) =
    database.insert_lexicon(db, "xyz.statusphere.status", status_lexicon)

  let assert Ok(lexicon_records) = database.get_all_lexicons(db)
  let parsed_lexicons =
    lexicon_records
    |> list.filter_map(fn(lex) { lexicon_parser.parse_lexicon(lex.json) })

  let empty_fetcher = fn(_collection, _params) {
    Ok(#([], option.None, False, False, option.None))
  }

  let delete_factory = option.Some(mock_delete_resolver_factory)

  let assert Ok(built_schema) =
    db_schema_builder.build_schema_with_fetcher(
      parsed_lexicons,
      empty_fetcher,
      option.None,
      option.None,
      option.None,
      delete_factory,
      option.None,
      option.None,
    )

  // Execute delete mutation WITHOUT rkey (should fail at GraphQL validation level)
  let mutation = "mutation { deleteXyzStatusphereStatus { uri } }"

  let ctx_data = value.Object([#("auth_token", value.String("mock_token_123"))])
  let ctx = schema.context(option.Some(ctx_data))
  let assert Ok(response) = executor.execute(mutation, built_schema, ctx)

  // Should have errors (validation error for missing required argument)
  let error_count = list.length(response.errors)
  case error_count > 0 {
    True -> Nil
    False -> should.fail()
  }
}

pub fn test_upload_blob_mutation_success() {
  // Parse a minimal lexicon (we just need a valid schema to test uploadBlob)
  let lexicon_json = create_status_lexicon()
  let assert Ok(lexicon) = lexicon_parser.parse_lexicon(lexicon_json)
  let parsed_lexicons = [lexicon]

  let empty_fetcher = fn(_collection, _params) {
    Ok(#([], option.None, False, False, option.None))
  }

  // Create a mock uploadBlob resolver factory
  let mock_upload_blob_resolver_factory = fn() {
    fn(_ctx) {
      // Return a mock BlobUploadResponse: { blob: { ... } }
      Ok(
        value.Object([
          #(
            "blob",
            value.Object([
              #("ref", value.String("bafyreiabc123xyz")),
              #("mimeType", value.String("image/jpeg")),
              #("size", value.Int(12_345)),
              #("did", value.String("did:plc:mockuser")),
            ]),
          ),
        ]),
      )
    }
  }

  let upload_blob_factory = option.Some(mock_upload_blob_resolver_factory)

  let assert Ok(built_schema) =
    db_schema_builder.build_schema_with_fetcher(
      parsed_lexicons,
      empty_fetcher,
      option.None,
      option.None,
      option.None,
      option.None,
      option.None,
      upload_blob_factory,
    )

  // Execute uploadBlob mutation WITH auth token
  // Note: Using a small base64 string for testing
  let mutation =
    "mutation { uploadBlob(data: \"SGVsbG8=\", mimeType: \"text/plain\") { blob { ref mimeType size } } }"

  let ctx_data = value.Object([#("auth_token", value.String("mock_token_123"))])
  let ctx = schema.context(option.Some(ctx_data))
  let assert Ok(response) = executor.execute(mutation, built_schema, ctx)

  // Should have no errors
  response.errors
  |> list.length
  |> should.equal(0)

  // Verify response data structure
  case response.data {
    value.Object(fields) -> {
      case list.key_find(fields, "uploadBlob") {
        Ok(value.Object(upload_response_fields)) -> {
          // Verify blob is nested under uploadBlob
          case list.key_find(upload_response_fields, "blob") {
            Ok(value.Object(blob_fields)) -> {
              // Verify blob fields
              case list.key_find(blob_fields, "ref") {
                Ok(value.String(ref)) -> ref |> should.equal("bafyreiabc123xyz")
                _ -> should.fail()
              }
              case list.key_find(blob_fields, "mimeType") {
                Ok(value.String(mime)) -> mime |> should.equal("image/jpeg")
                _ -> should.fail()
              }
              case list.key_find(blob_fields, "size") {
                Ok(value.Int(size)) -> size |> should.equal(12_345)
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

pub fn test_upload_blob_mutation_requires_auth() {
  // Parse a minimal lexicon
  let lexicon_json = create_status_lexicon()
  let assert Ok(lexicon) = lexicon_parser.parse_lexicon(lexicon_json)
  let parsed_lexicons = [lexicon]

  let empty_fetcher = fn(_collection, _params) {
    Ok(#([], option.None, False, False, option.None))
  }

  // Create a mock uploadBlob resolver factory that checks for auth
  let mock_upload_blob_resolver_factory = fn() {
    fn(_ctx) {
      Error("Authentication required. Please provide Authorization header.")
    }
  }

  let upload_blob_factory = option.Some(mock_upload_blob_resolver_factory)

  let assert Ok(built_schema) =
    db_schema_builder.build_schema_with_fetcher(
      parsed_lexicons,
      empty_fetcher,
      option.None,
      option.None,
      option.None,
      option.None,
      option.None,
      upload_blob_factory,
    )

  // Execute uploadBlob mutation WITHOUT auth token
  let mutation =
    "mutation { uploadBlob(data: \"SGVsbG8=\", mimeType: \"text/plain\") { blob { ref } } }"

  let ctx = schema.context(option.None)
  let assert Ok(response) = executor.execute(mutation, built_schema, ctx)

  // Should have errors
  let error_count = list.length(response.errors)
  case error_count > 0 {
    True -> {
      // Verify error message
      case response.errors {
        [error, ..] ->
          error.message
          |> should.equal(
            "Authentication required. Please provide Authorization header.",
          )
        _ -> should.fail()
      }
    }
    False -> should.fail()
  }
}
