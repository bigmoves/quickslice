import database
import gleam/json
import gleam/list
import gleam/option
import gleeunit/should
import graphql/schema
import lexicon_graphql/db_schema_builder
import lexicon_graphql/lexicon_parser
import sqlight

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

pub fn test_create_mutation_generates_correct_schema_test() {
  // Setup: Create in-memory database with test lexicons
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = database.create_lexicon_table(db)

  // Insert xyz.statusphere.status lexicon
  let status_lexicon = create_status_lexicon()

  let assert Ok(_) =
    database.insert_lexicon(db, "xyz.statusphere.status", status_lexicon)

  // Get lexicons and parse them
  let assert Ok(lexicon_records) = database.get_all_lexicons(db)
  let parsed_lexicons =
    lexicon_records
    |> list.filter_map(fn(lex) { lexicon_parser.parse_lexicon(lex.json) })

  // Build schema with empty fetcher
  let empty_fetcher = fn(_collection, _params) {
    Ok(#([], option.None, False, False, option.None))
  }

  let assert Ok(built_schema) =
    db_schema_builder.build_schema_with_fetcher(parsed_lexicons, empty_fetcher, option.None, option.None, option.None)

  // Test: Verify that the schema has mutation type
  let mutation_type_option = schema.get_mutation_type(built_schema)

  // Assert that mutation type exists
  mutation_type_option
  |> should.be_some()

  // Verify mutation type has expected fields
  let assert option.Some(mutation_type) = mutation_type_option
  let mutation_fields = schema.get_fields(mutation_type)

  // Check that we have 3 mutations per record type (create, update, delete)
  // For xyz.statusphere.status, we expect:
  // - createXyzStatusphereStatus
  // - updateXyzStatusphereStatus
  // - deleteXyzStatusphereStatus
  let field_names =
    mutation_fields
    |> list.map(schema.field_name)

  field_names
  |> should.equal([
    "createXyzStatusphereStatus",
    "updateXyzStatusphereStatus",
    "deleteXyzStatusphereStatus",
  ])
}

pub fn test_create_mutation_field_signature_test() {
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
  let assert Ok(built_schema) =
    db_schema_builder.build_schema_with_fetcher(parsed_lexicons, empty_fetcher, option.None, option.None, option.None)

  // Get mutation type and find createXyzStatusphereStatus field
  let assert option.Some(mutation_type) = schema.get_mutation_type(built_schema)
  let assert option.Some(create_field) =
    schema.get_field(mutation_type, "createXyzStatusphereStatus")

  // Verify arguments
  let arguments = schema.field_arguments(create_field)
  let arg_names = list.map(arguments, schema.argument_name)

  // Should have 'input' and 'rkey' arguments
  arg_names
  |> should.equal(["input", "rkey"])

  // Verify input argument is non-null
  let assert Ok(input_arg) =
    list.find(arguments, fn(arg) { schema.argument_name(arg) == "input" })
  let input_type = schema.argument_type(input_arg)

  schema.is_non_null(input_type)
  |> should.be_true()

  // Verify rkey argument is nullable (optional)
  let assert Ok(rkey_arg) =
    list.find(arguments, fn(arg) { schema.argument_name(arg) == "rkey" })
  let rkey_type = schema.argument_type(rkey_arg)

  schema.is_non_null(rkey_type)
  |> should.be_false()
}

pub fn test_update_mutation_field_signature_test() {
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
  let assert Ok(built_schema) =
    db_schema_builder.build_schema_with_fetcher(parsed_lexicons, empty_fetcher, option.None, option.None, option.None)

  // Get mutation type and find updateXyzStatusphereStatus field
  let assert option.Some(mutation_type) = schema.get_mutation_type(built_schema)
  let assert option.Some(update_field) =
    schema.get_field(mutation_type, "updateXyzStatusphereStatus")

  // Verify arguments
  let arguments = schema.field_arguments(update_field)
  let arg_names = list.map(arguments, schema.argument_name)

  // Should have 'rkey' and 'input' arguments
  arg_names
  |> should.equal(["rkey", "input"])

  // Both should be non-null (required)
  let assert Ok(rkey_arg) =
    list.find(arguments, fn(arg) { schema.argument_name(arg) == "rkey" })
  let rkey_type = schema.argument_type(rkey_arg)

  schema.is_non_null(rkey_type)
  |> should.be_true()

  let assert Ok(input_arg) =
    list.find(arguments, fn(arg) { schema.argument_name(arg) == "input" })
  let input_type = schema.argument_type(input_arg)

  schema.is_non_null(input_type)
  |> should.be_true()
}

pub fn test_delete_mutation_field_signature_test() {
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
  let assert Ok(built_schema) =
    db_schema_builder.build_schema_with_fetcher(parsed_lexicons, empty_fetcher, option.None, option.None, option.None)

  // Get mutation type and find deleteXyzStatusphereStatus field
  let assert option.Some(mutation_type) = schema.get_mutation_type(built_schema)
  let assert option.Some(delete_field) =
    schema.get_field(mutation_type, "deleteXyzStatusphereStatus")

  // Verify arguments
  let arguments = schema.field_arguments(delete_field)
  let arg_names = list.map(arguments, schema.argument_name)

  // Should have only 'rkey' argument
  arg_names
  |> should.equal(["rkey"])

  // Should be non-null (required)
  let assert Ok(rkey_arg) =
    list.find(arguments, fn(arg) { schema.argument_name(arg) == "rkey" })
  let rkey_type = schema.argument_type(rkey_arg)

  schema.is_non_null(rkey_type)
  |> should.be_true()
}
