/// Tests for mutation execution
import birdie
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit
import gleeunit/should
import graphql/executor
import graphql/schema
import graphql/value

pub fn main() {
  gleeunit.main()
}

fn format_response(response: executor.Response) -> String {
  string.inspect(response)
}

fn test_schema_with_mutations() -> schema.Schema {
  let user_type =
    schema.object_type("User", "A user", [
      schema.field("id", schema.id_type(), "User ID", fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case fields |> list.key_find("id") {
              Ok(id) -> Ok(id)
              Error(_) -> Ok(value.String("123"))
            }
          }
          _ -> Ok(value.String("123"))
        }
      }),
      schema.field("name", schema.string_type(), "User name", fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case fields |> list.key_find("name") {
              Ok(name) -> Ok(name)
              Error(_) -> Ok(value.String("Unknown"))
            }
          }
          _ -> Ok(value.String("Unknown"))
        }
      }),
    ])

  let query_type =
    schema.object_type("Query", "Root query", [
      schema.field("dummy", schema.string_type(), "Dummy field", fn(_) {
        Ok(value.String("dummy"))
      }),
    ])

  let mutation_type =
    schema.object_type("Mutation", "Root mutation", [
      schema.field_with_args(
        "createUser",
        user_type,
        "Create a user",
        [schema.argument("name", schema.string_type(), "User name", None)],
        fn(ctx) {
          case schema.get_argument(ctx, "name") {
            Some(value.String(name)) ->
              Ok(
                value.Object([
                  #("id", value.String("123")),
                  #("name", value.String(name)),
                ]),
              )
            _ ->
              Ok(
                value.Object([
                  #("id", value.String("123")),
                  #("name", value.String("Default Name")),
                ]),
              )
          }
        },
      ),
      schema.field_with_args(
        "deleteUser",
        schema.boolean_type(),
        "Delete a user",
        [
          schema.argument(
            "id",
            schema.non_null(schema.id_type()),
            "User ID",
            None,
          ),
        ],
        fn(_) { Ok(value.Boolean(True)) },
      ),
    ])

  schema.schema(query_type, Some(mutation_type))
}

pub fn execute_simple_mutation_test() {
  let schema = test_schema_with_mutations()
  let query = "mutation { createUser(name: \"Alice\") { id name } }"

  let result = executor.execute(query, schema, schema.context(None))

  let response = case result {
    Ok(r) -> r
    Error(_) -> panic as "Execution failed"
  }

  birdie.snap(
    title: "Execute simple mutation",
    content: format_response(response),
  )
}

pub fn execute_named_mutation_test() {
  let schema = test_schema_with_mutations()
  let query = "mutation CreateUser { createUser(name: \"Bob\") { id name } }"

  let result = executor.execute(query, schema, schema.context(None))

  should.be_ok(result)
}

pub fn execute_multiple_mutations_test() {
  let schema = test_schema_with_mutations()
  let query =
    "
    mutation {
      createUser(name: \"Alice\") { id name }
      deleteUser(id: \"123\")
    }
  "

  let result = executor.execute(query, schema, schema.context(None))

  let response = case result {
    Ok(r) -> r
    Error(_) -> panic as "Execution failed"
  }

  birdie.snap(
    title: "Execute multiple mutations",
    content: format_response(response),
  )
}

pub fn execute_mutation_without_argument_test() {
  let schema = test_schema_with_mutations()
  let query = "mutation { createUser { id name } }"

  let result = executor.execute(query, schema, schema.context(None))

  should.be_ok(result)
}

pub fn execute_mutation_with_context_test() {
  let schema = test_schema_with_mutations()
  let query = "mutation { createUser(name: \"Context User\") { id name } }"

  let ctx_data =
    value.Object([
      #("userId", value.String("456")),
      #("token", value.String("abc123")),
    ])
  let ctx = schema.context(Some(ctx_data))

  let result = executor.execute(query, schema, ctx)

  should.be_ok(result)
}
