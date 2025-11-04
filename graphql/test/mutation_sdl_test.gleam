/// Snapshot tests for mutation SDL generation
import birdie
import gleam/option.{None}
import gleeunit
import graphql/schema
import graphql/sdl
import graphql/value

pub fn main() {
  gleeunit.main()
}

pub fn simple_mutation_type_test() {
  let user_type =
    schema.object_type("User", "A user", [
      schema.field("id", schema.non_null(schema.id_type()), "User ID", fn(_) {
        Ok(value.String("1"))
      }),
      schema.field("name", schema.string_type(), "User name", fn(_) {
        Ok(value.String("Alice"))
      }),
    ])

  let mutation_type =
    schema.object_type("Mutation", "Root mutation type", [
      schema.field_with_args(
        "createUser",
        user_type,
        "Create a new user",
        [
          schema.argument(
            "name",
            schema.non_null(schema.string_type()),
            "User name",
            None,
          ),
        ],
        fn(_) { Ok(value.Null) },
      ),
    ])

  let serialized = sdl.print_type(mutation_type)

  birdie.snap(title: "Simple mutation type", content: serialized)
}

pub fn mutation_with_input_object_test() {
  let create_user_input =
    schema.input_object_type("CreateUserInput", "Input for creating a user", [
      schema.input_field(
        "name",
        schema.non_null(schema.string_type()),
        "User name",
        None,
      ),
      schema.input_field(
        "email",
        schema.non_null(schema.string_type()),
        "Email address",
        None,
      ),
      schema.input_field("age", schema.int_type(), "Age", None),
    ])

  let user_type =
    schema.object_type("User", "A user", [
      schema.field("id", schema.id_type(), "User ID", fn(_) { Ok(value.Null) }),
      schema.field("name", schema.string_type(), "User name", fn(_) {
        Ok(value.Null)
      }),
    ])

  let mutation_type =
    schema.object_type("Mutation", "Mutations", [
      schema.field_with_args(
        "createUser",
        user_type,
        "Create a new user",
        [
          schema.argument(
            "input",
            schema.non_null(create_user_input),
            "User data",
            None,
          ),
        ],
        fn(_) { Ok(value.Null) },
      ),
    ])

  let serialized =
    sdl.print_types([create_user_input, user_type, mutation_type])

  birdie.snap(title: "Mutation with input object argument", content: serialized)
}

pub fn multiple_mutations_test() {
  let user_type =
    schema.object_type("User", "A user", [
      schema.field("id", schema.id_type(), "User ID", fn(_) { Ok(value.Null) }),
    ])

  let delete_response =
    schema.object_type("DeleteResponse", "Delete response", [
      schema.field("success", schema.boolean_type(), "Success flag", fn(_) {
        Ok(value.Null)
      }),
    ])

  let mutation_type =
    schema.object_type("Mutation", "Mutations", [
      schema.field_with_args(
        "createUser",
        user_type,
        "Create a user",
        [schema.argument("name", schema.string_type(), "Name", None)],
        fn(_) { Ok(value.Null) },
      ),
      schema.field_with_args(
        "updateUser",
        user_type,
        "Update a user",
        [
          schema.argument(
            "id",
            schema.non_null(schema.id_type()),
            "User ID",
            None,
          ),
          schema.argument("name", schema.string_type(), "New name", None),
        ],
        fn(_) { Ok(value.Null) },
      ),
      schema.field_with_args(
        "deleteUser",
        delete_response,
        "Delete a user",
        [
          schema.argument(
            "id",
            schema.non_null(schema.id_type()),
            "User ID",
            None,
          ),
        ],
        fn(_) { Ok(value.Null) },
      ),
    ])

  let serialized = sdl.print_type(mutation_type)

  birdie.snap(
    title: "Multiple mutations (CRUD operations)",
    content: serialized,
  )
}

pub fn mutation_returning_list_test() {
  let user_type =
    schema.object_type("User", "A user", [
      schema.field("id", schema.id_type(), "User ID", fn(_) { Ok(value.Null) }),
    ])

  let mutation_type =
    schema.object_type("Mutation", "Mutations", [
      schema.field_with_args(
        "createUsers",
        schema.list_type(user_type),
        "Create multiple users",
        [
          schema.argument(
            "names",
            schema.list_type(schema.non_null(schema.string_type())),
            "User names",
            None,
          ),
        ],
        fn(_) { Ok(value.Null) },
      ),
    ])

  let serialized = sdl.print_type(mutation_type)

  birdie.snap(title: "Mutation returning list", content: serialized)
}

pub fn mutation_with_non_null_return_test() {
  let user_type =
    schema.object_type("User", "A user", [
      schema.field("id", schema.id_type(), "User ID", fn(_) { Ok(value.Null) }),
    ])

  let mutation_type =
    schema.object_type("Mutation", "Mutations", [
      schema.field_with_args(
        "createUser",
        schema.non_null(user_type),
        "Create a user (guaranteed to return)",
        [
          schema.argument(
            "name",
            schema.non_null(schema.string_type()),
            "User name",
            None,
          ),
        ],
        fn(_) { Ok(value.Null) },
      ),
    ])

  let serialized = sdl.print_type(mutation_type)

  birdie.snap(title: "Mutation with non-null return type", content: serialized)
}
