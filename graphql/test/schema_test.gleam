/// Tests for GraphQL Schema (Type System)
///
/// GraphQL spec Section 3 - Type System
/// Defines types, fields, and schema structure
import gleam/option.{None}
import gleeunit/should
import graphql/schema
import graphql/value

// Type system tests
pub fn create_scalar_type_test() {
  let string_type = schema.string_type()
  should.equal(schema.type_name(string_type), "String")
}

pub fn create_object_type_test() {
  let user_type =
    schema.object_type("User", "A user in the system", [
      schema.field("id", schema.id_type(), "User ID", fn(_ctx) {
        Ok(value.String("123"))
      }),
      schema.field("name", schema.string_type(), "User name", fn(_ctx) {
        Ok(value.String("Alice"))
      }),
    ])

  should.equal(schema.type_name(user_type), "User")
}

pub fn create_non_null_type_test() {
  let non_null_string = schema.non_null(schema.string_type())
  should.be_true(schema.is_non_null(non_null_string))
}

pub fn create_list_type_test() {
  let list_of_strings = schema.list_type(schema.string_type())
  should.be_true(schema.is_list(list_of_strings))
}

pub fn create_schema_test() {
  let query_type =
    schema.object_type("Query", "Root query type", [
      schema.field("hello", schema.string_type(), "Hello field", fn(_ctx) {
        Ok(value.String("world"))
      }),
    ])

  let graphql_schema = schema.schema(query_type, None)
  should.equal(schema.query_type(graphql_schema), query_type)
}

pub fn field_with_arguments_test() {
  let user_field =
    schema.field_with_args(
      "user",
      schema.string_type(),
      "Get user by ID",
      [schema.argument("id", schema.id_type(), "User ID", None)],
      fn(_ctx) { Ok(value.String("Alice")) },
    )

  should.equal(schema.field_name(user_field), "user")
}

pub fn enum_type_test() {
  let role_enum =
    schema.enum_type("Role", "User role", [
      schema.enum_value("ADMIN", "Administrator"),
      schema.enum_value("USER", "Regular user"),
    ])

  should.equal(schema.type_name(role_enum), "Role")
}

pub fn scalar_types_exist_test() {
  // Built-in scalar types
  let _string = schema.string_type()
  let _int = schema.int_type()
  let _float = schema.float_type()
  let _boolean = schema.boolean_type()
  let _id = schema.id_type()

  should.be_true(True)
}
