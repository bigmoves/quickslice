/// Tests for isNull field in where condition schema
import gleam/list
import gleeunit
import gleeunit/should
import lexicon_graphql/input/connection
import swell/schema

pub fn main() {
  gleeunit.main()
}

pub fn where_condition_has_is_null_field_test() {
  // Build a where condition type
  let condition_type =
    connection.build_where_condition_input_type("Test", schema.string_type())

  // Get the input object fields using accessor
  let fields = schema.get_input_fields(condition_type)

  // Find the isNull field
  let has_is_null =
    fields
    |> list.any(fn(field) { schema.input_field_name(field) == "isNull" })

  has_is_null |> should.be_true
}
