import gleam/list
import gleam/string
import gleeunit
import gleeunit/should
import lexicon_graphql/input/connection
import swell/schema

pub fn main() {
  gleeunit.main()
}

pub fn ref_field_condition_has_only_is_null_test() {
  let condition_type = connection.build_ref_where_condition_input_type("Test")

  let fields = schema.get_input_fields(condition_type)

  // Should have exactly 1 field
  list.length(fields) |> should.equal(1)

  // That field should be isNull
  let has_is_null =
    fields
    |> list.any(fn(field) { schema.input_field_name(field) == "isNull" })

  has_is_null |> should.be_true
}

pub fn ref_field_condition_no_eq_operator_test() {
  let condition_type = connection.build_ref_where_condition_input_type("Test")

  let fields = schema.get_input_fields(condition_type)

  // Should NOT have eq field
  let has_eq =
    fields
    |> list.any(fn(field) { schema.input_field_name(field) == "eq" })

  has_eq |> should.be_false
}

pub fn where_input_with_ref_field_uses_ref_condition_test() {
  // Fields: text is primitive (False), reply is ref (True)
  let fields = [#("text", False), #("reply", True)]

  let where_input =
    connection.build_where_input_type_with_field_types("Test", fields)

  let input_fields = schema.get_input_fields(where_input)

  // Find the reply field
  let reply_field =
    list.find(input_fields, fn(field) {
      schema.input_field_name(field) == "reply"
    })

  case reply_field {
    Ok(field) -> {
      // The type name should contain "RefFieldCondition"
      let type_name = schema.type_name(schema.input_field_type(field))
      should.be_true(string.contains(type_name, "RefFieldCondition"))
    }
    Error(_) -> should.fail()
  }
}
