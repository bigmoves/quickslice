/// Tests for GraphQL Value types
///
/// GraphQL spec Section 2 - Language
/// Values can be: Null, Int, Float, String, Boolean, Enum, List, Object
import gleeunit/should
import graphql/value.{Boolean, Enum, Float, Int, List, Null, Object, String}

pub fn null_value_test() {
  let val = Null
  should.equal(val, Null)
}

pub fn int_value_test() {
  let val = Int(42)
  should.equal(val, Int(42))
}

pub fn float_value_test() {
  let val = Float(3.14)
  should.equal(val, Float(3.14))
}

pub fn string_value_test() {
  let val = String("hello")
  should.equal(val, String("hello"))
}

pub fn boolean_true_value_test() {
  let val = Boolean(True)
  should.equal(val, Boolean(True))
}

pub fn boolean_false_value_test() {
  let val = Boolean(False)
  should.equal(val, Boolean(False))
}

pub fn enum_value_test() {
  let val = Enum("ACTIVE")
  should.equal(val, Enum("ACTIVE"))
}

pub fn empty_list_value_test() {
  let val = List([])
  should.equal(val, List([]))
}

pub fn list_of_ints_test() {
  let val = List([Int(1), Int(2), Int(3)])
  should.equal(val, List([Int(1), Int(2), Int(3)]))
}

pub fn nested_list_test() {
  let val = List([List([Int(1), Int(2)]), List([Int(3), Int(4)])])
  should.equal(val, List([List([Int(1), Int(2)]), List([Int(3), Int(4)])]))
}

pub fn empty_object_test() {
  let val = Object([])
  should.equal(val, Object([]))
}

pub fn simple_object_test() {
  let val = Object([#("name", String("Alice")), #("age", Int(30))])
  should.equal(val, Object([#("name", String("Alice")), #("age", Int(30))]))
}

pub fn nested_object_test() {
  let val =
    Object([
      #("user", Object([#("name", String("Bob")), #("active", Boolean(True))])),
      #("count", Int(5)),
    ])

  should.equal(
    val,
    Object([
      #("user", Object([#("name", String("Bob")), #("active", Boolean(True))])),
      #("count", Int(5)),
    ]),
  )
}

pub fn mixed_types_list_test() {
  let val = List([String("hello"), Int(42), Boolean(True), Null])
  should.equal(val, List([String("hello"), Int(42), Boolean(True), Null]))
}
