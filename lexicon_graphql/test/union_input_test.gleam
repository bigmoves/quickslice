/// Tests for Union Input
///
/// Tests the transformation of GraphQL discriminated union inputs
/// to AT Protocol $type format.
import gleeunit/should
import lexicon_graphql/input/union as union_input
import swell/value

// ─── transform_union_object Tests ──────────────────────────────────

pub fn transform_union_object_with_enum_type_test() {
  // GraphQL input format with enum discriminator
  let fields = [
    #("type", value.Enum("SELF_LABELS")),
    #(
      "selfLabels",
      value.Object([
        #("values", value.List([value.String("!no-unauthenticated")])),
      ]),
    ),
  ]
  let refs = ["com.atproto.label.defs#selfLabels"]

  let result = union_input.transform_union_object(fields, refs)

  // Should transform to AT Protocol format
  result
  |> should.equal(
    value.Object([
      #("$type", value.String("com.atproto.label.defs#selfLabels")),
      #("values", value.List([value.String("!no-unauthenticated")])),
    ]),
  )
}

pub fn transform_union_object_with_string_type_test() {
  // GraphQL input format with string discriminator (fallback)
  let fields = [
    #("type", value.String("SELF_LABELS")),
    #("selfLabels", value.Object([#("values", value.List([]))])),
  ]
  let refs = ["com.atproto.label.defs#selfLabels"]

  let result = union_input.transform_union_object(fields, refs)

  result
  |> should.equal(
    value.Object([
      #("$type", value.String("com.atproto.label.defs#selfLabels")),
      #("values", value.List([])),
    ]),
  )
}

pub fn transform_union_object_multi_variant_test() {
  // Test with multiple possible refs
  let fields = [
    #("type", value.Enum("OTHER_LABELS")),
    #("otherLabels", value.Object([#("data", value.String("test"))])),
    #("selfLabels", value.Null),
  ]
  let refs = [
    "com.atproto.label.defs#selfLabels",
    "com.example.defs#otherLabels",
  ]

  let result = union_input.transform_union_object(fields, refs)

  result
  |> should.equal(
    value.Object([
      #("$type", value.String("com.example.defs#otherLabels")),
      #("data", value.String("test")),
    ]),
  )
}

pub fn transform_union_object_no_variant_data_test() {
  // When variant field is missing or not an object
  let fields = [#("type", value.Enum("SELF_LABELS"))]
  let refs = ["com.atproto.label.defs#selfLabels"]

  let result = union_input.transform_union_object(fields, refs)

  // Should still output $type
  result
  |> should.equal(
    value.Object([#("$type", value.String("com.atproto.label.defs#selfLabels"))]),
  )
}

pub fn transform_union_object_unknown_type_test() {
  // When type doesn't match any ref
  let fields = [
    #("type", value.Enum("UNKNOWN_TYPE")),
    #("someData", value.String("test")),
  ]
  let refs = ["com.atproto.label.defs#selfLabels"]

  let result = union_input.transform_union_object(fields, refs)

  // Should return original fields unchanged
  result
  |> should.equal(value.Object(fields))
}

pub fn transform_union_object_no_type_field_test() {
  // When there's no type discriminator
  let fields = [#("someField", value.String("value"))]
  let refs = ["com.atproto.label.defs#selfLabels"]

  let result = union_input.transform_union_object(fields, refs)

  // Should return original fields unchanged
  result
  |> should.equal(value.Object(fields))
}

pub fn transform_union_object_with_numbers_in_name_test() {
  // Test edge case with numbers in variant name
  let fields = [
    #("type", value.Enum("OAUTH2_CLIENT")),
    #("oauth2Client", value.Object([#("id", value.String("client123"))])),
  ]
  let refs = ["com.example.oauth#oauth2Client"]

  let result = union_input.transform_union_object(fields, refs)

  result
  |> should.equal(
    value.Object([
      #("$type", value.String("com.example.oauth#oauth2Client")),
      #("id", value.String("client123")),
    ]),
  )
}
