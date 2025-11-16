import gleam/dict
import gleam/option
import gleeunit
import gleeunit/should
import lexicon_graphql/aggregate_input
import lexicon_graphql/db_schema_builder
import lexicon_graphql/types

pub fn main() {
  gleeunit.main()
}

// ===== Tests for property_to_field_type() =====

pub fn property_to_field_type_datetime_test() {
  let property =
    types.Property(
      type_: "string",
      required: True,
      format: option.Some("datetime"),
      ref: option.None,
    )

  db_schema_builder.property_to_field_type(property)
  |> should.equal(db_schema_builder.DateTimeField)
}

pub fn property_to_field_type_string_test() {
  let property =
    types.Property(
      type_: "string",
      required: True,
      format: option.None,
      ref: option.None,
    )

  db_schema_builder.property_to_field_type(property)
  |> should.equal(db_schema_builder.StringField)
}

pub fn property_to_field_type_integer_test() {
  let property =
    types.Property(
      type_: "integer",
      required: True,
      format: option.None,
      ref: option.None,
    )

  db_schema_builder.property_to_field_type(property)
  |> should.equal(db_schema_builder.IntField)
}

pub fn property_to_field_type_number_test() {
  let property =
    types.Property(
      type_: "number",
      required: True,
      format: option.None,
      ref: option.None,
    )

  db_schema_builder.property_to_field_type(property)
  |> should.equal(db_schema_builder.NumberField)
}

pub fn property_to_field_type_boolean_test() {
  let property =
    types.Property(
      type_: "boolean",
      required: True,
      format: option.None,
      ref: option.None,
    )

  db_schema_builder.property_to_field_type(property)
  |> should.equal(db_schema_builder.BoolField)
}

pub fn property_to_field_type_array_test() {
  let property =
    types.Property(
      type_: "array",
      required: True,
      format: option.None,
      ref: option.None,
    )

  db_schema_builder.property_to_field_type(property)
  |> should.equal(db_schema_builder.ArrayField)
}

pub fn property_to_field_type_blob_test() {
  let property =
    types.Property(type_: "blob", required: True, format: option.None, ref: option.None)

  db_schema_builder.property_to_field_type(property)
  |> should.equal(db_schema_builder.BlobField)
}

pub fn property_to_field_type_ref_test() {
  let property =
    types.Property(
      type_: "ref",
      required: True,
      format: option.None,
      ref: option.Some("app.bsky.actor.defs#profileView"),
    )

  db_schema_builder.property_to_field_type(property)
  |> should.equal(db_schema_builder.RefField)
}

// ===== Tests for build_field_type_map() =====

pub fn build_field_type_map_test() {
  let properties = [
    #(
      "createdAt",
      types.Property(
        type_: "string",
        required: True,
        format: option.Some("datetime"),
        ref: option.None,
      ),
    ),
    #(
      "trackName",
      types.Property(
        type_: "string",
        required: True,
        format: option.None,
        ref: option.None,
      ),
    ),
    #(
      "playCount",
      types.Property(
        type_: "integer",
        required: False,
        format: option.None,
        ref: option.None,
      ),
    ),
    #(
      "artists",
      types.Property(
        type_: "array",
        required: True,
        format: option.None,
        ref: option.None,
      ),
    ),
  ]

  let field_types = db_schema_builder.build_field_type_map(properties)

  // Check datetime field
  dict.get(field_types, "createdAt")
  |> should.be_ok
  |> should.equal(db_schema_builder.DateTimeField)

  // Check string field
  dict.get(field_types, "trackName")
  |> should.be_ok
  |> should.equal(db_schema_builder.StringField)

  // Check integer field
  dict.get(field_types, "playCount")
  |> should.be_ok
  |> should.equal(db_schema_builder.IntField)

  // Check array field
  dict.get(field_types, "artists")
  |> should.be_ok
  |> should.equal(db_schema_builder.ArrayField)

  // Check standard field is included
  dict.get(field_types, "indexedAt")
  |> should.be_ok
  |> should.equal(db_schema_builder.DateTimeField)
}

// ===== Tests for validate_interval_fields() =====

pub fn validate_interval_fields_valid_datetime_test() {
  let field_types =
    dict.from_list([
      #("indexedAt", db_schema_builder.DateTimeField),
      #("trackName", db_schema_builder.StringField),
    ])

  let group_by = [
    aggregate_input.GroupByFieldInput(
      field: "indexedAt",
      interval: option.Some(aggregate_input.Month),
    ),
  ]

  db_schema_builder.validate_interval_fields(group_by, field_types)
  |> should.be_ok
}

pub fn validate_interval_fields_invalid_string_field_test() {
  let field_types =
    dict.from_list([
      #("indexedAt", db_schema_builder.DateTimeField),
      #("trackName", db_schema_builder.StringField),
    ])

  let group_by = [
    aggregate_input.GroupByFieldInput(
      field: "trackName",
      interval: option.Some(aggregate_input.Month),
    ),
  ]

  db_schema_builder.validate_interval_fields(group_by, field_types)
  |> should.be_error
  |> should.equal(
    "Cannot apply date interval to field 'trackName': field is string, not datetime",
  )
}

pub fn validate_interval_fields_invalid_array_field_test() {
  let field_types =
    dict.from_list([
      #("artists", db_schema_builder.ArrayField),
      #("indexedAt", db_schema_builder.DateTimeField),
    ])

  let group_by = [
    aggregate_input.GroupByFieldInput(
      field: "artists",
      interval: option.Some(aggregate_input.Day),
    ),
  ]

  db_schema_builder.validate_interval_fields(group_by, field_types)
  |> should.be_error
  |> should.equal(
    "Cannot apply date interval to field 'artists': field is array, not datetime",
  )
}

pub fn validate_interval_fields_no_interval_any_field_test() {
  let field_types =
    dict.from_list([
      #("trackName", db_schema_builder.StringField),
      #("artists", db_schema_builder.ArrayField),
    ])

  let group_by = [
    aggregate_input.GroupByFieldInput(field: "trackName", interval: option.None),
    aggregate_input.GroupByFieldInput(field: "artists", interval: option.None),
  ]

  db_schema_builder.validate_interval_fields(group_by, field_types)
  |> should.be_ok
}

pub fn validate_interval_fields_mixed_valid_invalid_test() {
  let field_types =
    dict.from_list([
      #("indexedAt", db_schema_builder.DateTimeField),
      #("trackName", db_schema_builder.StringField),
    ])

  let group_by = [
    aggregate_input.GroupByFieldInput(
      field: "indexedAt",
      interval: option.Some(aggregate_input.Month),
    ),
    aggregate_input.GroupByFieldInput(
      field: "trackName",
      interval: option.Some(aggregate_input.Day),
    ),
  ]

  // Should fail on the second field
  db_schema_builder.validate_interval_fields(group_by, field_types)
  |> should.be_error
  |> should.equal(
    "Cannot apply date interval to field 'trackName': field is string, not datetime",
  )
}

pub fn validate_interval_fields_unknown_field_test() {
  let field_types =
    dict.from_list([#("indexedAt", db_schema_builder.DateTimeField)])

  let group_by = [
    aggregate_input.GroupByFieldInput(
      field: "unknownField",
      interval: option.Some(aggregate_input.Month),
    ),
  ]

  db_schema_builder.validate_interval_fields(group_by, field_types)
  |> should.be_error
  |> should.equal("Field 'unknownField' not found in schema")
}

// ===== Tests for validate_query_complexity() =====

pub fn validate_query_complexity_valid_test() {
  let group_by = [
    aggregate_input.GroupByFieldInput(field: "field1", interval: option.None),
    aggregate_input.GroupByFieldInput(field: "field2", interval: option.None),
    aggregate_input.GroupByFieldInput(field: "field3", interval: option.None),
  ]

  db_schema_builder.validate_query_complexity(group_by)
  |> should.be_ok
}

pub fn validate_query_complexity_max_fields_test() {
  let group_by = [
    aggregate_input.GroupByFieldInput(field: "field1", interval: option.None),
    aggregate_input.GroupByFieldInput(field: "field2", interval: option.None),
    aggregate_input.GroupByFieldInput(field: "field3", interval: option.None),
    aggregate_input.GroupByFieldInput(field: "field4", interval: option.None),
    aggregate_input.GroupByFieldInput(field: "field5", interval: option.None),
  ]

  db_schema_builder.validate_query_complexity(group_by)
  |> should.be_ok
}

pub fn validate_query_complexity_too_many_fields_test() {
  let group_by = [
    aggregate_input.GroupByFieldInput(field: "field1", interval: option.None),
    aggregate_input.GroupByFieldInput(field: "field2", interval: option.None),
    aggregate_input.GroupByFieldInput(field: "field3", interval: option.None),
    aggregate_input.GroupByFieldInput(field: "field4", interval: option.None),
    aggregate_input.GroupByFieldInput(field: "field5", interval: option.None),
    aggregate_input.GroupByFieldInput(field: "field6", interval: option.None),
  ]

  db_schema_builder.validate_query_complexity(group_by)
  |> should.be_error
  |> should.equal("Query too complex: maximum 5 group by fields allowed (got 6)")
}

pub fn validate_query_complexity_empty_test() {
  let group_by = []

  db_schema_builder.validate_query_complexity(group_by)
  |> should.be_error
  |> should.equal("Query must include at least 1 group by field")
}
