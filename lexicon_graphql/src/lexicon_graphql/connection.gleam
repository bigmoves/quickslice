/// Lexicon-specific Connection Extensions
///
/// Adds sortBy support to the base Relay Connection specification
import gleam/list
import gleam/option.{None}
import graphql/connection
import graphql/schema

/// SortDirection enum type for lexicon queries
pub fn sort_direction_enum() -> schema.Type {
  schema.enum_type("SortDirection", "Sort direction for query results", [
    schema.enum_value("ASC", "Ascending order"),
    schema.enum_value("DESC", "Descending order"),
  ])
}

/// SortFieldInput type for specifying sort order
pub fn sort_field_input_type() -> schema.Type {
  schema.input_object_type(
    "SortFieldInput",
    "Specifies a field to sort by and its direction",
    [
      schema.input_field(
        "field",
        schema.non_null(schema.string_type()),
        "Field name to sort by (e.g. 'indexed_at', 'createdAt', or nested like 'author.name')",
        None,
      ),
      schema.input_field(
        "direction",
        schema.non_null(sort_direction_enum()),
        "Sort direction (ASC or DESC)",
        None,
      ),
    ],
  )
}

/// Connection arguments with sortBy support for lexicon queries
/// Extends the base Relay connection args with custom sortBy
pub fn lexicon_connection_args() -> List(schema.Argument) {
  list.flatten([
    connection.forward_pagination_args(),
    connection.backward_pagination_args(),
    [
      schema.argument(
        "sortBy",
        schema.list_type(schema.non_null(sort_field_input_type())),
        "Sort order for the connection. Defaults to [{field: \"indexed_at\", direction: DESC}]",
        None,
      ),
    ],
  ])
}

/// SortFieldInput type with a custom field enum
pub fn sort_field_input_type_with_enum(field_enum: schema.Type) -> schema.Type {
  schema.input_object_type(
    "SortFieldInput",
    "Specifies a field to sort by and its direction",
    [
      schema.input_field(
        "field",
        schema.non_null(field_enum),
        "Field to sort by",
        None,
      ),
      schema.input_field(
        "direction",
        schema.non_null(sort_direction_enum()),
        "Sort direction (ASC or DESC)",
        None,
      ),
    ],
  )
}

/// Connection arguments with sortBy using a custom field enum
pub fn lexicon_connection_args_with_field_enum(
  field_enum: schema.Type,
) -> List(schema.Argument) {
  list.flatten([
    connection.forward_pagination_args(),
    connection.backward_pagination_args(),
    [
      schema.argument(
        "sortBy",
        schema.list_type(schema.non_null(
          sort_field_input_type_with_enum(field_enum),
        )),
        "Sort order for the connection",
        None,
      ),
    ],
  ])
}
