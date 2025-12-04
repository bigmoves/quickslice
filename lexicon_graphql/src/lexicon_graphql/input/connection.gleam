/// Lexicon-specific Connection Extensions
///
/// Adds sortBy support to the base Relay Connection specification
import gleam/list
import gleam/option.{None}
import swell/connection
import swell/schema

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
/// Creates a unique input type per collection (e.g., "SocialGrainGalleryItemSortFieldInput")
pub fn sort_field_input_type_with_enum(
  type_name: String,
  field_enum: schema.Type,
) -> schema.Type {
  let input_type_name = type_name <> "SortFieldInput"

  schema.input_object_type(
    input_type_name,
    "Specifies a field to sort by and its direction for " <> type_name,
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

/// Builds a WhereConditionInput type for filtering a specific field type
/// Supports: eq, in, contains, gt, gte, lt, lte, isNull operators
pub fn build_where_condition_input_type(
  type_name: String,
  field_type: schema.Type,
) -> schema.Type {
  let condition_type_name = type_name <> "FieldCondition"

  schema.input_object_type(
    condition_type_name,
    "Filter operators for " <> type_name <> " fields",
    [
      schema.input_field("eq", field_type, "Exact match (equals)", None),
      schema.input_field(
        "in",
        schema.list_type(schema.non_null(field_type)),
        "Match any value in the list",
        None,
      ),
      schema.input_field(
        "contains",
        schema.string_type(),
        "Case-insensitive substring match (string fields only)",
        None,
      ),
      schema.input_field("gt", field_type, "Greater than", None),
      schema.input_field("gte", field_type, "Greater than or equal to", None),
      schema.input_field("lt", field_type, "Less than", None),
      schema.input_field("lte", field_type, "Less than or equal to", None),
      schema.input_field(
        "isNull",
        schema.boolean_type(),
        "Filter for null or missing values (true) or non-null values (false)",
        None,
      ),
    ],
  )
}

/// Builds a WhereConditionInput type for ref fields (objects)
/// Only supports isNull operator - ref fields can only check for presence/absence
pub fn build_ref_where_condition_input_type(type_name: String) -> schema.Type {
  let condition_type_name = type_name <> "RefFieldCondition"

  schema.input_object_type(
    condition_type_name,
    "Filter for " <> type_name <> " reference fields (presence check only)",
    [
      schema.input_field(
        "isNull",
        schema.boolean_type(),
        "Filter for null (true) or non-null (false) values",
        None,
      ),
    ],
  )
}

/// Builds a WhereInput type with support for different field types
/// field_info is a list of (field_name, is_ref_field) tuples
pub fn build_where_input_type_with_field_types(
  type_name: String,
  field_info: List(#(String, Bool)),
) -> schema.Type {
  let where_input_name = type_name <> "WhereInput"

  // Build the condition types
  let string_condition_type =
    build_where_condition_input_type(type_name, schema.string_type())
  let ref_condition_type = build_ref_where_condition_input_type(type_name)

  // Build input fields - use ref condition for ref fields, string condition otherwise
  let field_input_fields =
    list.map(field_info, fn(info) {
      let #(field_name, is_ref) = info
      let condition_type = case is_ref {
        True -> ref_condition_type
        False -> string_condition_type
      }
      schema.input_field(
        field_name,
        condition_type,
        "Filter by " <> field_name,
        None,
      )
    })

  // Create placeholder type for recursive reference
  let where_input_type =
    schema.input_object_type(
      where_input_name,
      "Filter conditions for " <> type_name,
      field_input_fields,
    )

  // Add AND/OR fields
  let logic_fields = [
    schema.input_field(
      "and",
      schema.list_type(schema.non_null(where_input_type)),
      "All conditions must match (AND logic)",
      None,
    ),
    schema.input_field(
      "or",
      schema.list_type(schema.non_null(where_input_type)),
      "Any condition must match (OR logic)",
      None,
    ),
  ]

  schema.input_object_type(
    where_input_name,
    "Filter conditions for " <> type_name <> " with nested AND/OR support",
    list.append(field_input_fields, logic_fields),
  )
}

/// Builds a WhereInput type for a specific record type with all its fields
/// Includes recursive AND/OR support by creating the type in two passes
pub fn build_where_input_type(
  type_name: String,
  field_names: List(String),
) -> schema.Type {
  let where_input_name = type_name <> "WhereInput"

  // Build the string condition type (used for most fields)
  let string_condition_type =
    build_where_condition_input_type(type_name, schema.string_type())

  // Build input fields for each record field that can be filtered
  let field_input_fields =
    list.map(field_names, fn(field_name) {
      schema.input_field(
        field_name,
        string_condition_type,
        "Filter by " <> field_name,
        None,
      )
    })

  // Create a placeholder type to reference (this will be filled in later)
  // We need to create the type first, then add recursive references
  let where_input_type =
    schema.input_object_type(
      where_input_name,
      "Filter conditions for " <> type_name,
      field_input_fields,
    )

  // Add AND/OR fields that reference the type itself
  // Note: This creates a recursive type structure like Slice API does
  let logic_fields = [
    schema.input_field(
      "and",
      schema.list_type(schema.non_null(where_input_type)),
      "All conditions must match (AND logic)",
      None,
    ),
    schema.input_field(
      "or",
      schema.list_type(schema.non_null(where_input_type)),
      "Any condition must match (OR logic)",
      None,
    ),
  ]

  // Rebuild the type with all fields including the recursive AND/OR
  schema.input_object_type(
    where_input_name,
    "Filter conditions for " <> type_name <> " with nested AND/OR support",
    list.append(field_input_fields, logic_fields),
  )
}

/// Connection arguments with sortBy using a custom field enum and where filtering
pub fn lexicon_connection_args_with_field_enum_and_where(
  type_name: String,
  field_enum: schema.Type,
  where_input_type: schema.Type,
) -> List(schema.Argument) {
  list.flatten([
    connection.forward_pagination_args(),
    connection.backward_pagination_args(),
    [
      schema.argument(
        "sortBy",
        schema.list_type(
          schema.non_null(sort_field_input_type_with_enum(type_name, field_enum)),
        ),
        "Sort order for the connection",
        None,
      ),
      schema.argument(
        "where",
        where_input_type,
        "Filter conditions for the query",
        None,
      ),
    ],
  ])
}

/// Connection arguments with sortBy using a custom field enum (backward compatibility)
pub fn lexicon_connection_args_with_field_enum(
  type_name: String,
  field_enum: schema.Type,
) -> List(schema.Argument) {
  list.flatten([
    connection.forward_pagination_args(),
    connection.backward_pagination_args(),
    [
      schema.argument(
        "sortBy",
        schema.list_type(
          schema.non_null(sort_field_input_type_with_enum(type_name, field_enum)),
        ),
        "Sort order for the connection",
        None,
      ),
    ],
  ])
}
