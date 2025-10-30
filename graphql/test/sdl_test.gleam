/// Snapshot tests for SDL generation
///
/// Verifies that GraphQL types are correctly serialized to SDL format

import birdie
import gleam/option.{None, Some}
import gleeunit
import graphql/schema
import graphql/sdl
import graphql/value

pub fn main() {
  gleeunit.main()
}

// ===== Input Object Types =====

pub fn simple_input_object_test() {
  let input_type =
    schema.input_object_type(
      "UserInput",
      "Input for creating or updating a user",
      [
        schema.input_field("name", schema.string_type(), "User's name", None),
        schema.input_field(
          "email",
          schema.non_null(schema.string_type()),
          "User's email address",
          None,
        ),
        schema.input_field("age", schema.int_type(), "User's age", None),
      ],
    )

  let serialized = sdl.print_type(input_type)

  birdie.snap(title: "Simple input object with descriptions", content: serialized)
}

pub fn input_object_with_default_values_test() {
  let input_type =
    schema.input_object_type(
      "FilterInput",
      "Filter options for queries",
      [
        schema.input_field(
          "limit",
          schema.int_type(),
          "Maximum number of results",
          Some(value.Int(10)),
        ),
        schema.input_field(
          "offset",
          schema.int_type(),
          "Number of results to skip",
          Some(value.Int(0)),
        ),
      ],
    )

  let serialized = sdl.print_type(input_type)

  birdie.snap(
    title: "Input object with default values",
    content: serialized,
  )
}

pub fn nested_input_types_test() {
  let address_input =
    schema.input_object_type(
      "AddressInput",
      "Street address information",
      [
        schema.input_field("street", schema.string_type(), "Street name", None),
        schema.input_field("city", schema.string_type(), "City name", None),
      ],
    )

  let user_input =
    schema.input_object_type("UserInput", "User information", [
      schema.input_field("name", schema.string_type(), "Full name", None),
      schema.input_field("address", address_input, "Home address", None),
    ])

  let serialized = sdl.print_types([address_input, user_input])

  birdie.snap(title: "Nested input types", content: serialized)
}

// ===== Object Types =====

pub fn simple_object_type_test() {
  let user_type =
    schema.object_type("User", "A user in the system", [
      schema.field("id", schema.non_null(schema.id_type()), "User ID", fn(
        _ctx,
      ) { Ok(value.String("1")) }),
      schema.field("name", schema.string_type(), "User's name", fn(_ctx) {
        Ok(value.String("Alice"))
      }),
      schema.field("email", schema.string_type(), "Email address", fn(_ctx) {
        Ok(value.String("alice@example.com"))
      }),
    ])

  let serialized = sdl.print_type(user_type)

  birdie.snap(title: "Simple object type", content: serialized)
}

pub fn object_with_list_fields_test() {
  let post_type =
    schema.object_type("Post", "A blog post", [
      schema.field("id", schema.id_type(), "Post ID", fn(_ctx) {
        Ok(value.String("1"))
      }),
      schema.field("title", schema.string_type(), "Post title", fn(_ctx) {
        Ok(value.String("Hello"))
      }),
      schema.field(
        "tags",
        schema.list_type(schema.non_null(schema.string_type())),
        "Post tags",
        fn(_ctx) { Ok(value.List([])) },
      ),
    ])

  let serialized = sdl.print_type(post_type)

  birdie.snap(title: "Object type with list fields", content: serialized)
}

// ===== Enum Types =====

pub fn simple_enum_test() {
  let status_enum =
    schema.enum_type("Status", "Order status", [
      schema.enum_value("PENDING", "Order is pending"),
      schema.enum_value("PROCESSING", "Order is being processed"),
      schema.enum_value("SHIPPED", "Order has been shipped"),
      schema.enum_value("DELIVERED", "Order has been delivered"),
    ])

  let serialized = sdl.print_type(status_enum)

  birdie.snap(title: "Simple enum type", content: serialized)
}

pub fn enum_without_descriptions_test() {
  let color_enum =
    schema.enum_type("Color", "", [
      schema.enum_value("RED", ""),
      schema.enum_value("GREEN", ""),
      schema.enum_value("BLUE", ""),
    ])

  let serialized = sdl.print_type(color_enum)

  birdie.snap(title: "Enum without descriptions", content: serialized)
}

// ===== Scalar Types =====

pub fn built_in_scalars_test() {
  let scalars = [
    schema.string_type(),
    schema.int_type(),
    schema.float_type(),
    schema.boolean_type(),
    schema.id_type(),
  ]

  let serialized = sdl.print_types(scalars)

  birdie.snap(title: "Built-in scalar types", content: serialized)
}

// ===== Complex Types =====

pub fn type_with_non_null_and_list_test() {
  let input_type =
    schema.input_object_type("ComplexInput", "Complex type modifiers", [
      schema.input_field(
        "required",
        schema.non_null(schema.string_type()),
        "Required string",
        None,
      ),
      schema.input_field(
        "optionalList",
        schema.list_type(schema.string_type()),
        "Optional list of strings",
        None,
      ),
      schema.input_field(
        "requiredList",
        schema.non_null(schema.list_type(schema.string_type())),
        "Required list of optional strings",
        None,
      ),
      schema.input_field(
        "listOfRequired",
        schema.list_type(schema.non_null(schema.string_type())),
        "Optional list of required strings",
        None,
      ),
      schema.input_field(
        "requiredListOfRequired",
        schema.non_null(schema.list_type(schema.non_null(schema.string_type()))),
        "Required list of required strings",
        None,
      ),
    ])

  let serialized = sdl.print_type(input_type)

  birdie.snap(title: "Type with NonNull and List modifiers", content: serialized)
}

// ===== Multiple Related Types =====

pub fn related_types_test() {
  let sort_direction =
    schema.enum_type("SortDirection", "Sort direction for queries", [
      schema.enum_value("ASC", "Ascending order"),
      schema.enum_value("DESC", "Descending order"),
    ])

  let sort_field_enum =
    schema.enum_type("UserSortField", "Fields to sort users by", [
      schema.enum_value("NAME", "Sort by name"),
      schema.enum_value("EMAIL", "Sort by email"),
      schema.enum_value("CREATED_AT", "Sort by creation date"),
    ])

  let sort_input =
    schema.input_object_type("SortInput", "Sort configuration", [
      schema.input_field(
        "field",
        schema.non_null(sort_field_enum),
        "Field to sort by",
        None,
      ),
      schema.input_field(
        "direction",
        sort_direction,
        "Sort direction",
        Some(value.String("ASC")),
      ),
    ])

  let serialized = sdl.print_types([sort_direction, sort_field_enum, sort_input])

  birdie.snap(title: "Multiple related types", content: serialized)
}

// ===== Empty Types (Edge Cases) =====

pub fn empty_input_object_test() {
  let empty_input = schema.input_object_type("EmptyInput", "An empty input", [])

  let serialized = sdl.print_type(empty_input)

  birdie.snap(title: "Empty input object", content: serialized)
}

pub fn empty_enum_test() {
  let empty_enum = schema.enum_type("EmptyEnum", "An empty enum", [])

  let serialized = sdl.print_type(empty_enum)

  birdie.snap(title: "Empty enum", content: serialized)
}
