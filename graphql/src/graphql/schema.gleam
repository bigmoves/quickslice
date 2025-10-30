/// GraphQL Schema - Type System
///
/// Per GraphQL spec Section 3 - Type System
/// Defines the type system including scalars, objects, enums, etc.
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None}
import graphql/value

/// Resolver context - will contain request context, data loaders, etc.
pub type Context {
  Context(data: Option(value.Value), arguments: Dict(String, value.Value))
}

/// Helper to create a context without arguments
pub fn context(data: Option(value.Value)) -> Context {
  Context(data, dict.new())
}

/// Helper to get an argument value from context
pub fn get_argument(ctx: Context, name: String) -> Option(value.Value) {
  dict.get(ctx.arguments, name) |> option.from_result
}

/// Field resolver function type
pub type Resolver =
  fn(Context) -> Result(value.Value, String)

/// GraphQL Type
pub opaque type Type {
  ScalarType(name: String)
  ObjectType(name: String, description: String, fields: List(Field))
  InputObjectType(name: String, description: String, fields: List(InputField))
  EnumType(name: String, description: String, values: List(EnumValue))
  ListType(inner_type: Type)
  NonNullType(inner_type: Type)
}

/// GraphQL Field
pub opaque type Field {
  Field(
    name: String,
    field_type: Type,
    description: String,
    arguments: List(Argument),
    resolver: Resolver,
  )
}

/// GraphQL Argument
pub opaque type Argument {
  Argument(
    name: String,
    arg_type: Type,
    description: String,
    default_value: Option(value.Value),
  )
}

/// GraphQL Input Field (for InputObject types)
pub opaque type InputField {
  InputField(
    name: String,
    field_type: Type,
    description: String,
    default_value: Option(value.Value),
  )
}

/// GraphQL Enum Value
pub opaque type EnumValue {
  EnumValue(name: String, description: String)
}

/// GraphQL Schema
pub opaque type Schema {
  Schema(query_type: Type, mutation_type: Option(Type))
}

// Built-in scalar types
pub fn string_type() -> Type {
  ScalarType("String")
}

pub fn int_type() -> Type {
  ScalarType("Int")
}

pub fn float_type() -> Type {
  ScalarType("Float")
}

pub fn boolean_type() -> Type {
  ScalarType("Boolean")
}

pub fn id_type() -> Type {
  ScalarType("ID")
}

// Type constructors
pub fn object_type(
  name: String,
  description: String,
  fields: List(Field),
) -> Type {
  ObjectType(name, description, fields)
}

pub fn enum_type(
  name: String,
  description: String,
  values: List(EnumValue),
) -> Type {
  EnumType(name, description, values)
}

pub fn input_object_type(
  name: String,
  description: String,
  fields: List(InputField),
) -> Type {
  InputObjectType(name, description, fields)
}

pub fn list_type(inner_type: Type) -> Type {
  ListType(inner_type)
}

pub fn non_null(inner_type: Type) -> Type {
  NonNullType(inner_type)
}

// Field constructors
pub fn field(
  name: String,
  field_type: Type,
  description: String,
  resolver: Resolver,
) -> Field {
  Field(name, field_type, description, [], resolver)
}

pub fn field_with_args(
  name: String,
  field_type: Type,
  description: String,
  arguments: List(Argument),
  resolver: Resolver,
) -> Field {
  Field(name, field_type, description, arguments, resolver)
}

// Argument constructor
pub fn argument(
  name: String,
  arg_type: Type,
  description: String,
  default_value: Option(value.Value),
) -> Argument {
  Argument(name, arg_type, description, default_value)
}

// Input field constructor
pub fn input_field(
  name: String,
  field_type: Type,
  description: String,
  default_value: Option(value.Value),
) -> InputField {
  InputField(name, field_type, description, default_value)
}

// Enum value constructor
pub fn enum_value(name: String, description: String) -> EnumValue {
  EnumValue(name, description)
}

// Schema constructor
pub fn schema(query_type: Type, mutation_type: Option(Type)) -> Schema {
  Schema(query_type, mutation_type)
}

// Accessors
pub fn type_name(t: Type) -> String {
  case t {
    ScalarType(name) -> name
    ObjectType(name, _, _) -> name
    InputObjectType(name, _, _) -> name
    EnumType(name, _, _) -> name
    ListType(inner) -> "[" <> type_name(inner) <> "]"
    NonNullType(inner) -> type_name(inner) <> "!"
  }
}

pub fn field_name(f: Field) -> String {
  case f {
    Field(name, _, _, _, _) -> name
  }
}

pub fn query_type(s: Schema) -> Type {
  case s {
    Schema(query_type, _) -> query_type
  }
}

pub fn get_mutation_type(s: Schema) -> Option(Type) {
  case s {
    Schema(_, mutation_type) -> mutation_type
  }
}

pub fn is_non_null(t: Type) -> Bool {
  case t {
    NonNullType(_) -> True
    _ -> False
  }
}

pub fn is_list(t: Type) -> Bool {
  case t {
    ListType(_) -> True
    _ -> False
  }
}

pub fn is_input_object(t: Type) -> Bool {
  case t {
    InputObjectType(_, _, _) -> True
    _ -> False
  }
}

pub fn type_description(t: Type) -> String {
  case t {
    ObjectType(_, description, _) -> description
    InputObjectType(_, description, _) -> description
    EnumType(_, description, _) -> description
    _ -> ""
  }
}

// Field resolution helpers
pub fn resolve_field(field: Field, ctx: Context) -> Result(value.Value, String) {
  case field {
    Field(_, _, _, _, resolver) -> resolver(ctx)
  }
}

pub fn get_field(t: Type, field_name: String) -> Option(Field) {
  case t {
    ObjectType(_, _, fields) -> {
      list.find(fields, fn(f) {
        case f {
          Field(name, _, _, _, _) -> name == field_name
        }
      })
      |> option.from_result
    }
    NonNullType(inner) -> get_field(inner, field_name)
    _ -> None
  }
}

/// Get the type of a field
pub fn field_type(field: Field) -> Type {
  case field {
    Field(_, ft, _, _, _) -> ft
  }
}

/// Get all fields from an ObjectType
pub fn get_fields(t: Type) -> List(Field) {
  case t {
    ObjectType(_, _, fields) -> fields
    _ -> []
  }
}

/// Get all input fields from an InputObjectType
pub fn get_input_fields(t: Type) -> List(InputField) {
  case t {
    InputObjectType(_, _, fields) -> fields
    _ -> []
  }
}

/// Get field description
pub fn field_description(field: Field) -> String {
  case field {
    Field(_, _, desc, _, _) -> desc
  }
}

/// Get field arguments
pub fn field_arguments(field: Field) -> List(Argument) {
  case field {
    Field(_, _, _, args, _) -> args
  }
}

/// Get argument name
pub fn argument_name(arg: Argument) -> String {
  case arg {
    Argument(name, _, _, _) -> name
  }
}

/// Get argument type
pub fn argument_type(arg: Argument) -> Type {
  case arg {
    Argument(_, arg_type, _, _) -> arg_type
  }
}

/// Get argument description
pub fn argument_description(arg: Argument) -> String {
  case arg {
    Argument(_, _, desc, _) -> desc
  }
}

/// Get input field type
pub fn input_field_type(input_field: InputField) -> Type {
  case input_field {
    InputField(_, field_type, _, _) -> field_type
  }
}

/// Get input field name
pub fn input_field_name(input_field: InputField) -> String {
  case input_field {
    InputField(name, _, _, _) -> name
  }
}

/// Get input field description
pub fn input_field_description(input_field: InputField) -> String {
  case input_field {
    InputField(_, _, desc, _) -> desc
  }
}

/// Get all enum values from an EnumType
pub fn get_enum_values(t: Type) -> List(EnumValue) {
  case t {
    EnumType(_, _, values) -> values
    _ -> []
  }
}

/// Get enum value name
pub fn enum_value_name(enum_value: EnumValue) -> String {
  case enum_value {
    EnumValue(name, _) -> name
  }
}

/// Get enum value description
pub fn enum_value_description(enum_value: EnumValue) -> String {
  case enum_value {
    EnumValue(_, desc) -> desc
  }
}

/// Check if type is a scalar
pub fn is_scalar(t: Type) -> Bool {
  case t {
    ScalarType(_) -> True
    _ -> False
  }
}

/// Check if type is an object
pub fn is_object(t: Type) -> Bool {
  case t {
    ObjectType(_, _, _) -> True
    _ -> False
  }
}

/// Check if type is an enum
pub fn is_enum(t: Type) -> Bool {
  case t {
    EnumType(_, _, _) -> True
    _ -> False
  }
}

/// Get the inner type from a wrapping type (List or NonNull)
pub fn inner_type(t: Type) -> option.Option(Type) {
  case t {
    ListType(inner) -> option.Some(inner)
    NonNullType(inner) -> option.Some(inner)
    _ -> option.None
  }
}

/// Get the kind of a type as a string for introspection
pub fn type_kind(t: Type) -> String {
  case t {
    ScalarType(_) -> "SCALAR"
    ObjectType(_, _, _) -> "OBJECT"
    InputObjectType(_, _, _) -> "INPUT_OBJECT"
    EnumType(_, _, _) -> "ENUM"
    ListType(_) -> "LIST"
    NonNullType(_) -> "NON_NULL"
  }
}
