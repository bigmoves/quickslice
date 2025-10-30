/// GraphQL SDL (Schema Definition Language) Printer
///
/// Generates proper SDL output from GraphQL schema types
/// Follows the GraphQL specification for schema representation

import gleam/list
import gleam/option
import gleam/string
import graphql/schema

/// Print a single GraphQL type as SDL
pub fn print_type(type_: schema.Type) -> String {
  print_type_internal(type_, 0, False)
}

/// Print multiple GraphQL types as SDL with blank lines between them
pub fn print_types(types: List(schema.Type)) -> String {
  list.map(types, print_type)
  |> string.join("\n\n")
}

// Internal function that handles indentation and inline mode
fn print_type_internal(
  type_: schema.Type,
  indent_level: Int,
  inline: Bool,
) -> String {
  let kind = schema.type_kind(type_)

  case kind {
    "INPUT_OBJECT" -> print_input_object(type_, indent_level, inline)
    "OBJECT" -> print_object(type_, indent_level, inline)
    "ENUM" -> print_enum(type_, indent_level, inline)
    "SCALAR" -> print_scalar(type_, indent_level, inline)
    "LIST" -> {
      case schema.inner_type(type_) {
        option.Some(inner) ->
          "[" <> print_type_internal(inner, indent_level, True) <> "]"
        option.None -> "[Unknown]"
      }
    }
    "NON_NULL" -> {
      case schema.inner_type(type_) {
        option.Some(inner) ->
          print_type_internal(inner, indent_level, True) <> "!"
        option.None -> "Unknown!"
      }
    }
    _ -> schema.type_name(type_)
  }
}

fn print_scalar(type_: schema.Type, indent_level: Int, inline: Bool) -> String {
  case inline {
    True -> schema.type_name(type_)
    False -> {
      let indent = string.repeat(" ", indent_level * 2)
      let description = schema.type_description(type_)
      let desc_block = case description {
        "" -> ""
        _ -> indent <> format_description(description) <> "\n"
      }

      desc_block <> indent <> "scalar " <> schema.type_name(type_)
    }
  }
}

fn print_input_object(
  type_: schema.Type,
  indent_level: Int,
  inline: Bool,
) -> String {
  case inline {
    True -> schema.type_name(type_)
    False -> {
      let type_name = schema.type_name(type_)
      let indent = string.repeat(" ", indent_level * 2)
      let field_indent = string.repeat(" ", { indent_level + 1 } * 2)

      let description = schema.type_description(type_)
      let desc_block = case description {
        "" -> ""
        _ -> indent <> format_description(description) <> "\n"
      }

      let fields = schema.get_input_fields(type_)

      let field_lines =
        list.map(fields, fn(field) {
          let field_name = schema.input_field_name(field)
          let field_type = schema.input_field_type(field)
          let field_desc = schema.input_field_description(field)
          let field_type_str = print_type_internal(field_type, indent_level + 1, True)

          let field_desc_block = case field_desc {
            "" -> ""
            _ -> field_indent <> format_description(field_desc) <> "\n"
          }

          field_desc_block <> field_indent <> field_name <> ": " <> field_type_str
        })

      case list.is_empty(fields) {
        True -> desc_block <> indent <> "input " <> type_name <> " {}"
        False -> {
          desc_block
          <> indent
          <> "input "
          <> type_name
          <> " {\n"
          <> string.join(field_lines, "\n")
          <> "\n"
          <> indent
          <> "}"
        }
      }
    }
  }
}

fn print_object(type_: schema.Type, indent_level: Int, inline: Bool) -> String {
  case inline {
    True -> schema.type_name(type_)
    False -> {
      let type_name = schema.type_name(type_)
      let indent = string.repeat(" ", indent_level * 2)
      let field_indent = string.repeat(" ", { indent_level + 1 } * 2)

      let description = schema.type_description(type_)
      let desc_block = case description {
        "" -> ""
        _ -> indent <> format_description(description) <> "\n"
      }

      let fields = schema.get_fields(type_)

      let field_lines =
        list.map(fields, fn(field) {
          let field_name = schema.field_name(field)
          let field_type = schema.field_type(field)
          let field_desc = schema.field_description(field)
          let field_type_str = print_type_internal(field_type, indent_level + 1, True)

          let field_desc_block = case field_desc {
            "" -> ""
            _ -> field_indent <> format_description(field_desc) <> "\n"
          }

          field_desc_block <> field_indent <> field_name <> ": " <> field_type_str
        })

      case list.is_empty(fields) {
        True -> desc_block <> indent <> "type " <> type_name <> " {}"
        False -> {
          desc_block
          <> indent
          <> "type "
          <> type_name
          <> " {\n"
          <> string.join(field_lines, "\n")
          <> "\n"
          <> indent
          <> "}"
        }
      }
    }
  }
}

fn print_enum(type_: schema.Type, indent_level: Int, inline: Bool) -> String {
  case inline {
    True -> schema.type_name(type_)
    False -> {
      let type_name = schema.type_name(type_)
      let indent = string.repeat(" ", indent_level * 2)
      let value_indent = string.repeat(" ", { indent_level + 1 } * 2)

      let description = schema.type_description(type_)
      let desc_block = case description {
        "" -> ""
        _ -> indent <> format_description(description) <> "\n"
      }

      let values = schema.get_enum_values(type_)

      let value_lines =
        list.map(values, fn(value) {
          let value_name = schema.enum_value_name(value)
          let value_desc = schema.enum_value_description(value)

          let value_desc_block = case value_desc {
            "" -> ""
            _ -> value_indent <> format_description(value_desc) <> "\n"
          }

          value_desc_block <> value_indent <> value_name
        })

      case list.is_empty(values) {
        True -> desc_block <> indent <> "enum " <> type_name <> " {}"
        False -> {
          desc_block
          <> indent
          <> "enum "
          <> type_name
          <> " {\n"
          <> string.join(value_lines, "\n")
          <> "\n"
          <> indent
          <> "}"
        }
      }
    }
  }
}

/// Format a description as a triple-quoted string
fn format_description(description: String) -> String {
  case description {
    "" -> ""
    _ -> "\"\"\"" <> description <> "\"\"\""
  }
}
