/// GraphQL Parser - Build AST from tokens
///
/// Per GraphQL spec Section 2 - Language
/// Converts a token stream into an Abstract Syntax Tree
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import graphql/lexer

/// GraphQL Document (top-level)
pub type Document {
  Document(operations: List(Operation))
}

/// GraphQL Operation
pub type Operation {
  Query(SelectionSet)
  NamedQuery(name: String, variables: List(Variable), selections: SelectionSet)
  Mutation(SelectionSet)
  NamedMutation(
    name: String,
    variables: List(Variable),
    selections: SelectionSet,
  )
  FragmentDefinition(
    name: String,
    type_condition: String,
    selections: SelectionSet,
  )
}

/// Selection Set (list of fields)
pub type SelectionSet {
  SelectionSet(selections: List(Selection))
}

/// Selection (field or fragment)
pub type Selection {
  Field(
    name: String,
    alias: Option(String),
    arguments: List(Argument),
    selections: List(Selection),
  )
  FragmentSpread(name: String)
  InlineFragment(type_condition: Option(String), selections: List(Selection))
}

/// Argument (name: value)
pub type Argument {
  Argument(name: String, value: ArgumentValue)
}

/// Argument value types
pub type ArgumentValue {
  IntValue(String)
  FloatValue(String)
  StringValue(String)
  BooleanValue(Bool)
  NullValue
  EnumValue(String)
  ListValue(List(ArgumentValue))
  ObjectValue(List(#(String, ArgumentValue)))
  VariableValue(String)
}

/// Variable definition
pub type Variable {
  Variable(name: String, type_: String)
}

pub type ParseError {
  UnexpectedToken(lexer.Token, String)
  UnexpectedEndOfInput(String)
  LexerError(lexer.LexerError)
}

/// Parse a GraphQL query string into a Document
pub fn parse(source: String) -> Result(Document, ParseError) {
  source
  |> lexer.tokenize
  |> result.map_error(LexerError)
  |> result.try(parse_document)
}

/// Parse tokens into a Document
fn parse_document(tokens: List(lexer.Token)) -> Result(Document, ParseError) {
  case tokens {
    [] -> Error(UnexpectedEndOfInput("Expected query or operation"))
    _ -> {
      case parse_operations(tokens, []) {
        Ok(#(operations, _remaining)) -> Ok(Document(operations))
        Error(err) -> Error(err)
      }
    }
  }
}

/// Parse operations (queries/mutations)
fn parse_operations(
  tokens: List(lexer.Token),
  acc: List(Operation),
) -> Result(#(List(Operation), List(lexer.Token)), ParseError) {
  case tokens {
    [] -> Ok(#(list.reverse(acc), []))

    // Named query: "query Name { ... }"
    [lexer.Name("query"), lexer.Name(name), ..rest] -> {
      case parse_selection_set(rest) {
        Ok(#(selections, remaining)) -> {
          let op = NamedQuery(name, [], selections)
          parse_operations(remaining, [op, ..acc])
        }
        Error(err) -> Error(err)
      }
    }

    // Fragment definition: "fragment Name on Type { ... }"
    [
      lexer.Name("fragment"),
      lexer.Name(name),
      lexer.Name("on"),
      lexer.Name(type_condition),
      ..rest
    ] -> {
      case parse_selection_set(rest) {
        Ok(#(selections, remaining)) -> {
          let op = FragmentDefinition(name, type_condition, selections)
          parse_operations(remaining, [op, ..acc])
        }
        Error(err) -> Error(err)
      }
    }

    // Anonymous query: "{ ... }"
    [lexer.BraceOpen, ..] -> {
      case parse_selection_set(tokens) {
        Ok(#(selections, remaining)) -> {
          let op = Query(selections)
          // Don't continue parsing if we have operations already - single anonymous query
          case acc {
            [] -> Ok(#(list.reverse([op]), remaining))
            _ -> parse_operations(remaining, [op, ..acc])
          }
        }
        Error(err) -> Error(err)
      }
    }

    // Any other token when we have operations means we're done
    _ -> {
      case acc {
        [] ->
          Error(UnexpectedToken(
            list.first(tokens) |> result.unwrap(lexer.BraceClose),
            "Expected query, mutation, fragment, or '{'",
          ))
        _ -> Ok(#(list.reverse(acc), tokens))
      }
    }
  }
}

/// Parse selection set: { field1 field2 ... }
fn parse_selection_set(
  tokens: List(lexer.Token),
) -> Result(#(SelectionSet, List(lexer.Token)), ParseError) {
  case tokens {
    [lexer.BraceOpen, ..rest] -> {
      case parse_selections(rest, []) {
        Ok(#(selections, [lexer.BraceClose, ..remaining])) ->
          Ok(#(SelectionSet(selections), remaining))
        Ok(#(_, _remaining)) ->
          Error(UnexpectedEndOfInput("Expected '}' to close selection set"))
        Error(err) -> Error(err)
      }
    }
    [token, ..] -> Error(UnexpectedToken(token, "Expected '{'"))
    [] -> Error(UnexpectedEndOfInput("Expected '{'"))
  }
}

/// Parse selections (fields)
fn parse_selections(
  tokens: List(lexer.Token),
  acc: List(Selection),
) -> Result(#(List(Selection), List(lexer.Token)), ParseError) {
  case tokens {
    // End of selection set
    [lexer.BraceClose, ..] -> Ok(#(list.reverse(acc), tokens))

    // Inline fragment: "... on Type { ... }" - Check this BEFORE fragment spread
    [lexer.Spread, lexer.Name("on"), lexer.Name(type_condition), ..rest] -> {
      case parse_selection_set(rest) {
        Ok(#(SelectionSet(selections), remaining)) -> {
          let inline = InlineFragment(Some(type_condition), selections)
          parse_selections(remaining, [inline, ..acc])
        }
        Error(err) -> Error(err)
      }
    }

    // Fragment spread: "...FragmentName"
    [lexer.Spread, lexer.Name(name), ..rest] -> {
      let spread = FragmentSpread(name)
      parse_selections(rest, [spread, ..acc])
    }

    // Field
    [lexer.Name(name), ..rest] -> {
      case parse_field(name, rest) {
        Ok(#(field, remaining)) -> {
          parse_selections(remaining, [field, ..acc])
        }
        Error(err) -> Error(err)
      }
    }

    [] -> Error(UnexpectedEndOfInput("Expected field or '}'"))
    [token, ..] ->
      Error(UnexpectedToken(token, "Expected field name or fragment"))
  }
}

/// Parse a field with optional arguments and nested selections
fn parse_field(
  name: String,
  tokens: List(lexer.Token),
) -> Result(#(Selection, List(lexer.Token)), ParseError) {
  // Parse arguments if present
  let #(arguments, after_args) = case tokens {
    [lexer.ParenOpen, ..] -> {
      case parse_arguments(tokens) {
        Ok(result) -> result
        Error(_err) -> #([], tokens)
        // No arguments
      }
    }
    _ -> #([], tokens)
  }

  // Parse nested selection set if present
  case after_args {
    [lexer.BraceOpen, ..] -> {
      case parse_nested_selections(after_args) {
        Ok(#(nested, remaining)) ->
          Ok(#(Field(name, None, arguments, nested), remaining))
        Error(err) -> Error(err)
      }
    }
    _ -> Ok(#(Field(name, None, arguments, []), after_args))
  }
}

/// Parse nested selections for a field
fn parse_nested_selections(
  tokens: List(lexer.Token),
) -> Result(#(List(Selection), List(lexer.Token)), ParseError) {
  case tokens {
    [lexer.BraceOpen, ..rest] -> {
      case parse_selections(rest, []) {
        Ok(#(selections, [lexer.BraceClose, ..remaining])) ->
          Ok(#(selections, remaining))
        Ok(#(_, _remaining)) ->
          Error(UnexpectedEndOfInput(
            "Expected '}' to close nested selection set",
          ))
        Error(err) -> Error(err)
      }
    }
    _ -> Ok(#([], tokens))
  }
}

/// Parse arguments: (arg1: value1, arg2: value2)
fn parse_arguments(
  tokens: List(lexer.Token),
) -> Result(#(List(Argument), List(lexer.Token)), ParseError) {
  case tokens {
    [lexer.ParenOpen, ..rest] -> {
      case parse_argument_list(rest, []) {
        Ok(#(args, [lexer.ParenClose, ..remaining])) -> Ok(#(args, remaining))
        Ok(#(_, _remaining)) ->
          Error(UnexpectedEndOfInput("Expected ')' to close arguments"))
        Error(err) -> Error(err)
      }
    }
    _ -> Ok(#([], tokens))
  }
}

/// Parse list of arguments
fn parse_argument_list(
  tokens: List(lexer.Token),
  acc: List(Argument),
) -> Result(#(List(Argument), List(lexer.Token)), ParseError) {
  case tokens {
    // End of arguments
    [lexer.ParenClose, ..] -> Ok(#(list.reverse(acc), tokens))

    // Argument: name: value
    [lexer.Name(name), lexer.Colon, ..rest] -> {
      case parse_argument_value(rest) {
        Ok(#(value, remaining)) -> {
          let arg = Argument(name, value)
          // Skip optional comma
          let after_comma = case remaining {
            [lexer.Comma, ..r] -> r
            _ -> remaining
          }
          parse_argument_list(after_comma, [arg, ..acc])
        }
        Error(err) -> Error(err)
      }
    }

    [] -> Error(UnexpectedEndOfInput("Expected argument or ')'"))
    [token, ..] -> Error(UnexpectedToken(token, "Expected argument name"))
  }
}

/// Parse argument value
fn parse_argument_value(
  tokens: List(lexer.Token),
) -> Result(#(ArgumentValue, List(lexer.Token)), ParseError) {
  case tokens {
    [lexer.Int(val), ..rest] -> Ok(#(IntValue(val), rest))
    [lexer.Float(val), ..rest] -> Ok(#(FloatValue(val), rest))
    [lexer.String(val), ..rest] -> Ok(#(StringValue(val), rest))
    [lexer.Name("true"), ..rest] -> Ok(#(BooleanValue(True), rest))
    [lexer.Name("false"), ..rest] -> Ok(#(BooleanValue(False), rest))
    [lexer.Name("null"), ..rest] -> Ok(#(NullValue, rest))
    [lexer.Name(name), ..rest] -> Ok(#(EnumValue(name), rest))
    [lexer.Dollar, lexer.Name(name), ..rest] -> Ok(#(VariableValue(name), rest))
    [lexer.BracketOpen, ..rest] -> parse_list_value(rest)
    [lexer.BraceOpen, ..rest] -> parse_object_value(rest)
    [] -> Error(UnexpectedEndOfInput("Expected value"))
    [token, ..] -> Error(UnexpectedToken(token, "Expected value"))
  }
}

/// Parse list value: [value, value, ...]
fn parse_list_value(
  tokens: List(lexer.Token),
) -> Result(#(ArgumentValue, List(lexer.Token)), ParseError) {
  case tokens {
    [lexer.BracketClose, ..rest] -> Ok(#(ListValue([]), rest))
    _ -> parse_list_value_items(tokens, [])
  }
}

/// Parse list value items recursively
fn parse_list_value_items(
  tokens: List(lexer.Token),
  acc: List(ArgumentValue),
) -> Result(#(ArgumentValue, List(lexer.Token)), ParseError) {
  case tokens {
    [lexer.BracketClose, ..rest] -> Ok(#(ListValue(list.reverse(acc)), rest))
    [lexer.Comma, ..rest] -> parse_list_value_items(rest, acc)
    _ -> {
      use #(value, rest) <- result.try(parse_argument_value(tokens))
      parse_list_value_items(rest, [value, ..acc])
    }
  }
}

/// Parse object value: {field: value, field: value, ...}
fn parse_object_value(
  tokens: List(lexer.Token),
) -> Result(#(ArgumentValue, List(lexer.Token)), ParseError) {
  case tokens {
    [lexer.BraceClose, ..rest] -> Ok(#(ObjectValue([]), rest))
    _ -> parse_object_value_fields(tokens, [])
  }
}

/// Parse object value fields recursively
fn parse_object_value_fields(
  tokens: List(lexer.Token),
  acc: List(#(String, ArgumentValue)),
) -> Result(#(ArgumentValue, List(lexer.Token)), ParseError) {
  case tokens {
    [lexer.BraceClose, ..rest] -> Ok(#(ObjectValue(list.reverse(acc)), rest))
    [lexer.Comma, ..rest] -> parse_object_value_fields(rest, acc)
    [lexer.Name(field_name), lexer.Colon, ..rest] -> {
      use #(value, rest2) <- result.try(parse_argument_value(rest))
      parse_object_value_fields(rest2, [#(field_name, value), ..acc])
    }
    [] -> Error(UnexpectedEndOfInput("Expected field name or }"))
    [token, ..] -> Error(UnexpectedToken(token, "Expected field name or }"))
  }
}
