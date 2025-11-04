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

    // Named query: "query Name(...) { ... }" or "query Name { ... }"
    [lexer.Name("query"), lexer.Name(name), ..rest] -> {
      // Check if there are variable definitions
      case rest {
        [lexer.ParenOpen, ..vars_rest] -> {
          // Parse variable definitions
          case parse_variable_definitions(vars_rest) {
            Ok(#(variables, after_vars)) -> {
              case parse_selection_set(after_vars) {
                Ok(#(selections, remaining)) -> {
                  let op = NamedQuery(name, variables, selections)
                  parse_operations(remaining, [op, ..acc])
                }
                Error(err) -> Error(err)
              }
            }
            Error(err) -> Error(err)
          }
        }
        _ -> {
          // No variables, parse selection set directly
          case parse_selection_set(rest) {
            Ok(#(selections, remaining)) -> {
              let op = NamedQuery(name, [], selections)
              parse_operations(remaining, [op, ..acc])
            }
            Error(err) -> Error(err)
          }
        }
      }
    }

    // Named mutation: "mutation Name(...) { ... }" or "mutation Name { ... }"
    [lexer.Name("mutation"), lexer.Name(name), ..rest] -> {
      // Check if there are variable definitions
      case rest {
        [lexer.ParenOpen, ..vars_rest] -> {
          // Parse variable definitions
          case parse_variable_definitions(vars_rest) {
            Ok(#(variables, after_vars)) -> {
              case parse_selection_set(after_vars) {
                Ok(#(selections, remaining)) -> {
                  let op = NamedMutation(name, variables, selections)
                  parse_operations(remaining, [op, ..acc])
                }
                Error(err) -> Error(err)
              }
            }
            Error(err) -> Error(err)
          }
        }
        _ -> {
          // No variables, parse selection set directly
          case parse_selection_set(rest) {
            Ok(#(selections, remaining)) -> {
              let op = NamedMutation(name, [], selections)
              parse_operations(remaining, [op, ..acc])
            }
            Error(err) -> Error(err)
          }
        }
      }
    }

    // Anonymous query: "query { ... }"
    [lexer.Name("query"), lexer.BraceOpen, ..] -> {
      case parse_selection_set(list.drop(tokens, 1)) {
        Ok(#(selections, remaining)) -> {
          let op = Query(selections)
          parse_operations(remaining, [op, ..acc])
        }
        Error(err) -> Error(err)
      }
    }

    // Anonymous mutation: "mutation { ... }"
    [lexer.Name("mutation"), lexer.BraceOpen, ..] -> {
      case parse_selection_set(list.drop(tokens, 1)) {
        Ok(#(selections, remaining)) -> {
          let op = Mutation(selections)
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
          // Continue parsing to see if there are more operations (e.g., fragment definitions)
          parse_operations(remaining, [op, ..acc])
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

    // Field with alias: "alias: fieldName"
    [lexer.Name(alias), lexer.Colon, lexer.Name(field_name), ..rest] -> {
      case parse_field_with_alias(field_name, Some(alias), rest) {
        Ok(#(field, remaining)) -> {
          parse_selections(remaining, [field, ..acc])
        }
        Error(err) -> Error(err)
      }
    }

    // Field without alias
    [lexer.Name(name), ..rest] -> {
      case parse_field_with_alias(name, None, rest) {
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

/// Parse a field with optional alias, arguments and nested selections
fn parse_field_with_alias(
  name: String,
  alias: Option(String),
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
          Ok(#(Field(name, alias, arguments, nested), remaining))
        Error(err) -> Error(err)
      }
    }
    _ -> Ok(#(Field(name, alias, arguments, []), after_args))
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

/// Parse variable definitions: ($var1: Type!, $var2: Type)
/// Returns the list of variables and remaining tokens after the closing paren
fn parse_variable_definitions(
  tokens: List(lexer.Token),
) -> Result(#(List(Variable), List(lexer.Token)), ParseError) {
  parse_variable_definitions_loop(tokens, [])
}

/// Parse variable definitions loop
fn parse_variable_definitions_loop(
  tokens: List(lexer.Token),
  acc: List(Variable),
) -> Result(#(List(Variable), List(lexer.Token)), ParseError) {
  case tokens {
    // End of variable definitions
    [lexer.ParenClose, ..rest] -> Ok(#(list.reverse(acc), rest))

    // Skip commas
    [lexer.Comma, ..rest] -> parse_variable_definitions_loop(rest, acc)

    // Parse a variable: $name: Type! or $name: Type
    [lexer.Dollar, lexer.Name(var_name), lexer.Colon, ..rest] -> {
      // Parse the type (Name or Name!)
      case rest {
        [lexer.Name(type_name), lexer.Exclamation, ..rest2] -> {
          // Non-null type
          let variable = Variable(var_name, type_name <> "!")
          parse_variable_definitions_loop(rest2, [variable, ..acc])
        }
        [lexer.Name(type_name), ..rest2] -> {
          // Nullable type
          let variable = Variable(var_name, type_name)
          parse_variable_definitions_loop(rest2, [variable, ..acc])
        }
        [] -> Error(UnexpectedEndOfInput("Expected type after :"))
        [token, ..] -> Error(UnexpectedToken(token, "Expected type name"))
      }
    }

    [] -> Error(UnexpectedEndOfInput("Expected variable definition or )"))
    [token, ..] ->
      Error(UnexpectedToken(token, "Expected $variableName or )"))
  }
}
