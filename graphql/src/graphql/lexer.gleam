/// GraphQL Lexer - Tokenization
///
/// Per GraphQL spec Section 2 - Language
/// Converts source text into a sequence of lexical tokens
import gleam/list
import gleam/result
import gleam/string

/// GraphQL token types
pub type Token {
  // Punctuators
  BraceOpen
  BraceClose
  ParenOpen
  ParenClose
  BracketOpen
  BracketClose
  Colon
  Comma
  Pipe
  Equals
  At
  Dollar
  Spread

  // Values
  Name(String)
  Int(String)
  Float(String)
  String(String)

  // Ignored tokens (kept for optional whitespace preservation)
  Whitespace
  Comment(String)
}

pub type LexerError {
  UnexpectedCharacter(String, Int)
  UnterminatedString(Int)
  InvalidNumber(String, Int)
}

/// Tokenize a GraphQL source string into a list of tokens
///
/// Filters out whitespace and comments by default
pub fn tokenize(source: String) -> Result(List(Token), LexerError) {
  source
  |> string.to_graphemes
  |> tokenize_graphemes([], 0)
  |> result.map(filter_ignored)
}

/// Internal: Tokenize graphemes recursively
fn tokenize_graphemes(
  graphemes: List(String),
  acc: List(Token),
  pos: Int,
) -> Result(List(Token), LexerError) {
  case graphemes {
    [] -> Ok(list.reverse(acc))

    // Whitespace
    [" ", ..rest] | ["\t", ..rest] | ["\n", ..rest] | ["\r", ..rest] ->
      tokenize_graphemes(rest, [Whitespace, ..acc], pos + 1)

    // Comments
    ["#", ..rest] -> {
      let #(comment, remaining) = take_until_newline(rest)
      tokenize_graphemes(remaining, [Comment(comment), ..acc], pos + 1)
    }

    // Punctuators
    ["{", ..rest] -> tokenize_graphemes(rest, [BraceOpen, ..acc], pos + 1)
    ["}", ..rest] -> tokenize_graphemes(rest, [BraceClose, ..acc], pos + 1)
    ["(", ..rest] -> tokenize_graphemes(rest, [ParenOpen, ..acc], pos + 1)
    [")", ..rest] -> tokenize_graphemes(rest, [ParenClose, ..acc], pos + 1)
    ["[", ..rest] -> tokenize_graphemes(rest, [BracketOpen, ..acc], pos + 1)
    ["]", ..rest] -> tokenize_graphemes(rest, [BracketClose, ..acc], pos + 1)
    [":", ..rest] -> tokenize_graphemes(rest, [Colon, ..acc], pos + 1)
    [",", ..rest] -> tokenize_graphemes(rest, [Comma, ..acc], pos + 1)
    ["|", ..rest] -> tokenize_graphemes(rest, [Pipe, ..acc], pos + 1)
    ["=", ..rest] -> tokenize_graphemes(rest, [Equals, ..acc], pos + 1)
    ["@", ..rest] -> tokenize_graphemes(rest, [At, ..acc], pos + 1)
    ["$", ..rest] -> tokenize_graphemes(rest, [Dollar, ..acc], pos + 1)

    // Spread (...)
    [".", ".", ".", ..rest] ->
      tokenize_graphemes(rest, [Spread, ..acc], pos + 3)

    // Strings
    ["\"", ..rest] -> {
      case take_string(rest, []) {
        Ok(#(str, remaining)) ->
          tokenize_graphemes(remaining, [String(str), ..acc], pos + 1)
        Error(err) -> Error(err)
      }
    }

    // Numbers (Int or Float) - check for minus or digits
    ["-", ..]
    | ["0", ..]
    | ["1", ..]
    | ["2", ..]
    | ["3", ..]
    | ["4", ..]
    | ["5", ..]
    | ["6", ..]
    | ["7", ..]
    | ["8", ..]
    | ["9", ..] -> {
      case take_number(graphemes) {
        Ok(#(num_str, is_float, remaining)) -> {
          let token = case is_float {
            True -> Float(num_str)
            False -> Int(num_str)
          }
          tokenize_graphemes(remaining, [token, ..acc], pos + 1)
        }
        Error(err) -> Error(err)
      }
    }

    // Names (identifiers) - must start with letter or underscore
    [char, ..] -> {
      case is_name_start(char) {
        True -> {
          let #(name, remaining) = take_name(graphemes)
          tokenize_graphemes(remaining, [Name(name), ..acc], pos + 1)
        }
        False -> Error(UnexpectedCharacter(char, pos))
      }
    }
  }
}

/// Take characters until newline
fn take_until_newline(graphemes: List(String)) -> #(String, List(String)) {
  let #(chars, rest) = take_while(graphemes, fn(c) { c != "\n" && c != "\r" })
  #(string.concat(chars), rest)
}

/// Take string contents (handles escapes)
fn take_string(
  graphemes: List(String),
  acc: List(String),
) -> Result(#(String, List(String)), LexerError) {
  case graphemes {
    [] -> Error(UnterminatedString(0))

    ["\"", ..rest] -> Ok(#(string.concat(list.reverse(acc)), rest))

    ["\\", "n", ..rest] -> take_string(rest, ["\n", ..acc])
    ["\\", "r", ..rest] -> take_string(rest, ["\r", ..acc])
    ["\\", "t", ..rest] -> take_string(rest, ["\t", ..acc])
    ["\\", "\"", ..rest] -> take_string(rest, ["\"", ..acc])
    ["\\", "\\", ..rest] -> take_string(rest, ["\\", ..acc])

    [char, ..rest] -> take_string(rest, [char, ..acc])
  }
}

/// Take a number (int or float)
fn take_number(
  graphemes: List(String),
) -> Result(#(String, Bool, List(String)), LexerError) {
  let #(num_chars, rest) = take_while(graphemes, is_number_char)
  let num_str = string.concat(num_chars)

  let is_float =
    string.contains(num_str, ".")
    || string.contains(num_str, "e")
    || string.contains(num_str, "E")

  Ok(#(num_str, is_float, rest))
}

/// Take a name (identifier)
fn take_name(graphemes: List(String)) -> #(String, List(String)) {
  let #(name_chars, rest) = take_while(graphemes, is_name_char)
  #(string.concat(name_chars), rest)
}

/// Take characters while predicate is true
fn take_while(
  graphemes: List(String),
  predicate: fn(String) -> Bool,
) -> #(List(String), List(String)) {
  do_take_while(graphemes, predicate, [])
}

fn do_take_while(
  graphemes: List(String),
  predicate: fn(String) -> Bool,
  acc: List(String),
) -> #(List(String), List(String)) {
  case graphemes {
    [char, ..rest] -> {
      case predicate(char) {
        True -> do_take_while(rest, predicate, [char, ..acc])
        False -> #(list.reverse(acc), graphemes)
      }
    }
    _ -> #(list.reverse(acc), graphemes)
  }
}

/// Check if character can start a name
fn is_name_start(char: String) -> Bool {
  case char {
    "a"
    | "b"
    | "c"
    | "d"
    | "e"
    | "f"
    | "g"
    | "h"
    | "i"
    | "j"
    | "k"
    | "l"
    | "m"
    | "n"
    | "o"
    | "p"
    | "q"
    | "r"
    | "s"
    | "t"
    | "u"
    | "v"
    | "w"
    | "x"
    | "y"
    | "z" -> True
    "A"
    | "B"
    | "C"
    | "D"
    | "E"
    | "F"
    | "G"
    | "H"
    | "I"
    | "J"
    | "K"
    | "L"
    | "M"
    | "N"
    | "O"
    | "P"
    | "Q"
    | "R"
    | "S"
    | "T"
    | "U"
    | "V"
    | "W"
    | "X"
    | "Y"
    | "Z" -> True
    "_" -> True
    _ -> False
  }
}

/// Check if character can be part of a name
fn is_name_char(char: String) -> Bool {
  is_name_start(char) || is_digit(char)
}

/// Check if character is a digit
fn is_digit(char: String) -> Bool {
  case char {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

/// Check if character can be part of a number
fn is_number_char(char: String) -> Bool {
  is_digit(char)
  || char == "."
  || char == "e"
  || char == "E"
  || char == "-"
  || char == "+"
}

/// Filter out ignored tokens (whitespace and comments)
fn filter_ignored(tokens: List(Token)) -> List(Token) {
  list.filter(tokens, fn(token) {
    case token {
      Whitespace -> False
      Comment(_) -> False
      _ -> True
    }
  })
}
