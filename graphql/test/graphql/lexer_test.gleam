/// Tests for GraphQL Lexer (tokenization)
///
/// GraphQL spec Section 2 - Language
/// Token types: Punctuator, Name, IntValue, FloatValue, StringValue
/// Ignored: Whitespace, LineTerminator, Comment, Comma
import gleeunit/should
import graphql/lexer.{
  BraceClose, BraceOpen, Colon, Float, Int, Name, ParenClose, ParenOpen, String,
}

// Punctuator tests
pub fn tokenize_brace_open_test() {
  lexer.tokenize("{")
  |> should.equal(Ok([BraceOpen]))
}

pub fn tokenize_brace_close_test() {
  lexer.tokenize("}")
  |> should.equal(Ok([BraceClose]))
}

pub fn tokenize_paren_open_test() {
  lexer.tokenize("(")
  |> should.equal(Ok([ParenOpen]))
}

pub fn tokenize_paren_close_test() {
  lexer.tokenize(")")
  |> should.equal(Ok([ParenClose]))
}

pub fn tokenize_colon_test() {
  lexer.tokenize(":")
  |> should.equal(Ok([Colon]))
}

// Name tests (identifiers)
pub fn tokenize_simple_name_test() {
  lexer.tokenize("query")
  |> should.equal(Ok([Name("query")]))
}

pub fn tokenize_name_with_underscore_test() {
  lexer.tokenize("user_name")
  |> should.equal(Ok([Name("user_name")]))
}

pub fn tokenize_name_with_numbers_test() {
  lexer.tokenize("field123")
  |> should.equal(Ok([Name("field123")]))
}

// Int value tests
pub fn tokenize_positive_int_test() {
  lexer.tokenize("42")
  |> should.equal(Ok([Int("42")]))
}

pub fn tokenize_negative_int_test() {
  lexer.tokenize("-42")
  |> should.equal(Ok([Int("-42")]))
}

pub fn tokenize_zero_test() {
  lexer.tokenize("0")
  |> should.equal(Ok([Int("0")]))
}

// Float value tests
pub fn tokenize_simple_float_test() {
  lexer.tokenize("3.14")
  |> should.equal(Ok([Float("3.14")]))
}

pub fn tokenize_negative_float_test() {
  lexer.tokenize("-3.14")
  |> should.equal(Ok([Float("-3.14")]))
}

pub fn tokenize_float_with_exponent_test() {
  lexer.tokenize("1.5e10")
  |> should.equal(Ok([Float("1.5e10")]))
}

pub fn tokenize_float_with_negative_exponent_test() {
  lexer.tokenize("1.5e-10")
  |> should.equal(Ok([Float("1.5e-10")]))
}

// String value tests
pub fn tokenize_empty_string_test() {
  lexer.tokenize("\"\"")
  |> should.equal(Ok([String("")]))
}

pub fn tokenize_simple_string_test() {
  lexer.tokenize("\"hello\"")
  |> should.equal(Ok([String("hello")]))
}

pub fn tokenize_string_with_spaces_test() {
  lexer.tokenize("\"hello world\"")
  |> should.equal(Ok([String("hello world")]))
}

pub fn tokenize_string_with_escape_test() {
  lexer.tokenize("\"hello\\nworld\"")
  |> should.equal(Ok([String("hello\nworld")]))
}

// Whitespace handling (should be filtered out by default)
pub fn tokenize_with_spaces_test() {
  lexer.tokenize("query  user")
  |> should.equal(Ok([Name("query"), Name("user")]))
}

pub fn tokenize_with_tabs_test() {
  lexer.tokenize("query\tuser")
  |> should.equal(Ok([Name("query"), Name("user")]))
}

pub fn tokenize_with_newlines_test() {
  lexer.tokenize("query\nuser")
  |> should.equal(Ok([Name("query"), Name("user")]))
}

// Comment tests (should be filtered out)
pub fn tokenize_with_comment_test() {
  lexer.tokenize("query # this is a comment\nuser")
  |> should.equal(Ok([Name("query"), Name("user")]))
}

// Complex query tests
pub fn tokenize_simple_query_test() {
  lexer.tokenize("{ user }")
  |> should.equal(Ok([BraceOpen, Name("user"), BraceClose]))
}

pub fn tokenize_query_with_field_test() {
  lexer.tokenize("{ user { name } }")
  |> should.equal(
    Ok([
      BraceOpen,
      Name("user"),
      BraceOpen,
      Name("name"),
      BraceClose,
      BraceClose,
    ]),
  )
}

pub fn tokenize_query_with_argument_test() {
  lexer.tokenize("{ user(id: 42) }")
  |> should.equal(
    Ok([
      BraceOpen,
      Name("user"),
      ParenOpen,
      Name("id"),
      Colon,
      Int("42"),
      ParenClose,
      BraceClose,
    ]),
  )
}

pub fn tokenize_query_with_string_argument_test() {
  lexer.tokenize("{ user(name: \"Alice\") }")
  |> should.equal(
    Ok([
      BraceOpen,
      Name("user"),
      ParenOpen,
      Name("name"),
      Colon,
      String("Alice"),
      ParenClose,
      BraceClose,
    ]),
  )
}

// Error cases - use a truly invalid character like backslash
pub fn tokenize_invalid_character_test() {
  lexer.tokenize("query \\invalid")
  |> should.be_error()
}

pub fn tokenize_unclosed_string_test() {
  lexer.tokenize("\"unclosed")
  |> should.be_error()
}
