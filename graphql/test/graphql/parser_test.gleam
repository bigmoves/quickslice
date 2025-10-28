/// Tests for GraphQL Parser (AST building)
///
/// GraphQL spec Section 2 - Language
/// Parse tokens into Abstract Syntax Tree
import gleam/option.{None}
import gleeunit/should
import graphql/parser

// Simple query tests
pub fn parse_empty_query_test() {
  "{ }"
  |> parser.parse
  |> should.be_ok
}

pub fn parse_single_field_test() {
  "{ user }"
  |> parser.parse
  |> should.be_ok
  |> fn(doc) {
    case doc {
      parser.Document([
        parser.Query(parser.SelectionSet([
          parser.Field(name: "user", alias: None, arguments: [], selections: []),
        ])),
      ]) -> True
      _ -> False
    }
  }
  |> should.be_true
}

pub fn parse_nested_fields_test() {
  "{ user { name } }"
  |> parser.parse
  |> should.be_ok
  |> fn(doc) {
    case doc {
      parser.Document([
        parser.Query(parser.SelectionSet([
          parser.Field(
            name: "user",
            alias: None,
            arguments: [],
            selections: [parser.Field("name", None, [], [])],
          ),
        ])),
      ]) -> True
      _ -> False
    }
  }
  |> should.be_true
}

pub fn parse_multiple_fields_test() {
  "{ user posts }"
  |> parser.parse
  |> should.be_ok
  |> fn(doc) {
    case doc {
      parser.Document([
        parser.Query(parser.SelectionSet([
          parser.Field(name: "user", alias: None, arguments: [], selections: []),
          parser.Field(
            name: "posts",
            alias: None,
            arguments: [],
            selections: [],
          ),
        ])),
      ]) -> True
      _ -> False
    }
  }
  |> should.be_true
}

// Arguments tests
pub fn parse_field_with_int_argument_test() {
  "{ user(id: 42) }"
  |> parser.parse
  |> should.be_ok
  |> fn(doc) {
    case doc {
      parser.Document([
        parser.Query(parser.SelectionSet([
          parser.Field(
            name: "user",
            alias: None,
            arguments: [parser.Argument("id", parser.IntValue("42"))],
            selections: [],
          ),
        ])),
      ]) -> True
      _ -> False
    }
  }
  |> should.be_true
}

pub fn parse_field_with_string_argument_test() {
  "{ user(name: \"Alice\") }"
  |> parser.parse
  |> should.be_ok
  |> fn(doc) {
    case doc {
      parser.Document([
        parser.Query(parser.SelectionSet([
          parser.Field(
            name: "user",
            alias: None,
            arguments: [parser.Argument("name", parser.StringValue("Alice"))],
            selections: [],
          ),
        ])),
      ]) -> True
      _ -> False
    }
  }
  |> should.be_true
}

pub fn parse_field_with_multiple_arguments_test() {
  "{ user(id: 42, name: \"Alice\") }"
  |> parser.parse
  |> should.be_ok
  |> fn(doc) {
    case doc {
      parser.Document([
        parser.Query(parser.SelectionSet([
          parser.Field(
            name: "user",
            alias: None,
            arguments: [
              parser.Argument("id", parser.IntValue("42")),
              parser.Argument("name", parser.StringValue("Alice")),
            ],
            selections: [],
          ),
        ])),
      ]) -> True
      _ -> False
    }
  }
  |> should.be_true
}

// Named operation tests
pub fn parse_named_query_test() {
  "query GetUser { user }"
  |> parser.parse
  |> should.be_ok
  |> fn(doc) {
    case doc {
      parser.Document([
        parser.NamedQuery(
          name: "GetUser",
          variables: [],
          selections: parser.SelectionSet([parser.Field("user", None, [], [])]),
        ),
      ]) -> True
      _ -> False
    }
  }
  |> should.be_true
}

// Complex query test
pub fn parse_complex_query_test() {
  "
  query GetUserPosts {
    user(id: 1) {
      name
      posts {
        title
        content
      }
    }
  }
  "
  |> parser.parse
  |> should.be_ok
}

// Error cases
pub fn parse_invalid_syntax_test() {
  "{ user"
  |> parser.parse
  |> should.be_error
}

pub fn parse_empty_string_test() {
  ""
  |> parser.parse
  |> should.be_error
}

pub fn parse_invalid_field_name_test() {
  "{ 123 }"
  |> parser.parse
  |> should.be_error
}

// Fragment tests
pub fn parse_fragment_definition_test() {
  "
  fragment UserFields on User {
    id
    name
  }
  { user { ...UserFields } }
  "
  |> parser.parse
  |> should.be_ok
  |> fn(doc) {
    case doc {
      parser.Document([
        parser.FragmentDefinition(
          name: "UserFields",
          type_condition: "User",
          selections: parser.SelectionSet([
            parser.Field("id", None, [], []),
            parser.Field("name", None, [], []),
          ]),
        ),
        parser.Query(parser.SelectionSet([
          parser.Field(
            name: "user",
            alias: None,
            arguments: [],
            selections: [parser.FragmentSpread("UserFields")],
          ),
        ])),
      ]) -> True
      _ -> False
    }
  }
  |> should.be_true
}

pub fn parse_inline_fragment_test() {
  "
  { user { ... on User { name } } }
  "
  |> parser.parse
  |> should.be_ok
}
