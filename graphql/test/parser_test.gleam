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

pub fn parse_anonymous_query_with_keyword_test() {
  "query { user }"
  |> parser.parse
  |> should.be_ok
  |> fn(doc) {
    case doc {
      parser.Document([parser.Query(parser.SelectionSet([parser.Field("user", None, [], [])]))]) -> True
      _ -> False
    }
  }
  |> should.be_true
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

// List value tests
pub fn parse_empty_list_argument_test() {
  "{ user(tags: []) }"
  |> parser.parse
  |> should.be_ok
  |> fn(doc) {
    case doc {
      parser.Document([
        parser.Query(parser.SelectionSet([
          parser.Field(
            name: "user",
            alias: None,
            arguments: [parser.Argument("tags", parser.ListValue([]))],
            selections: [],
          ),
        ])),
      ]) -> True
      _ -> False
    }
  }
  |> should.be_true
}

pub fn parse_list_of_ints_test() {
  "{ user(ids: [1, 2, 3]) }"
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
              parser.Argument(
                "ids",
                parser.ListValue([
                  parser.IntValue("1"),
                  parser.IntValue("2"),
                  parser.IntValue("3"),
                ]),
              ),
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

pub fn parse_list_of_strings_test() {
  "{ user(tags: [\"foo\", \"bar\"]) }"
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
              parser.Argument(
                "tags",
                parser.ListValue([
                  parser.StringValue("foo"),
                  parser.StringValue("bar"),
                ]),
              ),
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

// Object value tests
pub fn parse_empty_object_argument_test() {
  "{ user(filter: {}) }"
  |> parser.parse
  |> should.be_ok
  |> fn(doc) {
    case doc {
      parser.Document([
        parser.Query(parser.SelectionSet([
          parser.Field(
            name: "user",
            alias: None,
            arguments: [parser.Argument("filter", parser.ObjectValue([]))],
            selections: [],
          ),
        ])),
      ]) -> True
      _ -> False
    }
  }
  |> should.be_true
}

pub fn parse_object_with_fields_test() {
  "{ user(filter: {name: \"Alice\", age: 30}) }"
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
              parser.Argument(
                "filter",
                parser.ObjectValue([
                  #("name", parser.StringValue("Alice")),
                  #("age", parser.IntValue("30")),
                ]),
              ),
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

// Nested structures
pub fn parse_list_of_objects_test() {
  "{ posts(sortBy: [{field: \"date\", direction: DESC}]) }"
  |> parser.parse
  |> should.be_ok
  |> fn(doc) {
    case doc {
      parser.Document([
        parser.Query(parser.SelectionSet([
          parser.Field(
            name: "posts",
            alias: None,
            arguments: [
              parser.Argument(
                "sortBy",
                parser.ListValue([
                  parser.ObjectValue([
                    #("field", parser.StringValue("date")),
                    #("direction", parser.EnumValue("DESC")),
                  ]),
                ]),
              ),
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

pub fn parse_object_with_nested_list_test() {
  "{ user(filter: {tags: [\"a\", \"b\"]}) }"
  |> parser.parse
  |> should.be_ok
}

// Variable definition tests
pub fn parse_query_with_one_variable_test() {
  "query Test($name: String!) { user }"
  |> parser.parse
  |> should.be_ok
  |> fn(doc) {
    case doc {
      parser.Document([
        parser.NamedQuery(
          name: "Test",
          variables: [parser.Variable("name", "String!")],
          selections: parser.SelectionSet([
            parser.Field("user", None, [], []),
          ]),
        ),
      ]) -> True
      _ -> False
    }
  }
  |> should.be_true
}

pub fn parse_query_with_multiple_variables_test() {
  "query Test($name: String!, $age: Int) { user }"
  |> parser.parse
  |> should.be_ok
  |> fn(doc) {
    case doc {
      parser.Document([
        parser.NamedQuery(
          name: "Test",
          variables: [
            parser.Variable("name", "String!"),
            parser.Variable("age", "Int"),
          ],
          selections: parser.SelectionSet([
            parser.Field("user", None, [], []),
          ]),
        ),
      ]) -> True
      _ -> False
    }
  }
  |> should.be_true
}

pub fn parse_mutation_with_variables_test() {
  "mutation CreateUser($name: String!, $email: String!) { createUser }"
  |> parser.parse
  |> should.be_ok
  |> fn(doc) {
    case doc {
      parser.Document([
        parser.NamedMutation(
          name: "CreateUser",
          variables: [
            parser.Variable("name", "String!"),
            parser.Variable("email", "String!"),
          ],
          selections: parser.SelectionSet([
            parser.Field("createUser", None, [], []),
          ]),
        ),
      ]) -> True
      _ -> False
    }
  }
  |> should.be_true
}

pub fn parse_variable_value_in_argument_test() {
  "{ user(name: $userName) }"
  |> parser.parse
  |> should.be_ok
  |> fn(doc) {
    case doc {
      parser.Document([
        parser.Query(parser.SelectionSet([
          parser.Field(
            name: "user",
            alias: None,
            arguments: [parser.Argument("name", parser.VariableValue("userName"))],
            selections: [],
          ),
        ])),
      ]) -> True
      _ -> False
    }
  }
  |> should.be_true
}
