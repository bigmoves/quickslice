/// Snapshot tests for mutation parsing
import birdie
import gleam/string
import gleeunit
import graphql/parser

pub fn main() {
  gleeunit.main()
}

// Helper to format AST as string for snapshots
fn format_ast(doc: parser.Document) -> String {
  string.inspect(doc)
}

pub fn parse_simple_anonymous_mutation_test() {
  let query = "mutation { createUser(name: \"Alice\") { id name } }"

  let doc = case parser.parse(query) {
    Ok(d) -> d
    Error(_) -> panic as "Parse failed"
  }

  birdie.snap(title: "Simple anonymous mutation", content: format_ast(doc))
}

pub fn parse_named_mutation_test() {
  let query = "mutation CreateUser { createUser(name: \"Alice\") { id name } }"

  let doc = case parser.parse(query) {
    Ok(d) -> d
    Error(_) -> panic as "Parse failed"
  }

  birdie.snap(title: "Named mutation", content: format_ast(doc))
}

pub fn parse_mutation_with_input_object_test() {
  let query =
    "
    mutation {
      createUser(input: { name: \"Alice\", email: \"alice@example.com\", age: 30 }) {
        id
        name
        email
      }
    }
  "

  let doc = case parser.parse(query) {
    Ok(d) -> d
    Error(_) -> panic as "Parse failed"
  }

  birdie.snap(
    title: "Parse mutation with input object argument",
    content: format_ast(doc),
  )
}

pub fn parse_multiple_mutations_test() {
  let query =
    "
    mutation {
      createUser(name: \"Alice\") { id }
      deleteUser(id: \"123\") { success }
    }
  "

  let doc = case parser.parse(query) {
    Ok(d) -> d
    Error(_) -> panic as "Parse failed"
  }

  birdie.snap(
    title: "Multiple mutations in one operation",
    content: format_ast(doc),
  )
}

pub fn parse_mutation_with_nested_selections_test() {
  let query =
    "
    mutation {
      createPost(input: { title: \"Hello\" }) {
        id
        author {
          id
          name
        }
        tags
      }
    }
  "

  let doc = case parser.parse(query) {
    Ok(d) -> d
    Error(_) -> panic as "Parse failed"
  }

  birdie.snap(
    title: "Mutation with nested selections",
    content: format_ast(doc),
  )
}

