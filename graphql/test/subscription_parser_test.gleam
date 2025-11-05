/// Snapshot tests for subscription parsing
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

pub fn parse_simple_anonymous_subscription_test() {
  let query = "subscription { messageAdded { content author } }"

  let doc = case parser.parse(query) {
    Ok(d) -> d
    Error(_) -> panic as "Parse failed"
  }

  birdie.snap(title: "Simple anonymous subscription", content: format_ast(doc))
}

pub fn parse_named_subscription_test() {
  let query = "subscription OnMessage { messageAdded { id content } }"

  let doc = case parser.parse(query) {
    Ok(d) -> d
    Error(_) -> panic as "Parse failed"
  }

  birdie.snap(title: "Named subscription", content: format_ast(doc))
}

pub fn parse_subscription_with_nested_selections_test() {
  let query =
    "
    subscription {
      postCreated {
        id
        title
        author {
          id
          name
          email
        }
        comments {
          content
        }
      }
    }
  "

  let doc = case parser.parse(query) {
    Ok(d) -> d
    Error(_) -> panic as "Parse failed"
  }

  birdie.snap(
    title: "Subscription with nested selections",
    content: format_ast(doc),
  )
}
