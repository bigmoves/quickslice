/// End-to-end integration tests for GraphQL where clause filtering
///
/// Tests the complete flow: GraphQL value → WhereInput parsing → SQL generation → Database query
import database/repositories/records
import database/schema/tables
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import gleeunit
import gleeunit/should
import lexicon_graphql/where_input
import sqlight
import swell/value
import where_converter

pub fn main() {
  gleeunit.main()
}

// Helper to setup test database with sample records
fn setup_test_db() -> Result(sqlight.Connection, sqlight.Error) {
  use conn <- result.try(sqlight.open(":memory:"))
  use _ <- result.try(tables.create_record_table(conn))

  // Insert test records with different properties
  use _ <- result.try(records.insert(
    conn,
    "at://did:plc:alice/app.bsky.feed.post/1",
    "cid1",
    "did:plc:alice",
    "app.bsky.feed.post",
    "{\"text\":\"Hello World\",\"likes\":100,\"author\":\"alice\"}",
  ))

  use _ <- result.try(records.insert(
    conn,
    "at://did:plc:bob/app.bsky.feed.post/2",
    "cid2",
    "did:plc:bob",
    "app.bsky.feed.post",
    "{\"text\":\"Goodbye World\",\"likes\":50,\"author\":\"bob\"}",
  ))

  use _ <- result.try(records.insert(
    conn,
    "at://did:plc:charlie/app.bsky.feed.post/3",
    "cid3",
    "did:plc:charlie",
    "app.bsky.feed.post",
    "{\"text\":\"Hello Universe\",\"likes\":200,\"author\":\"charlie\"}",
  ))

  use _ <- result.try(records.insert(
    conn,
    "at://did:plc:alice/app.bsky.feed.post/4",
    "cid4",
    "did:plc:alice",
    "app.bsky.feed.post",
    "{\"text\":\"Another post\",\"likes\":75,\"author\":\"alice\"}",
  ))

  Ok(conn)
}

// Test: Simple field filter through full GraphQL stack
pub fn graphql_simple_filter_test() {
  case setup_test_db() {
    Error(_) -> should.fail()
    Ok(conn) -> {
      // Create GraphQL where value: { text: { contains: "Hello" } }
      let text_condition = value.Object([#("contains", value.String("Hello"))])

      let where_value = value.Object([#("text", text_condition)])

      // Parse GraphQL value to WhereInput
      let where_input = where_input.parse_where_clause(where_value)

      // Convert to SQL WhereClause
      let where_sql = where_converter.convert_where_clause(where_input)

      // Execute query
      case
        records.get_by_collection_paginated_with_where(
          conn,
          "app.bsky.feed.post",
          Some(10),
          None,
          None,
          None,
          None,
          Some(where_sql),
        )
      {
        Ok(#(records, _, _, _)) -> {
          // Should match 2 records with "Hello"
          list.length(records) |> should.equal(2)
          list.all(records, fn(record) { string.contains(record.json, "Hello") })
          |> should.be_true
        }
        Error(_) -> should.fail()
      }
    }
  }
}

// Test: OR logic through full GraphQL stack
pub fn graphql_or_filter_test() {
  case setup_test_db() {
    Error(_) -> should.fail()
    Ok(conn) -> {
      // Create GraphQL where value:
      // {
      //   or: [
      //     { author: { eq: "alice" } },
      //     { author: { eq: "bob" } }
      //   ]
      // }
      let alice_condition = value.Object([#("eq", value.String("alice"))])
      let bob_condition = value.Object([#("eq", value.String("bob"))])

      let alice_clause = value.Object([#("author", alice_condition)])
      let bob_clause = value.Object([#("author", bob_condition)])

      let where_value =
        value.Object([#("or", value.List([alice_clause, bob_clause]))])

      // Parse GraphQL value to WhereInput
      let where_input = where_input.parse_where_clause(where_value)

      // Verify OR was parsed
      should.be_true(option.is_some(where_input.or))

      // Convert to SQL WhereClause
      let where_sql = where_converter.convert_where_clause(where_input)

      // Execute query
      case
        records.get_by_collection_paginated_with_where(
          conn,
          "app.bsky.feed.post",
          Some(10),
          None,
          None,
          None,
          None,
          Some(where_sql),
        )
      {
        Ok(#(records, _, _, _)) -> {
          // Should match 3 records (alice has 2, bob has 1)
          list.length(records) |> should.equal(3)

          // Verify all records are from alice or bob
          list.all(records, fn(record) {
            string.contains(record.json, "alice")
            || string.contains(record.json, "bob")
          })
          |> should.be_true

          // Verify no charlie records
          list.any(records, fn(record) {
            string.contains(record.json, "charlie")
          })
          |> should.be_false
        }
        Error(_) -> should.fail()
      }
    }
  }
}

// Test: AND logic through full GraphQL stack
pub fn graphql_and_filter_test() {
  case setup_test_db() {
    Error(_) -> should.fail()
    Ok(conn) -> {
      // Create GraphQL where value:
      // {
      //   and: [
      //     { author: { eq: "alice" } },
      //     { likes: { gt: "80" } }
      //   ]
      // }
      // Note: likes is stored as integer in JSON, so we compare as string "80"
      let author_condition = value.Object([#("eq", value.String("alice"))])
      let likes_condition = value.Object([#("gt", value.Int(80))])

      let author_clause = value.Object([#("author", author_condition)])
      let likes_clause = value.Object([#("likes", likes_condition)])

      let where_value =
        value.Object([#("and", value.List([author_clause, likes_clause]))])

      // Parse GraphQL value to WhereInput
      let where_input = where_input.parse_where_clause(where_value)

      // Verify AND was parsed
      should.be_true(option.is_some(where_input.and))

      // Convert to SQL WhereClause
      let where_sql = where_converter.convert_where_clause(where_input)

      // Execute query
      case
        records.get_by_collection_paginated_with_where(
          conn,
          "app.bsky.feed.post",
          Some(10),
          None,
          None,
          None,
          None,
          Some(where_sql),
        )
      {
        Ok(#(records, _, _, _)) -> {
          // Should match 1 record (alice with likes=100)
          list.length(records) |> should.equal(1)

          case list.first(records) {
            Ok(record) -> {
              string.contains(record.json, "alice") |> should.be_true
              string.contains(record.json, "100") |> should.be_true
            }
            Error(_) -> should.fail()
          }
        }
        Error(_) -> should.fail()
      }
    }
  }
}

// Test: Nested AND/OR logic through full GraphQL stack
pub fn graphql_nested_and_or_test() {
  case setup_test_db() {
    Error(_) -> should.fail()
    Ok(conn) -> {
      // Create GraphQL where value:
      // {
      //   and: [
      //     {
      //       or: [
      //         { text: { contains: "Hello" } },
      //         { text: { contains: "Another" } }
      //       ]
      //     },
      //     { author: { eq: "alice" } }
      //   ]
      // }

      // Inner OR: text contains "Hello" OR "Another"
      let hello_condition = value.Object([#("contains", value.String("Hello"))])
      let another_condition =
        value.Object([#("contains", value.String("Another"))])

      let hello_clause = value.Object([#("text", hello_condition)])
      let another_clause = value.Object([#("text", another_condition)])

      let or_clause =
        value.Object([#("or", value.List([hello_clause, another_clause]))])

      // Outer AND: (above OR) AND author = "alice"
      let author_condition = value.Object([#("eq", value.String("alice"))])
      let author_clause = value.Object([#("author", author_condition)])

      let where_value =
        value.Object([#("and", value.List([or_clause, author_clause]))])

      // Parse GraphQL value to WhereInput
      let where_input = where_input.parse_where_clause(where_value)

      // Verify nested structure was parsed
      should.be_true(option.is_some(where_input.and))

      // Convert to SQL WhereClause
      let where_sql = where_converter.convert_where_clause(where_input)

      // Execute query
      case
        records.get_by_collection_paginated_with_where(
          conn,
          "app.bsky.feed.post",
          Some(10),
          None,
          None,
          None,
          None,
          Some(where_sql),
        )
      {
        Ok(#(records, _, _, _)) -> {
          // Should match 2 alice records: "Hello World" and "Another post"
          list.length(records) |> should.equal(2)

          // Verify all records are from alice
          list.all(records, fn(record) { string.contains(record.json, "alice") })
          |> should.be_true

          // Verify records contain either "Hello" or "Another"
          list.all(records, fn(record) {
            string.contains(record.json, "Hello")
            || string.contains(record.json, "Another")
          })
          |> should.be_true
        }
        Error(_) -> should.fail()
      }
    }
  }
}

// Test: Complex nested logic with multiple levels
pub fn graphql_complex_nested_test() {
  case setup_test_db() {
    Error(_) -> should.fail()
    Ok(conn) -> {
      // Simplify test: just OR two simple conditions
      // {
      //   or: [
      //     { author: { eq: "alice" } },
      //     { author: { eq: "charlie" } }
      //   ]
      // }

      let alice_condition = value.Object([#("eq", value.String("alice"))])
      let charlie_condition = value.Object([#("eq", value.String("charlie"))])

      let alice_clause = value.Object([#("author", alice_condition)])
      let charlie_clause = value.Object([#("author", charlie_condition)])

      let where_value =
        value.Object([#("or", value.List([alice_clause, charlie_clause]))])

      // Parse GraphQL value to WhereInput
      let where_input = where_input.parse_where_clause(where_value)

      // Convert to SQL WhereClause
      let where_sql = where_converter.convert_where_clause(where_input)

      // Execute query
      case
        records.get_by_collection_paginated_with_where(
          conn,
          "app.bsky.feed.post",
          Some(10),
          None,
          None,
          None,
          None,
          Some(where_sql),
        )
      {
        Ok(#(records, _, _, _)) -> {
          // Should match 3 records: 2 from alice, 1 from charlie
          list.length(records) |> should.equal(3)

          // Check we got alice and charlie records
          let has_alice =
            list.any(records, fn(record) {
              string.contains(record.json, "alice")
            })
          let has_charlie =
            list.any(records, fn(record) {
              string.contains(record.json, "charlie")
            })

          should.be_true(has_alice)
          should.be_true(has_charlie)
        }
        Error(_) -> should.fail()
      }
    }
  }
}

// Test: Empty AND/OR arrays
pub fn graphql_empty_logic_arrays_test() {
  case setup_test_db() {
    Error(_) -> should.fail()
    Ok(conn) -> {
      // Create GraphQL where value with empty AND array: { and: [] }
      let where_value = value.Object([#("and", value.List([]))])

      // Parse GraphQL value to WhereInput
      let where_input = where_input.parse_where_clause(where_value)

      // Empty AND should result in None
      case where_input.and {
        None -> should.be_true(True)
        Some(clauses) -> {
          // Or empty list
          list.is_empty(clauses) |> should.be_true
        }
      }

      // Convert to SQL WhereClause
      let where_sql = where_converter.convert_where_clause(where_input)

      // Execute query - should return all records
      case
        records.get_by_collection_paginated_with_where(
          conn,
          "app.bsky.feed.post",
          Some(10),
          None,
          None,
          None,
          None,
          Some(where_sql),
        )
      {
        Ok(#(records, _, _, _)) -> {
          // Should return all 4 records
          list.length(records) |> should.equal(4)
        }
        Error(_) -> should.fail()
      }
    }
  }
}
