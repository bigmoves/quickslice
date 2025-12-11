/// Integration tests for GraphQL totalCount field
///
/// These tests verify that totalCount is correctly returned in connection queries
import database/repositories/actors
import database/repositories/lexicons
import database/repositories/records
import gleam/http
import gleam/int
import gleam/json
import gleam/list
import gleam/option
import gleam/string
import gleeunit/should
import handlers/graphql as graphql_handler
import lib/oauth/did_cache
import test_helpers
import wisp
import wisp/simulate

// Helper to create a status lexicon
fn create_status_lexicon() -> String {
  json.object([
    #("lexicon", json.int(1)),
    #("id", json.string("xyz.statusphere.status")),
    #(
      "defs",
      json.object([
        #(
          "main",
          json.object([
            #("type", json.string("record")),
            #("key", json.string("tid")),
            #(
              "record",
              json.object([
                #("type", json.string("object")),
                #(
                  "required",
                  json.array(
                    [json.string("status"), json.string("createdAt")],
                    of: fn(x) { x },
                  ),
                ),
                #(
                  "properties",
                  json.object([
                    #(
                      "status",
                      json.object([
                        #("type", json.string("string")),
                        #("minLength", json.int(1)),
                        #("maxGraphemes", json.int(1)),
                        #("maxLength", json.int(32)),
                      ]),
                    ),
                    #(
                      "createdAt",
                      json.object([
                        #("type", json.string("string")),
                        #("format", json.string("datetime")),
                      ]),
                    ),
                  ]),
                ),
              ]),
            ),
          ]),
        ),
      ]),
    ),
  ])
  |> json.to_string
}

// Helper function to create a range of integers
fn list_range(from: Int, to: Int) -> List(Int) {
  list_range_helper(from, to, [])
  |> list.reverse
}

fn list_range_helper(current: Int, to: Int, acc: List(Int)) -> List(Int) {
  case current > to {
    True -> acc
    False -> list_range_helper(current + 1, to, [current, ..acc])
  }
}

pub fn graphql_total_count_basic_test() {
  // Create in-memory database
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_lexicon_table(exec)
  let assert Ok(_) = test_helpers.create_record_table(exec)
  let assert Ok(_) = test_helpers.create_actor_table(exec)

  // Insert a lexicon
  let lexicon = create_status_lexicon()
  let assert Ok(_) = lexicons.insert(exec, "xyz.statusphere.status", lexicon)

  // Insert 5 test records
  let _ =
    list_range(1, 5)
    |> list.each(fn(i) {
      let uri = "at://did:plc:test/xyz.statusphere.status/" <> int.to_string(i)
      let cid = "cid" <> int.to_string(i)
      let json_data =
        json.object([
          #("status", json.string("✨")),
          #("createdAt", json.string("2024-01-01T00:00:00Z")),
        ])
        |> json.to_string
      let assert Ok(_) =
        records.insert(
          exec,
          uri,
          cid,
          "did:plc:test",
          "xyz.statusphere.status",
          json_data,
        )
      Nil
    })

  // Query with totalCount field
  let query =
    json.object([
      #(
        "query",
        json.string(
          "{ xyzStatusphereStatus { totalCount edges { node { uri } } pageInfo { hasNextPage } } }",
        ),
      ),
    ])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query)
    |> simulate.header("content-type", "application/json")

  let assert Ok(cache) = did_cache.start()
  let response =
    graphql_handler.handle_graphql_request(
      request,
      exec,
      cache,
      option.None,
      "",
      "https://plc.directory",
    )

  // Verify response
  response.status
  |> should.equal(200)

  let assert wisp.Text(body) = response.body

  // Should contain totalCount field
  string.contains(body, "totalCount")
  |> should.be_true

  // Should contain the count value (5 records)
  string.contains(body, "\"totalCount\": 5")
  |> should.be_true
  // Clean up
}

pub fn graphql_total_count_with_filter_test() {
  // Create in-memory database
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_lexicon_table(exec)
  let assert Ok(_) = test_helpers.create_record_table(exec)
  let assert Ok(_) = test_helpers.create_actor_table(exec)

  // Insert a lexicon
  let lexicon = create_status_lexicon()
  let assert Ok(_) = lexicons.insert(exec, "xyz.statusphere.status", lexicon)

  // Insert test actors
  let assert Ok(_) = actors.upsert(exec, "did:plc:alice", "alice.bsky.social")
  let assert Ok(_) = actors.upsert(exec, "did:plc:bob", "bob.bsky.social")

  // Insert 3 records for alice
  let _ =
    list_range(1, 3)
    |> list.each(fn(i) {
      let uri = "at://did:plc:alice/xyz.statusphere.status/" <> int.to_string(i)
      let cid = "alice_cid" <> int.to_string(i)
      let json_data =
        json.object([
          #("status", json.string("👍")),
          #("createdAt", json.string("2024-01-01T00:00:00Z")),
        ])
        |> json.to_string
      let assert Ok(_) =
        records.insert(
          exec,
          uri,
          cid,
          "did:plc:alice",
          "xyz.statusphere.status",
          json_data,
        )
      Nil
    })

  // Insert 2 records for bob
  let _ =
    list_range(1, 2)
    |> list.each(fn(i) {
      let uri = "at://did:plc:bob/xyz.statusphere.status/" <> int.to_string(i)
      let cid = "bob_cid" <> int.to_string(i)
      let json_data =
        json.object([
          #("status", json.string("🔥")),
          #("createdAt", json.string("2024-01-02T00:00:00Z")),
        ])
        |> json.to_string
      let assert Ok(_) =
        records.insert(
          exec,
          uri,
          cid,
          "did:plc:bob",
          "xyz.statusphere.status",
          json_data,
        )
      Nil
    })

  // Query with totalCount and filter by actorHandle
  let query =
    json.object([
      #(
        "query",
        json.string(
          "{ xyzStatusphereStatus(where: {actorHandle: {eq: \"alice.bsky.social\"}}) { totalCount edges { node { uri actorHandle } } } }",
        ),
      ),
    ])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query)
    |> simulate.header("content-type", "application/json")

  let assert Ok(cache) = did_cache.start()
  let response =
    graphql_handler.handle_graphql_request(
      request,
      exec,
      cache,
      option.None,
      "",
      "https://plc.directory",
    )

  // Verify response
  response.status
  |> should.equal(200)

  let assert wisp.Text(body) = response.body

  // Should contain totalCount field
  string.contains(body, "totalCount")
  |> should.be_true

  // Should contain count of 3 (only alice's records)
  string.contains(body, "\"totalCount\": 3")
  |> should.be_true

  // Should only contain alice's records
  string.contains(body, "alice.bsky.social")
  |> should.be_true

  string.contains(body, "bob.bsky.social")
  |> should.be_false
  // Clean up
}

pub fn graphql_total_count_empty_result_test() {
  // Create in-memory database
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_lexicon_table(exec)
  let assert Ok(_) = test_helpers.create_record_table(exec)

  // Insert a lexicon
  let lexicon = create_status_lexicon()
  let assert Ok(_) = lexicons.insert(exec, "xyz.statusphere.status", lexicon)

  // Query with totalCount field (no records inserted)
  let query =
    json.object([
      #(
        "query",
        json.string(
          "{ xyzStatusphereStatus { totalCount edges { node { uri } } pageInfo { hasNextPage } } }",
        ),
      ),
    ])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query)
    |> simulate.header("content-type", "application/json")

  let assert Ok(cache) = did_cache.start()
  let response =
    graphql_handler.handle_graphql_request(
      request,
      exec,
      cache,
      option.None,
      "",
      "https://plc.directory",
    )

  // Verify response
  response.status
  |> should.equal(200)

  let assert wisp.Text(body) = response.body

  // Should contain totalCount of 0
  string.contains(body, "\"totalCount\": 0")
  |> should.be_true
  // Clean up
}

pub fn graphql_total_count_with_pagination_test() {
  // Create in-memory database
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_lexicon_table(exec)
  let assert Ok(_) = test_helpers.create_record_table(exec)

  // Insert a lexicon
  let lexicon = create_status_lexicon()
  let assert Ok(_) = lexicons.insert(exec, "xyz.statusphere.status", lexicon)

  // Insert 10 test records
  let _ =
    list_range(1, 10)
    |> list.each(fn(i) {
      let uri = "at://did:plc:test/xyz.statusphere.status/" <> int.to_string(i)
      let cid = "cid" <> int.to_string(i)
      let json_data =
        json.object([
          #("status", json.string("✨")),
          #("createdAt", json.string("2024-01-01T00:00:00Z")),
        ])
        |> json.to_string
      let assert Ok(_) =
        records.insert(
          exec,
          uri,
          cid,
          "did:plc:test",
          "xyz.statusphere.status",
          json_data,
        )
      Nil
    })

  // Query with totalCount and pagination (first: 3)
  let query =
    json.object([
      #(
        "query",
        json.string(
          "{ xyzStatusphereStatus(first: 3) { totalCount edges { node { uri } } pageInfo { hasNextPage } } }",
        ),
      ),
    ])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query)
    |> simulate.header("content-type", "application/json")

  let assert Ok(cache) = did_cache.start()
  let response =
    graphql_handler.handle_graphql_request(
      request,
      exec,
      cache,
      option.None,
      "",
      "https://plc.directory",
    )

  // Verify response
  response.status
  |> should.equal(200)

  let assert wisp.Text(body) = response.body

  // totalCount should still be 10 (total records, not just the page)
  string.contains(body, "\"totalCount\": 10")
  |> should.be_true

  // hasNextPage should be true (more records available)
  string.contains(body, "\"hasNextPage\": true")
  |> should.be_true
  // Clean up
}
