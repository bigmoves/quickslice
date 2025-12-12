/// Integration tests for GraphQL handler with database
///
/// These tests verify the full GraphQL query flow:
/// 1. Database setup with lexicons and records
/// 2. GraphQL schema building from database lexicons
/// 3. Query execution and result formatting
/// 4. JSON parsing and encoding throughout the pipeline
import database/repositories/actors
import database/repositories/lexicons
import database/repositories/records
import gleam/http
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None}
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

// Helper to create a simple lexicon with just properties
fn create_simple_lexicon(nsid: String) -> String {
  json.object([
    #("lexicon", json.int(1)),
    #("id", json.string(nsid)),
    #(
      "defs",
      json.object([
        #(
          "main",
          json.object([
            #("type", json.string("record")),
            #(
              "record",
              json.object([
                #(
                  "properties",
                  json.object([
                    #("status", json.object([#("type", json.string("string"))])),
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

pub fn graphql_post_request_with_records_test() {
  // Create in-memory database
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_lexicon_table(exec)
  let assert Ok(_) = test_helpers.create_record_table(exec)

  // Insert a lexicon for xyz.statusphere.status
  let lexicon = create_status_lexicon()
  let assert Ok(_) = lexicons.insert(exec, "xyz.statusphere.status", lexicon)

  // Insert some test records
  let record1_json =
    json.object([
      #("status", json.string("🎉")),
      #("createdAt", json.string("2024-01-01T00:00:00Z")),
    ])
    |> json.to_string

  let assert Ok(_) =
    records.insert(
      exec,
      "at://did:plc:test1/xyz.statusphere.status/123",
      "cid1",
      "did:plc:test1",
      "xyz.statusphere.status",
      record1_json,
    )

  let record2_json =
    json.object([
      #("status", json.string("🔥")),
      #("createdAt", json.string("2024-01-02T00:00:00Z")),
    ])
    |> json.to_string

  let assert Ok(_) =
    records.insert(
      exec,
      "at://did:plc:test2/xyz.statusphere.status/456",
      "cid2",
      "did:plc:test2",
      "xyz.statusphere.status",
      record2_json,
    )

  // Create GraphQL query request with Connection structure
  let query =
    json.object([
      #(
        "query",
        json.string(
          "{ xyzStatusphereStatus { edges { node { uri cid did collection status createdAt } cursor } pageInfo { hasNextPage hasPreviousPage startCursor endCursor } } }",
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
      None,
      "",
      "https://plc.directory",
    )

  // Verify response
  response.status
  |> should.equal(200)

  // Get response body
  let assert wisp.Text(body) = response.body

  // Verify response contains data structure
  body
  |> should.not_equal("")

  // Response should contain "data"
  string.contains(body, "data")
  |> should.be_true

  // Response should contain field name
  string.contains(body, "xyzStatusphereStatus")
  |> should.be_true

  // Response should contain our test URIs
  string.contains(body, "at://did:plc:test1/xyz.statusphere.status/123")
  |> should.be_true

  string.contains(body, "at://did:plc:test2/xyz.statusphere.status/456")
  |> should.be_true

  // Response should contain our test data
  string.contains(body, "🎉")
  |> should.be_true

  string.contains(body, "🔥")
  |> should.be_true
  // Clean up handled automatically
}

pub fn graphql_post_request_empty_results_test() {
  // Create in-memory database
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_lexicon_table(exec)
  let assert Ok(_) = test_helpers.create_record_table(exec)

  // Insert a lexicon but no records
  let lexicon = create_simple_lexicon("xyz.statusphere.status")
  let assert Ok(_) = lexicons.insert(exec, "xyz.statusphere.status", lexicon)

  // Create GraphQL query request with Connection structure
  let query =
    json.object([
      #(
        "query",
        json.string(
          "{ xyzStatusphereStatus { edges { node { uri } } pageInfo { hasNextPage } } }",
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
      None,
      "",
      "https://plc.directory",
    )

  // Verify response
  response.status
  |> should.equal(200)

  // Get response body
  let assert wisp.Text(body) = response.body

  // Should return empty array
  string.contains(body, "[]")
  |> should.be_true
  // Clean up handled automatically
}

pub fn graphql_get_request_test() {
  // Create in-memory database
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_lexicon_table(exec)
  let assert Ok(_) = test_helpers.create_record_table(exec)

  // Insert a lexicon
  let lexicon = create_simple_lexicon("xyz.statusphere.status")
  let assert Ok(_) = lexicons.insert(exec, "xyz.statusphere.status", lexicon)

  // Create GraphQL GET request with query parameter
  let request =
    simulate.request(
      http.Get,
      "/graphql?query={ xyzStatusphereStatus { edges { node { uri } } } }",
    )

  let assert Ok(cache) = did_cache.start()
  let response =
    graphql_handler.handle_graphql_request(
      request,
      exec,
      cache,
      None,
      "",
      "https://plc.directory",
    )

  // Verify response
  response.status
  |> should.equal(200)

  // Get response body
  let assert wisp.Text(body) = response.body

  // Should contain data
  string.contains(body, "data")
  |> should.be_true
  // Clean up handled automatically
}

pub fn graphql_invalid_json_request_test() {
  // Create in-memory database
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_lexicon_table(exec)

  // Create request with invalid JSON
  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body("not valid json")
    |> simulate.header("content-type", "application/json")

  let assert Ok(cache) = did_cache.start()
  let response =
    graphql_handler.handle_graphql_request(
      request,
      exec,
      cache,
      None,
      "",
      "https://plc.directory",
    )

  // Should return 400 Bad Request
  response.status
  |> should.equal(400)

  // Get response body
  let assert wisp.Text(body) = response.body

  // Should contain error
  string.contains(body, "error")
  |> should.be_true
  // Clean up handled automatically
}

pub fn graphql_missing_query_field_test() {
  // Create in-memory database
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_lexicon_table(exec)

  // Create request with JSON but no query field
  let body_json =
    json.object([#("foo", json.string("bar"))])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(body_json)
    |> simulate.header("content-type", "application/json")

  let assert Ok(cache) = did_cache.start()
  let response =
    graphql_handler.handle_graphql_request(
      request,
      exec,
      cache,
      None,
      "",
      "https://plc.directory",
    )

  // Should return 400 Bad Request
  response.status
  |> should.equal(400)

  // Get response body
  let assert wisp.Text(body) = response.body

  // Should contain error about missing query
  string.contains(body, "query")
  |> should.be_true
  // Clean up handled automatically
}

pub fn graphql_method_not_allowed_test() {
  // Create in-memory database
  let assert Ok(exec) = test_helpers.create_test_db()

  // Create DELETE request (not allowed)
  let request = simulate.request(http.Delete, "/graphql")

  let assert Ok(cache) = did_cache.start()
  let response =
    graphql_handler.handle_graphql_request(
      request,
      exec,
      cache,
      None,
      "",
      "https://plc.directory",
    )

  // Should return 405 Method Not Allowed
  response.status
  |> should.equal(405)

  // Get response body
  let assert wisp.Text(body) = response.body

  // Should contain error
  string.contains(body, "MethodNotAllowed")
  |> should.be_true
  // Clean up handled automatically
}

pub fn graphql_multiple_lexicons_test() {
  // Create in-memory database
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_lexicon_table(exec)
  let assert Ok(_) = test_helpers.create_record_table(exec)

  // Insert multiple lexicons
  let lexicon1 = create_simple_lexicon("xyz.statusphere.status")
  let lexicon2 =
    json.object([
      #("lexicon", json.int(1)),
      #("id", json.string("app.bsky.feed.post")),
      #(
        "defs",
        json.object([
          #(
            "main",
            json.object([
              #("type", json.string("record")),
              #(
                "record",
                json.object([
                  #(
                    "properties",
                    json.object([
                      #("text", json.object([#("type", json.string("string"))])),
                      #(
                        "createdAt",
                        json.object([#("type", json.string("string"))]),
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

  let assert Ok(_) = lexicons.insert(exec, "xyz.statusphere.status", lexicon1)
  let assert Ok(_) = lexicons.insert(exec, "app.bsky.feed.post", lexicon2)

  // Insert records for first collection
  let record1_json =
    json.object([#("status", json.string("✨"))])
    |> json.to_string

  let assert Ok(_) =
    records.insert(
      exec,
      "at://did:plc:test/xyz.statusphere.status/1",
      "cid1",
      "did:plc:test",
      "xyz.statusphere.status",
      record1_json,
    )

  // Query the first collection
  let query1 =
    json.object([
      #(
        "query",
        json.string(
          "{ xyzStatusphereStatus { edges { node { uri } } pageInfo { hasNextPage } } }",
        ),
      ),
    ])
    |> json.to_string
  let request1 =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query1)
    |> simulate.header("content-type", "application/json")

  let assert Ok(cache1) = did_cache.start()
  let response1 =
    graphql_handler.handle_graphql_request(
      request1,
      exec,
      cache1,
      None,
      "",
      "https://plc.directory",
    )

  response1.status
  |> should.equal(200)

  let assert wisp.Text(body1) = response1.body

  string.contains(body1, "xyzStatusphereStatus")
  |> should.be_true

  // Insert records for second collection
  let record2_json =
    json.object([
      #("text", json.string("Hello World")),
      #("createdAt", json.string("2024-01-01T00:00:00Z")),
    ])
    |> json.to_string

  let assert Ok(_) =
    records.insert(
      exec,
      "at://did:plc:test/app.bsky.feed.post/1",
      "cid2",
      "did:plc:test",
      "app.bsky.feed.post",
      record2_json,
    )

  // Query the second collection
  let query2 =
    json.object([
      #(
        "query",
        json.string(
          "{ appBskyFeedPost { edges { node { uri } } pageInfo { hasNextPage } } }",
        ),
      ),
    ])
    |> json.to_string
  let request2 =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query2)
    |> simulate.header("content-type", "application/json")

  let assert Ok(cache2) = did_cache.start()
  let response2 =
    graphql_handler.handle_graphql_request(
      request2,
      exec,
      cache2,
      None,
      "",
      "https://plc.directory",
    )

  response2.status
  |> should.equal(200)

  let assert wisp.Text(body2) = response2.body

  string.contains(body2, "appBskyFeedPost")
  |> should.be_true
  // Clean up handled automatically
}

pub fn graphql_record_limit_test() {
  // Create in-memory database
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_lexicon_table(exec)
  let assert Ok(_) = test_helpers.create_record_table(exec)

  // Insert a lexicon
  let lexicon = create_simple_lexicon("xyz.statusphere.status")
  let assert Ok(_) = lexicons.insert(exec, "xyz.statusphere.status", lexicon)

  // Insert 150 records (handler should limit to 100)
  let _ =
    list_range(1, 150)
    |> list.each(fn(i) {
      let uri = "at://did:plc:test/xyz.statusphere.status/" <> int.to_string(i)
      let cid = "cid" <> int.to_string(i)
      let json_data =
        json.object([#("status", json.string(int.to_string(i)))])
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

  // Query all records with Connection structure
  let query =
    json.object([
      #(
        "query",
        json.string(
          "{ xyzStatusphereStatus { edges { node { uri } } pageInfo { hasNextPage } } }",
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
      None,
      "",
      "https://plc.directory",
    )

  response.status
  |> should.equal(200)

  let assert wisp.Text(body) = response.body

  // Count how many URIs are in the response
  // With default pagination (50 items), we should get 50 records
  let uri_count = count_occurrences(body, "\"uri\"")

  // Should return 50 records (the default page size)
  uri_count
  |> should.equal(50)
  // Clean up handled automatically
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

// Helper to count occurrences of a substring
fn count_occurrences(text: String, pattern: String) -> Int {
  string.split(text, pattern)
  |> list.length
  |> fn(n) { n - 1 }
}

pub fn graphql_actor_handle_lookup_test() {
  // Create in-memory database
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_lexicon_table(exec)
  let assert Ok(_) = test_helpers.create_record_table(exec)
  let assert Ok(_) = test_helpers.create_actor_table(exec)

  // Insert a lexicon for xyz.statusphere.status
  let lexicon = create_status_lexicon()
  let assert Ok(_) = lexicons.insert(exec, "xyz.statusphere.status", lexicon)

  // Insert test actors
  let assert Ok(_) = actors.upsert(exec, "did:plc:alice", "alice.bsky.social")
  let assert Ok(_) = actors.upsert(exec, "did:plc:bob", "bob.bsky.social")

  // Insert test records with those DIDs
  let record1_json =
    json.object([
      #("status", json.string("👍")),
      #("createdAt", json.string("2024-01-01T00:00:00Z")),
    ])
    |> json.to_string

  let assert Ok(_) =
    records.insert(
      exec,
      "at://did:plc:alice/xyz.statusphere.status/123",
      "cid1",
      "did:plc:alice",
      "xyz.statusphere.status",
      record1_json,
    )

  let record2_json =
    json.object([
      #("status", json.string("🔥")),
      #("createdAt", json.string("2024-01-02T00:00:00Z")),
    ])
    |> json.to_string

  let assert Ok(_) =
    records.insert(
      exec,
      "at://did:plc:bob/xyz.statusphere.status/456",
      "cid2",
      "did:plc:bob",
      "xyz.statusphere.status",
      record2_json,
    )

  // Query with actorHandle field
  let query =
    json.object([
      #(
        "query",
        json.string(
          "{ xyzStatusphereStatus(where: {status: {contains: \"👍\"}}, sortBy: [{direction: DESC, field: createdAt}]) { edges { node { actorHandle did status createdAt } cursor } pageInfo { hasNextPage } } }",
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
      None,
      "",
      "https://plc.directory",
    )

  // Verify response
  response.status
  |> should.equal(200)

  let assert wisp.Text(body) = response.body

  // Should contain the actor handle
  string.contains(body, "alice.bsky.social")
  |> should.be_true

  // Should contain the record data
  string.contains(body, "did:plc:alice")
  |> should.be_true

  string.contains(body, "👍")
  |> should.be_true
  // Clean up handled automatically
}

pub fn graphql_filter_by_actor_handle_test() {
  // Create in-memory database
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_lexicon_table(exec)
  let assert Ok(_) = test_helpers.create_record_table(exec)
  let assert Ok(_) = test_helpers.create_actor_table(exec)

  // Insert a lexicon for xyz.statusphere.status
  let lexicon = create_status_lexicon()
  let assert Ok(_) = lexicons.insert(exec, "xyz.statusphere.status", lexicon)

  // Insert test actors
  let assert Ok(_) = actors.upsert(exec, "did:plc:alice", "alice.bsky.social")
  let assert Ok(_) = actors.upsert(exec, "did:plc:bob", "bob.bsky.social")
  let assert Ok(_) =
    actors.upsert(exec, "did:plc:charlie", "charlie.bsky.social")

  // Insert test records with those DIDs
  let record1_json =
    json.object([
      #("status", json.string("👍")),
      #("createdAt", json.string("2024-01-01T00:00:00Z")),
    ])
    |> json.to_string

  let assert Ok(_) =
    records.insert(
      exec,
      "at://did:plc:alice/xyz.statusphere.status/123",
      "cid1",
      "did:plc:alice",
      "xyz.statusphere.status",
      record1_json,
    )

  let record2_json =
    json.object([
      #("status", json.string("🔥")),
      #("createdAt", json.string("2024-01-02T00:00:00Z")),
    ])
    |> json.to_string

  let assert Ok(_) =
    records.insert(
      exec,
      "at://did:plc:bob/xyz.statusphere.status/456",
      "cid2",
      "did:plc:bob",
      "xyz.statusphere.status",
      record2_json,
    )

  let record3_json =
    json.object([
      #("status", json.string("⭐")),
      #("createdAt", json.string("2024-01-03T00:00:00Z")),
    ])
    |> json.to_string

  let assert Ok(_) =
    records.insert(
      exec,
      "at://did:plc:charlie/xyz.statusphere.status/789",
      "cid3",
      "did:plc:charlie",
      "xyz.statusphere.status",
      record3_json,
    )

  // Query filtering by actorHandle
  let query =
    json.object([
      #(
        "query",
        json.string(
          "{ xyzStatusphereStatus(where: {actorHandle: {eq: \"alice.bsky.social\"}}) { edges { node { actorHandle did status } } } }",
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
      None,
      "",
      "https://plc.directory",
    )

  // Verify response
  response.status
  |> should.equal(200)

  let assert wisp.Text(body) = response.body

  // Should contain alice's handle and record
  string.contains(body, "alice.bsky.social")
  |> should.be_true

  string.contains(body, "did:plc:alice")
  |> should.be_true

  string.contains(body, "👍")
  |> should.be_true

  // Should NOT contain bob or charlie's records
  string.contains(body, "bob.bsky.social")
  |> should.be_false

  string.contains(body, "charlie.bsky.social")
  |> should.be_false
  // Clean up handled automatically
}
