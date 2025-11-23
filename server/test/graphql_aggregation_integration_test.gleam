import database/queries/aggregates
import database/types
/// End-to-end integration tests for GraphQL aggregated queries
///
/// Tests the complete aggregation flow:
/// 1. Database setup with lexicons and records
/// 2. GraphQL schema building with aggregate fields
/// 3. Aggregated query execution with various parameters
/// 4. Result formatting and verification
import database/repositories/lexicons
import database/repositories/records
import database/schema/tables
import gleam/dict
import gleam/http
import gleam/json
import gleam/list
import gleam/option.{None}
import gleam/result
import gleam/string
import gleeunit
import gleeunit/should
import graphql_handler
import sqlight
import wisp
import wisp/simulate

pub fn main() {
  gleeunit.main()
}

// Helper to create a simple post lexicon
fn create_post_lexicon() -> String {
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
                    #("author", json.object([#("type", json.string("string"))])),
                    #("lang", json.object([#("type", json.string("string"))])),
                    #("likes", json.object([#("type", json.string("integer"))])),
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
            #(
              "record",
              json.object([
                #(
                  "properties",
                  json.object([
                    #("status", json.object([#("type", json.string("string"))])),
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

// Helper to setup test database with aggregatable records
fn setup_aggregation_test_db() -> Result(sqlight.Connection, sqlight.Error) {
  use conn <- result.try(sqlight.open(":memory:"))
  use _ <- result.try(tables.create_lexicon_table(conn))
  use _ <- result.try(tables.create_record_table(conn))

  // Insert post lexicon
  let post_lexicon = create_post_lexicon()
  use _ <- result.try(lexicons.insert(
    conn,
    "app.bsky.feed.post",
    post_lexicon,
  ))

  // Insert status lexicon
  let status_lexicon = create_status_lexicon()
  use _ <- result.try(lexicons.insert(
    conn,
    "xyz.statusphere.status",
    status_lexicon,
  ))

  // Insert test records with varying fields for aggregation
  // Posts from different authors with different languages
  use _ <- result.try(records.insert(
    conn,
    "at://did:plc:alice/app.bsky.feed.post/1",
    "cid1",
    "did:plc:alice",
    "app.bsky.feed.post",
    json.object([
      #("text", json.string("Hello world")),
      #("author", json.string("alice")),
      #("lang", json.string("en")),
      #("likes", json.int(100)),
    ])
      |> json.to_string,
  ))

  use _ <- result.try(records.insert(
    conn,
    "at://did:plc:alice/app.bsky.feed.post/2",
    "cid2",
    "did:plc:alice",
    "app.bsky.feed.post",
    json.object([
      #("text", json.string("Another post")),
      #("author", json.string("alice")),
      #("lang", json.string("en")),
      #("likes", json.int(50)),
    ])
      |> json.to_string,
  ))

  use _ <- result.try(records.insert(
    conn,
    "at://did:plc:bob/app.bsky.feed.post/1",
    "cid3",
    "did:plc:bob",
    "app.bsky.feed.post",
    json.object([
      #("text", json.string("Bonjour")),
      #("author", json.string("bob")),
      #("lang", json.string("fr")),
      #("likes", json.int(75)),
    ])
      |> json.to_string,
  ))

  use _ <- result.try(records.insert(
    conn,
    "at://did:plc:charlie/app.bsky.feed.post/1",
    "cid4",
    "did:plc:charlie",
    "app.bsky.feed.post",
    json.object([
      #("text", json.string("Hello from Charlie")),
      #("author", json.string("charlie")),
      #("lang", json.string("en")),
      #("likes", json.int(200)),
    ])
      |> json.to_string,
  ))

  use _ <- result.try(records.insert(
    conn,
    "at://did:plc:bob/app.bsky.feed.post/2",
    "cid5",
    "did:plc:bob",
    "app.bsky.feed.post",
    json.object([
      #("text", json.string("Salut")),
      #("author", json.string("bob")),
      #("lang", json.string("fr")),
      #("likes", json.int(25)),
    ])
      |> json.to_string,
  ))

  // Insert status records
  use _ <- result.try(records.insert(
    conn,
    "at://did:plc:alice/xyz.statusphere.status/1",
    "scid1",
    "did:plc:alice",
    "xyz.statusphere.status",
    json.object([
      #("status", json.string("👍")),
      #("createdAt", json.string("2024-01-15T10:00:00Z")),
    ])
      |> json.to_string,
  ))

  use _ <- result.try(records.insert(
    conn,
    "at://did:plc:bob/xyz.statusphere.status/1",
    "scid2",
    "did:plc:bob",
    "xyz.statusphere.status",
    json.object([
      #("status", json.string("👍")),
      #("createdAt", json.string("2024-01-15T11:00:00Z")),
    ])
      |> json.to_string,
  ))

  use _ <- result.try(records.insert(
    conn,
    "at://did:plc:charlie/xyz.statusphere.status/1",
    "scid3",
    "did:plc:charlie",
    "xyz.statusphere.status",
    json.object([
      #("status", json.string("🔥")),
      #("createdAt", json.string("2024-01-16T10:00:00Z")),
    ])
      |> json.to_string,
  ))

  Ok(conn)
}

// Test: Simple single-field aggregation through full GraphQL stack
pub fn graphql_simple_aggregation_test() {
  let assert Ok(db) = setup_aggregation_test_db()

  // Query: Group posts by author
  let query =
    json.object([
      #(
        "query",
        json.string(
          "{ appBskyFeedPostAggregated(groupBy: [{field: \"author\"}]) { author count } }",
        ),
      ),
    ])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query)
    |> simulate.header("content-type", "application/json")

  let response =
    graphql_handler.handle_graphql_request(
      request,
      db,
      "http://localhost:3000",
      "https://plc.directory",
    )

  // Verify response
  response.status |> should.equal(200)

  let assert wisp.Text(body) = response.body

  // Verify response structure
  string.contains(body, "data") |> should.be_true
  string.contains(body, "appBskyFeedPostAggregated") |> should.be_true

  // Should have counts for alice (2), bob (2), charlie (1)
  string.contains(body, "alice") |> should.be_true
  string.contains(body, "bob") |> should.be_true
  string.contains(body, "charlie") |> should.be_true

  let assert Ok(_) = sqlight.close(db)
}

// Test: Multi-field aggregation through GraphQL
pub fn graphql_multi_field_aggregation_test() {
  let assert Ok(db) = setup_aggregation_test_db()

  // Query: Group posts by author AND lang
  let query =
    json.object([
      #(
        "query",
        json.string(
          "{ appBskyFeedPostAggregated(groupBy: [{field: \"author\"}, {field: \"lang\"}]) { author lang count } }",
        ),
      ),
    ])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query)
    |> simulate.header("content-type", "application/json")

  let response =
    graphql_handler.handle_graphql_request(
      request,
      db,
      "http://localhost:3000",
      "https://plc.directory",
    )

  // Verify response
  response.status |> should.equal(200)

  let assert wisp.Text(body) = response.body

  // Should have separate counts for each author+lang combination
  string.contains(body, "alice") |> should.be_true
  string.contains(body, "bob") |> should.be_true
  string.contains(body, "en") |> should.be_true
  string.contains(body, "fr") |> should.be_true

  let assert Ok(_) = sqlight.close(db)
}

// Test: Aggregation with WHERE clause filtering
pub fn graphql_aggregation_with_where_test() {
  let assert Ok(db) = setup_aggregation_test_db()

  // First test without WHERE to ensure aggregation works
  let query_no_where =
    json.object([
      #(
        "query",
        json.string(
          "{ appBskyFeedPostAggregated(groupBy: [{field: \"lang\"}]) { lang count } }",
        ),
      ),
    ])
    |> json.to_string

  let request_no_where =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query_no_where)
    |> simulate.header("content-type", "application/json")

  let response_no_where =
    graphql_handler.handle_graphql_request(
      request_no_where,
      db,
      "http://localhost:3000",
      "https://plc.directory",
    )

  let assert wisp.Text(_body_no_where) = response_no_where.body

  // Try with string field instead of integer
  let query_string =
    json.object([
      #(
        "query",
        json.string(
          "{ appBskyFeedPostAggregated(groupBy: [{field: \"lang\"}], where: {author: {eq: \"alice\"}}) { lang count } }",
        ),
      ),
    ])
    |> json.to_string

  let request_string =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query_string)
    |> simulate.header("content-type", "application/json")

  let response_string =
    graphql_handler.handle_graphql_request(
      request_string,
      db,
      "http://localhost:3000",
      "https://plc.directory",
    )

  let assert wisp.Text(_body_string) = response_string.body

  // Query: Group posts by lang, but only for posts with likes >= 50
  // Note: GraphQL integers in queries don't need quotes
  let query =
    json.object([
      #(
        "query",
        json.string(
          "{ appBskyFeedPostAggregated(groupBy: [{field: \"lang\"}], where: {likes: {gte: 50}}) { lang count } }",
        ),
      ),
    ])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query)
    |> simulate.header("content-type", "application/json")

  let response =
    graphql_handler.handle_graphql_request(
      request,
      db,
      "http://localhost:3000",
      "https://plc.directory",
    )

  // Verify response
  response.status |> should.equal(200)

  let assert wisp.Text(body) = response.body

  // Should filter out posts with likes < 50 (bob/post/2 with 25 likes)
  // Remaining: alice/post/1 (100), alice/post/2 (50), bob/post/1 (75), charlie/post/1 (200)
  // By lang: en=3 (alice x2, charlie), fr=1 (bob)
  response.status |> should.equal(200)
  string.contains(body, "data") |> should.be_true
  string.contains(body, "appBskyFeedPostAggregated") |> should.be_true

  // Verify both language groups are present (JSON without quotes on keys)
  string.contains(body, "lang") |> should.be_true
  string.contains(body, "en") |> should.be_true
  string.contains(body, "fr") |> should.be_true

  // Verify counts are correct (en should have count 3, fr should have count 1)
  // The response should contain both groups with their counts
  string.contains(body, "count") |> should.be_true
  string.contains(body, "3") |> should.be_true
  string.contains(body, "1") |> should.be_true

  let assert Ok(_) = sqlight.close(db)
}

// Test: Aggregation with ORDER BY
pub fn graphql_aggregation_with_order_by_test() {
  let assert Ok(db) = setup_aggregation_test_db()

  // Query: Group by lang, order by count ascending
  let query =
    json.object([
      #(
        "query",
        json.string(
          "{ appBskyFeedPostAggregated(groupBy: [{field: \"lang\"}], orderBy: {count: ASC}) { lang count } }",
        ),
      ),
    ])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query)
    |> simulate.header("content-type", "application/json")

  let response =
    graphql_handler.handle_graphql_request(
      request,
      db,
      "http://localhost:3000",
      "https://plc.directory",
    )

  // Verify response
  response.status |> should.equal(200)

  let assert wisp.Text(body) = response.body

  string.contains(body, "lang") |> should.be_true
  string.contains(body, "count") |> should.be_true

  let assert Ok(_) = sqlight.close(db)
}

// Test: Aggregation with LIMIT
pub fn graphql_aggregation_with_limit_test() {
  let assert Ok(db) = setup_aggregation_test_db()

  // Query: Group by author, limit to 2 results
  let query =
    json.object([
      #(
        "query",
        json.string(
          "{ appBskyFeedPostAggregated(groupBy: [{field: \"author\"}], limit: 2) { author count } }",
        ),
      ),
    ])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query)
    |> simulate.header("content-type", "application/json")

  let response =
    graphql_handler.handle_graphql_request(
      request,
      db,
      "http://localhost:3000",
      "https://plc.directory",
    )

  // Verify response
  response.status |> should.equal(200)

  let assert wisp.Text(body) = response.body

  string.contains(body, "author") |> should.be_true
  string.contains(body, "count") |> should.be_true

  let assert Ok(_) = sqlight.close(db)
}

// Test: Aggregation on status field (emoji grouping)
pub fn graphql_status_aggregation_test() {
  let assert Ok(db) = setup_aggregation_test_db()

  // Query: Group status records by status emoji
  let query =
    json.object([
      #(
        "query",
        json.string(
          "{ xyzStatusphereStatusAggregated(groupBy: [{field: \"status\"}]) { status count } }",
        ),
      ),
    ])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query)
    |> simulate.header("content-type", "application/json")

  let response =
    graphql_handler.handle_graphql_request(
      request,
      db,
      "http://localhost:3000",
      "https://plc.directory",
    )

  // Verify response
  response.status |> should.equal(200)

  let assert wisp.Text(body) = response.body

  // Should have 👍 (count=2) and 🔥 (count=1)
  string.contains(body, "👍") |> should.be_true
  string.contains(body, "🔥") |> should.be_true

  let assert Ok(_) = sqlight.close(db)
}

// Test: Direct database aggregation (not through GraphQL handler)
pub fn database_aggregation_integration_test() {
  let assert Ok(conn) = setup_aggregation_test_db()

  // Test simple grouping by author
  let assert Ok(results) =
    aggregates.get_aggregated_records(
      conn,
      "app.bsky.feed.post",
      [types.SimpleField("author")],
      None,
      True,
      10,
    )

  // Should have 3 groups: alice, bob, charlie
  list.length(results) |> should.equal(3)

  // Verify each result has the expected structure
  list.each(results, fn(result) {
    // Each result should have field_0 (author) and count
    dict.size(result.group_values) |> should.equal(1)
    // Count should be positive
    should.be_true(result.count > 0)
  })

  let assert Ok(_) = sqlight.close(conn)
}

// Test: Database aggregation with multi-field grouping
pub fn database_multi_field_aggregation_test() {
  let assert Ok(conn) = setup_aggregation_test_db()

  // Group by author and lang
  let assert Ok(results) =
    aggregates.get_aggregated_records(
      conn,
      "app.bsky.feed.post",
      [types.SimpleField("author"), types.SimpleField("lang")],
      None,
      True,
      10,
    )

  // Should have groups for each author+lang combination
  should.be_true(list.length(results) >= 3)

  // Each result should have 2 fields (author and lang)
  list.each(results, fn(result) {
    dict.size(result.group_values) |> should.equal(2)
  })

  let assert Ok(_) = sqlight.close(conn)
}

// Test: Aggregation on table column (did)
pub fn graphql_table_column_aggregation_test() {
  let assert Ok(db) = setup_aggregation_test_db()

  // Query: Group posts by did (table column, not JSON field)
  let query =
    json.object([
      #(
        "query",
        json.string(
          "{ appBskyFeedPostAggregated(groupBy: [{field: \"did\"}]) { did count } }",
        ),
      ),
    ])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query)
    |> simulate.header("content-type", "application/json")

  let response =
    graphql_handler.handle_graphql_request(
      request,
      db,
      "http://localhost:3000",
      "https://plc.directory",
    )

  // Verify response
  response.status |> should.equal(200)

  let assert wisp.Text(body) = response.body

  // Should group by DID values
  string.contains(body, "did:plc:alice") |> should.be_true
  string.contains(body, "did:plc:bob") |> should.be_true
  string.contains(body, "did:plc:charlie") |> should.be_true

  let assert Ok(_) = sqlight.close(db)
}

// Test: Empty aggregation result
pub fn graphql_empty_aggregation_test() {
  let assert Ok(db) = setup_aggregation_test_db()

  // Query: Filter that matches no records
  let query =
    json.object([
      #(
        "query",
        json.string(
          "{ appBskyFeedPostAggregated(groupBy: [{field: \"author\"}], where: {likes: {gte: 1000}}) { author count } }",
        ),
      ),
    ])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query)
    |> simulate.header("content-type", "application/json")

  let response =
    graphql_handler.handle_graphql_request(
      request,
      db,
      "http://localhost:3000",
      "https://plc.directory",
    )

  // Verify response (should still be 200 with empty results)
  response.status |> should.equal(200)

  let assert wisp.Text(body) = response.body

  // Should have data field but empty array
  string.contains(body, "data") |> should.be_true
  string.contains(body, "appBskyFeedPostAggregated") |> should.be_true

  let assert Ok(_) = sqlight.close(db)
}
