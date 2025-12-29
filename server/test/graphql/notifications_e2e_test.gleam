/// End-to-end tests for notifications GraphQL query
///
/// Tests verify that:
/// - notifications query returns records mentioning the given DID
/// - Self-authored records are excluded
/// - Collection filtering works correctly
/// - Union type resolution works across different record types
import database/repositories/actors
import database/repositories/lexicons
import database/repositories/records
import gleam/json
import gleam/option
import gleam/string
import gleeunit/should
import graphql/lexicon/schema as lexicon_schema
import lib/oauth/did_cache
import test_helpers

// Helper to create a post lexicon JSON
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
            #("key", json.string("tid")),
            #(
              "record",
              json.object([
                #("type", json.string("object")),
                #(
                  "required",
                  json.array([json.string("text")], of: fn(x) { x }),
                ),
                #(
                  "properties",
                  json.object([
                    #(
                      "text",
                      json.object([
                        #("type", json.string("string")),
                        #("maxLength", json.int(300)),
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

// Helper to create a like lexicon JSON with subject field
fn create_like_lexicon() -> String {
  json.object([
    #("lexicon", json.int(1)),
    #("id", json.string("app.bsky.feed.like")),
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
                  json.array([json.string("subject")], of: fn(x) { x }),
                ),
                #(
                  "properties",
                  json.object([
                    #(
                      "subject",
                      json.object([
                        #("type", json.string("string")),
                        #("format", json.string("at-uri")),
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

// Helper to create a follow lexicon JSON with subject field (DID)
fn create_follow_lexicon() -> String {
  json.object([
    #("lexicon", json.int(1)),
    #("id", json.string("app.bsky.graph.follow")),
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
                  json.array([json.string("subject")], of: fn(x) { x }),
                ),
                #(
                  "properties",
                  json.object([
                    #(
                      "subject",
                      json.object([#("type", json.string("string"))]),
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

// Test: notifications query returns records mentioning the target DID
pub fn notifications_returns_mentioning_records_test() {
  // Setup database
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_lexicon_table(exec)
  let assert Ok(_) = test_helpers.create_record_table(exec)
  let assert Ok(_) = test_helpers.create_actor_table(exec)
  let assert Ok(_) = test_helpers.create_oauth_tables(exec)
  let assert Ok(_) =
    test_helpers.insert_test_token(
      exec,
      "test-notification-token",
      "did:plc:target",
    )

  // Insert lexicons
  let assert Ok(_) =
    lexicons.insert(exec, "app.bsky.feed.post", create_post_lexicon())
  let assert Ok(_) =
    lexicons.insert(exec, "app.bsky.feed.like", create_like_lexicon())
  let assert Ok(_) =
    lexicons.insert(exec, "app.bsky.graph.follow", create_follow_lexicon())

  // Setup actors
  let assert Ok(_) = actors.upsert(exec, "did:plc:target", "target.bsky.social")
  let assert Ok(_) = actors.upsert(exec, "did:plc:alice", "alice.bsky.social")
  let assert Ok(_) = actors.upsert(exec, "did:plc:bob", "bob.bsky.social")

  // Target's own post (should NOT appear in notifications)
  let assert Ok(_) =
    records.insert(
      exec,
      "at://did:plc:target/app.bsky.feed.post/post1",
      "bafy001",
      "did:plc:target",
      "app.bsky.feed.post",
      "{\"text\":\"Hello world\",\"createdAt\":\"2024-01-01T00:00:00Z\"}",
    )

  // Alice likes target's post (SHOULD appear)
  let assert Ok(_) =
    records.insert(
      exec,
      "at://did:plc:alice/app.bsky.feed.like/like1",
      "bafy002",
      "did:plc:alice",
      "app.bsky.feed.like",
      "{\"subject\":\"at://did:plc:target/app.bsky.feed.post/post1\",\"createdAt\":\"2024-01-02T00:00:00Z\"}",
    )

  // Bob follows target (SHOULD appear)
  let assert Ok(_) =
    records.insert(
      exec,
      "at://did:plc:bob/app.bsky.graph.follow/follow1",
      "bafy003",
      "did:plc:bob",
      "app.bsky.graph.follow",
      "{\"subject\":\"did:plc:target\",\"createdAt\":\"2024-01-03T00:00:00Z\"}",
    )

  // Alice's unrelated post (should NOT appear)
  let assert Ok(_) =
    records.insert(
      exec,
      "at://did:plc:alice/app.bsky.feed.post/post2",
      "bafy004",
      "did:plc:alice",
      "app.bsky.feed.post",
      "{\"text\":\"Unrelated post\",\"createdAt\":\"2024-01-04T00:00:00Z\"}",
    )

  // Query all notifications - verify union type resolution works correctly
  let query =
    "
    query {
      notifications(first: 10) {
        edges {
          cursor
          node {
            __typename
            ... on AppBskyFeedLike {
              uri
            }
            ... on AppBskyGraphFollow {
              uri
            }
          }
        }
        pageInfo {
          hasNextPage
          hasPreviousPage
        }
      }
    }
  "

  let assert Ok(cache) = did_cache.start()
  let assert Ok(response_json) =
    lexicon_schema.execute_query_with_db(
      exec,
      query,
      "{}",
      Ok("test-notification-token"),
      cache,
      option.None,
      "",
      "https://plc.directory",
    )

  // Verify union type resolution returns concrete types
  string.contains(response_json, "AppBskyFeedLike")
  |> should.be_true

  string.contains(response_json, "AppBskyGraphFollow")
  |> should.be_true

  // Verify URIs are returned from inline fragments
  string.contains(response_json, "like1")
  |> should.be_true

  string.contains(response_json, "follow1")
  |> should.be_true

  // Should have pagination info
  string.contains(response_json, "hasNextPage")
  |> should.be_true
}

// Test: notifications query with collection filter
pub fn notifications_filters_by_collection_test() {
  // Setup database
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_lexicon_table(exec)
  let assert Ok(_) = test_helpers.create_record_table(exec)
  let assert Ok(_) = test_helpers.create_actor_table(exec)
  let assert Ok(_) = test_helpers.create_oauth_tables(exec)
  let assert Ok(_) =
    test_helpers.insert_test_token(
      exec,
      "test-notification-token",
      "did:plc:target",
    )

  // Insert lexicons
  let assert Ok(_) =
    lexicons.insert(exec, "app.bsky.feed.post", create_post_lexicon())
  let assert Ok(_) =
    lexicons.insert(exec, "app.bsky.feed.like", create_like_lexicon())
  let assert Ok(_) =
    lexicons.insert(exec, "app.bsky.graph.follow", create_follow_lexicon())

  // Setup actors
  let assert Ok(_) = actors.upsert(exec, "did:plc:target", "target.bsky.social")
  let assert Ok(_) = actors.upsert(exec, "did:plc:alice", "alice.bsky.social")
  let assert Ok(_) = actors.upsert(exec, "did:plc:bob", "bob.bsky.social")

  // Alice likes target's post
  let assert Ok(_) =
    records.insert(
      exec,
      "at://did:plc:alice/app.bsky.feed.like/like1",
      "bafy002",
      "did:plc:alice",
      "app.bsky.feed.like",
      "{\"subject\":\"at://did:plc:target/app.bsky.feed.post/post1\",\"createdAt\":\"2024-01-02T00:00:00Z\"}",
    )

  // Bob follows target
  let assert Ok(_) =
    records.insert(
      exec,
      "at://did:plc:bob/app.bsky.graph.follow/follow1",
      "bafy003",
      "did:plc:bob",
      "app.bsky.graph.follow",
      "{\"subject\":\"did:plc:target\",\"createdAt\":\"2024-01-03T00:00:00Z\"}",
    )

  // Query only likes (not follows)
  let query =
    "
    query {
      notifications(collections: [APP_BSKY_FEED_LIKE], first: 10) {
        edges {
          cursor
          node {
            __typename
            ... on AppBskyFeedLike {
              uri
            }
          }
        }
      }
    }
  "

  let assert Ok(cache) = did_cache.start()
  let assert Ok(response_json) =
    lexicon_schema.execute_query_with_db(
      exec,
      query,
      "{}",
      Ok("test-notification-token"),
      cache,
      option.None,
      "",
      "https://plc.directory",
    )

  // Should have the like with correct type
  string.contains(response_json, "AppBskyFeedLike")
  |> should.be_true

  string.contains(response_json, "like1")
  |> should.be_true

  // Should NOT have the follow (filtered out)
  string.contains(response_json, "follow1")
  |> should.be_false

  string.contains(response_json, "AppBskyGraphFollow")
  |> should.be_false
}

// Test: notifications query excludes self-authored records
pub fn notifications_excludes_self_authored_test() {
  // Setup database
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_lexicon_table(exec)
  let assert Ok(_) = test_helpers.create_record_table(exec)
  let assert Ok(_) = test_helpers.create_actor_table(exec)
  let assert Ok(_) = test_helpers.create_oauth_tables(exec)
  let assert Ok(_) =
    test_helpers.insert_test_token(
      exec,
      "test-notification-token",
      "did:plc:target",
    )

  // Insert lexicons
  let assert Ok(_) =
    lexicons.insert(exec, "app.bsky.feed.post", create_post_lexicon())

  // Setup actors
  let assert Ok(_) = actors.upsert(exec, "did:plc:target", "target.bsky.social")

  // Target's own post that mentions themselves (should NOT appear)
  let assert Ok(_) =
    records.insert(
      exec,
      "at://did:plc:target/app.bsky.feed.post/post1",
      "bafy001",
      "did:plc:target",
      "app.bsky.feed.post",
      "{\"text\":\"Talking about did:plc:target\",\"createdAt\":\"2024-01-01T00:00:00Z\"}",
    )

  let query =
    "
    query {
      notifications(first: 10) {
        edges {
          cursor
          node {
            __typename
          }
        }
      }
    }
  "

  let assert Ok(cache) = did_cache.start()
  let assert Ok(response_json) =
    lexicon_schema.execute_query_with_db(
      exec,
      query,
      "{}",
      Ok("test-notification-token"),
      cache,
      option.None,
      "",
      "https://plc.directory",
    )

  // Should have empty edges since self-authored is excluded
  // Check for empty edges array (with space after colon)
  string.contains(response_json, "\"edges\": []")
  |> should.be_true
}
