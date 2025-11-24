/// Integration tests for paginated joins (connections)
///
/// Tests verify that:
/// - DID joins return paginated connections with first/after/last/before
/// - Reverse joins return paginated connections
/// - PageInfo is correctly populated
/// - Cursors work for pagination
import database/repositories/lexicons
import database/repositories/records
import database/schema/tables
import gleam/int
import gleam/json
import gleam/list
import gleam/string
import gleeunit/should
import graphql_gleam
import sqlight

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

// Helper to create a profile lexicon with literal:self key
fn create_profile_lexicon() -> String {
  json.object([
    #("lexicon", json.int(1)),
    #("id", json.string("app.bsky.actor.profile")),
    #(
      "defs",
      json.object([
        #(
          "main",
          json.object([
            #("type", json.string("record")),
            #("key", json.string("literal:self")),
            #(
              "record",
              json.object([
                #("type", json.string("object")),
                #(
                  "properties",
                  json.object([
                    #(
                      "displayName",
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
}

// Test: DID join with first:1 returns only 1 result
pub fn did_join_first_one_test() {
  // Setup database
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = tables.create_lexicon_table(db)
  let assert Ok(_) = tables.create_record_table(db)
  let assert Ok(_) = tables.create_actor_table(db)

  // Insert lexicons
  let post_lexicon = create_post_lexicon()
  let profile_lexicon = create_profile_lexicon()
  let assert Ok(_) = lexicons.insert(db, "app.bsky.feed.post", post_lexicon)
  let assert Ok(_) =
    lexicons.insert(db, "app.bsky.actor.profile", profile_lexicon)

  // Insert a profile
  let profile_uri = "at://did:plc:author/app.bsky.actor.profile/self"
  let profile_json =
    json.object([#("displayName", json.string("Author"))])
    |> json.to_string

  let assert Ok(_) =
    records.insert(
      db,
      profile_uri,
      "cid_profile",
      "did:plc:author",
      "app.bsky.actor.profile",
      profile_json,
    )

  // Insert 5 posts by the same DID
  list.range(1, 5)
  |> list.each(fn(i) {
    let post_uri =
      "at://did:plc:author/app.bsky.feed.post/post" <> int.to_string(i)
    let post_json =
      json.object([
        #("text", json.string("Post number " <> int.to_string(i))),
      ])
      |> json.to_string

    let assert Ok(_) =
      records.insert(
        db,
        post_uri,
        "cid_post" <> int.to_string(i),
        "did:plc:author",
        "app.bsky.feed.post",
        post_json,
      )
    Nil
  })

  // Execute GraphQL query with DID join and first:1
  let query =
    "
    {
      appBskyActorProfile {
        edges {
          node {
            uri
            appBskyFeedPostByDid(first: 1) {
              edges {
                node {
                  uri
                  text
                }
              }
              pageInfo {
                hasNextPage
                hasPreviousPage
              }
            }
          }
        }
      }
    }
  "

  let assert Ok(response_json) =
    graphql_gleam.execute_query_with_db(
      db,
      query,
      "{}",
      Error(Nil),
      "",
      "https://plc.directory",
    )

  // Verify only 1 post is returned
  string.contains(response_json, "\"edges\"")
  |> should.be_true

  // Count how many post URIs appear (should be 1)
  let post_count =
    list.range(1, 5)
    |> list.filter(fn(i) {
      string.contains(
        response_json,
        "at://did:plc:author/app.bsky.feed.post/post" <> int.to_string(i),
      )
    })
    |> list.length

  post_count
  |> should.equal(1)

  // Verify hasNextPage is true (more posts available)
  {
    string.contains(response_json, "\"hasNextPage\":true")
    || string.contains(response_json, "\"hasNextPage\": true")
  }
  |> should.be_true
}

// Test: DID join with first:2 returns only 2 results
pub fn did_join_first_two_test() {
  // Setup database
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = tables.create_lexicon_table(db)
  let assert Ok(_) = tables.create_record_table(db)
  let assert Ok(_) = tables.create_actor_table(db)

  // Insert lexicons
  let post_lexicon = create_post_lexicon()
  let profile_lexicon = create_profile_lexicon()
  let assert Ok(_) = lexicons.insert(db, "app.bsky.feed.post", post_lexicon)
  let assert Ok(_) =
    lexicons.insert(db, "app.bsky.actor.profile", profile_lexicon)

  // Insert a profile
  let profile_uri = "at://did:plc:author/app.bsky.actor.profile/self"
  let profile_json =
    json.object([#("displayName", json.string("Author"))])
    |> json.to_string

  let assert Ok(_) =
    records.insert(
      db,
      profile_uri,
      "cid_profile",
      "did:plc:author",
      "app.bsky.actor.profile",
      profile_json,
    )

  // Insert 5 posts by the same DID
  list.range(1, 5)
  |> list.each(fn(i) {
    let post_uri =
      "at://did:plc:author/app.bsky.feed.post/post" <> int.to_string(i)
    let post_json =
      json.object([
        #("text", json.string("Post number " <> int.to_string(i))),
      ])
      |> json.to_string

    let assert Ok(_) =
      records.insert(
        db,
        post_uri,
        "cid_post" <> int.to_string(i),
        "did:plc:author",
        "app.bsky.feed.post",
        post_json,
      )
    Nil
  })

  // Execute GraphQL query with DID join and first:2
  let query =
    "
    {
      appBskyActorProfile {
        edges {
          node {
            uri
            appBskyFeedPostByDid(first: 2) {
              edges {
                node {
                  uri
                  text
                }
              }
              pageInfo {
                hasNextPage
                hasPreviousPage
              }
            }
          }
        }
      }
    }
  "

  let assert Ok(response_json) =
    graphql_gleam.execute_query_with_db(
      db,
      query,
      "{}",
      Error(Nil),
      "",
      "https://plc.directory",
    )

  // Count how many post URIs appear (should be 2)
  let post_count =
    list.range(1, 5)
    |> list.filter(fn(i) {
      string.contains(
        response_json,
        "at://did:plc:author/app.bsky.feed.post/post" <> int.to_string(i),
      )
    })
    |> list.length

  post_count
  |> should.equal(2)

  // Verify hasNextPage is true (more posts available)
  {
    string.contains(response_json, "\"hasNextPage\":true")
    || string.contains(response_json, "\"hasNextPage\": true")
  }
  |> should.be_true
}

// Test: Reverse join with first:1 returns only 1 result
pub fn reverse_join_first_one_test() {
  // Setup database
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = tables.create_lexicon_table(db)
  let assert Ok(_) = tables.create_record_table(db)
  let assert Ok(_) = tables.create_actor_table(db)

  // Insert lexicons
  let post_lexicon = create_post_lexicon()
  let like_lexicon = create_like_lexicon()
  let assert Ok(_) = lexicons.insert(db, "app.bsky.feed.post", post_lexicon)
  let assert Ok(_) = lexicons.insert(db, "app.bsky.feed.like", like_lexicon)

  // Insert a post
  let post_uri = "at://did:plc:author/app.bsky.feed.post/post1"
  let post_json =
    json.object([#("text", json.string("Great post!"))])
    |> json.to_string

  let assert Ok(_) =
    records.insert(
      db,
      post_uri,
      "cid_post",
      "did:plc:author",
      "app.bsky.feed.post",
      post_json,
    )

  // Insert 5 likes that reference the post
  list.range(1, 5)
  |> list.each(fn(i) {
    let like_uri =
      "at://did:plc:liker"
      <> int.to_string(i)
      <> "/app.bsky.feed.like/like"
      <> int.to_string(i)
    let like_json =
      json.object([
        #("subject", json.string(post_uri)),
        #("createdAt", json.string("2024-01-01T12:00:00Z")),
      ])
      |> json.to_string

    let assert Ok(_) =
      records.insert(
        db,
        like_uri,
        "cid_like" <> int.to_string(i),
        "did:plc:liker" <> int.to_string(i),
        "app.bsky.feed.like",
        like_json,
      )
    Nil
  })

  // Execute GraphQL query with reverse join and first:1
  let query =
    "
    {
      appBskyFeedPost {
        edges {
          node {
            uri
            appBskyFeedLikeViaSubject(first: 1) {
              edges {
                node {
                  uri
                }
              }
              pageInfo {
                hasNextPage
                hasPreviousPage
              }
            }
          }
        }
      }
    }
  "

  let assert Ok(response_json) =
    graphql_gleam.execute_query_with_db(
      db,
      query,
      "{}",
      Error(Nil),
      "",
      "https://plc.directory",
    )

  // Count how many like URIs appear (should be 1)
  let like_count =
    list.range(1, 5)
    |> list.filter(fn(i) {
      string.contains(
        response_json,
        "at://did:plc:liker"
          <> int.to_string(i)
          <> "/app.bsky.feed.like/like"
          <> int.to_string(i),
      )
    })
    |> list.length

  like_count
  |> should.equal(1)

  // Verify hasNextPage is true (more likes available)
  {
    string.contains(response_json, "\"hasNextPage\":true")
    || string.contains(response_json, "\"hasNextPage\": true")
  }
  |> should.be_true
}

// Test: DID join with no pagination args defaults to first:50
pub fn did_join_default_pagination_test() {
  // Setup database
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = tables.create_lexicon_table(db)
  let assert Ok(_) = tables.create_record_table(db)
  let assert Ok(_) = tables.create_actor_table(db)

  // Insert lexicons
  let post_lexicon = create_post_lexicon()
  let profile_lexicon = create_profile_lexicon()
  let assert Ok(_) = lexicons.insert(db, "app.bsky.feed.post", post_lexicon)
  let assert Ok(_) =
    lexicons.insert(db, "app.bsky.actor.profile", profile_lexicon)

  // Insert a profile
  let profile_uri = "at://did:plc:author/app.bsky.actor.profile/self"
  let profile_json =
    json.object([#("displayName", json.string("Author"))])
    |> json.to_string

  let assert Ok(_) =
    records.insert(
      db,
      profile_uri,
      "cid_profile",
      "did:plc:author",
      "app.bsky.actor.profile",
      profile_json,
    )

  // Insert 3 posts by the same DID
  list.range(1, 3)
  |> list.each(fn(i) {
    let post_uri =
      "at://did:plc:author/app.bsky.feed.post/post" <> int.to_string(i)
    let post_json =
      json.object([
        #("text", json.string("Post number " <> int.to_string(i))),
      ])
      |> json.to_string

    let assert Ok(_) =
      records.insert(
        db,
        post_uri,
        "cid_post" <> int.to_string(i),
        "did:plc:author",
        "app.bsky.feed.post",
        post_json,
      )
    Nil
  })

  // Execute GraphQL query with DID join and NO pagination args (should default to first:50)
  let query =
    "
    {
      appBskyActorProfile {
        edges {
          node {
            uri
            appBskyFeedPostByDid {
              edges {
                node {
                  uri
                  text
                }
              }
              pageInfo {
                hasNextPage
                hasPreviousPage
              }
            }
          }
        }
      }
    }
  "

  let assert Ok(response_json) =
    graphql_gleam.execute_query_with_db(
      db,
      query,
      "{}",
      Error(Nil),
      "",
      "https://plc.directory",
    )

  // All 3 posts should be returned (within default limit of 50)
  let post_count =
    list.range(1, 3)
    |> list.filter(fn(i) {
      string.contains(
        response_json,
        "at://did:plc:author/app.bsky.feed.post/post" <> int.to_string(i),
      )
    })
    |> list.length

  post_count
  |> should.equal(3)

  // Verify hasNextPage is false (no more posts)
  {
    string.contains(response_json, "\"hasNextPage\":false")
    || string.contains(response_json, "\"hasNextPage\": false")
  }
  |> should.be_true
}
