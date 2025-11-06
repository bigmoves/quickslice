/// Integration tests for joins and DataLoader with actual database
///
/// Tests verify that:
/// - Forward joins (at-uri and strongRef) resolve correctly
/// - Reverse joins discover and resolve relationships
/// - DataLoader batches queries efficiently
/// - All join types work with actual SQLite database queries
import database
import gleam/json
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
                    #(
                      "replyTo",
                      json.object([
                        #("type", json.string("string")),
                        #("format", json.string("at-uri")),
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

// Helper to create a like lexicon JSON with subject field (at-uri)
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

// Helper to create a profile lexicon with strongRef
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
            #("key", json.string("self")),
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
                    #(
                      "pinnedPost",
                      json.object([
                        #("type", json.string("ref")),
                        #("ref", json.string("com.atproto.repo.strongRef")),
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

// Test: Forward join with at-uri field resolves correctly
pub fn forward_join_at_uri_resolves_test() {
  // Setup database
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = database.create_lexicon_table(db)
  let assert Ok(_) = database.create_record_table(db)
  let assert Ok(_) = database.create_actor_table(db)

  // Insert lexicons
  let post_lexicon = create_post_lexicon()
  let assert Ok(_) =
    database.insert_lexicon(db, "app.bsky.feed.post", post_lexicon)

  // Insert test records
  // Parent post
  let parent_uri = "at://did:plc:parent123/app.bsky.feed.post/parent1"
  let parent_json =
    json.object([#("text", json.string("This is the parent post"))])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      parent_uri,
      "cid_parent",
      "did:plc:parent123",
      "app.bsky.feed.post",
      parent_json,
    )

  // Reply post that references parent
  let reply_uri = "at://did:plc:user456/app.bsky.feed.post/reply1"
  let reply_json =
    json.object([
      #("text", json.string("This is a reply")),
      #("replyTo", json.string(parent_uri)),
    ])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      reply_uri,
      "cid_reply",
      "did:plc:user456",
      "app.bsky.feed.post",
      reply_json,
    )

  // Execute GraphQL query with forward join
  let query =
    "
    {
      appBskyFeedPost {
        edges {
          node {
            uri
            replyToResolved {
              uri
            }
          }
        }
      }
    }
  "

  let assert Ok(response_json) =
    graphql_gleam.execute_query_with_db(db, query, "{}", Error(Nil), "", "https://plc.directory")

  // Verify the response contains resolved join with parent URI
  string.contains(response_json, reply_uri)
  |> should.be_true

  string.contains(response_json, parent_uri)
  |> should.be_true

  string.contains(response_json, "replyToResolved")
  |> should.be_true
}

// Test: Forward join with strongRef resolves correctly
pub fn forward_join_strong_ref_resolves_test() {
  // Setup database
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = database.create_lexicon_table(db)
  let assert Ok(_) = database.create_record_table(db)
  let assert Ok(_) = database.create_actor_table(db)

  // Insert lexicons
  let post_lexicon = create_post_lexicon()
  let profile_lexicon = create_profile_lexicon()
  let assert Ok(_) =
    database.insert_lexicon(db, "app.bsky.feed.post", post_lexicon)
  let assert Ok(_) =
    database.insert_lexicon(db, "app.bsky.actor.profile", profile_lexicon)

  // Insert test records
  // A post
  let post_uri = "at://did:plc:user123/app.bsky.feed.post/post1"
  let post_json =
    json.object([#("text", json.string("My favorite post"))])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      post_uri,
      "cid_post1",
      "did:plc:user123",
      "app.bsky.feed.post",
      post_json,
    )

  // A profile that pins the post (using strongRef)
  let profile_uri = "at://did:plc:user123/app.bsky.actor.profile/self"
  let profile_json =
    json.object([
      #("displayName", json.string("Alice")),
      #(
        "pinnedPost",
        json.object([
          #("uri", json.string(post_uri)),
          #("cid", json.string("cid_post1")),
        ]),
      ),
    ])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      profile_uri,
      "cid_profile",
      "did:plc:user123",
      "app.bsky.actor.profile",
      profile_json,
    )

  // Execute GraphQL query with forward join on strongRef
  let query =
    "
    {
      appBskyActorProfile {
        edges {
          node {
            uri
            pinnedPostResolved {
              uri
            }
          }
        }
      }
    }
  "

  let assert Ok(response_json) =
    graphql_gleam.execute_query_with_db(db, query, "{}", Error(Nil), "", "https://plc.directory")

  // Verify the response contains resolved strongRef join with post URI
  string.contains(response_json, profile_uri)
  |> should.be_true

  string.contains(response_json, post_uri)
  |> should.be_true

  string.contains(response_json, "pinnedPostResolved")
  |> should.be_true
}

// Test: Reverse join discovers and resolves relationships
pub fn reverse_join_resolves_test() {
  // Setup database
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = database.create_lexicon_table(db)
  let assert Ok(_) = database.create_record_table(db)
  let assert Ok(_) = database.create_actor_table(db)

  // Insert lexicons
  let post_lexicon = create_post_lexicon()
  let like_lexicon = create_like_lexicon()

  let assert Ok(_) =
    database.insert_lexicon(db, "app.bsky.feed.post", post_lexicon)

  let assert Ok(_) =
    database.insert_lexicon(db, "app.bsky.feed.like", like_lexicon)

  // Insert test records
  // A post
  let post_uri = "at://did:plc:author789/app.bsky.feed.post/post1"
  let post_json =
    json.object([#("text", json.string("Great content!"))])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      post_uri,
      "cid_post",
      "did:plc:author789",
      "app.bsky.feed.post",
      post_json,
    )

  // Multiple likes that reference the post (to test batching)
  let like1_uri = "at://did:plc:liker1/app.bsky.feed.like/like1"
  let like1_json =
    json.object([
      #("subject", json.string(post_uri)),
      #("createdAt", json.string("2024-01-01T12:00:00Z")),
    ])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      like1_uri,
      "cid_like1",
      "did:plc:liker1",
      "app.bsky.feed.like",
      like1_json,
    )

  let like2_uri = "at://did:plc:liker2/app.bsky.feed.like/like2"
  let like2_json =
    json.object([
      #("subject", json.string(post_uri)),
      #("createdAt", json.string("2024-01-01T12:05:00Z")),
    ])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      like2_uri,
      "cid_like2",
      "did:plc:liker2",
      "app.bsky.feed.like",
      like2_json,
    )

  // Execute GraphQL query with reverse join (now returns connection)
  let query =
    "
    {
      appBskyFeedPost {
        edges {
          node {
            uri
            appBskyFeedLikeViaSubject {
              edges {
                node {
                  uri
                }
              }
            }
          }
        }
      }
    }
  "

  let assert Ok(response_json) =
    graphql_gleam.execute_query_with_db(db, query, "{}", Error(Nil), "", "https://plc.directory")

  // Verify the response contains reverse join results
  string.contains(response_json, post_uri)
  |> should.be_true

  string.contains(response_json, "appBskyFeedLikeViaSubject")
  |> should.be_true

  // Both likes should be in the response
  string.contains(response_json, like1_uri)
  |> should.be_true

  string.contains(response_json, like2_uri)
  |> should.be_true
}

// Test: DataLoader batches multiple forward joins efficiently
pub fn dataloader_batches_forward_joins_test() {
  // Setup database
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = database.create_lexicon_table(db)
  let assert Ok(_) = database.create_record_table(db)
  let assert Ok(_) = database.create_actor_table(db)

  // Insert lexicons
  let post_lexicon = create_post_lexicon()
  let assert Ok(_) =
    database.insert_lexicon(db, "app.bsky.feed.post", post_lexicon)

  // Insert multiple parent posts
  let parent1_uri = "at://did:plc:user1/app.bsky.feed.post/parent1"
  let parent1_json =
    json.object([#("text", json.string("Parent post 1"))])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      parent1_uri,
      "cid_p1",
      "did:plc:user1",
      "app.bsky.feed.post",
      parent1_json,
    )

  let parent2_uri = "at://did:plc:user2/app.bsky.feed.post/parent2"
  let parent2_json =
    json.object([#("text", json.string("Parent post 2"))])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      parent2_uri,
      "cid_p2",
      "did:plc:user2",
      "app.bsky.feed.post",
      parent2_json,
    )

  // Insert multiple reply posts
  let reply1_uri = "at://did:plc:user3/app.bsky.feed.post/reply1"
  let reply1_json =
    json.object([
      #("text", json.string("Reply to post 1")),
      #("replyTo", json.string(parent1_uri)),
    ])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      reply1_uri,
      "cid_r1",
      "did:plc:user3",
      "app.bsky.feed.post",
      reply1_json,
    )

  let reply2_uri = "at://did:plc:user4/app.bsky.feed.post/reply2"
  let reply2_json =
    json.object([
      #("text", json.string("Reply to post 2")),
      #("replyTo", json.string(parent2_uri)),
    ])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      reply2_uri,
      "cid_r2",
      "did:plc:user4",
      "app.bsky.feed.post",
      reply2_json,
    )

  // Execute GraphQL query that fetches multiple posts with joins
  // DataLoader should batch the replyToResolved lookups
  let query =
    "
    {
      appBskyFeedPost {
        edges {
          node {
            uri
            replyToResolved {
              uri
            }
          }
        }
      }
    }
  "

  let assert Ok(response_json) =
    graphql_gleam.execute_query_with_db(db, query, "{}", Error(Nil), "", "https://plc.directory")

  // Verify all posts appear
  string.contains(response_json, reply1_uri)
  |> should.be_true

  string.contains(response_json, reply2_uri)
  |> should.be_true

  string.contains(response_json, parent1_uri)
  |> should.be_true

  string.contains(response_json, parent2_uri)
  |> should.be_true
  // Note: To truly verify batching, we'd need to instrument the database
  // layer to count queries. For now, this test ensures correctness.
}

// Test: Reverse joins work with strongRef fields
pub fn reverse_join_with_strong_ref_test() {
  // Setup database
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = database.create_lexicon_table(db)
  let assert Ok(_) = database.create_record_table(db)
  let assert Ok(_) = database.create_actor_table(db)

  // Insert lexicons
  let post_lexicon = create_post_lexicon()
  let profile_lexicon = create_profile_lexicon()
  let assert Ok(_) =
    database.insert_lexicon(db, "app.bsky.feed.post", post_lexicon)
  let assert Ok(_) =
    database.insert_lexicon(db, "app.bsky.actor.profile", profile_lexicon)

  // Insert a post
  let post_uri = "at://did:plc:creator/app.bsky.feed.post/amazing"
  let post_json =
    json.object([#("text", json.string("Amazing post"))])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      post_uri,
      "cid_amazing",
      "did:plc:creator",
      "app.bsky.feed.post",
      post_json,
    )

  // Multiple profiles pin this post (using strongRef)
  let profile1_uri = "at://did:plc:user1/app.bsky.actor.profile/self"
  let profile1_json =
    json.object([
      #("displayName", json.string("User One")),
      #(
        "pinnedPost",
        json.object([
          #("uri", json.string(post_uri)),
          #("cid", json.string("cid_amazing")),
        ]),
      ),
    ])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      profile1_uri,
      "cid_prof1",
      "did:plc:user1",
      "app.bsky.actor.profile",
      profile1_json,
    )

  let profile2_uri = "at://did:plc:user2/app.bsky.actor.profile/self"
  let profile2_json =
    json.object([
      #("displayName", json.string("User Two")),
      #(
        "pinnedPost",
        json.object([
          #("uri", json.string(post_uri)),
          #("cid", json.string("cid_amazing")),
        ]),
      ),
    ])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      profile2_uri,
      "cid_prof2",
      "did:plc:user2",
      "app.bsky.actor.profile",
      profile2_json,
    )

  // Query post with reverse join to find all profiles that pinned it (now returns connection)
  let query =
    "
    {
      appBskyFeedPost {
        edges {
          node {
            uri
            appBskyActorProfileViaPinnedPost {
              edges {
                node {
                  uri
                }
              }
            }
          }
        }
      }
    }
  "

  let assert Ok(response_json) =
    graphql_gleam.execute_query_with_db(db, query, "{}", Error(Nil), "", "https://plc.directory")

  // Verify the reverse join through strongRef works
  string.contains(response_json, post_uri)
  |> should.be_true

  string.contains(response_json, "appBskyActorProfileViaPinnedPost")
  |> should.be_true

  string.contains(response_json, profile1_uri)
  |> should.be_true

  string.contains(response_json, profile2_uri)
  |> should.be_true
}

// Test: Forward join with union type and inline fragments
pub fn forward_join_union_inline_fragments_test() {
  // Setup database
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = database.create_lexicon_table(db)
  let assert Ok(_) = database.create_record_table(db)
  let assert Ok(_) = database.create_actor_table(db)

  // Insert lexicons
  let post_lexicon = create_post_lexicon()
  let like_lexicon = create_like_lexicon()
  let assert Ok(_) =
    database.insert_lexicon(db, "app.bsky.feed.post", post_lexicon)
  let assert Ok(_) =
    database.insert_lexicon(db, "app.bsky.feed.like", like_lexicon)

  // Insert a parent post
  let parent_post_uri = "at://did:plc:parent/app.bsky.feed.post/parent1"
  let parent_post_json =
    json.object([#("text", json.string("This is the parent post"))])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      parent_post_uri,
      "cid_parent_post",
      "did:plc:parent",
      "app.bsky.feed.post",
      parent_post_json,
    )

  // Insert a like that will be referenced
  let target_like_uri = "at://did:plc:liker/app.bsky.feed.like/like1"
  let target_like_json =
    json.object([
      #("subject", json.string(parent_post_uri)),
      #("createdAt", json.string("2024-01-01T12:00:00Z")),
    ])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      target_like_uri,
      "cid_like",
      "did:plc:liker",
      "app.bsky.feed.like",
      target_like_json,
    )

  // Insert a reply post that references the parent (post)
  let reply_to_post_uri = "at://did:plc:user1/app.bsky.feed.post/reply1"
  let reply_to_post_json =
    json.object([
      #("text", json.string("Reply to a post")),
      #("replyTo", json.string(parent_post_uri)),
    ])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      reply_to_post_uri,
      "cid_reply1",
      "did:plc:user1",
      "app.bsky.feed.post",
      reply_to_post_json,
    )

  // Insert a reply post that references the like
  let reply_to_like_uri = "at://did:plc:user2/app.bsky.feed.post/reply2"
  let reply_to_like_json =
    json.object([
      #("text", json.string("Reply to a like")),
      #("replyTo", json.string(target_like_uri)),
    ])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      reply_to_like_uri,
      "cid_reply2",
      "did:plc:user2",
      "app.bsky.feed.post",
      reply_to_like_json,
    )

  // Execute GraphQL query with inline fragments to access type-specific fields
  let query =
    "
    {
      appBskyFeedPost {
        edges {
          node {
            uri
            text
            replyToResolved {
              ... on AppBskyFeedPost {
                uri
                text
              }
              ... on AppBskyFeedLike {
                uri
                subject
                createdAt
              }
            }
          }
        }
      }
    }
  "

  let assert Ok(response_json) =
    graphql_gleam.execute_query_with_db(db, query, "{}", Error(Nil), "", "https://plc.directory")

  // Verify we can access type-specific fields through inline fragments

  // For the post reply, we should see the parent post's text
  string.contains(response_json, reply_to_post_uri)
  |> should.be_true

  string.contains(response_json, "Reply to a post")
  |> should.be_true

  string.contains(response_json, "This is the parent post")
  |> should.be_true

  // For the like reply, we should see the like's subject and createdAt
  string.contains(response_json, reply_to_like_uri)
  |> should.be_true

  string.contains(response_json, "Reply to a like")
  |> should.be_true

  string.contains(response_json, "2024-01-01T12:00:00Z")
  |> should.be_true

  // Verify the resolved records have the correct URIs
  string.contains(response_json, parent_post_uri)
  |> should.be_true

  string.contains(response_json, target_like_uri)
  |> should.be_true
}

// Helper to create a profile lexicon with literal:self key
fn create_profile_lexicon_with_literal_self() -> String {
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
                    #("bio", json.object([#("type", json.string("string"))])),
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

// Test: DID join to literal:self collection returns single nullable object
pub fn did_join_to_literal_self_returns_single_test() {
  // Setup database
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = database.create_lexicon_table(db)
  let assert Ok(_) = database.create_record_table(db)
  let assert Ok(_) = database.create_actor_table(db)

  // Insert lexicons
  let post_lexicon = create_post_lexicon()
  let profile_lexicon = create_profile_lexicon_with_literal_self()
  let assert Ok(_) =
    database.insert_lexicon(db, "app.bsky.feed.post", post_lexicon)
  let assert Ok(_) =
    database.insert_lexicon(db, "app.bsky.actor.profile", profile_lexicon)

  // Insert a profile with literal:self key
  let profile_uri = "at://did:plc:user123/app.bsky.actor.profile/self"
  let profile_json =
    json.object([
      #("displayName", json.string("Alice")),
      #("bio", json.string("Software engineer")),
    ])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      profile_uri,
      "cid_profile",
      "did:plc:user123",
      "app.bsky.actor.profile",
      profile_json,
    )

  // Insert a post by the same DID
  let post_uri = "at://did:plc:user123/app.bsky.feed.post/post1"
  let post_json =
    json.object([#("text", json.string("My first post"))])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      post_uri,
      "cid_post1",
      "did:plc:user123",
      "app.bsky.feed.post",
      post_json,
    )

  // Execute GraphQL query with DID join from post to profile
  let query =
    "
    {
      appBskyFeedPost {
        edges {
          node {
            uri
            text
            appBskyActorProfileByDid {
              uri
              displayName
              bio
            }
          }
        }
      }
    }
  "

  let assert Ok(response_json) =
    graphql_gleam.execute_query_with_db(db, query, "{}", Error(Nil), "", "https://plc.directory")

  // Verify the response contains the DID-joined profile as a single object (not array)
  string.contains(response_json, post_uri)
  |> should.be_true

  string.contains(response_json, "appBskyActorProfileByDid")
  |> should.be_true

  string.contains(response_json, profile_uri)
  |> should.be_true

  string.contains(response_json, "Alice")
  |> should.be_true

  string.contains(response_json, "Software engineer")
  |> should.be_true
}

// Test: DID join to non-literal:self collection returns list
pub fn did_join_to_non_literal_self_returns_list_test() {
  // Setup database
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = database.create_lexicon_table(db)
  let assert Ok(_) = database.create_record_table(db)
  let assert Ok(_) = database.create_actor_table(db)

  // Insert lexicons
  let post_lexicon = create_post_lexicon()
  let profile_lexicon = create_profile_lexicon_with_literal_self()
  let assert Ok(_) =
    database.insert_lexicon(db, "app.bsky.feed.post", post_lexicon)
  let assert Ok(_) =
    database.insert_lexicon(db, "app.bsky.actor.profile", profile_lexicon)

  // Insert a profile
  let profile_uri = "at://did:plc:author/app.bsky.actor.profile/self"
  let profile_json =
    json.object([
      #("displayName", json.string("Bob")),
      #("bio", json.string("Writes a lot")),
    ])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      profile_uri,
      "cid_profile",
      "did:plc:author",
      "app.bsky.actor.profile",
      profile_json,
    )

  // Insert multiple posts by the same DID
  let post1_uri = "at://did:plc:author/app.bsky.feed.post/post1"
  let post1_json =
    json.object([#("text", json.string("First post"))])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      post1_uri,
      "cid_post1",
      "did:plc:author",
      "app.bsky.feed.post",
      post1_json,
    )

  let post2_uri = "at://did:plc:author/app.bsky.feed.post/post2"
  let post2_json =
    json.object([#("text", json.string("Second post"))])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      post2_uri,
      "cid_post2",
      "did:plc:author",
      "app.bsky.feed.post",
      post2_json,
    )

  // Execute GraphQL query with DID join from profile to posts (now returns connection)
  let query =
    "
    {
      appBskyActorProfile {
        edges {
          node {
            uri
            displayName
            appBskyFeedPostByDid {
              edges {
                node {
                  uri
                  text
                }
              }
            }
          }
        }
      }
    }
  "

  let assert Ok(response_json) =
    graphql_gleam.execute_query_with_db(db, query, "{}", Error(Nil), "", "https://plc.directory")

  // Verify the response contains the DID-joined posts as a list
  string.contains(response_json, profile_uri)
  |> should.be_true

  string.contains(response_json, "appBskyFeedPostByDid")
  |> should.be_true

  string.contains(response_json, post1_uri)
  |> should.be_true

  string.contains(response_json, "First post")
  |> should.be_true

  string.contains(response_json, post2_uri)
  |> should.be_true

  string.contains(response_json, "Second post")
  |> should.be_true
}

// Test: DID join batches queries efficiently for multiple records
pub fn did_join_batches_queries_test() {
  // Setup database
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = database.create_lexicon_table(db)
  let assert Ok(_) = database.create_record_table(db)
  let assert Ok(_) = database.create_actor_table(db)

  // Insert lexicons
  let post_lexicon = create_post_lexicon()
  let profile_lexicon = create_profile_lexicon_with_literal_self()
  let assert Ok(_) =
    database.insert_lexicon(db, "app.bsky.feed.post", post_lexicon)
  let assert Ok(_) =
    database.insert_lexicon(db, "app.bsky.actor.profile", profile_lexicon)

  // Insert multiple profiles
  let profile1_uri = "at://did:plc:user1/app.bsky.actor.profile/self"
  let profile1_json =
    json.object([
      #("displayName", json.string("User One")),
      #("bio", json.string("First user")),
    ])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      profile1_uri,
      "cid_profile1",
      "did:plc:user1",
      "app.bsky.actor.profile",
      profile1_json,
    )

  let profile2_uri = "at://did:plc:user2/app.bsky.actor.profile/self"
  let profile2_json =
    json.object([
      #("displayName", json.string("User Two")),
      #("bio", json.string("Second user")),
    ])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      profile2_uri,
      "cid_profile2",
      "did:plc:user2",
      "app.bsky.actor.profile",
      profile2_json,
    )

  // Insert posts by different DIDs
  let post1_uri = "at://did:plc:user1/app.bsky.feed.post/post1"
  let post1_json =
    json.object([#("text", json.string("Post by user 1"))])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      post1_uri,
      "cid_post1",
      "did:plc:user1",
      "app.bsky.feed.post",
      post1_json,
    )

  let post2_uri = "at://did:plc:user2/app.bsky.feed.post/post2"
  let post2_json =
    json.object([#("text", json.string("Post by user 2"))])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      post2_uri,
      "cid_post2",
      "did:plc:user2",
      "app.bsky.feed.post",
      post2_json,
    )

  // Execute GraphQL query that fetches multiple posts with DID joins to profiles
  // DataLoader should batch the profile lookups by DID
  let query =
    "
    {
      appBskyFeedPost {
        edges {
          node {
            uri
            text
            appBskyActorProfileByDid {
              uri
              displayName
            }
          }
        }
      }
    }
  "

  let assert Ok(response_json) =
    graphql_gleam.execute_query_with_db(db, query, "{}", Error(Nil), "", "https://plc.directory")

  // Verify all posts and their associated profiles appear
  string.contains(response_json, post1_uri)
  |> should.be_true

  string.contains(response_json, "Post by user 1")
  |> should.be_true

  string.contains(response_json, profile1_uri)
  |> should.be_true

  string.contains(response_json, "User One")
  |> should.be_true

  string.contains(response_json, post2_uri)
  |> should.be_true

  string.contains(response_json, "Post by user 2")
  |> should.be_true

  string.contains(response_json, profile2_uri)
  |> should.be_true

  string.contains(response_json, "User Two")
  |> should.be_true
  // Note: To truly verify batching, we'd need to instrument the database
  // layer to count queries. For now, this test ensures correctness.
}
