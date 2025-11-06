/// Integration tests for sortBy and where on nested join connections
///
/// Tests verify that:
/// - sortBy works on nested DID joins and reverse joins
/// - where filters work on nested joins
/// - totalCount reflects filtered results
/// - Combination of sortBy + where works correctly
import database
import gleam/int
import gleam/json
import gleam/list
import gleam/string
import gleeunit/should
import graphql_gleam
import sqlight

// Helper to create a status lexicon with createdAt field
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
                  json.array([json.string("status")], of: fn(x) { x }),
                ),
                #(
                  "properties",
                  json.object([
                    #(
                      "status",
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

// Helper to create a profile lexicon
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

// Test: DID join with sortBy on createdAt DESC
pub fn did_join_sortby_createdat_desc_test() {
  // Setup database
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = database.create_lexicon_table(db)
  let assert Ok(_) = database.create_record_table(db)
  let assert Ok(_) = database.create_actor_table(db)

  // Insert lexicons
  let status_lexicon = create_status_lexicon()
  let profile_lexicon = create_profile_lexicon()
  let assert Ok(_) =
    database.insert_lexicon(db, "xyz.statusphere.status", status_lexicon)
  let assert Ok(_) =
    database.insert_lexicon(db, "app.bsky.actor.profile", profile_lexicon)

  // Insert a profile
  let profile_uri = "at://did:plc:user1/app.bsky.actor.profile/self"
  let profile_json =
    json.object([#("displayName", json.string("User One"))])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      profile_uri,
      "cid_profile",
      "did:plc:user1",
      "app.bsky.actor.profile",
      profile_json,
    )

  // Insert 5 statuses with DIFFERENT createdAt timestamps
  // Status 1: 2024-01-01 (oldest)
  // Status 2: 2024-01-02
  // Status 3: 2024-01-03
  // Status 4: 2024-01-04
  // Status 5: 2024-01-05 (newest)
  list.range(1, 5)
  |> list.each(fn(i) {
    let status_uri =
      "at://did:plc:user1/xyz.statusphere.status/status" <> int.to_string(i)
    let status_json =
      json.object([
        #("status", json.string("Status #" <> int.to_string(i))),
        #(
          "createdAt",
          json.string("2024-01-0" <> int.to_string(i) <> "T12:00:00Z"),
        ),
      ])
      |> json.to_string

    let assert Ok(_) =
      database.insert_record(
        db,
        status_uri,
        "cid_status" <> int.to_string(i),
        "did:plc:user1",
        "xyz.statusphere.status",
        status_json,
      )
    Nil
  })

  // Execute GraphQL query with sortBy createdAt DESC and first:3
  let query =
    "
    {
      appBskyActorProfile {
        edges {
          node {
            uri
            statuses: xyzStatusphereStatusByDid(
              first: 3
              sortBy: [{field: \"createdAt\", direction: DESC}]
            ) {
              totalCount
              edges {
                node {
                  status
                  createdAt
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

  // Verify totalCount is 5 (all statuses)
  {
    string.contains(response_json, "\"totalCount\":5")
    || string.contains(response_json, "\"totalCount\": 5")
  }
  |> should.be_true

  // With sortBy createdAt DESC, first:3 should return Status 5, 4, 3 (newest first)
  string.contains(response_json, "Status #5")
  |> should.be_true

  string.contains(response_json, "Status #4")
  |> should.be_true

  string.contains(response_json, "Status #3")
  |> should.be_true

  // Should NOT contain Status 1 or 2 (they're older)
  string.contains(response_json, "Status #1")
  |> should.be_false

  string.contains(response_json, "Status #2")
  |> should.be_false

  // Verify order: Status 5 should appear before Status 4
  let pos_5 = case string.split(response_json, "Status #5") {
    [before, ..] -> string.length(before)
    [] -> 999_999
  }

  let pos_4 = case string.split(response_json, "Status #4") {
    [before, ..] -> string.length(before)
    [] -> 999_999
  }

  let pos_3 = case string.split(response_json, "Status #3") {
    [before, ..] -> string.length(before)
    [] -> 999_999
  }

  // Status 5 should come before Status 4
  { pos_5 < pos_4 }
  |> should.be_true

  // Status 4 should come before Status 3
  { pos_4 < pos_3 }
  |> should.be_true
}

// Test: DID join with sortBy createdAt ASC
pub fn did_join_sortby_createdat_asc_test() {
  // Setup database
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = database.create_lexicon_table(db)
  let assert Ok(_) = database.create_record_table(db)
  let assert Ok(_) = database.create_actor_table(db)

  // Insert lexicons
  let status_lexicon = create_status_lexicon()
  let profile_lexicon = create_profile_lexicon()
  let assert Ok(_) =
    database.insert_lexicon(db, "xyz.statusphere.status", status_lexicon)
  let assert Ok(_) =
    database.insert_lexicon(db, "app.bsky.actor.profile", profile_lexicon)

  // Insert a profile
  let profile_uri = "at://did:plc:user1/app.bsky.actor.profile/self"
  let profile_json =
    json.object([#("displayName", json.string("User One"))])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      profile_uri,
      "cid_profile",
      "did:plc:user1",
      "app.bsky.actor.profile",
      profile_json,
    )

  // Insert 5 statuses with different timestamps
  list.range(1, 5)
  |> list.each(fn(i) {
    let status_uri =
      "at://did:plc:user1/xyz.statusphere.status/status" <> int.to_string(i)
    let status_json =
      json.object([
        #("status", json.string("Status #" <> int.to_string(i))),
        #(
          "createdAt",
          json.string("2024-01-0" <> int.to_string(i) <> "T12:00:00Z"),
        ),
      ])
      |> json.to_string

    let assert Ok(_) =
      database.insert_record(
        db,
        status_uri,
        "cid_status" <> int.to_string(i),
        "did:plc:user1",
        "xyz.statusphere.status",
        status_json,
      )
    Nil
  })

  // Execute GraphQL query with sortBy createdAt ASC and first:3
  let query =
    "
    {
      appBskyActorProfile {
        edges {
          node {
            statuses: xyzStatusphereStatusByDid(
              first: 3
              sortBy: [{field: \"createdAt\", direction: ASC}]
            ) {
              totalCount
              edges {
                node {
                  status
                  createdAt
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

  // With sortBy createdAt ASC, first:3 should return Status 1, 2, 3 (oldest first)
  string.contains(response_json, "Status #1")
  |> should.be_true

  string.contains(response_json, "Status #2")
  |> should.be_true

  string.contains(response_json, "Status #3")
  |> should.be_true

  // Should NOT contain Status 4 or 5
  string.contains(response_json, "Status #4")
  |> should.be_false

  string.contains(response_json, "Status #5")
  |> should.be_false
}

// Test: DID join with where filter on status field
pub fn did_join_where_filter_test() {
  // Setup database
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = database.create_lexicon_table(db)
  let assert Ok(_) = database.create_record_table(db)
  let assert Ok(_) = database.create_actor_table(db)

  // Insert lexicons
  let status_lexicon = create_status_lexicon()
  let profile_lexicon = create_profile_lexicon()
  let assert Ok(_) =
    database.insert_lexicon(db, "xyz.statusphere.status", status_lexicon)
  let assert Ok(_) =
    database.insert_lexicon(db, "app.bsky.actor.profile", profile_lexicon)

  // Insert a profile
  let profile_uri = "at://did:plc:user1/app.bsky.actor.profile/self"
  let profile_json =
    json.object([#("displayName", json.string("User One"))])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      profile_uri,
      "cid_profile",
      "did:plc:user1",
      "app.bsky.actor.profile",
      profile_json,
    )

  // Insert 5 statuses: 3 containing "gleam", 2 not
  let statuses = [
    #("Status about gleam programming", "2024-01-01T12:00:00Z"),
    #("Random post", "2024-01-02T12:00:00Z"),
    #("Learning gleam today", "2024-01-03T12:00:00Z"),
    #("Another random post", "2024-01-04T12:00:00Z"),
    #("Gleam is awesome", "2024-01-05T12:00:00Z"),
  ]

  list.index_map(statuses, fn(status_data, idx) {
    let #(status_text, created_at) = status_data
    let i = idx + 1
    let status_uri =
      "at://did:plc:user1/xyz.statusphere.status/status" <> int.to_string(i)
    let status_json =
      json.object([
        #("status", json.string(status_text)),
        #("createdAt", json.string(created_at)),
      ])
      |> json.to_string

    let assert Ok(_) =
      database.insert_record(
        db,
        status_uri,
        "cid_status" <> int.to_string(i),
        "did:plc:user1",
        "xyz.statusphere.status",
        status_json,
      )
    Nil
  })

  // Execute GraphQL query with where filter: status contains "gleam"
  let query =
    "
    {
      appBskyActorProfile {
        edges {
          node {
            statuses: xyzStatusphereStatusByDid(
              sortBy: [{field: \"createdAt\", direction: DESC}]
              where: {status: {contains: \"gleam\"}}
            ) {
              totalCount
              edges {
                node {
                  status
                  createdAt
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

  // totalCount should be 3 (only statuses containing "gleam")
  {
    string.contains(response_json, "\"totalCount\":3")
    || string.contains(response_json, "\"totalCount\": 3")
  }
  |> should.be_true

  // Should contain statuses with "gleam"
  string.contains(response_json, "gleam programming")
  |> should.be_true

  string.contains(response_json, "Learning gleam")
  |> should.be_true

  string.contains(response_json, "Gleam is awesome")
  |> should.be_true

  // Should NOT contain "Random post"
  string.contains(response_json, "Random post")
  |> should.be_false
}

// Test: Combination of sortBy + where + first
pub fn did_join_sortby_where_first_test() {
  // Setup database
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = database.create_lexicon_table(db)
  let assert Ok(_) = database.create_record_table(db)
  let assert Ok(_) = database.create_actor_table(db)

  // Insert lexicons
  let status_lexicon = create_status_lexicon()
  let profile_lexicon = create_profile_lexicon()
  let assert Ok(_) =
    database.insert_lexicon(db, "xyz.statusphere.status", status_lexicon)
  let assert Ok(_) =
    database.insert_lexicon(db, "app.bsky.actor.profile", profile_lexicon)

  // Insert a profile
  let profile_uri = "at://did:plc:user1/app.bsky.actor.profile/self"
  let profile_json =
    json.object([#("displayName", json.string("User One"))])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      profile_uri,
      "cid_profile",
      "did:plc:user1",
      "app.bsky.actor.profile",
      profile_json,
    )

  // Insert 5 statuses: 3 containing "rust", 2 not
  let statuses = [
    #("Learning rust basics", "2024-01-01T12:00:00Z"),
    #("Random gleam post", "2024-01-02T12:00:00Z"),
    #("Rust ownership is cool", "2024-01-03T12:00:00Z"),
    #("Another elixir post", "2024-01-04T12:00:00Z"),
    #("Rust async programming", "2024-01-05T12:00:00Z"),
  ]

  list.index_map(statuses, fn(status_data, idx) {
    let #(status_text, created_at) = status_data
    let i = idx + 1
    let status_uri =
      "at://did:plc:user1/xyz.statusphere.status/status" <> int.to_string(i)
    let status_json =
      json.object([
        #("status", json.string(status_text)),
        #("createdAt", json.string(created_at)),
      ])
      |> json.to_string

    let assert Ok(_) =
      database.insert_record(
        db,
        status_uri,
        "cid_status" <> int.to_string(i),
        "did:plc:user1",
        "xyz.statusphere.status",
        status_json,
      )
    Nil
  })

  // Execute query: where status contains "rust", sortBy createdAt DESC, first: 2
  // Should return the 2 newest rust posts
  let query =
    "
    {
      appBskyActorProfile {
        edges {
          node {
            statuses: xyzStatusphereStatusByDid(
              first: 2
              sortBy: [{field: \"createdAt\", direction: DESC}]
              where: {status: {contains: \"rust\"}}
            ) {
              totalCount
              edges {
                node {
                  status
                  createdAt
                }
              }
              pageInfo {
                hasNextPage
              }
            }
          }
        }
      }
    }
  "

  let assert Ok(response_json) =
    graphql_gleam.execute_query_with_db(db, query, "{}", Error(Nil), "", "https://plc.directory")

  // totalCount should be 3 (all rust posts)
  {
    string.contains(response_json, "\"totalCount\":3")
    || string.contains(response_json, "\"totalCount\": 3")
  }
  |> should.be_true

  // Should contain the 2 newest rust posts
  string.contains(response_json, "Rust async programming")
  |> should.be_true

  string.contains(response_json, "Rust ownership")
  |> should.be_true

  // Should NOT contain the oldest rust post
  string.contains(response_json, "Learning rust basics")
  |> should.be_false

  // Should NOT contain non-rust posts
  string.contains(response_json, "elixir")
  |> should.be_false

  string.contains(response_json, "gleam")
  |> should.be_false

  // hasNextPage should be true (1 more rust post available)
  {
    string.contains(response_json, "\"hasNextPage\":true")
    || string.contains(response_json, "\"hasNextPage\": true")
  }
  |> should.be_true
}

// Test: Exact query pattern from user - top-level where + nested sortBy
pub fn user_query_pattern_test() {
  // Setup database
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = database.create_lexicon_table(db)
  let assert Ok(_) = database.create_record_table(db)
  let assert Ok(_) = database.create_actor_table(db)

  // Insert lexicons
  let status_lexicon = create_status_lexicon()
  let profile_lexicon = create_profile_lexicon()
  let assert Ok(_) =
    database.insert_lexicon(db, "xyz.statusphere.status", status_lexicon)
  let assert Ok(_) =
    database.insert_lexicon(db, "app.bsky.actor.profile", profile_lexicon)

  // Insert 2 profiles with different handles
  let profile1_uri = "at://did:plc:user1/app.bsky.actor.profile/self"
  let profile1_json =
    json.object([#("displayName", json.string("User One"))])
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

  let assert Ok(_) =
    database.upsert_actor(db, "did:plc:user1", "chadtmiller.com")

  let profile2_uri = "at://did:plc:user2/app.bsky.actor.profile/self"
  let profile2_json =
    json.object([#("displayName", json.string("User Two"))])
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

  let assert Ok(_) = database.upsert_actor(db, "did:plc:user2", "other.com")

  // Insert statuses for user1 (chadtmiller.com)
  let statuses1 = [
    #("First status", "2024-01-01T12:00:00Z"),
    #("Second status", "2024-01-02T12:00:00Z"),
    #("Third status", "2024-01-03T12:00:00Z"),
  ]

  list.index_map(statuses1, fn(status_data, idx) {
    let #(status_text, created_at) = status_data
    let i = idx + 1
    let status_uri =
      "at://did:plc:user1/xyz.statusphere.status/status" <> int.to_string(i)
    let status_json =
      json.object([
        #("status", json.string(status_text)),
        #("createdAt", json.string(created_at)),
      ])
      |> json.to_string

    let assert Ok(_) =
      database.insert_record(
        db,
        status_uri,
        "cid_status1_" <> int.to_string(i),
        "did:plc:user1",
        "xyz.statusphere.status",
        status_json,
      )
    Nil
  })

  // Insert statuses for user2 (should be filtered out)
  let assert Ok(_) =
    database.insert_record(
      db,
      "at://did:plc:user2/xyz.statusphere.status/status1",
      "cid_status2_1",
      "did:plc:user2",
      "xyz.statusphere.status",
      json.object([
        #("status", json.string("Other user status")),
        #("createdAt", json.string("2024-01-01T12:00:00Z")),
      ])
        |> json.to_string,
    )

  // Execute query matching user's pattern: top-level where + nested sortBy
  let query =
    "
    {
      appBskyActorProfile(where: {actorHandle: {eq: \"chadtmiller.com\"}}) {
        totalCount
        edges {
          node {
            actorHandle
            statuses: xyzStatusphereStatusByDid(
              first: 12
              sortBy: [{field: \"createdAt\", direction: DESC}]
            ) {
              totalCount
              edges {
                node {
                  status
                  createdAt
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

  // Should only return 1 profile (chadtmiller.com)
  {
    string.contains(response_json, "\"totalCount\":1")
    || string.contains(response_json, "\"totalCount\": 1")
  }
  |> should.be_true

  // Should contain chadtmiller.com handle
  string.contains(response_json, "chadtmiller.com")
  |> should.be_true

  // Should NOT contain other.com
  string.contains(response_json, "other.com")
  |> should.be_false

  // Nested statuses should have totalCount 3
  // Count occurrences of totalCount in response
  let total_count_occurrences =
    string.split(response_json, "\"totalCount\"")
    |> list.length
    |> fn(x) { x - 1 }

  // Should have 2 totalCount fields (one for profiles, one for statuses)
  total_count_occurrences
  |> should.equal(2)

  // Statuses should be in DESC order: Third, Second, First
  string.contains(response_json, "Third status")
  |> should.be_true

  string.contains(response_json, "Second status")
  |> should.be_true

  string.contains(response_json, "First status")
  |> should.be_true

  // Verify order: Third should come before Second
  let pos_third = case string.split(response_json, "Third status") {
    [before, ..] -> string.length(before)
    [] -> 999_999
  }

  let pos_second = case string.split(response_json, "Second status") {
    [before, ..] -> string.length(before)
    [] -> 999_999
  }

  let pos_first = case string.split(response_json, "First status") {
    [before, ..] -> string.length(before)
    [] -> 999_999
  }

  { pos_third < pos_second }
  |> should.be_true

  { pos_second < pos_first }
  |> should.be_true

  // Should NOT contain other user's status
  string.contains(response_json, "Other user status")
  |> should.be_false
}
