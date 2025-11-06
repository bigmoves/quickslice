/// Integration tests for Blob type in GraphQL queries
///
/// Tests the full flow of querying blob fields:
/// 1. Create a lexicon with blob fields
/// 2. Insert records with blob data in AT Protocol format
/// 3. Execute GraphQL queries with blob field selection
/// 4. Verify blob fields are resolved correctly with all sub-fields
import database
import gleam/http
import gleam/json
import gleam/string
import gleeunit/should
import graphql_handler
import sqlight
import wisp
import wisp/simulate

/// Create a lexicon with a blob field (profile with avatar)
fn create_profile_lexicon() -> String {
  json.object([
    #("lexicon", json.int(1)),
    #("id", json.string("app.test.profile")),
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
                  "required",
                  json.array([json.string("displayName")], of: fn(x) { x }),
                ),
                #(
                  "properties",
                  json.object([
                    #(
                      "displayName",
                      json.object([#("type", json.string("string"))]),
                    ),
                    #(
                      "description",
                      json.object([#("type", json.string("string"))]),
                    ),
                    #("avatar", json.object([#("type", json.string("blob"))])),
                    #("banner", json.object([#("type", json.string("blob"))])),
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

pub fn blob_field_query_test() {
  // Create in-memory database
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = database.create_lexicon_table(db)
  let assert Ok(_) = database.create_record_table(db)

  // Insert profile lexicon with blob fields
  let lexicon = create_profile_lexicon()
  let assert Ok(_) = database.insert_lexicon(db, "app.test.profile", lexicon)

  // Insert a profile record with avatar blob
  // AT Protocol blob format: { ref: { $link: "cid" }, mimeType: "...", size: 123 }
  let record_json =
    json.object([
      #("displayName", json.string("Alice")),
      #("description", json.string("Software developer")),
      #(
        "avatar",
        json.object([
          #("ref", json.object([#("$link", json.string("bafyreiabc123"))])),
          #("mimeType", json.string("image/jpeg")),
          #("size", json.int(45_678)),
        ]),
      ),
    ])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      "at://did:plc:alice123/app.test.profile/self",
      "cidprofile1",
      "did:plc:alice123",
      "app.test.profile",
      record_json,
    )

  // Query blob fields with all sub-fields
  let query =
    json.object([
      #(
        "query",
        json.string(
          "{ appTestProfile { edges { node { displayName avatar { ref mimeType size url(preset: \"avatar\") } } } } }",
        ),
      ),
    ])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query)
    |> simulate.header("content-type", "application/json")

  let response =
    graphql_handler.handle_graphql_request(request, db, "http://localhost:3000", "https://plc.directory")

  // Verify response
  response.status
  |> should.equal(200)

  let assert wisp.Text(body) = response.body

  // Verify response contains data
  string.contains(body, "\"data\"")
  |> should.be_true

  // Verify displayName is present
  string.contains(body, "Alice")
  |> should.be_true

  // Verify blob ref field
  string.contains(body, "bafyreiabc123")
  |> should.be_true

  // Verify blob mimeType field
  string.contains(body, "image/jpeg")
  |> should.be_true

  // Verify blob size field
  string.contains(body, "45678")
  |> should.be_true

  // Verify blob url field contains CDN URL with avatar preset
  string.contains(
    body,
    "https://cdn.bsky.app/img/avatar/plain/did:plc:alice123/bafyreiabc123@jpeg",
  )
  |> should.be_true
}

pub fn blob_field_with_different_presets_test() {
  // Create in-memory database
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = database.create_lexicon_table(db)
  let assert Ok(_) = database.create_record_table(db)

  // Insert profile lexicon
  let lexicon = create_profile_lexicon()
  let assert Ok(_) = database.insert_lexicon(db, "app.test.profile", lexicon)

  // Insert a profile with banner blob
  let record_json =
    json.object([
      #("displayName", json.string("Bob")),
      #(
        "banner",
        json.object([
          #("ref", json.object([#("$link", json.string("bafyreibanner789"))])),
          #("mimeType", json.string("image/png")),
          #("size", json.int(98_765)),
        ]),
      ),
    ])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      "at://did:plc:bob456/app.test.profile/self",
      "cidbanner1",
      "did:plc:bob456",
      "app.test.profile",
      record_json,
    )

  // Query with banner preset
  let query =
    json.object([
      #(
        "query",
        json.string(
          "{ appTestProfile { edges { node { banner { url(preset: \"banner\") } } } } }",
        ),
      ),
    ])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query)
    |> simulate.header("content-type", "application/json")

  let response =
    graphql_handler.handle_graphql_request(request, db, "http://localhost:3000", "https://plc.directory")

  response.status
  |> should.equal(200)

  let assert wisp.Text(body) = response.body

  // Verify banner URL with banner preset
  string.contains(
    body,
    "https://cdn.bsky.app/img/banner/plain/did:plc:bob456/bafyreibanner789@jpeg",
  )
  |> should.be_true
}

pub fn blob_field_default_preset_test() {
  // Test that when no preset is specified, feed_fullsize is used
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = database.create_lexicon_table(db)
  let assert Ok(_) = database.create_record_table(db)

  let lexicon = create_profile_lexicon()
  let assert Ok(_) = database.insert_lexicon(db, "app.test.profile", lexicon)

  let record_json =
    json.object([
      #("displayName", json.string("Charlie")),
      #(
        "avatar",
        json.object([
          #("ref", json.object([#("$link", json.string("bafyreidefault"))])),
          #("mimeType", json.string("image/jpeg")),
          #("size", json.int(12_345)),
        ]),
      ),
    ])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      "at://did:plc:charlie/app.test.profile/self",
      "cidcharlie",
      "did:plc:charlie",
      "app.test.profile",
      record_json,
    )

  // Query without specifying preset (should default to feed_fullsize)
  let query =
    json.object([
      #(
        "query",
        json.string("{ appTestProfile { edges { node { avatar { url } } } } }"),
      ),
    ])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query)
    |> simulate.header("content-type", "application/json")

  let response =
    graphql_handler.handle_graphql_request(request, db, "http://localhost:3000", "https://plc.directory")

  response.status
  |> should.equal(200)

  let assert wisp.Text(body) = response.body

  // Verify default preset is feed_fullsize
  string.contains(
    body,
    "https://cdn.bsky.app/img/feed_fullsize/plain/did:plc:charlie/bafyreidefault@jpeg",
  )
  |> should.be_true
}

pub fn blob_field_null_when_missing_test() {
  // Test that blob fields return null when not present in record
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = database.create_lexicon_table(db)
  let assert Ok(_) = database.create_record_table(db)

  let lexicon = create_profile_lexicon()
  let assert Ok(_) = database.insert_lexicon(db, "app.test.profile", lexicon)

  // Insert record without avatar field
  let record_json =
    json.object([#("displayName", json.string("Dave"))])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      "at://did:plc:dave/app.test.profile/self",
      "ciddave",
      "did:plc:dave",
      "app.test.profile",
      record_json,
    )

  let query =
    json.object([
      #(
        "query",
        json.string(
          "{ appTestProfile { edges { node { displayName avatar { ref } } } } }",
        ),
      ),
    ])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query)
    |> simulate.header("content-type", "application/json")

  let response =
    graphql_handler.handle_graphql_request(request, db, "http://localhost:3000", "https://plc.directory")

  response.status
  |> should.equal(200)

  let assert wisp.Text(body) = response.body

  // Verify displayName is present
  string.contains(body, "Dave")
  |> should.be_true

  // Verify avatar is null (JSON encoder adds space after colon)
  string.contains(body, "\"avatar\": null")
  |> should.be_true
}
