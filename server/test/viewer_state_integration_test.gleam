/// Integration tests for viewer state fields
///
/// Verifies that viewer fields show the authenticated viewer's relationship
/// to records (e.g., viewer's like on a gallery)
import database/repositories/lexicons
import database/repositories/records
import gleam/http
import gleam/json
import gleam/option.{None}
import gleam/string
import gleeunit/should
import handlers/graphql as graphql_handler
import lib/oauth/did_cache
import test_helpers
import wisp
import wisp/simulate

// Gallery lexicon with subject field for favorites
fn create_gallery_lexicon() -> String {
  json.object([
    #("lexicon", json.int(1)),
    #("id", json.string("social.grain.gallery")),
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
                  json.array([json.string("title")], of: fn(x) { x }),
                ),
                #(
                  "properties",
                  json.object([
                    #("title", json.object([#("type", json.string("string"))])),
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

// Favorite lexicon with AT-URI subject field
fn create_favorite_lexicon() -> String {
  json.object([
    #("lexicon", json.int(1)),
    #("id", json.string("social.grain.favorite")),
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

// Follow lexicon with DID subject field
fn create_follow_lexicon() -> String {
  json.object([
    #("lexicon", json.int(1)),
    #("id", json.string("social.grain.graph.follow")),
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
                        #("format", json.string("did")),
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

/// Test: Viewer favorite field returns null when not favorited
pub fn viewer_favorite_null_when_not_favorited_test() {
  // Setup database
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_lexicon_table(exec)
  let assert Ok(_) = test_helpers.create_record_table(exec)
  let assert Ok(_) = test_helpers.create_config_table(exec)
  let assert Ok(_) = test_helpers.create_actor_table(exec)
  let assert Ok(_) = test_helpers.create_oauth_tables(exec)
  let assert Ok(_) =
    test_helpers.insert_test_token(exec, "test-viewer-token", "did:plc:viewer")

  // Insert lexicons
  let assert Ok(_) =
    lexicons.insert(exec, "social.grain.gallery", create_gallery_lexicon())
  let assert Ok(_) =
    lexicons.insert(exec, "social.grain.favorite", create_favorite_lexicon())

  // Create a gallery record
  let gallery_uri = "at://did:plc:author/social.grain.gallery/gallery1"
  let gallery_json =
    json.object([#("title", json.string("Test Gallery"))])
    |> json.to_string

  let assert Ok(_) =
    records.insert(
      exec,
      gallery_uri,
      "cid1",
      "did:plc:author",
      "social.grain.gallery",
      gallery_json,
    )

  // Query with auth token - should show null for viewerSocialGrainFavoriteViaSubject
  let query =
    json.object([
      #(
        "query",
        json.string(
          "{ socialGrainGallery { edges { node { uri viewerSocialGrainFavoriteViaSubject { uri } } } } }",
        ),
      ),
    ])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query)
    |> simulate.header("content-type", "application/json")
    |> simulate.header("authorization", "Bearer test-viewer-token")

  let assert Ok(cache) = did_cache.start()
  let response =
    graphql_handler.handle_graphql_request(request, exec, cache, None, "", "")

  let assert wisp.Text(body) = response.body

  // Debug: print the response if not 200
  case response.status {
    200 -> Nil
    _ -> {
      // Print error for debugging
      should.fail()
    }
  }

  // Should contain the gallery URI
  string.contains(body, gallery_uri) |> should.be_true

  // The viewer field should be null since there's no favorite
  // JSON formatting may have spaces, so check for the field and null value
  string.contains(body, "viewerSocialGrainFavoriteViaSubject")
  |> should.be_true
  string.contains(body, "null")
  |> should.be_true
}

/// Test: Schema includes viewer favorite field and query succeeds
pub fn viewer_favorite_schema_test() {
  // Setup database
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_lexicon_table(exec)
  let assert Ok(_) = test_helpers.create_record_table(exec)
  let assert Ok(_) = test_helpers.create_config_table(exec)
  let assert Ok(_) = test_helpers.create_actor_table(exec)
  let assert Ok(_) = test_helpers.create_oauth_tables(exec)
  let assert Ok(_) =
    test_helpers.insert_test_token(exec, "test-viewer-token", "did:plc:viewer")

  // Insert lexicons
  let assert Ok(_) =
    lexicons.insert(exec, "social.grain.gallery", create_gallery_lexicon())
  let assert Ok(_) =
    lexicons.insert(exec, "social.grain.favorite", create_favorite_lexicon())

  // Create a gallery record
  let gallery_uri = "at://did:plc:author/social.grain.gallery/gallery1"
  let gallery_json =
    json.object([#("title", json.string("Test Gallery"))])
    |> json.to_string

  let assert Ok(_) =
    records.insert(
      exec,
      gallery_uri,
      "cid1",
      "did:plc:author",
      "social.grain.gallery",
      gallery_json,
    )

  // Query WITH viewer field (verifies schema includes it)
  let query =
    json.object([
      #(
        "query",
        json.string(
          "{ socialGrainGallery { edges { node { uri viewerSocialGrainFavoriteViaSubject { uri subject } } } } }",
        ),
      ),
    ])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query)
    |> simulate.header("content-type", "application/json")
    |> simulate.header("authorization", "Bearer test-viewer-token")

  let assert Ok(cache) = did_cache.start()
  let response =
    graphql_handler.handle_graphql_request(request, exec, cache, None, "", "")

  let assert wisp.Text(body) = response.body

  // Query should succeed (schema generation works)
  response.status |> should.equal(200)

  // Should contain the gallery URI
  string.contains(body, gallery_uri) |> should.be_true

  // Viewer field should exist in response (currently null, data lookup needs work)
  string.contains(body, "viewerSocialGrainFavoriteViaSubject")
  |> should.be_true
}

/// Test: Viewer follow field returns null when not following
pub fn viewer_follow_null_when_not_following_test() {
  // Setup database
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_lexicon_table(exec)
  let assert Ok(_) = test_helpers.create_record_table(exec)
  let assert Ok(_) = test_helpers.create_config_table(exec)
  let assert Ok(_) = test_helpers.create_actor_table(exec)
  let assert Ok(_) = test_helpers.create_oauth_tables(exec)
  let assert Ok(_) =
    test_helpers.insert_test_token(exec, "test-viewer-token", "did:plc:viewer")

  // Insert lexicons
  let assert Ok(_) =
    lexicons.insert(exec, "social.grain.gallery", create_gallery_lexicon())
  let assert Ok(_) =
    lexicons.insert(exec, "social.grain.graph.follow", create_follow_lexicon())

  // Create a gallery record by a different author
  let gallery_uri = "at://did:plc:author/social.grain.gallery/gallery1"
  let gallery_json =
    json.object([#("title", json.string("Author's Gallery"))])
    |> json.to_string

  let assert Ok(_) =
    records.insert(
      exec,
      gallery_uri,
      "cid1",
      "did:plc:author",
      "social.grain.gallery",
      gallery_json,
    )

  // Query with auth token - should show null for viewerSocialGrainGraphFollowViaSubject
  let query =
    json.object([
      #(
        "query",
        json.string(
          "{ socialGrainGallery { edges { node { uri did viewerSocialGrainGraphFollowViaSubject { uri } } } } }",
        ),
      ),
    ])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query)
    |> simulate.header("content-type", "application/json")
    |> simulate.header("authorization", "Bearer test-viewer-token")

  let assert Ok(cache) = did_cache.start()
  let response =
    graphql_handler.handle_graphql_request(request, exec, cache, None, "", "")

  let assert wisp.Text(body) = response.body

  response.status |> should.equal(200)

  // Should contain the gallery URI
  string.contains(body, gallery_uri) |> should.be_true

  // The viewer follow field should be null since viewer doesn't follow the author
  string.contains(body, "viewerSocialGrainGraphFollowViaSubject")
  |> should.be_true
  string.contains(body, "null")
  |> should.be_true
}

/// Test: Viewer follow field returns follow when viewer follows the author
pub fn viewer_follow_returns_follow_when_following_test() {
  // Setup database
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_lexicon_table(exec)
  let assert Ok(_) = test_helpers.create_record_table(exec)
  let assert Ok(_) = test_helpers.create_config_table(exec)
  let assert Ok(_) = test_helpers.create_actor_table(exec)
  let assert Ok(_) = test_helpers.create_oauth_tables(exec)
  let assert Ok(_) =
    test_helpers.insert_test_token(exec, "test-viewer-token", "did:plc:viewer")

  // Insert lexicons
  let assert Ok(_) =
    lexicons.insert(exec, "social.grain.gallery", create_gallery_lexicon())
  let assert Ok(_) =
    lexicons.insert(exec, "social.grain.graph.follow", create_follow_lexicon())

  // Create a gallery record by a different author
  let gallery_uri = "at://did:plc:author/social.grain.gallery/gallery1"
  let gallery_json =
    json.object([#("title", json.string("Author's Gallery"))])
    |> json.to_string

  let assert Ok(_) =
    records.insert(
      exec,
      gallery_uri,
      "cid1",
      "did:plc:author",
      "social.grain.gallery",
      gallery_json,
    )

  // Create a follow record from the viewer to the author
  let follow_uri = "at://did:plc:viewer/social.grain.graph.follow/follow1"
  let follow_json =
    json.object([#("subject", json.string("did:plc:author"))])
    |> json.to_string

  let assert Ok(_) =
    records.insert(
      exec,
      follow_uri,
      "cid2",
      "did:plc:viewer",
      "social.grain.graph.follow",
      follow_json,
    )

  // Query with auth token only (no variables)
  let query =
    json.object([
      #(
        "query",
        json.string(
          "{ socialGrainGallery { edges { node { uri did viewerSocialGrainGraphFollowViaSubject { uri } } } } }",
        ),
      ),
    ])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query)
    |> simulate.header("content-type", "application/json")
    |> simulate.header("authorization", "Bearer test-viewer-token")

  let assert Ok(cache) = did_cache.start()
  let response =
    graphql_handler.handle_graphql_request(request, exec, cache, None, "", "")

  let assert wisp.Text(body) = response.body

  response.status |> should.equal(200)

  // Should contain the gallery URI
  string.contains(body, gallery_uri) |> should.be_true

  // The viewer follow field should contain the follow URI
  string.contains(body, follow_uri) |> should.be_true
}

/// Test: Viewer favorite field returns favorite when viewer has favorited (AT-URI subject)
pub fn viewer_favorite_returns_favorite_when_favorited_test() {
  // Setup database
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_lexicon_table(exec)
  let assert Ok(_) = test_helpers.create_record_table(exec)
  let assert Ok(_) = test_helpers.create_config_table(exec)
  let assert Ok(_) = test_helpers.create_actor_table(exec)
  let assert Ok(_) = test_helpers.create_oauth_tables(exec)
  let assert Ok(_) =
    test_helpers.insert_test_token(exec, "test-viewer-token", "did:plc:viewer")

  // Insert lexicons
  let assert Ok(_) =
    lexicons.insert(exec, "social.grain.gallery", create_gallery_lexicon())
  let assert Ok(_) =
    lexicons.insert(exec, "social.grain.favorite", create_favorite_lexicon())

  // Create a gallery record
  let gallery_uri = "at://did:plc:author/social.grain.gallery/gallery1"
  let gallery_json =
    json.object([#("title", json.string("Test Gallery"))])
    |> json.to_string

  let assert Ok(_) =
    records.insert(
      exec,
      gallery_uri,
      "cid1",
      "did:plc:author",
      "social.grain.gallery",
      gallery_json,
    )

  // Create a favorite record from the viewer for the gallery
  let favorite_uri = "at://did:plc:viewer/social.grain.favorite/fav1"
  let favorite_json =
    json.object([#("subject", json.string(gallery_uri))])
    |> json.to_string

  let assert Ok(_) =
    records.insert(
      exec,
      favorite_uri,
      "cid2",
      "did:plc:viewer",
      "social.grain.favorite",
      favorite_json,
    )

  // Query with auth token
  let query =
    json.object([
      #(
        "query",
        json.string(
          "{ socialGrainGallery { edges { node { uri viewerSocialGrainFavoriteViaSubject { uri subject } } } } }",
        ),
      ),
    ])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query)
    |> simulate.header("content-type", "application/json")
    |> simulate.header("authorization", "Bearer test-viewer-token")

  let assert Ok(cache) = did_cache.start()
  let response =
    graphql_handler.handle_graphql_request(request, exec, cache, None, "", "")

  let assert wisp.Text(body) = response.body

  response.status |> should.equal(200)

  // Should contain the gallery URI
  string.contains(body, gallery_uri) |> should.be_true

  // The viewer favorite field should contain the favorite URI
  string.contains(body, favorite_uri) |> should.be_true
}
