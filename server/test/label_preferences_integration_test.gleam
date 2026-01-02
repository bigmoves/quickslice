/// Integration tests for viewer label preferences
///
/// Tests the viewerLabelPreferences query and setLabelPreference mutation
/// via the GraphQL API endpoint
import database/repositories/lexicons
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

/// Create a minimal lexicon for schema building
fn create_minimal_lexicon() -> String {
  json.object([
    #("lexicon", json.int(1)),
    #("id", json.string("test.minimal.record")),
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
                  json.array([json.string("name")], of: fn(x) { x }),
                ),
                #(
                  "properties",
                  json.object([
                    #("name", json.object([#("type", json.string("string"))])),
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

/// Test: viewerLabelPreferences returns non-system labels with default visibility
pub fn viewer_label_preferences_returns_defaults_test() {
  // Setup database with moderation tables
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_core_tables(exec)
  let assert Ok(_) = test_helpers.create_oauth_tables(exec)
  let assert Ok(_) = test_helpers.create_moderation_tables(exec)
  let assert Ok(_) =
    test_helpers.insert_test_token(exec, "test-viewer-token", "did:plc:viewer")
  // Insert a minimal lexicon so schema can build
  let assert Ok(_) =
    lexicons.insert(exec, "test.minimal.record", create_minimal_lexicon())

  // Query for viewer label preferences
  let query =
    json.object([
      #(
        "query",
        json.string(
          "{ viewerLabelPreferences { val description defaultVisibility visibility } }",
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

  // Query should succeed
  response.status |> should.equal(200)

  // Should contain non-system labels (porn, spam, sexual, nudity)
  string.contains(body, "porn") |> should.be_true
  string.contains(body, "spam") |> should.be_true
  string.contains(body, "sexual") |> should.be_true
  string.contains(body, "nudity") |> should.be_true

  // Should NOT contain system labels (starting with !)
  string.contains(body, "!takedown") |> should.be_false
  string.contains(body, "!suspend") |> should.be_false
  string.contains(body, "!warn") |> should.be_false
  string.contains(body, "!hide") |> should.be_false

  // Should have visibility field with default values
  string.contains(body, "visibility") |> should.be_true
}

/// Test: viewerLabelPreferences requires authentication
pub fn viewer_label_preferences_requires_auth_test() {
  // Setup database
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_core_tables(exec)
  let assert Ok(_) = test_helpers.create_oauth_tables(exec)
  let assert Ok(_) = test_helpers.create_moderation_tables(exec)
  // Insert a minimal lexicon so schema can build
  let assert Ok(_) =
    lexicons.insert(exec, "test.minimal.record", create_minimal_lexicon())

  // Query WITHOUT auth token
  let query =
    json.object([
      #("query", json.string("{ viewerLabelPreferences { val visibility } }")),
    ])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query)
    |> simulate.header("content-type", "application/json")

  let assert Ok(cache) = did_cache.start()
  let response =
    graphql_handler.handle_graphql_request(request, exec, cache, None, "", "")

  let assert wisp.Text(body) = response.body

  // Should return error about authentication
  string.contains(body, "error") |> should.be_true
  string.contains(body, "Authentication") |> should.be_true
}

/// Test: setLabelPreference updates visibility for non-system label
pub fn set_label_preference_updates_visibility_test() {
  // Setup database
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_core_tables(exec)
  let assert Ok(_) = test_helpers.create_oauth_tables(exec)
  let assert Ok(_) = test_helpers.create_moderation_tables(exec)
  let assert Ok(_) =
    test_helpers.insert_test_token(exec, "test-viewer-token", "did:plc:viewer")
  // Insert a minimal lexicon so schema can build
  let assert Ok(_) =
    lexicons.insert(exec, "test.minimal.record", create_minimal_lexicon())

  // Set preference for 'spam' label to 'hide'
  let mutation =
    json.object([
      #(
        "query",
        json.string(
          "mutation { setLabelPreference(val: \"spam\", visibility: HIDE) { val visibility } }",
        ),
      ),
    ])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(mutation)
    |> simulate.header("content-type", "application/json")
    |> simulate.header("authorization", "Bearer test-viewer-token")

  let assert Ok(cache) = did_cache.start()
  let response =
    graphql_handler.handle_graphql_request(request, exec, cache, None, "", "")

  let assert wisp.Text(body) = response.body

  // Mutation should succeed
  response.status |> should.equal(200)

  // Should return the updated preference
  string.contains(body, "spam") |> should.be_true
  string.contains(body, "HIDE") |> should.be_true

  // Verify the preference persisted by querying again
  let query =
    json.object([
      #("query", json.string("{ viewerLabelPreferences { val visibility } }")),
    ])
    |> json.to_string

  let verify_request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query)
    |> simulate.header("content-type", "application/json")
    |> simulate.header("authorization", "Bearer test-viewer-token")

  let verify_response =
    graphql_handler.handle_graphql_request(
      verify_request,
      exec,
      cache,
      None,
      "",
      "",
    )

  let assert wisp.Text(verify_body) = verify_response.body

  // Should show the updated visibility for spam
  string.contains(verify_body, "HIDE") |> should.be_true
}

/// Test: setLabelPreference rejects system labels (starting with !)
pub fn set_label_preference_rejects_system_labels_test() {
  // Setup database
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_core_tables(exec)
  let assert Ok(_) = test_helpers.create_oauth_tables(exec)
  let assert Ok(_) = test_helpers.create_moderation_tables(exec)
  let assert Ok(_) =
    test_helpers.insert_test_token(exec, "test-viewer-token", "did:plc:viewer")
  // Insert a minimal lexicon so schema can build
  let assert Ok(_) =
    lexicons.insert(exec, "test.minimal.record", create_minimal_lexicon())

  // Try to set preference for system label '!takedown'
  let mutation =
    json.object([
      #(
        "query",
        json.string(
          "mutation { setLabelPreference(val: \"!takedown\", visibility: IGNORE) { val visibility } }",
        ),
      ),
    ])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(mutation)
    |> simulate.header("content-type", "application/json")
    |> simulate.header("authorization", "Bearer test-viewer-token")

  let assert Ok(cache) = did_cache.start()
  let response =
    graphql_handler.handle_graphql_request(request, exec, cache, None, "", "")

  let assert wisp.Text(body) = response.body

  // Should return error about system labels
  string.contains(body, "error") |> should.be_true
  string.contains(body, "system") |> should.be_true
}

/// Test: setLabelPreference requires authentication
pub fn set_label_preference_requires_auth_test() {
  // Setup database
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_core_tables(exec)
  let assert Ok(_) = test_helpers.create_oauth_tables(exec)
  let assert Ok(_) = test_helpers.create_moderation_tables(exec)
  // Insert a minimal lexicon so schema can build
  let assert Ok(_) =
    lexicons.insert(exec, "test.minimal.record", create_minimal_lexicon())

  // Try to set preference WITHOUT auth token
  let mutation =
    json.object([
      #(
        "query",
        json.string(
          "mutation { setLabelPreference(val: \"spam\", visibility: HIDE) { val visibility } }",
        ),
      ),
    ])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(mutation)
    |> simulate.header("content-type", "application/json")

  let assert Ok(cache) = did_cache.start()
  let response =
    graphql_handler.handle_graphql_request(request, exec, cache, None, "", "")

  let assert wisp.Text(body) = response.body

  // Should return error about authentication
  string.contains(body, "error") |> should.be_true
  string.contains(body, "Authentication") |> should.be_true
}

/// Test: setLabelPreference validates visibility value
pub fn set_label_preference_validates_visibility_test() {
  // Setup database
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_core_tables(exec)
  let assert Ok(_) = test_helpers.create_oauth_tables(exec)
  let assert Ok(_) = test_helpers.create_moderation_tables(exec)
  let assert Ok(_) =
    test_helpers.insert_test_token(exec, "test-viewer-token", "did:plc:viewer")
  // Insert a minimal lexicon so schema can build
  let assert Ok(_) =
    lexicons.insert(exec, "test.minimal.record", create_minimal_lexicon())

  // Try to set invalid visibility (GraphQL enum validation should catch this)
  let mutation =
    json.object([
      #(
        "query",
        json.string(
          "mutation { setLabelPreference(val: \"spam\", visibility: INVALID) { val visibility } }",
        ),
      ),
    ])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(mutation)
    |> simulate.header("content-type", "application/json")
    |> simulate.header("authorization", "Bearer test-viewer-token")

  let assert Ok(cache) = did_cache.start()
  let response =
    graphql_handler.handle_graphql_request(request, exec, cache, None, "", "")

  let assert wisp.Text(body) = response.body

  // Should return error about invalid enum value
  string.contains(body, "error") |> should.be_true
}

/// Test: setLabelPreference rejects non-existent label
pub fn set_label_preference_rejects_unknown_label_test() {
  // Setup database
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_core_tables(exec)
  let assert Ok(_) = test_helpers.create_oauth_tables(exec)
  let assert Ok(_) = test_helpers.create_moderation_tables(exec)
  let assert Ok(_) =
    test_helpers.insert_test_token(exec, "test-viewer-token", "did:plc:viewer")
  // Insert a minimal lexicon so schema can build
  let assert Ok(_) =
    lexicons.insert(exec, "test.minimal.record", create_minimal_lexicon())

  // Try to set preference for non-existent label
  let mutation =
    json.object([
      #(
        "query",
        json.string(
          "mutation { setLabelPreference(val: \"nonexistent\", visibility: HIDE) { val visibility } }",
        ),
      ),
    ])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(mutation)
    |> simulate.header("content-type", "application/json")
    |> simulate.header("authorization", "Bearer test-viewer-token")

  let assert Ok(cache) = did_cache.start()
  let response =
    graphql_handler.handle_graphql_request(request, exec, cache, None, "", "")

  let assert wisp.Text(body) = response.body

  // Should return error about label not found
  string.contains(body, "error") |> should.be_true
}
