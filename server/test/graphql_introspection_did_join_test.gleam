/// Integration test for GraphQL introspection with DID joins
///
/// This test verifies that DID join fields are properly generated in the GraphQL schema
/// by running a full introspection query and checking for the expected join fields.
import database
import gleam/dynamic/decode
import gleam/http
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import gleeunit/should
import graphql_handler
import importer
import lexicon
import simplifile
import sqlight
import wisp
import wisp/simulate

// Helper to load all lexicons from the grain fixtures directory
fn load_grain_lexicons(db: sqlight.Connection) -> Result(Nil, String) {
  let lexicons_dir = "test/fixtures/grain/lexicons"

  // Scan directory for all JSON files
  use file_paths <- result.try(importer.scan_directory_recursive(lexicons_dir))

  // Read all files first to get their content
  let file_contents =
    file_paths
    |> list.filter_map(fn(file_path) {
      case simplifile.read(file_path) {
        Ok(content) -> Ok(#(file_path, content))
        Error(_) -> Error(Nil)
      }
    })

  // Extract all JSON strings for validation
  let all_json_strings = list.map(file_contents, fn(pair) { pair.1 })

  // Validate all schemas together (this allows cross-references to be resolved)
  use _ <- result.try(case lexicon.validate_schemas(all_json_strings) {
    Ok(_) -> Ok(Nil)
    Error(err) -> Error("Validation failed: " <> string.inspect(err))
  })

  // Import each lexicon (skipping individual validation since we already validated all together)
  use _ <- result.try(
    case
      list.try_map(file_contents, fn(pair) {
        let #(_file_path, json_content) = pair
        // Extract lexicon ID and insert into database
        case
          json.parse(json_content, {
            use id <- decode.field("id", decode.string)
            decode.success(id)
          })
        {
          Ok(lexicon_id) ->
            case database.insert_lexicon(db, lexicon_id, json_content) {
              Ok(_) -> Ok(Nil)
              Error(_) -> Error("Database insertion failed")
            }
          Error(_) -> Error("Missing 'id' field")
        }
      })
    {
      Ok(_) -> Ok(Nil)
      Error(err) -> Error("Import failed: " <> err)
    },
  )

  Ok(Nil)
}

pub fn introspection_query_includes_did_join_fields_test() {
  // NOTE: This test documents a known limitation with GraphQL introspection.
  // Types may appear in introspection without all their join fields if they're
  // encountered through a Connection in another type's join field before being
  // encountered through their Query field.
  //
  // This test is kept to document the issue and ensure we don't regress further.
  // The actual GraphQL queries work correctly (see introspection_query_did_join_field_structure_test).

  // Create in-memory database
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = database.create_lexicon_table(db)
  let assert Ok(_) = database.create_record_table(db)

  // Load all grain lexicons from fixtures
  let assert Ok(_) = load_grain_lexicons(db)

  // Use __type introspection to query the SocialGrainGallery type
  let introspection_query =
    "query IntrospectionQuery {
      __type(name: \"SocialGrainGallery\") {
        name
        kind
        fields {
          name
          type {
            name
            kind
            ofType {
              name
              kind
            }
          }
        }
      }
    }"

  let query =
    json.object([#("query", json.string(introspection_query))])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query)
    |> simulate.header("content-type", "application/json")

  let response =
    graphql_handler.handle_graphql_request(request, db, "http://localhost:3000")

  // Verify response
  response.status
  |> should.equal(200)

  // Get response body
  let assert wisp.Text(body) = response.body

  // Verify that introspection worked (no errors or empty errors array)
  let has_non_empty_errors =
    string.contains(body, "\"errors\":[")
    && !string.contains(body, "\"errors\":[]")
  has_non_empty_errors
  |> should.be_false

  // Verify the type was found
  string.contains(body, "SocialGrainGallery")
  |> should.be_true

  // NOTE: These assertions are enabled to verify DID join fields appear in introspection
  // after refactoring the type builder to use topological sort.

  string.contains(body, "socialGrainActorProfileByDid")
  |> should.be_true

  string.contains(body, "socialGrainPhotoByDid")
  |> should.be_true

  string.contains(body, "socialGrainCommentByDid")
  |> should.be_true

  string.contains(body, "socialGrainFavoriteByDid")
  |> should.be_true

  // Clean up
  let assert Ok(_) = sqlight.close(db)
}

pub fn introspection_query_profile_join_fields_test() {
  // This test verifies that the SocialGrainActorProfile type has the reverse DID join
  // field to galleries.

  // Create in-memory database
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = database.create_lexicon_table(db)
  let assert Ok(_) = database.create_record_table(db)

  // Load all grain lexicons from fixtures
  let assert Ok(_) = load_grain_lexicons(db)

  // Use __type introspection to query the SocialGrainActorProfile type
  let introspection_query =
    "query IntrospectionQuery {
      __type(name: \"SocialGrainActorProfile\") {
        name
        kind
        fields {
          name
          type {
            name
            kind
            ofType {
              name
              kind
            }
          }
        }
      }
    }"

  let query =
    json.object([#("query", json.string(introspection_query))])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query)
    |> simulate.header("content-type", "application/json")

  let response =
    graphql_handler.handle_graphql_request(request, db, "http://localhost:3000")

  // Verify response
  response.status
  |> should.equal(200)

  // Get response body
  let assert wisp.Text(body) = response.body

  // Verify that introspection worked (no errors or empty errors array)
  let has_non_empty_errors =
    string.contains(body, "\"errors\":[")
    && !string.contains(body, "\"errors\":[]")
  has_non_empty_errors
  |> should.be_false

  // Verify the type was found
  string.contains(body, "SocialGrainActorProfile")
  |> should.be_true

  // Verify that the DID join field to galleries exists
  string.contains(body, "socialGrainGalleryByDid")
  |> should.be_true

  // Clean up
  let assert Ok(_) = sqlight.close(db)
}

pub fn introspection_query_did_join_field_structure_test() {
  // This test verifies DID joins work end-to-end by querying the actual fields
  // and retrieving data. This complements the introspection tests by verifying
  // that the join fields not only exist in the schema but also execute correctly.

  // Create in-memory database
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = database.create_lexicon_table(db)
  let assert Ok(_) = database.create_record_table(db)

  // Load all grain lexicons from fixtures
  let assert Ok(_) = load_grain_lexicons(db)

  let test_did = "did:plc:test"

  // Insert a test profile record
  let profile_json =
    json.object([
      #("displayName", json.string("Test User")),
      #("createdAt", json.string("2024-01-01T00:00:00.000Z")),
    ])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      "at://" <> test_did <> "/social.grain.actor.profile/self",
      "cid1",
      test_did,
      "social.grain.actor.profile",
      profile_json,
    )

  // Insert a gallery record
  let gallery_json =
    json.object([
      #("title", json.string("My Gallery")),
      #("createdAt", json.string("2024-01-01T00:00:00.000Z")),
    ])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      "at://" <> test_did <> "/social.grain.gallery/123",
      "cid2",
      test_did,
      "social.grain.gallery",
      gallery_json,
    )

  // Insert a photo record
  let photo_json =
    json.object([
      #("alt", json.string("Test Photo")),
      #("createdAt", json.string("2024-01-01T00:00:00.000Z")),
    ])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      "at://" <> test_did <> "/social.grain.photo/456",
      "cid3",
      test_did,
      "social.grain.photo",
      photo_json,
    )

  // Insert a comment record
  let comment_json =
    json.object([
      #("text", json.string("Test Comment")),
      #("createdAt", json.string("2024-01-01T00:00:00.000Z")),
    ])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      "at://" <> test_did <> "/social.grain.comment/789",
      "cid4",
      test_did,
      "social.grain.comment",
      comment_json,
    )

  // Insert a favorite record
  let favorite_json =
    json.object([
      #(
        "subject",
        json.string("at://" <> test_did <> "/social.grain.photo/456"),
      ),
      #("createdAt", json.string("2024-01-01T00:00:00.000Z")),
    ])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      "at://" <> test_did <> "/social.grain.favorite/abc",
      "cid5",
      test_did,
      "social.grain.favorite",
      favorite_json,
    )

  // Query SocialGrainGallery and verify it has DID join fields to profile, photo, comment, and favorite
  let query_text =
    "{
      socialGrainGallery {
        edges {
          node {
            title
            socialGrainActorProfileByDid { displayName }
            socialGrainPhotoByDid { edges { node { alt } } }
            socialGrainCommentByDid { edges { node { text } } }
            socialGrainFavoriteByDid { edges { node { createdAt } } }
          }
        }
      }
    }"

  let query =
    json.object([#("query", json.string(query_text))])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query)
    |> simulate.header("content-type", "application/json")

  let response =
    graphql_handler.handle_graphql_request(request, db, "http://localhost:3000")

  // Verify response
  response.status
  |> should.equal(200)

  let assert wisp.Text(body) = response.body

  // Check for errors - if any DID join field doesn't exist, we'll get an error
  let has_field_error =
    string.contains(body, "Cannot query field")
    || string.contains(body, "not found")

  // If there are errors about fields not being found, the test should fail
  case has_field_error {
    True -> should.fail()
    False -> {
      // Verify response contains data
      string.contains(body, "data")
      |> should.be_true

      // Verify the gallery data is returned
      string.contains(body, "My Gallery")
      |> should.be_true

      // Verify the DID join fields are present and returned data
      string.contains(body, "Test User")
      |> should.be_true

      string.contains(body, "Test Photo")
      |> should.be_true

      string.contains(body, "Test Comment")
      |> should.be_true
    }
  }

  // Clean up
  let assert Ok(_) = sqlight.close(db)
}

pub fn did_join_field_query_execution_test() {
  // Create in-memory database
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = database.create_lexicon_table(db)
  let assert Ok(_) = database.create_record_table(db)

  // Load all grain lexicons from fixtures
  let assert Ok(_) = load_grain_lexicons(db)

  // Insert a profile record
  let profile_json =
    json.object([
      #("displayName", json.string("Test User")),
      #("createdAt", json.string("2024-01-01T00:00:00.000Z")),
    ])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      "at://did:plc:test/social.grain.actor.profile/self",
      "cid1",
      "did:plc:test",
      "social.grain.actor.profile",
      profile_json,
    )

  // Insert gallery records for the same DID
  let gallery1_json =
    json.object([
      #("title", json.string("Gallery 1")),
      #("createdAt", json.string("2024-01-01T00:00:00.000Z")),
    ])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      "at://did:plc:test/social.grain.gallery/123",
      "cid2",
      "did:plc:test",
      "social.grain.gallery",
      gallery1_json,
    )

  let gallery2_json =
    json.object([
      #("title", json.string("Gallery 2")),
      #("createdAt", json.string("2024-01-01T00:00:00.000Z")),
    ])
    |> json.to_string

  let assert Ok(_) =
    database.insert_record(
      db,
      "at://did:plc:test/social.grain.gallery/456",
      "cid3",
      "did:plc:test",
      "social.grain.gallery",
      gallery2_json,
    )

  // Query profile with DID join field
  let query_text =
    "{ socialGrainActorProfile { edges { node { displayName socialGrainGalleryByDid { edges { node { title } } } } } } }"

  let query =
    json.object([#("query", json.string(query_text))])
    |> json.to_string

  let request =
    simulate.request(http.Post, "/graphql")
    |> simulate.string_body(query)
    |> simulate.header("content-type", "application/json")

  let response =
    graphql_handler.handle_graphql_request(request, db, "http://localhost:3000")

  // Verify response
  response.status
  |> should.equal(200)

  let assert wisp.Text(body) = response.body

  // Should contain the profile data
  string.contains(body, "Test User")
  |> should.be_true

  // Should contain the joined gallery data
  string.contains(body, "Gallery 1")
  |> should.be_true

  string.contains(body, "Gallery 2")
  |> should.be_true

  // Clean up
  let assert Ok(_) = sqlight.close(db)
}
