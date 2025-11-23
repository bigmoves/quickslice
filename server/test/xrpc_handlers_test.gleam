import database/repositories/lexicons
import database/repositories/records
import database/schema/tables
import gleam/http
import gleeunit/should
import sqlight
import wisp/simulate
import xrpc_handlers

// Test configuration
const test_auth_base_url = "https://auth.test.example.com"

// Helper function to create a test database
fn setup_test_db() -> sqlight.Connection {
  let assert Ok(db) = sqlight.open(":memory:")

  // Create tables
  let assert Ok(_) = tables.create_record_table(db)
  let assert Ok(_) = tables.create_lexicon_table(db)

  db
}

// Helper to insert a test lexicon
fn insert_test_lexicon(db: sqlight.Connection, nsid: String) -> Nil {
  let lexicon_json = "{
  \"lexicon\": 1,
  \"id\": \"" <> nsid <> "\",
  \"defs\": {
    \"main\": {
      \"type\": \"record\",
      \"key\": \"tid\",
      \"record\": {
        \"type\": \"object\",
        \"required\": [\"text\", \"createdAt\"],
        \"properties\": {
          \"text\": {
            \"type\": \"string\",
            \"minLength\": 1,
            \"maxLength\": 300
          },
          \"createdAt\": {
            \"type\": \"string\",
            \"format\": \"datetime\"
          }
        }
      }
    }
  }
}"

  let assert Ok(_) = lexicons.insert(db, nsid, lexicon_json)
  Nil
}

// Helper to insert a test record
fn insert_test_record(
  db: sqlight.Connection,
  uri: String,
  collection: String,
) -> Nil {
  let record_json =
    "{\"text\": \"Hello world\", \"createdAt\": \"2025-01-01T00:00:00Z\"}"

  let assert Ok(_) =
    records.insert(
      db,
      uri,
      "bafytest123",
      "did:plc:test123",
      collection,
      record_json,
    )
  Nil
}

// Tests for createRecord

pub fn create_record_with_valid_data_test() {
  let db = setup_test_db()
  let nsid = "xyz.statusphere.status"
  insert_test_lexicon(db, nsid)

  let body =
    "{\"text\": \"Test status\", \"createdAt\": \"2025-01-01T12:00:00Z\"}"

  let request =
    simulate.request(http.Post, "/xrpc/" <> nsid <> ".createRecord")
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/json")

  let response =
    xrpc_handlers.handle_create_record(request, db, nsid, test_auth_base_url)

  // Now requires authentication, should return 401 without auth header
  response.status
  |> should.equal(401)
}

pub fn create_record_with_invalid_json_test() {
  let db = setup_test_db()
  let nsid = "xyz.statusphere.status"
  insert_test_lexicon(db, nsid)

  let body = "{invalid json"

  let request =
    simulate.request(http.Post, "/xrpc/" <> nsid <> ".createRecord")
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/json")

  let response =
    xrpc_handlers.handle_create_record(request, db, nsid, test_auth_base_url)

  // Auth required first, so 401 instead of 400
  response.status
  |> should.equal(401)
}

pub fn create_record_with_missing_required_field_test() {
  let db = setup_test_db()
  let nsid = "xyz.statusphere.status"
  insert_test_lexicon(db, nsid)

  // Missing "createdAt" field
  let body = "{\"text\": \"Test status\"}"

  let request =
    simulate.request(http.Post, "/xrpc/" <> nsid <> ".createRecord")
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/json")

  let response =
    xrpc_handlers.handle_create_record(request, db, nsid, test_auth_base_url)

  // Auth required first, so 401 instead of 400
  response.status
  |> should.equal(401)
}

pub fn create_record_with_wrong_http_method_test() {
  let db = setup_test_db()
  let nsid = "xyz.statusphere.status"
  insert_test_lexicon(db, nsid)

  let request = simulate.request(http.Get, "/xrpc/" <> nsid <> ".createRecord")

  let response =
    xrpc_handlers.handle_create_record(request, db, nsid, test_auth_base_url)

  // Method check happens before auth, still 405
  response.status
  |> should.equal(405)
}

pub fn create_record_without_lexicon_test() {
  let db = setup_test_db()
  let nsid = "xyz.nonexistent.collection"
  // Don't insert lexicon

  let body = "{\"text\": \"Test\", \"createdAt\": \"2025-01-01T12:00:00Z\"}"

  let request =
    simulate.request(http.Post, "/xrpc/" <> nsid <> ".createRecord")
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/json")

  let response =
    xrpc_handlers.handle_create_record(request, db, nsid, test_auth_base_url)

  // Auth required first, so 401 instead of 404
  response.status
  |> should.equal(401)
}

// Tests for getRecord

pub fn get_record_success_test() {
  let db = setup_test_db()
  let nsid = "xyz.statusphere.status"
  let uri = "at://did:plc:test123/" <> nsid <> "/abc123"

  insert_test_lexicon(db, nsid)
  insert_test_record(db, uri, nsid)

  let request =
    simulate.request(http.Get, "/xrpc/" <> nsid <> ".getRecord?uri=" <> uri)

  let response = xrpc_handlers.handle_get_record(request, db, nsid)

  response.status
  |> should.equal(200)
}

pub fn get_record_not_found_test() {
  let db = setup_test_db()
  let nsid = "xyz.statusphere.status"
  let uri = "at://did:plc:test123/" <> nsid <> "/nonexistent"

  insert_test_lexicon(db, nsid)

  let request =
    simulate.request(http.Get, "/xrpc/" <> nsid <> ".getRecord?uri=" <> uri)

  let response = xrpc_handlers.handle_get_record(request, db, nsid)

  response.status
  |> should.equal(404)
}

pub fn get_record_missing_uri_param_test() {
  let db = setup_test_db()
  let nsid = "xyz.statusphere.status"

  insert_test_lexicon(db, nsid)

  let request = simulate.request(http.Get, "/xrpc/" <> nsid <> ".getRecord")

  let response = xrpc_handlers.handle_get_record(request, db, nsid)

  response.status
  |> should.equal(400)
}

pub fn get_record_wrong_http_method_test() {
  let db = setup_test_db()
  let nsid = "xyz.statusphere.status"
  let uri = "at://did:plc:test123/" <> nsid <> "/abc123"

  insert_test_lexicon(db, nsid)

  let request =
    simulate.request(http.Post, "/xrpc/" <> nsid <> ".getRecord?uri=" <> uri)

  let response = xrpc_handlers.handle_get_record(request, db, nsid)

  response.status
  |> should.equal(405)
}

// Tests for updateRecord

pub fn update_record_with_valid_data_test() {
  let db = setup_test_db()
  let nsid = "xyz.statusphere.status"
  let uri = "at://did:plc:test123/" <> nsid <> "/abc123"

  insert_test_lexicon(db, nsid)
  insert_test_record(db, uri, nsid)

  let body =
    "{\"text\": \"Updated status\", \"createdAt\": \"2025-01-01T12:00:00Z\"}"

  let request =
    simulate.request(http.Post, "/xrpc/" <> nsid <> ".updateRecord?uri=" <> uri)
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/json")

  let response = xrpc_handlers.handle_update_record(request, db, nsid)

  response.status
  |> should.equal(200)
}

pub fn update_record_with_invalid_data_test() {
  let db = setup_test_db()
  let nsid = "xyz.statusphere.status"
  let uri = "at://did:plc:test123/" <> nsid <> "/abc123"

  insert_test_lexicon(db, nsid)
  insert_test_record(db, uri, nsid)

  // Missing required field
  let body = "{\"text\": \"Updated status\"}"

  let request =
    simulate.request(http.Post, "/xrpc/" <> nsid <> ".updateRecord?uri=" <> uri)
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/json")

  let response = xrpc_handlers.handle_update_record(request, db, nsid)

  response.status
  |> should.equal(400)
}

pub fn update_record_missing_uri_param_test() {
  let db = setup_test_db()
  let nsid = "xyz.statusphere.status"

  insert_test_lexicon(db, nsid)

  let body = "{\"text\": \"Updated\", \"createdAt\": \"2025-01-01T12:00:00Z\"}"

  let request =
    simulate.request(http.Post, "/xrpc/" <> nsid <> ".updateRecord")
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/json")

  let response = xrpc_handlers.handle_update_record(request, db, nsid)

  response.status
  |> should.equal(400)
}

pub fn update_record_wrong_http_method_test() {
  let db = setup_test_db()
  let nsid = "xyz.statusphere.status"
  let uri = "at://did:plc:test123/" <> nsid <> "/abc123"

  insert_test_lexicon(db, nsid)

  let request =
    simulate.request(http.Get, "/xrpc/" <> nsid <> ".updateRecord?uri=" <> uri)

  let response = xrpc_handlers.handle_update_record(request, db, nsid)

  response.status
  |> should.equal(405)
}

// Tests for deleteRecord

pub fn delete_record_success_test() {
  let db = setup_test_db()
  let nsid = "xyz.statusphere.status"
  let uri = "at://did:plc:test123/" <> nsid <> "/abc123"

  insert_test_lexicon(db, nsid)
  insert_test_record(db, uri, nsid)

  let request =
    simulate.request(http.Post, "/xrpc/" <> nsid <> ".deleteRecord?uri=" <> uri)

  let response = xrpc_handlers.handle_delete_record(request, db, nsid)

  response.status
  |> should.equal(200)
}

pub fn delete_record_missing_uri_param_test() {
  let db = setup_test_db()
  let nsid = "xyz.statusphere.status"

  insert_test_lexicon(db, nsid)

  let request = simulate.request(http.Post, "/xrpc/" <> nsid <> ".deleteRecord")

  let response = xrpc_handlers.handle_delete_record(request, db, nsid)

  response.status
  |> should.equal(400)
}

pub fn delete_record_wrong_http_method_test() {
  let db = setup_test_db()
  let nsid = "xyz.statusphere.status"
  let uri = "at://did:plc:test123/" <> nsid <> "/abc123"

  insert_test_lexicon(db, nsid)

  // Use GET instead of POST to test wrong method
  let request =
    simulate.request(http.Get, "/xrpc/" <> nsid <> ".deleteRecord?uri=" <> uri)

  let response = xrpc_handlers.handle_delete_record(request, db, nsid)

  response.status
  |> should.equal(405)
}
