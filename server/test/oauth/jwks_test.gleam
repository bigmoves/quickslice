import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import handlers/oauth/jwks
import wisp

/// Test JWKS endpoint with no signing key returns empty JWKS
pub fn handle_no_signing_key_test() {
  let response = jwks.handle(None)

  // Should return 200 with empty JWKS
  response.status |> should.equal(200)

  // Should have correct content type
  let content_type = get_header(response, "content-type")
  content_type |> should.equal(Some("application/json"))

  // Body should contain empty keys array
  let body = get_body_text(response)
  should.be_true(string_contains(body, "\"keys\":[]"))
}

/// Test JWKS endpoint with valid signing key returns proper JWKS
pub fn handle_with_signing_key_test() {
  // Use the test key from environment (same format as OAUTH_SIGNING_KEY)
  let test_key = "z42tn4DpCKfYBwSYEbaoR5z5Amir9HsY5WvzB3wSvFQcXvSH"
  let response = jwks.handle(Some(test_key))

  // Should return 200
  response.status |> should.equal(200)

  // Should have correct content type
  let content_type = get_header(response, "content-type")
  content_type |> should.equal(Some("application/json"))

  // Body should contain required JWK fields
  let body = get_body_text(response)

  // Should contain "keys" array
  should.be_true(string_contains(body, "\"keys\""))

  // Should contain EC key type
  should.be_true(string_contains(body, "\"kty\":\"EC\""))

  // Should contain P-256 curve
  should.be_true(string_contains(body, "\"crv\":\"P-256\""))

  // Should contain x and y coordinates
  should.be_true(string_contains(body, "\"x\":"))
  should.be_true(string_contains(body, "\"y\":"))

  // Should contain ES256 algorithm
  should.be_true(string_contains(body, "\"alg\":\"ES256\""))

  // Should contain sig use
  should.be_true(string_contains(body, "\"use\":\"sig\""))

  // Should contain kid (key ID) as did:key
  should.be_true(string_contains(body, "\"kid\":\"did:key:"))
}

/// Test JWKS endpoint with invalid signing key returns empty JWKS
pub fn handle_with_invalid_signing_key_test() {
  // Invalid key format
  let invalid_key = "invalid-key-format"
  let response = jwks.handle(Some(invalid_key))

  // Should return 200 (graceful degradation)
  response.status |> should.equal(200)

  // Body should contain empty keys array
  let body = get_body_text(response)
  should.be_true(string_contains(body, "\"keys\":[]"))
}

// Helper functions

fn get_header(response: wisp.Response, name: String) -> option.Option(String) {
  case
    response.headers
    |> find_header(name)
  {
    Ok(value) -> Some(value)
    Error(_) -> None
  }
}

fn find_header(
  headers: List(#(String, String)),
  name: String,
) -> Result(String, Nil) {
  case headers {
    [] -> Error(Nil)
    [#(key, value), ..rest] ->
      case string.lowercase(key) == string.lowercase(name) {
        True -> Ok(value)
        False -> find_header(rest, name)
      }
  }
}

fn get_body_text(response: wisp.Response) -> String {
  case response.body {
    wisp.Text(text) -> text
    _ -> ""
  }
}

fn string_contains(haystack: String, needle: String) -> Bool {
  erlang_string_find(haystack, needle) != "nomatch"
}

@external(erlang, "string", "find")
fn erlang_string_find(haystack: String, needle: String) -> String
