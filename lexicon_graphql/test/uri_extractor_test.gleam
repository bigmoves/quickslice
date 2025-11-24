/// Tests for URI Extraction
///
/// Tests that we correctly extract URIs from strongRef objects and at-uri strings
import gleam/dict
import gleam/option.{None, Some}
import gleeunit/should
import lexicon_graphql/internal/lexicon/uri_extractor
import test_helpers

// Test extracting URI from a strongRef object
pub fn extract_uri_from_strong_ref_test() {
  // Create a strongRef object as a dynamic value
  let strong_ref =
    dict.from_list([
      #("$type", test_helpers.to_dynamic("com.atproto.repo.strongRef")),
      #(
        "uri",
        test_helpers.to_dynamic(
          "at://did:plc:abc123/app.bsky.feed.post/3k7h8xyz",
        ),
      ),
      #("cid", test_helpers.to_dynamic("bafyreiabc123...")),
    ])
    |> test_helpers.to_dynamic

  case uri_extractor.extract_uri(strong_ref) {
    Some(uri) ->
      uri
      |> should.equal("at://did:plc:abc123/app.bsky.feed.post/3k7h8xyz")
    None -> panic as "Expected to extract URI from strongRef"
  }
}

// Test extracting URI from a plain at-uri string
pub fn extract_uri_from_at_uri_string_test() {
  let at_uri =
    test_helpers.to_dynamic("at://did:plc:xyz789/app.bsky.actor.profile/self")

  case uri_extractor.extract_uri(at_uri) {
    Some(uri) ->
      uri
      |> should.equal("at://did:plc:xyz789/app.bsky.actor.profile/self")
    None -> panic as "Expected to extract URI from at-uri string"
  }
}

// Test that non-URI strings return None
pub fn extract_uri_from_non_uri_string_test() {
  let non_uri = test_helpers.to_dynamic("not-an-at-uri")

  uri_extractor.extract_uri(non_uri)
  |> should.equal(None)
}

// Test that invalid objects return None
pub fn extract_uri_from_invalid_object_test() {
  let invalid_obj =
    dict.from_list([
      #("foo", test_helpers.to_dynamic("bar")),
      #("baz", test_helpers.to_dynamic("qux")),
    ])
    |> test_helpers.to_dynamic

  uri_extractor.extract_uri(invalid_obj)
  |> should.equal(None)
}

// Test that null/None returns None
pub fn extract_uri_from_null_test() {
  let null_val = test_helpers.to_dynamic(None)

  uri_extractor.extract_uri(null_val)
  |> should.equal(None)
}

// Test strongRef without $type field (should still work)
pub fn extract_uri_from_strong_ref_without_type_test() {
  let strong_ref_no_type =
    dict.from_list([
      #("uri", test_helpers.to_dynamic("at://did:plc:test/collection/rkey")),
      #("cid", test_helpers.to_dynamic("bafyrei...")),
    ])
    |> test_helpers.to_dynamic

  case uri_extractor.extract_uri(strong_ref_no_type) {
    Some(uri) -> uri |> should.equal("at://did:plc:test/collection/rkey")
    None -> panic as "Expected to extract URI even without $type"
  }
}

// Test is_strong_ref helper
pub fn is_strong_ref_test() {
  let strong_ref =
    dict.from_list([
      #("$type", test_helpers.to_dynamic("com.atproto.repo.strongRef")),
      #("uri", test_helpers.to_dynamic("at://did:plc:abc/collection/key")),
      #("cid", test_helpers.to_dynamic("bafyrei...")),
    ])
    |> test_helpers.to_dynamic

  uri_extractor.is_strong_ref(strong_ref)
  |> should.be_true
}

// Test is_strong_ref with non-strongRef
pub fn is_strong_ref_negative_test() {
  let not_strong_ref =
    dict.from_list([#("$type", test_helpers.to_dynamic("some.other.type"))])
    |> test_helpers.to_dynamic

  uri_extractor.is_strong_ref(not_strong_ref)
  |> should.be_false
}

// Test is_at_uri_string helper
pub fn is_at_uri_string_test() {
  let at_uri =
    test_helpers.to_dynamic("at://did:plc:test/app.bsky.feed.post/123")

  uri_extractor.is_at_uri_string(at_uri)
  |> should.be_true
}

// Test is_at_uri_string with non-at-uri
pub fn is_at_uri_string_negative_test() {
  let not_at_uri = test_helpers.to_dynamic("https://example.com")

  uri_extractor.is_at_uri_string(not_at_uri)
  |> should.be_false
}

// Test is_at_uri_string with object
pub fn is_at_uri_string_with_object_test() {
  let obj =
    dict.from_list([#("uri", test_helpers.to_dynamic("at://..."))])
    |> test_helpers.to_dynamic

  uri_extractor.is_at_uri_string(obj)
  |> should.be_false
}

// Test extracting from strongRef with wrong $type
pub fn extract_uri_from_wrong_type_test() {
  let wrong_type =
    dict.from_list([
      #("$type", test_helpers.to_dynamic("wrong.type")),
      #("uri", test_helpers.to_dynamic("at://did:plc:test/collection/key")),
    ])
    |> test_helpers.to_dynamic

  // Should still extract the URI even if $type is wrong, as long as uri field exists
  case uri_extractor.extract_uri(wrong_type) {
    Some(uri) -> uri |> should.equal("at://did:plc:test/collection/key")
    None -> panic as "Expected to extract URI with wrong $type"
  }
}

// Test extracting from number (should return None)
pub fn extract_uri_from_number_test() {
  let number = test_helpers.to_dynamic(42)

  uri_extractor.extract_uri(number)
  |> should.equal(None)
}

// Test extracting from boolean (should return None)
pub fn extract_uri_from_boolean_test() {
  let boolean = test_helpers.to_dynamic(True)

  uri_extractor.extract_uri(boolean)
  |> should.equal(None)
}

// Test extracting from empty string (should return None)
pub fn extract_uri_from_empty_string_test() {
  let empty = test_helpers.to_dynamic("")

  uri_extractor.extract_uri(empty)
  |> should.equal(None)
}

// Test extracting URI with various valid formats
pub fn extract_uri_various_formats_test() {
  // With tid
  let uri1 =
    test_helpers.to_dynamic("at://did:plc:abc/app.bsky.feed.post/3k7h8...")
  case uri_extractor.extract_uri(uri1) {
    Some(_) -> Nil
    None -> panic as "Expected valid URI with tid"
  }

  // With literal:self
  let uri2 =
    test_helpers.to_dynamic("at://did:plc:abc/app.bsky.actor.profile/self")
  case uri_extractor.extract_uri(uri2) {
    Some(_) -> Nil
    None -> panic as "Expected valid URI with literal:self"
  }

  // With custom rkey
  let uri3 =
    test_helpers.to_dynamic("at://did:plc:abc/collection/custom-key-123")
  case uri_extractor.extract_uri(uri3) {
    Some(_) -> Nil
    None -> panic as "Expected valid URI with custom key"
  }
}
