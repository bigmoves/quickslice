/// Regression tests for reverse join field resolution bugs
///
/// Tests verify fixes for:
/// 1. Forward join fields (like itemResolved) available through reverse joins
/// 2. Integer and object fields resolved correctly (not always converted to strings)
/// 3. Nested queries work correctly: profile → galleries → items → photos
import database/repositories/lexicons
import database/repositories/records
import database/schema/tables
import gleam/bool
import gleam/json
import gleam/string
import gleeunit/should
import graphql_gleam
import sqlight

// Helper to create gallery lexicon
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
                  json.array(
                    [json.string("title"), json.string("createdAt")],
                    of: fn(x) { x },
                  ),
                ),
                #(
                  "properties",
                  json.object([
                    #(
                      "title",
                      json.object([
                        #("type", json.string("string")),
                        #("maxLength", json.int(100)),
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

// Helper to create gallery item lexicon with position field (integer)
fn create_gallery_item_lexicon() -> String {
  json.object([
    #("lexicon", json.int(1)),
    #("id", json.string("social.grain.gallery.item")),
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
                  json.array(
                    [
                      json.string("createdAt"),
                      json.string("gallery"),
                      json.string("item"),
                    ],
                    of: fn(x) { x },
                  ),
                ),
                #(
                  "properties",
                  json.object([
                    #(
                      "createdAt",
                      json.object([
                        #("type", json.string("string")),
                        #("format", json.string("datetime")),
                      ]),
                    ),
                    #(
                      "gallery",
                      json.object([
                        #("type", json.string("string")),
                        #("format", json.string("at-uri")),
                      ]),
                    ),
                    #(
                      "item",
                      json.object([
                        #("type", json.string("string")),
                        #("format", json.string("at-uri")),
                      ]),
                    ),
                    #(
                      "position",
                      json.object([
                        #("type", json.string("integer")),
                        #("default", json.int(0)),
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

// Helper to create photo lexicon
fn create_photo_lexicon() -> String {
  json.object([
    #("lexicon", json.int(1)),
    #("id", json.string("social.grain.photo")),
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
                  json.array([json.string("createdAt")], of: fn(x) { x }),
                ),
                #(
                  "properties",
                  json.object([
                    #("alt", json.object([#("type", json.string("string"))])),
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

// Helper to create profile lexicon
fn create_profile_lexicon() -> String {
  json.object([
    #("lexicon", json.int(1)),
    #("id", json.string("social.grain.actor.profile")),
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
                  "required",
                  json.array(
                    [json.string("displayName"), json.string("createdAt")],
                    of: fn(x) { x },
                  ),
                ),
                #(
                  "properties",
                  json.object([
                    #(
                      "displayName",
                      json.object([
                        #("type", json.string("string")),
                        #("maxGraphemes", json.int(64)),
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

/// Test that forward join fields (itemResolved) are available through reverse joins
/// This tests the fix for the circular dependency issue in schema building
pub fn reverse_join_includes_forward_join_fields_test() {
  let assert Ok(conn) = sqlight.open(":memory:")

  let assert Ok(_) = tables.create_lexicon_table(conn)
  let assert Ok(_) = tables.create_record_table(conn)
  let assert Ok(_) = tables.create_actor_table(conn)

  // Insert lexicons
  let assert Ok(_) =
    lexicons.insert(conn, "social.grain.gallery", create_gallery_lexicon())
  let assert Ok(_) =
    lexicons.insert(
      conn,
      "social.grain.gallery.item",
      create_gallery_item_lexicon(),
    )
  let assert Ok(_) =
    lexicons.insert(conn, "social.grain.photo", create_photo_lexicon())

  // Create test data
  let did1 = "did:test:user1"
  let gallery_uri = "at://" <> did1 <> "/social.grain.gallery/gallery1"
  let item_uri = "at://" <> did1 <> "/social.grain.gallery.item/item1"
  let photo_uri = "at://" <> did1 <> "/social.grain.photo/photo1"

  // Insert gallery
  let gallery_json =
    json.object([
      #("title", json.string("Test Gallery")),
      #("createdAt", json.string("2024-01-01T00:00:00.000Z")),
    ])
    |> json.to_string
  let assert Ok(_) =
    records.insert(
      conn,
      gallery_uri,
      "cid1",
      did1,
      "social.grain.gallery",
      gallery_json,
    )

  // Insert photo
  let photo_json =
    json.object([
      #("alt", json.string("A beautiful sunset")),
      #("createdAt", json.string("2024-01-01T00:00:00.000Z")),
    ])
    |> json.to_string
  let assert Ok(_) =
    records.insert(
      conn,
      photo_uri,
      "cid2",
      did1,
      "social.grain.photo",
      photo_json,
    )

  // Insert gallery item linking gallery to photo
  let item_json =
    json.object([
      #("gallery", json.string(gallery_uri)),
      #("item", json.string(photo_uri)),
      #("position", json.int(0)),
      #("createdAt", json.string("2024-01-01T00:00:00.000Z")),
    ])
    |> json.to_string
  let assert Ok(_) =
    records.insert(
      conn,
      item_uri,
      "cid3",
      did1,
      "social.grain.gallery.item",
      item_json,
    )

  // Query gallery with reverse join to items, then forward join to photos
  let query =
    "{
    socialGrainGallery {
      edges {
        node {
          title
          socialGrainGalleryItemViaGallery {
            edges {
              node {
                uri
                itemResolved {
                  ... on SocialGrainPhoto {
                    uri
                    alt
                  }
                }
              }
            }
          }
        }
      }
    }
  }"

  let assert Ok(response_json) =
    graphql_gleam.execute_query_with_db(
      conn,
      query,
      "{}",
      Error(Nil),
      "",
      "https://plc.directory",
    )

  // Verify the response includes the gallery
  string.contains(response_json, "Test Gallery")
  |> should.be_true

  // Verify the reverse join worked (gallery item is present)
  string.contains(response_json, item_uri)
  |> should.be_true

  // CRITICAL: Verify itemResolved field exists and resolved the photo
  // This tests the fix for forward join fields being available through reverse joins
  string.contains(response_json, photo_uri)
  |> should.be_true

  string.contains(response_json, "A beautiful sunset")
  |> should.be_true
}

/// Test that integer fields are correctly resolved (not converted to strings)
/// This tests the fix for field value type handling
pub fn integer_field_resolves_correctly_test() {
  let assert Ok(conn) = sqlight.open(":memory:")
  let assert Ok(_) = tables.create_lexicon_table(conn)
  let assert Ok(_) = tables.create_record_table(conn)
  let assert Ok(_) = tables.create_actor_table(conn)

  let assert Ok(_) =
    lexicons.insert(
      conn,
      "social.grain.gallery.item",
      create_gallery_item_lexicon(),
    )

  let did1 = "did:test:user1"
  let gallery_uri = "at://" <> did1 <> "/social.grain.gallery/gallery1"
  let item_uri = "at://" <> did1 <> "/social.grain.gallery.item/item1"
  let photo_uri = "at://" <> did1 <> "/social.grain.photo/photo1"

  // Insert gallery item with position = 42
  let item_json =
    json.object([
      #("gallery", json.string(gallery_uri)),
      #("item", json.string(photo_uri)),
      #("position", json.int(42)),
      #("createdAt", json.string("2024-01-01T00:00:00.000Z")),
    ])
    |> json.to_string

  let assert Ok(_) =
    records.insert(
      conn,
      item_uri,
      "cid1",
      did1,
      "social.grain.gallery.item",
      item_json,
    )

  let query =
    "{
    socialGrainGalleryItem {
      edges {
        node {
          uri
          position
        }
      }
    }
  }"

  let assert Ok(response_json) =
    graphql_gleam.execute_query_with_db(
      conn,
      query,
      "{}",
      Error(Nil),
      "",
      "https://plc.directory",
    )

  // Verify position is returned as integer, not string or null
  { string.contains(response_json, "\"position\":42") }
  |> bool.or(string.contains(response_json, "\"position\": 42"))
  |> should.be_true

  // Ensure it's not returned as null
  { string.contains(response_json, "\"position\":null") }
  |> bool.or(string.contains(response_json, "\"position\": null"))
  |> should.be_false
}

/// Test complete nested query: profile → galleries → items → photos with sorting
/// This is the actual use case that was failing before the fixes
pub fn nested_query_profile_to_photos_test() {
  let assert Ok(conn) = sqlight.open(":memory:")
  let assert Ok(_) = tables.create_lexicon_table(conn)
  let assert Ok(_) = tables.create_record_table(conn)
  let assert Ok(_) = tables.create_actor_table(conn)

  // Insert all lexicons
  let assert Ok(_) =
    lexicons.insert(
      conn,
      "social.grain.actor.profile",
      create_profile_lexicon(),
    )
  let assert Ok(_) =
    lexicons.insert(conn, "social.grain.gallery", create_gallery_lexicon())
  let assert Ok(_) =
    lexicons.insert(
      conn,
      "social.grain.gallery.item",
      create_gallery_item_lexicon(),
    )
  let assert Ok(_) =
    lexicons.insert(conn, "social.grain.photo", create_photo_lexicon())

  let did1 = "did:test:alice"
  let profile_uri = "at://" <> did1 <> "/social.grain.actor.profile/self"
  let gallery_uri = "at://" <> did1 <> "/social.grain.gallery/vacation"
  let photo1_uri = "at://" <> did1 <> "/social.grain.photo/photo1"
  let photo2_uri = "at://" <> did1 <> "/social.grain.photo/photo2"
  let item1_uri = "at://" <> did1 <> "/social.grain.gallery.item/item1"
  let item2_uri = "at://" <> did1 <> "/social.grain.gallery.item/item2"

  // Insert profile
  let assert Ok(_) =
    records.insert(
      conn,
      profile_uri,
      "cid1",
      did1,
      "social.grain.actor.profile",
      "{\"displayName\":\"Alice\",\"createdAt\":\"2024-01-01T00:00:00.000Z\"}",
    )

  // Insert gallery
  let assert Ok(_) =
    records.insert(
      conn,
      gallery_uri,
      "cid2",
      did1,
      "social.grain.gallery",
      "{\"title\":\"Summer Vacation\",\"createdAt\":\"2024-01-01T00:00:00.000Z\"}",
    )

  // Insert photos
  let assert Ok(_) =
    records.insert(
      conn,
      photo1_uri,
      "cid3",
      did1,
      "social.grain.photo",
      "{\"alt\":\"Beach\",\"createdAt\":\"2024-01-02T00:00:00.000Z\"}",
    )
  let assert Ok(_) =
    records.insert(
      conn,
      photo2_uri,
      "cid4",
      did1,
      "social.grain.photo",
      "{\"alt\":\"Mountains\",\"createdAt\":\"2024-01-03T00:00:00.000Z\"}",
    )

  // Insert gallery items with positions
  let assert Ok(_) =
    records.insert(
      conn,
      item1_uri,
      "cid5",
      did1,
      "social.grain.gallery.item",
      "{\"gallery\":\""
        <> gallery_uri
        <> "\",\"item\":\""
        <> photo1_uri
        <> "\",\"position\":1,\"createdAt\":\"2024-01-01T00:00:00.000Z\"}",
    )
  let assert Ok(_) =
    records.insert(
      conn,
      item2_uri,
      "cid6",
      did1,
      "social.grain.gallery.item",
      "{\"gallery\":\""
        <> gallery_uri
        <> "\",\"item\":\""
        <> photo2_uri
        <> "\",\"position\":0,\"createdAt\":\"2024-01-01T00:00:00.000Z\"}",
    )

  // The complete nested query that was failing
  let query =
    "{
    socialGrainActorProfile {
      edges {
        node {
          displayName
          socialGrainGalleryByDid {
            edges {
              node {
                title
                socialGrainGalleryItemViaGallery(
                  sortBy: [{ field: \"position\", direction: ASC }]
                ) {
                  edges {
                    node {
                      position
                      itemResolved {
                        ... on SocialGrainPhoto {
                          uri
                          alt
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }"

  let assert Ok(response_json) =
    graphql_gleam.execute_query_with_db(
      conn,
      query,
      "{}",
      Error(Nil),
      "",
      "https://plc.directory",
    )

  // Verify all levels of nesting work
  string.contains(response_json, "Alice")
  |> should.be_true

  string.contains(response_json, "Summer Vacation")
  |> should.be_true

  // Verify positions are integers
  { string.contains(response_json, "\"position\":0") }
  |> bool.or(string.contains(response_json, "\"position\": 0"))
  |> should.be_true

  { string.contains(response_json, "\"position\":1") }
  |> bool.or(string.contains(response_json, "\"position\": 1"))
  |> should.be_true

  // CRITICAL: Verify itemResolved works through the reverse join
  string.contains(response_json, photo1_uri)
  |> should.be_true

  string.contains(response_json, photo2_uri)
  |> should.be_true

  string.contains(response_json, "Beach")
  |> should.be_true

  string.contains(response_json, "Mountains")
  |> should.be_true
}
