/// Tests for reverse join field generation
///
/// Verifies that reverse join fields are discovered and added to GraphQL schemas
import gleam/dict
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import lexicon_graphql/db_schema_builder
import lexicon_graphql/types
import swell/introspection
import swell/schema
import swell/sdl

// Helper to create a test schema with a mock fetcher
fn create_test_schema_from_lexicons(
  lexicons: List(types.Lexicon),
) -> schema.Schema {
  // Mock fetcher that returns empty results (we're only testing schema generation)
  let fetcher = fn(_collection, _params) {
    Ok(#([], option.None, False, False, option.None))
  }

  case
    db_schema_builder.build_schema_with_fetcher(
      lexicons,
      fetcher,
      option.None,
      option.None,
      option.None,
      option.None,
      option.None,
      option.None,
    )
  {
    Ok(s) -> s
    Error(_) -> panic as "Failed to build test schema"
  }
}

// Test that a forward join in one collection creates a reverse join field in the target
pub fn forward_join_creates_reverse_join_test() {
  // Create a Post collection (target)
  let post_lexicon =
    types.Lexicon(
      id: "app.bsky.feed.post",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "text",
              types.Property(
                type_: "string",
                required: True,
                format: None,
                ref: None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  // Create a Like collection with a subject field that references posts
  let like_lexicon =
    types.Lexicon(
      id: "app.bsky.feed.like",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "subject",
              types.Property(
                type_: "string",
                required: True,
                format: Some("at-uri"),
                ref: None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let test_schema =
    create_test_schema_from_lexicons([post_lexicon, like_lexicon])

  // Get all types and serialize to SDL
  let all_types = introspection.get_all_schema_types(test_schema)
  let serialized = sdl.print_types(all_types)

  // Verify that the Post type has a reverse join field for likes
  // Field name should be: appBskyFeedLikeViaSubject (camelCase)
  string.contains(serialized, "appBskyFeedLikeViaSubject")
  |> should.be_true
}

// Test that strongRef fields also create reverse joins
pub fn strong_ref_creates_reverse_join_test() {
  // Create a Post collection (target)
  let post_lexicon =
    types.Lexicon(
      id: "app.bsky.feed.post",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "text",
              types.Property(
                type_: "string",
                required: True,
                format: None,
                ref: None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  // Create a Profile collection with a pinnedPost strongRef field
  let profile_lexicon =
    types.Lexicon(
      id: "app.bsky.actor.profile",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "pinnedPost",
              types.Property(
                type_: "ref",
                required: False,
                format: None,
                ref: Some("com.atproto.repo.strongRef"),
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let test_schema =
    create_test_schema_from_lexicons([post_lexicon, profile_lexicon])

  let all_types = introspection.get_all_schema_types(test_schema)
  let serialized = sdl.print_types(all_types)

  // Verify that the Post type has a reverse join field for pinned posts
  // Field name should be: appBskyActorProfileViaPinnedPost (camelCase)
  string.contains(serialized, "appBskyActorProfileViaPinnedPost")
  |> should.be_true
}

// Test that multiple reverse joins are all generated
pub fn multiple_reverse_joins_test() {
  // Create a Post collection (target)
  let post_lexicon =
    types.Lexicon(
      id: "app.bsky.feed.post",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "text",
              types.Property(
                type_: "string",
                required: True,
                format: None,
                ref: None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  // Create a Like collection
  let like_lexicon =
    types.Lexicon(
      id: "app.bsky.feed.like",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "subject",
              types.Property(
                type_: "string",
                required: True,
                format: Some("at-uri"),
                ref: None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  // Create a Repost collection
  let repost_lexicon =
    types.Lexicon(
      id: "app.bsky.feed.repost",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "subject",
              types.Property(
                type_: "string",
                required: True,
                format: Some("at-uri"),
                ref: None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let test_schema =
    create_test_schema_from_lexicons([
      post_lexicon,
      like_lexicon,
      repost_lexicon,
    ])

  let all_types = introspection.get_all_schema_types(test_schema)
  let serialized = sdl.print_types(all_types)

  // Check both reverse join fields exist on Post type (camelCase)
  string.contains(serialized, "appBskyFeedLikeViaSubject")
  |> should.be_true

  string.contains(serialized, "appBskyFeedRepostViaSubject")
  |> should.be_true
}

// Test that collections without forward join fields don't appear in reverse joins
pub fn no_false_positive_reverse_joins_test() {
  // Create a Post collection
  let post_lexicon =
    types.Lexicon(
      id: "app.bsky.feed.post",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "text",
              types.Property(
                type_: "string",
                required: True,
                format: None,
                ref: None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  // Create a Status collection with no join fields
  let status_lexicon =
    types.Lexicon(
      id: "xyz.statusosphere.status",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "message",
              types.Property(
                type_: "string",
                required: True,
                format: None,
                ref: None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let test_schema =
    create_test_schema_from_lexicons([post_lexicon, status_lexicon])

  let all_types = introspection.get_all_schema_types(test_schema)
  let serialized = sdl.print_types(all_types)

  // Status collection has no forward joins, so Post should not have a reverse join field
  // that references Status. We check that the field name XyzStatusosphereStatusVia* doesn't appear
  string.contains(serialized, "XyzStatusosphereStatusVia")
  |> should.be_false
}
