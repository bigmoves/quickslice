/// Tests for DID-based join field generation
///
/// Verifies that DID join fields are added to GraphQL schemas correctly:
/// - All collections get DID join fields to all other collections
/// - Cardinality is determined by has_unique_did (literal:self key)
/// - Field naming follows {TypeName}ByDid pattern
import gleam/dict
import gleam/option
import gleam/string
import gleeunit/should
import graphql/introspection
import graphql/schema
import graphql/sdl
import lexicon_graphql/db_schema_builder
import lexicon_graphql/types

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

// Test that collections get DID join fields to other collections
pub fn collections_get_did_join_fields_test() {
  // Create two collections: a status and a profile (with literal:self)
  let status_lexicon =
    types.Lexicon(
      id: "xyz.statusphere.status",
      defs: types.Defs(
        main: option.Some(
          types.RecordDef(type_: "record", key: option.None, properties: [
            #(
              "text",
              types.Property(
                type_: "string",
                required: True,
                format: option.None,
                ref: option.None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let profile_lexicon =
    types.Lexicon(
      id: "app.bsky.actor.profile",
      defs: types.Defs(
        main: option.Some(
          types.RecordDef(
            type_: "record",
            key: option.Some("literal:self"),
            properties: [
              #(
                "displayName",
                types.Property(
                  type_: "string",
                  required: False,
                  format: option.None,
                  ref: option.None,
                ),
              ),
            ],
          ),
        ),
        others: dict.new(),
      ),
    )

  let test_schema =
    create_test_schema_from_lexicons([status_lexicon, profile_lexicon])

  // Get all types and serialize to SDL
  let all_types = introspection.get_all_schema_types(test_schema)
  let serialized = sdl.print_types(all_types)

  // Verify that Status has a DID join field to Profile
  string.contains(serialized, "appBskyActorProfileByDid")
  |> should.be_true

  // Verify that Profile has a DID join field to Status
  string.contains(serialized, "xyzStatusphereStatusByDid")
  |> should.be_true
}

// Test that literal:self collections return single nullable objects
pub fn literal_self_returns_single_object_test() {
  let status_lexicon =
    types.Lexicon(
      id: "xyz.statusphere.status",
      defs: types.Defs(
        main: option.Some(
          types.RecordDef(type_: "record", key: option.None, properties: [
            #(
              "text",
              types.Property(
                type_: "string",
                required: True,
                format: option.None,
                ref: option.None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let profile_lexicon =
    types.Lexicon(
      id: "app.bsky.actor.profile",
      defs: types.Defs(
        main: option.Some(
          types.RecordDef(
            type_: "record",
            key: option.Some("literal:self"),
            properties: [
              #(
                "displayName",
                types.Property(
                  type_: "string",
                  required: False,
                  format: option.None,
                  ref: option.None,
                ),
              ),
            ],
          ),
        ),
        others: dict.new(),
      ),
    )

  let test_schema =
    create_test_schema_from_lexicons([status_lexicon, profile_lexicon])

  let all_types = introspection.get_all_schema_types(test_schema)
  let serialized = sdl.print_types(all_types)

  // Profile should be returned as single object (not list) from Status
  // We check that it's NOT wrapped in a list (no brackets)
  // The field should be: appBskyActorProfileByDid: AppBskyActorProfile
  string.contains(serialized, "appBskyActorProfileByDid: AppBskyActorProfile")
  |> should.be_true
}

// Test that non-literal:self collections return lists
pub fn non_literal_self_returns_list_test() {
  let status_lexicon =
    types.Lexicon(
      id: "xyz.statusphere.status",
      defs: types.Defs(
        main: option.Some(
          types.RecordDef(type_: "record", key: option.None, properties: [
            #(
              "text",
              types.Property(
                type_: "string",
                required: True,
                format: option.None,
                ref: option.None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let post_lexicon =
    types.Lexicon(
      id: "app.bsky.feed.post",
      defs: types.Defs(
        main: option.Some(
          types.RecordDef(type_: "record", key: option.None, properties: [
            #(
              "text",
              types.Property(
                type_: "string",
                required: True,
                format: option.None,
                ref: option.None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let test_schema =
    create_test_schema_from_lexicons([status_lexicon, post_lexicon])

  let all_types = introspection.get_all_schema_types(test_schema)
  let serialized = sdl.print_types(all_types)

  // Post should be returned as a connection (paginated list) from Status
  // The field should be: appBskyFeedPostByDid: AppBskyFeedPostConnection
  string.contains(serialized, "appBskyFeedPostByDid: AppBskyFeedPostConnection")
  |> should.be_true
}

// Test that multiple collections all get DID joins to each other
pub fn multiple_collections_get_cross_joins_test() {
  let status_lexicon =
    types.Lexicon(
      id: "xyz.statusphere.status",
      defs: types.Defs(
        main: option.Some(
          types.RecordDef(type_: "record", key: option.None, properties: [
            #(
              "text",
              types.Property(
                type_: "string",
                required: True,
                format: option.None,
                ref: option.None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let post_lexicon =
    types.Lexicon(
      id: "app.bsky.feed.post",
      defs: types.Defs(
        main: option.Some(
          types.RecordDef(type_: "record", key: option.None, properties: [
            #(
              "text",
              types.Property(
                type_: "string",
                required: True,
                format: option.None,
                ref: option.None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let like_lexicon =
    types.Lexicon(
      id: "app.bsky.feed.like",
      defs: types.Defs(
        main: option.Some(
          types.RecordDef(type_: "record", key: option.None, properties: [
            #(
              "subject",
              types.Property(
                type_: "string",
                required: True,
                format: option.Some("at-uri"),
                ref: option.None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let test_schema =
    create_test_schema_from_lexicons([
      status_lexicon,
      post_lexicon,
      like_lexicon,
    ])

  let all_types = introspection.get_all_schema_types(test_schema)
  let serialized = sdl.print_types(all_types)

  // Status should have joins to Post and Like
  string.contains(serialized, "appBskyFeedPostByDid")
  |> should.be_true

  string.contains(serialized, "appBskyFeedLikeByDid")
  |> should.be_true

  // Post should have joins to Status and Like
  string.contains(serialized, "xyzStatusphereStatusByDid")
  |> should.be_true
  // Like should have joins to Status and Post
  // (already checked above)
}

// Test that collections don't get DID join fields to themselves
pub fn no_self_join_test() {
  let status_lexicon =
    types.Lexicon(
      id: "xyz.statusphere.status",
      defs: types.Defs(
        main: option.Some(
          types.RecordDef(type_: "record", key: option.None, properties: [
            #(
              "text",
              types.Property(
                type_: "string",
                required: True,
                format: option.None,
                ref: option.None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let test_schema = create_test_schema_from_lexicons([status_lexicon])

  let all_types = introspection.get_all_schema_types(test_schema)
  let serialized = sdl.print_types(all_types)

  // Status should NOT have a join field to itself
  string.contains(serialized, "xyzStatusphereStatusByDid")
  |> should.be_false
}
