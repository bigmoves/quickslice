/// Tests for viewer state field generation
///
/// Verifies that viewer fields are generated for reverse join relationships
import gleam/dict
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import lexicon_graphql/schema/database as db_schema_builder
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

// Test lexicons
fn gallery_lexicon() -> types.Lexicon {
  types.Lexicon(
    id: "social.grain.gallery",
    defs: types.Defs(
      main: Some(
        types.RecordDef(type_: "record", key: Some("tid"), properties: [
          #(
            "title",
            types.Property(
              type_: "string",
              required: True,
              format: None,
              ref: None,
              refs: None,
              items: None,
            ),
          ),
        ]),
      ),
      others: dict.new(),
    ),
  )
}

fn favorite_lexicon() -> types.Lexicon {
  types.Lexicon(
    id: "social.grain.favorite",
    defs: types.Defs(
      main: Some(
        types.RecordDef(type_: "record", key: Some("tid"), properties: [
          #(
            "subject",
            types.Property(
              type_: "string",
              required: True,
              format: Some("at-uri"),
              ref: None,
              refs: None,
              items: None,
            ),
          ),
        ]),
      ),
      others: dict.new(),
    ),
  )
}

fn follow_lexicon() -> types.Lexicon {
  types.Lexicon(
    id: "social.grain.graph.follow",
    defs: types.Defs(
      main: Some(
        types.RecordDef(type_: "record", key: Some("tid"), properties: [
          #(
            "subject",
            types.Property(
              type_: "string",
              required: True,
              format: Some("did"),
              ref: None,
              refs: None,
              items: None,
            ),
          ),
        ]),
      ),
      others: dict.new(),
    ),
  )
}

// Test that viewer field is generated for AT-URI reverse joins
pub fn generates_viewer_field_for_reverse_join_test() {
  let lexicons = [gallery_lexicon(), favorite_lexicon()]

  let test_schema = create_test_schema_from_lexicons(lexicons)

  let all_types = introspection.get_all_schema_types(test_schema)
  let serialized = sdl.print_types(all_types)

  // Should have the regular reverse join field
  string.contains(serialized, "socialGrainFavoriteViaSubject")
  |> should.be_true

  // Should also have the viewer field
  string.contains(serialized, "viewerSocialGrainFavoriteViaSubject")
  |> should.be_true
}

// Test that viewer field is NOT a connection (returns single nullable object)
pub fn viewer_field_returns_nullable_object_test() {
  let lexicons = [gallery_lexicon(), favorite_lexicon()]

  let test_schema = create_test_schema_from_lexicons(lexicons)

  let all_types = introspection.get_all_schema_types(test_schema)
  let serialized = sdl.print_types(all_types)

  // The regular reverse join returns a Connection
  string.contains(
    serialized,
    "socialGrainFavoriteViaSubject: SocialGrainFavoriteConnection",
  )
  |> should.be_true

  // The viewer field should NOT return a Connection (just the type, nullable)
  string.contains(
    serialized,
    "viewerSocialGrainFavoriteViaSubject: SocialGrainFavorite",
  )
  |> should.be_true

  // It should NOT contain Connection for the viewer field
  string.contains(
    serialized,
    "viewerSocialGrainFavoriteViaSubject: SocialGrainFavoriteConnection",
  )
  |> should.be_false
}

// Test that viewer field is generated for DID-based subjects (e.g., follows)
// Collections with format: "did" subjects should create viewer fields on all types
pub fn generates_viewer_field_for_did_subject_test() {
  let lexicons = [gallery_lexicon(), follow_lexicon()]

  let test_schema = create_test_schema_from_lexicons(lexicons)

  let all_types = introspection.get_all_schema_types(test_schema)
  let serialized = sdl.print_types(all_types)

  // Gallery type should have the viewer follow field
  // The field shows whether the viewer follows the gallery author
  string.contains(serialized, "viewerSocialGrainGraphFollowViaSubject")
  |> should.be_true
}
