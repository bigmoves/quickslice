/// Tests that reverse join fields are available through forward join resolution
///
/// This tests the fix for PASS 4 where *Resolved fields need to return
/// the Record union with complete types including reverse joins.
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit
import gleeunit/should
import lexicon_graphql/schema/database as db_schema_builder
import lexicon_graphql/types
import swell/introspection
import swell/schema
import swell/sdl

pub fn main() {
  gleeunit.main()
}

// Helper to create a test schema with a mock fetcher
fn create_test_schema_from_lexicons(
  lexicons: List(types.Lexicon),
) -> schema.Schema {
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

/// Test that the Record union members have reverse join fields
///
/// Setup:
/// - Photo collection (target of forward join)
/// - Exif collection with forward join to Photo (creates reverse join on Photo)
/// - GalleryItem collection with forward join to Photo (item field)
///
/// Expected: When resolving itemResolved on GalleryItem, the Photo type
/// should have socialGrainPhotoExifViaPhoto reverse join available
pub fn record_union_members_have_reverse_joins_test() {
  // Photo collection - will be target of both forward and reverse joins
  let photo_lexicon =
    types.Lexicon(
      id: "social.grain.photo",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "title",
              types.Property(
                type_: "string",
                required: False,
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

  // Exif collection - has forward join to Photo (creates reverse join)
  let exif_lexicon =
    types.Lexicon(
      id: "social.grain.photo.exif",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "photo",
              types.Property(
                type_: "string",
                required: True,
                format: Some("at-uri"),
                ref: None,
                refs: None,
                items: None,
              ),
            ),
            #(
              "exposureTime",
              types.Property(
                type_: "integer",
                required: False,
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

  // GalleryItem collection - has forward join to Photo via item field
  let gallery_item_lexicon =
    types.Lexicon(
      id: "social.grain.gallery.item",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "item",
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

  let test_schema =
    create_test_schema_from_lexicons([
      photo_lexicon,
      exif_lexicon,
      gallery_item_lexicon,
    ])

  // Get all types and serialize to SDL
  let all_types = introspection.get_all_schema_types(test_schema)
  let serialized = sdl.print_types(all_types)

  // Verify Photo type has the reverse join from Exif
  // This confirms the direct query works
  string.contains(serialized, "socialGrainPhotoExifViaPhoto")
  |> should.be_true

  // Verify GalleryItem has itemResolved field
  string.contains(serialized, "itemResolved: Record")
  |> should.be_true

  // Find the standalone SocialGrainPhoto type (from query field edges)
  let standalone_photo =
    list.find(all_types, fn(t) { schema.type_name(t) == "SocialGrainPhoto" })

  case standalone_photo {
    Ok(photo) -> {
      let fields = schema.get_fields(photo)
      let field_names = list.map(fields, schema.field_name)

      // Standalone Photo should have the reverse join
      list.contains(field_names, "socialGrainPhotoExifViaPhoto")
      |> should.be_true
    }
    Error(_) -> should.fail()
  }

  // Now the key test: The Record union's SocialGrainPhoto member
  // should have the reverse join field
  //
  // Find the Record union definition and check its members have reverse joins
  // The SDL should show SocialGrainPhoto in the union with all its fields

  // Get the Record union type
  let record_union =
    list.find(all_types, fn(t) { schema.type_name(t) == "Record" })

  case record_union {
    Ok(union_type) -> {
      // Get the possible types (union members)
      let possible_types = schema.get_possible_types(union_type)

      // Find the Photo type in the union
      let photo_type =
        list.find(possible_types, fn(t) {
          schema.type_name(t) == "SocialGrainPhoto"
        })

      case photo_type {
        Ok(photo) -> {
          // Get the fields of the Photo type within the union
          let fields = schema.get_fields(photo)
          let field_names = list.map(fields, schema.field_name)

          // Count fields on each version for comparison
          let standalone_field_count = case standalone_photo {
            Ok(sp) -> list.length(schema.get_fields(sp))
            Error(_) -> 0
          }
          let union_field_count = list.length(fields)

          // The union's Photo type should have the same number of fields
          // as the standalone Photo type
          union_field_count |> should.equal(standalone_field_count)

          // Check that socialGrainPhotoExifViaPhoto is present
          list.contains(field_names, "socialGrainPhotoExifViaPhoto")
          |> should.be_true
        }
        Error(_) -> {
          // Photo type not found in union - fail
          should.fail()
        }
      }
    }
    Error(_) -> {
      // Record union not found - fail
      should.fail()
    }
  }
}
