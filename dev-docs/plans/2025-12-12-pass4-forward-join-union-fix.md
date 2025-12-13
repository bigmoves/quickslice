# PASS 4: Fix Forward Join Fields to Use Final Record Union

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix reverse joins not being available when querying through `*Resolved` forward join fields on the Record union type.

**Architecture:** Add a PASS 4 to the schema builder that rebuilds forward join fields after `final_record_union` is constructed, ensuring `*Resolved` fields return the complete Record union with all reverse/DID join fields.

**Tech Stack:** Gleam, swell GraphQL library

---

## Problem Summary

When querying through `itemResolved` (or any `*Resolved` forward join field), reverse joins like `socialGrainPhotoExifViaPhoto` are not available:

```graphql
query {
  socialGrainGalleryItemViaGallery {
    edges {
      node {
        itemResolved {
          ... on SocialGrainPhoto {
            socialGrainPhotoExifViaPhoto {  # Returns null - field not found!
              edges { node { exposureTime } }
            }
          }
        }
      }
    }
  }
}
```

**Root cause:** In PASS 3, forward join fields are built using `complete_object_types` which contains `complete_record_union` from PASS 2. But `complete_record_union` was built from types that used `basic_object_types` - they don't have the final reverse join fields.

---

## Task 1: Write Failing Test

**Files:**
- Create: `lexicon_graphql/test/forward_join_reverse_join_test.gleam`

**Step 1: Write the failing test**

```gleam
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

          // Check that socialGrainPhotoExifViaPhoto is present
          let has_reverse_join =
            list.any(fields, fn(f) {
              schema.field_name(f) == "socialGrainPhotoExifViaPhoto"
            })

          has_reverse_join |> should.be_true
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
```

**Step 2: Run test to verify it fails**

Run: `cd lexicon_graphql && gleam test -- --filter="record_union_members_have_reverse_joins"`

Expected: FAIL - the Photo type within the Record union does not have `socialGrainPhotoExifViaPhoto` field

---

## Task 2: Implement PASS 4

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/schema/database.gleam:591-600`

**Step 1: Add PASS 4 after final_record_union is built**

Insert the following code after line 591 (after `final_object_types` is created) and before the `// Merge ref_object_types` comment:

```gleam
  // =============================================================================
  // PASS 4: Rebuild forward join fields to use final_record_union
  // =============================================================================
  // The forward join fields built in PASS 3 reference complete_record_union,
  // which contains types without the final reverse/DID joins. We need to rebuild
  // them to reference final_record_union so that *Resolved fields return types
  // with all join fields available.

  let pass4_record_types =
    list.map(final_record_types, fn(record_type) {
      // Rebuild forward join fields using final_object_types
      // (which has final_record_union at "_generic_record")
      let new_forward_join_fields =
        build_forward_join_fields_with_types(
          record_type.meta,
          batch_fetcher,
          final_object_types,
        )

      // Get names of forward join fields to filter out old ones
      let forward_join_names =
        list.map(record_type.meta.forward_join_fields, fn(jf) {
          let name = case jf {
            collection_meta.StrongRefField(n) -> n
            collection_meta.AtUriField(n) -> n
          }
          name <> "Resolved"
        })

      // Filter out old forward join fields, keep everything else
      let other_fields =
        list.filter(record_type.fields, fn(field) {
          let name = schema.field_name(field)
          !list.contains(forward_join_names, name)
        })

      // Combine: other fields + new forward join fields
      let all_fields = list.append(other_fields, new_forward_join_fields)

      RecordType(..record_type, fields: all_fields)
    })

  // Rebuild object types with corrected forward joins
  let pass4_object_types_without_generic =
    list.fold(pass4_record_types, dict.new(), fn(acc, record_type) {
      let object_type =
        schema.object_type(
          record_type.type_name,
          "Record type: " <> record_type.nsid,
          record_type.fields,
        )
      dict.insert(acc, record_type.nsid, object_type)
    })

  // Rebuild Record union with truly final types
  let pass4_possible_types = dict.values(pass4_object_types_without_generic)
  let pass4_record_union = build_record_union(pass4_possible_types)
  let pass4_object_types =
    dict.insert(
      pass4_object_types_without_generic,
      "_generic_record",
      pass4_record_union,
    )
```

**Step 2: Update the merge and return statements**

Replace the existing merge (lines ~593-600) with:

```gleam
  // Merge ref_object_types (from lexicon defs, already has forward joins from PASS 0b)
  // into pass4_object_types to make them available for ref resolution
  let final_object_types_with_refs =
    dict.fold(ref_object_types, pass4_object_types, fn(acc, ref, obj_type) {
      dict.insert(acc, ref, obj_type)
    })

  #(pass4_record_types, final_object_types_with_refs, field_type_registry)
```

**Step 3: Run test to verify it passes**

Run: `cd lexicon_graphql && gleam test -- --filter="record_union_members_have_reverse_joins"`

Expected: PASS

**Step 4: Run all tests**

Run: `cd lexicon_graphql && gleam test`

Expected: All tests pass

**Step 5: Commit**

```bash
git add lexicon_graphql/test/forward_join_reverse_join_test.gleam lexicon_graphql/src/lexicon_graphql/schema/database.gleam
git commit -m "fix: add PASS 4 to rebuild forward joins with final Record union

Forward join *Resolved fields now return the Record union with complete
types that include all reverse join and DID join fields.

Previously, *Resolved fields used complete_record_union from PASS 2,
which contained types without the final join fields. Now we rebuild
the forward join fields in PASS 4 using final_record_union.

Fixes: reverse joins not available through itemResolved queries"
```

---

## Task 3: Integration Test with Server

**Files:**
- No new files - manual testing

**Step 1: Build and start the server**

Run: `cd server && gleam build && gleam run`

**Step 2: Test the query that was failing**

Execute via GraphQL playground or curl:

```graphql
query {
  socialGrainGallery(where: {actorHandle: {eq: "chadtmiller.com"}, title: {eq: "Berlin"}}) {
    edges {
      node {
        title
        socialGrainGalleryItemViaGallery(first: 3) {
          edges {
            node {
              itemResolved {
                ... on SocialGrainPhoto {
                  uri
                  socialGrainPhotoExifViaPhoto(first: 1) {
                    edges {
                      node {
                        exposureTime
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
}
```

Expected: `socialGrainPhotoExifViaPhoto` returns EXIF data with `exposureTime`, not null

**Step 3: Verify no regressions**

Test a few other queries to ensure nothing broke:
- Direct photo query with reverse joins
- Gallery query without forward join traversal
- Other forward join fields

---

## Summary

| Task | Description | Files |
|------|-------------|-------|
| 1 | Write failing test | `test/forward_join_reverse_join_test.gleam` |
| 2 | Implement PASS 4 | `schema/database.gleam` |
| 3 | Integration test | Manual testing |

**Total estimated changes:** ~60 lines of new code in database.gleam, ~120 lines of test code
