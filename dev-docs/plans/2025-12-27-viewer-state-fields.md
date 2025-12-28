# Viewer State Fields Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Auto-generate viewer fields that show a logged-in user's relationship to content (e.g., "did I like this post?", "do I follow this author?").

**Architecture:** For each reverse join field, generate a corresponding viewer field that returns the viewer's single record (if any) instead of a connection. Two patterns: AT-URI subjects (existing reverse joins) and DID subjects (follows).

**Tech Stack:** Gleam, lexicon_graphql schema builder

---

## Task 1: Extend CollectionMeta to Track DID-Typed Subject Fields

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/internal/lexicon/collection_meta.gleam`
- Test: `lexicon_graphql/test/collection_meta_test.gleam`

**Step 1: Write the failing test**

Create test file `lexicon_graphql/test/collection_meta_test.gleam`:

```gleam
import gleeunit/should
import lexicon_graphql/internal/lexicon/collection_meta
import lexicon_graphql/types
import gleam/option.{None, Some}

pub fn detects_did_subject_field_test() {
  // Follow lexicon has subject with format: "did"
  let lexicon =
    types.Lexicon(
      lexicon: 1,
      id: "social.grain.graph.follow",
      revision: None,
      description: None,
      defs: types.Defs(
        main: Some(types.MainDef(
          type_: "record",
          description: None,
          key: Some("tid"),
          properties: [
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
          ],
          required: None,
          ref: None,
          refs: None,
        )),
        other: [],
      ),
    )

  let meta = collection_meta.extract_metadata(lexicon)

  // Should detect subject as a DID-typed field
  meta.did_subject_fields
  |> should.equal(["subject"])
}

pub fn detects_at_uri_subject_field_test() {
  // Favorite lexicon has subject with format: "at-uri"
  let lexicon =
    types.Lexicon(
      lexicon: 1,
      id: "social.grain.favorite",
      revision: None,
      description: None,
      defs: types.Defs(
        main: Some(types.MainDef(
          type_: "record",
          description: None,
          key: Some("tid"),
          properties: [
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
          ],
          required: None,
          ref: None,
          refs: None,
        )),
        other: [],
      ),
    )

  let meta = collection_meta.extract_metadata(lexicon)

  // Should detect subject as at-uri (existing behavior)
  meta.reverse_join_fields
  |> should.equal(["subject"])

  // Should NOT detect as DID field
  meta.did_subject_fields
  |> should.equal([])
}
```

**Step 2: Run test to verify it fails**

Run: `cd lexicon_graphql && gleam test -- --filter=detects_did_subject_field`
Expected: FAIL - `did_subject_fields` field doesn't exist on CollectionMeta

**Step 3: Add did_subject_fields to CollectionMeta type**

In `collection_meta.gleam`, update the type definition:

```gleam
pub type CollectionMeta {
  CollectionMeta(
    nsid: String,
    type_name: String,
    key_type: String,
    has_unique_did: Bool,
    forward_join_fields: List(ForwardJoinField),
    reverse_join_fields: List(String),
    /// Fields with format: "did" that can be used for DID-based viewer joins
    did_subject_fields: List(String),
  )
}
```

**Step 4: Update scan_properties to detect DID fields**

```gleam
fn scan_properties(
  properties: List(#(String, types.Property)),
) -> #(List(ForwardJoinField), List(String), List(String)) {
  list.fold(properties, #([], [], []), fn(acc, prop) {
    let #(forward_fields, reverse_fields, did_fields) = acc
    let #(name, property) = prop

    // Check if this is a forward join field
    let new_forward = case is_forward_join_field(property) {
      Some(field_type) -> [field_type(name), ..forward_fields]
      None -> forward_fields
    }

    // Check if this is a reverse join field (at-uri format)
    let new_reverse = case is_reverse_join_field(property) {
      True -> [name, ..reverse_fields]
      False -> reverse_fields
    }

    // Check if this is a DID-typed field
    let new_did = case is_did_field(property) {
      True -> [name, ..did_fields]
      False -> did_fields
    }

    #(new_forward, new_reverse, new_did)
  })
}

/// Check if a property has DID format
fn is_did_field(property: types.Property) -> Bool {
  case property.format {
    Some(fmt) if fmt == "did" -> True
    _ -> False
  }
}
```

**Step 5: Update extract_metadata to use new scan_properties**

```gleam
pub fn extract_metadata(lexicon: types.Lexicon) -> CollectionMeta {
  let type_name = nsid.to_type_name(lexicon.id)

  case lexicon.defs.main {
    Some(main_def) -> {
      let key_type = case main_def.key {
        Some(k) -> k
        None -> "tid"
      }
      let has_unique_did = key_type == "literal:self"

      let #(forward_fields, reverse_fields, did_fields) =
        scan_properties(main_def.properties)

      CollectionMeta(
        nsid: lexicon.id,
        type_name: type_name,
        key_type: key_type,
        has_unique_did: has_unique_did,
        forward_join_fields: forward_fields,
        reverse_join_fields: reverse_fields,
        did_subject_fields: did_fields,
      )
    }
    None -> {
      CollectionMeta(
        nsid: lexicon.id,
        type_name: type_name,
        key_type: "tid",
        has_unique_did: False,
        forward_join_fields: [],
        reverse_join_fields: [],
        did_subject_fields: [],
      )
    }
  }
}
```

**Step 6: Run test to verify it passes**

Run: `cd lexicon_graphql && gleam test -- --filter=detects_`
Expected: PASS

**Step 7: Fix any compilation errors in other files**

Update any files that construct `CollectionMeta` to include the new field.

**Step 8: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/internal/lexicon/collection_meta.gleam lexicon_graphql/test/collection_meta_test.gleam
git commit -m "$(cat <<'EOF'
feat(collection_meta): track DID-typed subject fields

Add did_subject_fields to CollectionMeta to enable viewer field generation
for collections where the subject field contains a DID (e.g., follows).
EOF
)"
```

---

## Task 2: Add Viewer State Fetcher Type

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/query/dataloader.gleam`
- Test: `lexicon_graphql/test/dataloader_test.gleam`

**Step 1: Write the failing test**

Add to `dataloader_test.gleam`:

```gleam
pub fn batch_fetch_viewer_state_test() {
  // Mock fetcher that returns records for specific viewer+subject combinations
  let fetcher = fn(
    viewer_did: String,
    collection: String,
    reference_field: String,
    parent_keys: List(String),
  ) -> Result(Dict(String, value.Value), String) {
    // Simulate: viewer "did:plc:viewer" liked gallery1 and gallery3
    case viewer_did, collection {
      "did:plc:viewer", "social.grain.favorite" -> {
        let results =
          dict.new()
          |> dict.insert(
            "at://did:plc:author/social.grain.gallery/gallery1",
            value.Object([
              #("uri", value.String("at://did:plc:viewer/social.grain.favorite/abc")),
              #("subject", value.String("at://did:plc:author/social.grain.gallery/gallery1")),
            ]),
          )
          |> dict.insert(
            "at://did:plc:author/social.grain.gallery/gallery3",
            value.Object([
              #("uri", value.String("at://did:plc:viewer/social.grain.favorite/def")),
              #("subject", value.String("at://did:plc:author/social.grain.gallery/gallery3")),
            ]),
          )
        Ok(results)
      }
      _, _ -> Ok(dict.new())
    }
  }

  let parent_uris = [
    "at://did:plc:author/social.grain.gallery/gallery1",
    "at://did:plc:author/social.grain.gallery/gallery2",
    "at://did:plc:author/social.grain.gallery/gallery3",
  ]

  let result =
    dataloader.batch_fetch_viewer_state(
      "did:plc:viewer",
      "social.grain.favorite",
      "subject",
      parent_uris,
      fetcher,
    )

  result |> should.be_ok

  let records = result |> result.unwrap(dict.new())

  // gallery1 should have a record
  dict.get(records, "at://did:plc:author/social.grain.gallery/gallery1")
  |> should.be_ok

  // gallery2 should NOT have a record (viewer didn't like it)
  dict.get(records, "at://did:plc:author/social.grain.gallery/gallery2")
  |> should.be_error

  // gallery3 should have a record
  dict.get(records, "at://did:plc:author/social.grain.gallery/gallery3")
  |> should.be_ok
}
```

**Step 2: Run test to verify it fails**

Run: `cd lexicon_graphql && gleam test -- --filter=batch_fetch_viewer_state`
Expected: FAIL - function doesn't exist

**Step 3: Add ViewerStateFetcher type and batch function**

In `dataloader.gleam`:

```gleam
/// Fetcher for viewer state queries
/// Takes viewer DID, collection, reference field, and list of parent keys
/// Returns a dict mapping parent keys to the viewer's record (if any)
pub type ViewerStateFetcher =
  fn(String, String, String, List(String)) ->
    Result(Dict(String, value.Value), String)

/// Batch fetch viewer state for multiple parent records
///
/// Given a viewer DID and list of parent URIs/DIDs, finds the viewer's
/// record for each parent (if any).
/// Returns a Dict mapping parent keys to the viewer's record.
pub fn batch_fetch_viewer_state(
  viewer_did: String,
  collection: String,
  reference_field: String,
  parent_keys: List(String),
  fetcher: ViewerStateFetcher,
) -> Result(Dict(String, value.Value), String) {
  fetcher(viewer_did, collection, reference_field, parent_keys)
}
```

**Step 4: Run test to verify it passes**

Run: `cd lexicon_graphql && gleam test -- --filter=batch_fetch_viewer_state`
Expected: PASS

**Step 5: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/query/dataloader.gleam lexicon_graphql/test/dataloader_test.gleam
git commit -m "$(cat <<'EOF'
feat(dataloader): add viewer state batch fetcher

Add ViewerStateFetcher type and batch_fetch_viewer_state function
for efficiently fetching viewer relationships across multiple records.
EOF
)"
```

---

## Task 3: Generate AT-URI Viewer Fields

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/schema/database.gleam`
- Test: `lexicon_graphql/test/viewer_state_test.gleam`

**Step 1: Write the failing test**

Create `lexicon_graphql/test/viewer_state_test.gleam`:

```gleam
import gleeunit/should
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import lexicon_graphql/schema/database as db_schema_builder
import lexicon_graphql/types
import swell/schema

// Test lexicons
fn gallery_lexicon() -> types.Lexicon {
  types.Lexicon(
    lexicon: 1,
    id: "social.grain.gallery",
    revision: None,
    description: None,
    defs: types.Defs(
      main: Some(types.MainDef(
        type_: "record",
        description: None,
        key: Some("tid"),
        properties: [
          #("title", types.Property(
            type_: "string",
            required: True,
            format: None,
            ref: None,
            refs: None,
            items: None,
          )),
        ],
        required: None,
        ref: None,
        refs: None,
      )),
      other: [],
    ),
  )
}

fn favorite_lexicon() -> types.Lexicon {
  types.Lexicon(
    lexicon: 1,
    id: "social.grain.favorite",
    revision: None,
    description: None,
    defs: types.Defs(
      main: Some(types.MainDef(
        type_: "record",
        description: None,
        key: Some("tid"),
        properties: [
          #("subject", types.Property(
            type_: "string",
            required: True,
            format: Some("at-uri"),
            ref: None,
            refs: None,
            items: None,
          )),
        ],
        required: None,
        ref: None,
        refs: None,
      )),
      other: [],
    ),
  )
}

pub fn generates_viewer_field_for_reverse_join_test() {
  let lexicons = [gallery_lexicon(), favorite_lexicon()]

  let schema_result = db_schema_builder.build_schema_with_fetchers(
    lexicons,
    stub_fetcher(),
    None,
    None,
    None,
    None,
    None,
    None,
    None,
  )

  schema_result |> should.be_ok
  let schema = schema_result |> result.unwrap(panic)

  // Get the SocialGrainGallery type
  let gallery_type = schema.types
    |> dict.get("SocialGrainGallery")

  gallery_type |> should.be_ok
  let gallery = gallery_type |> result.unwrap(panic)

  // Should have the viewer field
  let has_viewer_favorite = case gallery {
    schema.ObjectType(_, fields, _) ->
      list.any(fields, fn(f) {
        f.name == "viewerSocialGrainFavoriteViaSubject"
      })
    _ -> False
  }

  has_viewer_favorite |> should.be_true
}

fn stub_fetcher() -> db_schema_builder.RecordFetcher {
  fn(_collection, _params) {
    Ok(#([], None, False, False, Some(0)))
  }
}
```

**Step 2: Run test to verify it fails**

Run: `cd lexicon_graphql && gleam test -- --filter=generates_viewer_field`
Expected: FAIL - no viewerSocialGrainFavoriteViaSubject field

**Step 3: Add viewer field generation to database.gleam**

After `build_reverse_join_fields_with_types`, add a new function:

```gleam
/// Build viewer state fields for reverse joins
/// These return a single nullable record (the viewer's record, if any)
fn build_viewer_fields_for_reverse_joins(
  reverse_joins: List(ReverseJoinRelationship),
  viewer_state_fetcher: option.Option(dataloader.ViewerStateFetcher),
  object_types: dict.Dict(String, schema.Type),
) -> List(schema.Field) {
  list.map(reverse_joins, fn(relationship) {
    // Generate field name: viewer<SourceTypeName>Via<FieldName>
    let field_name =
      "viewer"
      <> relationship.source_type_name
      <> "Via"
      <> capitalize_first(relationship.source_field)

    // Get the source object type
    let source_object_type = case
      dict.get(object_types, relationship.source_collection)
    {
      Ok(obj_type) -> obj_type
      Error(_) -> schema.string_type()
    }

    // Return type is nullable single object (not a connection)
    let return_type = schema.nullable(source_object_type)

    schema.field(
      field_name,
      return_type,
      "Viewer's "
        <> relationship.source_collection
        <> " record referencing this via "
        <> relationship.source_field
        <> " (null if not authenticated or no record)",
      fn(ctx) {
        // Get viewer DID from context
        case get_viewer_did_from_context(ctx) {
          Ok(viewer_did) -> {
            // Get parent URI
            case get_field_from_context(ctx, "uri") {
              Ok(parent_uri) -> {
                case viewer_state_fetcher {
                  option.Some(fetcher) -> {
                    case
                      dataloader.batch_fetch_viewer_state(
                        viewer_did,
                        relationship.source_collection,
                        relationship.source_field,
                        [parent_uri],
                        fetcher,
                      )
                    {
                      Ok(results) -> {
                        case dict.get(results, parent_uri) {
                          Ok(record) -> Ok(record)
                          Error(_) -> Ok(value.Null)
                        }
                      }
                      Error(_) -> Ok(value.Null)
                    }
                  }
                  option.None -> Ok(value.Null)
                }
              }
              Error(_) -> Ok(value.Null)
            }
          }
          Error(_) -> Ok(value.Null)
        }
      },
    )
  })
}

/// Extract viewer DID from context
fn get_viewer_did_from_context(
  ctx: schema.Context,
) -> Result(String, Nil) {
  // The viewer DID should be in the top-level context
  case dict.get(ctx.context, "viewer_did") {
    Ok(dynamic_val) -> {
      case dynamic.string(dynamic_val) {
        Ok(did) -> Ok(did)
        Error(_) -> Error(Nil)
      }
    }
    Error(_) -> Error(Nil)
  }
}
```

**Step 4: Wire viewer fields into type building**

In the type building section (around line 515), add viewer fields:

```gleam
// Combine all fields (base + forward + reverse + DID joins + viewer fields)
let viewer_fields =
  build_viewer_fields_for_reverse_joins(
    record_type.reverse_joins,
    viewer_state_fetcher,
    pass3_object_types,
  )

let all_fields =
  list.flatten([
    base_fields,
    forward_join_fields,
    reverse_join_fields,
    did_join_fields,
    viewer_fields,
  ])
```

**Step 5: Add viewer_state_fetcher parameter to build_schema_with_fetchers**

Update the function signature and pass the fetcher through.

**Step 6: Run test to verify it passes**

Run: `cd lexicon_graphql && gleam test -- --filter=generates_viewer_field`
Expected: PASS

**Step 7: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/schema/database.gleam lexicon_graphql/test/viewer_state_test.gleam
git commit -m "$(cat <<'EOF'
feat(schema): generate viewer fields for AT-URI reverse joins

For each reverse join (e.g., socialGrainFavoriteViaSubject), generate
a corresponding viewer field (viewerSocialGrainFavoriteViaSubject) that
returns the viewer's single record or null.
EOF
)"
```

---

## Task 4: Generate DID-Based Viewer Fields

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/schema/database.gleam`
- Test: `lexicon_graphql/test/viewer_state_test.gleam`

**Step 1: Write the failing test**

Add to `viewer_state_test.gleam`:

```gleam
fn follow_lexicon() -> types.Lexicon {
  types.Lexicon(
    lexicon: 1,
    id: "social.grain.graph.follow",
    revision: None,
    description: None,
    defs: types.Defs(
      main: Some(types.MainDef(
        type_: "record",
        description: None,
        key: Some("tid"),
        properties: [
          #("subject", types.Property(
            type_: "string",
            required: True,
            format: Some("did"),
            ref: None,
            refs: None,
            items: None,
          )),
        ],
        required: None,
        ref: None,
        refs: None,
      )),
      other: [],
    ),
  )
}

pub fn generates_viewer_field_for_did_subject_test() {
  let lexicons = [gallery_lexicon(), follow_lexicon()]

  let schema_result = db_schema_builder.build_schema_with_fetchers(
    lexicons,
    stub_fetcher(),
    None,
    None,
    None,
    None,
    None,
    None,
    None,
  )

  schema_result |> should.be_ok
  let schema = schema_result |> result.unwrap(panic)

  // Get the SocialGrainGallery type (has did field)
  let gallery_type = schema.types
    |> dict.get("SocialGrainGallery")

  gallery_type |> should.be_ok
  let gallery = gallery_type |> result.unwrap(panic)

  // Should have the viewer follow field
  let has_viewer_follow = case gallery {
    schema.ObjectType(_, fields, _) ->
      list.any(fields, fn(f) {
        f.name == "viewerSocialGrainGraphFollowViaDid"
      })
    _ -> False
  }

  has_viewer_follow |> should.be_true
}
```

**Step 2: Run test to verify it fails**

Run: `cd lexicon_graphql && gleam test -- --filter=generates_viewer_field_for_did`
Expected: FAIL - no viewerSocialGrainGraphFollowViaDid field

**Step 3: Build DID-based viewer field map**

Add function to discover collections with DID-typed subject fields:

```gleam
/// Build a map of collections with DID-typed subject fields
/// Returns: Dict(source_nsid, List(field_name))
fn build_did_subject_collection_map(
  metas: List(collection_meta.CollectionMeta),
) -> dict.Dict(String, List(String)) {
  list.fold(metas, dict.new(), fn(acc, meta) {
    case meta.did_subject_fields {
      [] -> acc
      fields -> dict.insert(acc, meta.nsid, fields)
    }
  })
}
```

**Step 4: Add DID-based viewer field generation**

```gleam
/// Build viewer state fields for DID-based relationships
/// These are added to any type with a did field
fn build_viewer_fields_for_did_subjects(
  did_subject_collections: dict.Dict(String, List(String)),
  viewer_state_fetcher: option.Option(dataloader.ViewerStateFetcher),
  object_types: dict.Dict(String, schema.Type),
  record_types: List(RecordType),
) -> List(schema.Field) {
  // For each collection with DID-typed subject fields
  dict.to_list(did_subject_collections)
  |> list.flat_map(fn(entry) {
    let #(source_nsid, did_fields) = entry

    // Find the source type name
    let source_type_name = case
      list.find(record_types, fn(rt) { rt.nsid == source_nsid })
    {
      Ok(rt) -> rt.type_name
      Error(_) -> nsid.to_type_name(source_nsid)
    }

    // Get the source object type
    let source_object_type = case dict.get(object_types, source_nsid) {
      Ok(obj_type) -> obj_type
      Error(_) -> schema.string_type()
    }

    // Generate a viewer field for each DID field
    list.map(did_fields, fn(field_name) {
      let viewer_field_name =
        "viewer"
        <> source_type_name
        <> "Via"
        <> capitalize_first(field_name)

      let return_type = schema.nullable(source_object_type)

      schema.field(
        viewer_field_name,
        return_type,
        "Viewer's "
          <> source_nsid
          <> " record where "
          <> field_name
          <> " matches this record's DID (null if not authenticated or no record)",
        fn(ctx) {
          // Get viewer DID from context
          case get_viewer_did_from_context(ctx) {
            Ok(viewer_did) -> {
              // Get parent DID (extract from URI)
              case get_field_from_context(ctx, "uri") {
                Ok(parent_uri) -> {
                  case extract_did_from_uri(parent_uri) {
                    option.Some(parent_did) -> {
                      case viewer_state_fetcher {
                        option.Some(fetcher) -> {
                          case
                            dataloader.batch_fetch_viewer_state(
                              viewer_did,
                              source_nsid,
                              field_name,
                              [parent_did],
                              fetcher,
                            )
                          {
                            Ok(results) -> {
                              case dict.get(results, parent_did) {
                                Ok(record) -> Ok(record)
                                Error(_) -> Ok(value.Null)
                              }
                            }
                            Error(_) -> Ok(value.Null)
                          }
                        }
                        option.None -> Ok(value.Null)
                      }
                    }
                    option.None -> Ok(value.Null)
                  }
                }
                Error(_) -> Ok(value.Null)
              }
            }
            Error(_) -> Ok(value.Null)
          }
        },
      )
    })
  })
}
```

**Step 5: Wire DID viewer fields into type building**

Add DID-based viewer fields to types that have a `did` field:

```gleam
// Only add DID viewer fields if this type has a did field
let did_viewer_fields = case has_did_field(record_type) {
  True ->
    build_viewer_fields_for_did_subjects(
      did_subject_collections,
      viewer_state_fetcher,
      pass3_object_types,
      record_types,
    )
  False -> []
}

let all_fields =
  list.flatten([
    base_fields,
    forward_join_fields,
    reverse_join_fields,
    did_join_fields,
    viewer_fields,
    did_viewer_fields,
  ])
```

**Step 6: Run test to verify it passes**

Run: `cd lexicon_graphql && gleam test -- --filter=generates_viewer_field_for_did`
Expected: PASS

**Step 7: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/schema/database.gleam lexicon_graphql/test/viewer_state_test.gleam
git commit -m "$(cat <<'EOF'
feat(schema): generate viewer fields for DID-based subjects

For collections where subject is a DID (e.g., follows), generate viewer
fields on any type with a did field. For example, SocialGrainGallery
gets viewerSocialGrainGraphFollowViaDid.
EOF
)"
```

---

## Task 5: Update Server to Pass Viewer State Fetcher

**Files:**
- Modify: `server/src/server/graphql/schema.gleam`
- Modify: `server/src/server/database/viewer_state.gleam` (create)

**Step 1: Create viewer state database module**

Create `server/src/server/database/viewer_state.gleam`:

```gleam
import gleam/dict.{type Dict}
import gleam/list
import gleam/pgo
import gleam/option.{type Option, None, Some}
import gleam/dynamic
import swell/value

/// Fetch viewer state for multiple parent records
/// Returns a dict mapping parent keys to the viewer's record (if any)
pub fn fetch_viewer_state(
  db: pgo.Connection,
  viewer_did: String,
  collection: String,
  reference_field: String,
  parent_keys: List(String),
) -> Result(Dict(String, value.Value), String) {
  // Build query to find viewer's records
  // SELECT * FROM {collection} WHERE did = $1 AND {reference_field} IN ($2, $3, ...)
  let table_name = collection_to_table_name(collection)

  let placeholders =
    list.index_map(parent_keys, fn(_, i) { "$" <> int.to_string(i + 2) })
    |> string.join(", ")

  let query =
    "SELECT uri, data FROM "
    <> table_name
    <> " WHERE did = $1 AND data->>'"
    <> reference_field
    <> "' IN ("
    <> placeholders
    <> ")"

  // Execute query and build result dict
  // ... (implementation details)

  Ok(dict.new())
}

fn collection_to_table_name(collection: String) -> String {
  collection
  |> string.replace(".", "_")
}
```

**Step 2: Wire fetcher into schema building**

Update `server/src/server/graphql/schema.gleam` to pass the viewer state fetcher.

**Step 3: Commit**

```bash
git add server/src/server/database/viewer_state.gleam server/src/server/graphql/schema.gleam
git commit -m "$(cat <<'EOF'
feat(server): implement viewer state database fetcher

Add database query to fetch viewer's records for multiple parent keys,
used by viewer fields to show relationship state.
EOF
)"
```

---

## Task 6: Integration Test

**Files:**
- Test: `server/test/viewer_state_integration_test.gleam`

**Step 1: Write integration test**

```gleam
import gleeunit/should
import server/test_helpers
import gleam/json

pub fn viewer_favorite_shows_liked_state_test() {
  use ctx <- test_helpers.with_authenticated_context("did:plc:viewer")

  // Create a gallery
  let gallery_uri = test_helpers.create_gallery(ctx, "Test Gallery")

  // Query without liking - should be null
  let query = "
    query {
      socialGrainGallery(where: { uri: { eq: \"" <> gallery_uri <> "\" } }) {
        edges {
          node {
            uri
            viewerSocialGrainFavoriteViaSubject { uri }
          }
        }
      }
    }
  "

  let result = test_helpers.execute_query(ctx, query)
  result.data
  |> json.get(["socialGrainGallery", "edges", 0, "node", "viewerSocialGrainFavoriteViaSubject"])
  |> should.equal(json.Null)

  // Like the gallery
  let like_uri = test_helpers.create_favorite(ctx, gallery_uri)

  // Query again - should show the like
  let result2 = test_helpers.execute_query(ctx, query)
  result2.data
  |> json.get(["socialGrainGallery", "edges", 0, "node", "viewerSocialGrainFavoriteViaSubject", "uri"])
  |> should.equal(json.String(like_uri))
}

pub fn viewer_follow_shows_followed_state_test() {
  use ctx <- test_helpers.with_authenticated_context("did:plc:viewer")

  // Create a gallery by another user
  let gallery_uri = test_helpers.create_gallery_by(ctx, "did:plc:author", "Author's Gallery")

  // Query without following - should be null
  let query = "
    query {
      socialGrainGallery(where: { uri: { eq: \"" <> gallery_uri <> "\" } }) {
        edges {
          node {
            uri
            did
            viewerSocialGrainGraphFollowViaDid { uri }
          }
        }
      }
    }
  "

  let result = test_helpers.execute_query(ctx, query)
  result.data
  |> json.get(["socialGrainGallery", "edges", 0, "node", "viewerSocialGrainGraphFollowViaDid"])
  |> should.equal(json.Null)

  // Follow the author
  let follow_uri = test_helpers.create_follow(ctx, "did:plc:author")

  // Query again - should show the follow
  let result2 = test_helpers.execute_query(ctx, query)
  result2.data
  |> json.get(["socialGrainGallery", "edges", 0, "node", "viewerSocialGrainGraphFollowViaDid", "uri"])
  |> should.equal(json.String(follow_uri))
}
```

**Step 2: Run integration tests**

Run: `cd server && gleam test -- --filter=viewer_`
Expected: PASS

**Step 3: Commit**

```bash
git add server/test/viewer_state_integration_test.gleam
git commit -m "$(cat <<'EOF'
test: add viewer state integration tests

Verify that viewer fields correctly show liked/followed state
when authenticated.
EOF
)"
```

---

## Summary

| Pattern | Field Name | Subject Type | Join On |
|---------|------------|--------------|---------|
| AT-URI | `viewer{TypeName}Via{FieldName}` | `format: "at-uri"` | `subject = parent.uri` |
| DID | `viewer{TypeName}Via{FieldName}` | `format: "did"` | `subject = parent.did` |

Both return single nullable object and require viewer authentication.
