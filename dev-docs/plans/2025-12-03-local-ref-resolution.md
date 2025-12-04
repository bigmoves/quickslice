# Local Ref Resolution Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix local refs (like `#replyRef`) being resolved as `String` instead of their proper object types.

**Architecture:** Add `lexicon_id` parameter to `map_property_type_with_context` and expand local refs (refs starting with `#`) to fully-qualified refs before looking them up in the object types dict.

**Tech Stack:** Gleam, lexicon_graphql library, swell GraphQL library

---

## Background

Local refs in AT Protocol lexicons use the format `#defName` to reference definitions within the same lexicon. For example, in `app.bsky.feed.post`:

```json
"reply": {
  "ref": "#replyRef",
  "type": "ref"
}
```

The `replyRef` object is defined in the same lexicon's `defs.others`. The object types dict is keyed by fully-qualified refs like `app.bsky.feed.post#replyRef`, but the lookup is using the raw `#replyRef`, causing a miss and fallback to `String`.

---

### Task 1: Add failing test for local ref resolution

**Files:**
- Create: `lexicon_graphql/test/local_ref_test.gleam`

**Step 1: Write the failing test**

```gleam
/// Tests for local ref resolution in schema builder
///
/// Verifies that refs like "#replyRef" resolve to their object types
import gleam/dict
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import lexicon_graphql/schema/builder
import lexicon_graphql/types
import swell/introspection
import swell/sdl

/// Test that a local ref (starting with #) resolves to its object type
pub fn local_ref_resolves_to_object_type_test() {
  // Create a lexicon with a record that has a local ref field
  let lexicon =
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
                refs: None,
                items: None,
              ),
            ),
            #(
              "reply",
              types.Property(
                type_: "ref",
                required: False,
                format: None,
                ref: Some("#replyRef"),  // Local ref!
                refs: None,
                items: None,
              ),
            ),
          ]),
        ),
        others: dict.from_list([
          #(
            "replyRef",
            types.Object(
              types.ObjectDef(type_: "object", required_fields: [], properties: [
                #(
                  "root",
                  types.Property(
                    type_: "string",
                    required: True,
                    format: None,
                    ref: None,
                    refs: None,
                    items: None,
                  ),
                ),
                #(
                  "parent",
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
          ),
        ]),
      ),
    )

  let result = builder.build_schema([lexicon])
  should.be_ok(result)

  case result {
    Ok(schema_val) -> {
      let all_types = introspection.get_all_schema_types(schema_val)
      let serialized = sdl.print_types(all_types)

      // The replyRef object type should exist
      string.contains(serialized, "type AppBskyFeedPostReplyRef")
      |> should.be_true

      // The reply field should be AppBskyFeedPostReplyRef, NOT String
      string.contains(serialized, "reply: AppBskyFeedPostReplyRef")
      |> should.be_true
    }
    Error(_) -> should.fail()
  }
}

/// Test that local refs in arrays also resolve correctly
pub fn local_ref_in_array_resolves_to_object_type_test() {
  let lexicon =
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
                refs: None,
                items: None,
              ),
            ),
            #(
              "entities",
              types.Property(
                type_: "array",
                required: False,
                format: None,
                ref: None,
                refs: None,
                items: Some(types.ArrayItems(
                  type_: "ref",
                  ref: Some("#entity"),  // Local ref in array!
                  refs: None,
                )),
              ),
            ),
          ]),
        ),
        others: dict.from_list([
          #(
            "entity",
            types.Object(
              types.ObjectDef(type_: "object", required_fields: [], properties: [
                #(
                  "type",
                  types.Property(
                    type_: "string",
                    required: True,
                    format: None,
                    ref: None,
                    refs: None,
                    items: None,
                  ),
                ),
                #(
                  "value",
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
          ),
        ]),
      ),
    )

  let result = builder.build_schema([lexicon])
  should.be_ok(result)

  case result {
    Ok(schema_val) -> {
      let all_types = introspection.get_all_schema_types(schema_val)
      let serialized = sdl.print_types(all_types)

      // The entity object type should exist
      string.contains(serialized, "type AppBskyFeedPostEntity")
      |> should.be_true

      // The entities field should be [AppBskyFeedPostEntity!], NOT [String!]
      string.contains(serialized, "entities: [AppBskyFeedPostEntity!]")
      |> should.be_true
    }
    Error(_) -> should.fail()
  }
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`

Expected: FAIL - `reply: String` instead of `reply: AppBskyFeedPostReplyRef`

**Step 3: Commit failing test**

```bash
git add lexicon_graphql/test/local_ref_test.gleam
git commit -m "test: add failing tests for local ref resolution"
```

---

### Task 2: Add expand_ref helper to type_mapper

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/internal/graphql/type_mapper.gleam`

**Step 1: Add expand_ref function at end of file**

Add after line 338 (after `capitalize_first` function):

```gleam
/// Expand a local ref to a fully-qualified ref
/// "#replyRef" with lexicon_id "app.bsky.feed.post" -> "app.bsky.feed.post#replyRef"
/// External refs pass through unchanged
pub fn expand_ref(ref: String, lexicon_id: String) -> String {
  case string.starts_with(ref, "#") {
    True -> lexicon_id <> ref
    False -> ref
  }
}
```

**Step 2: Run tests to verify no regression**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`

Expected: All existing tests pass (new function not yet used)

**Step 3: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/internal/graphql/type_mapper.gleam
git commit -m "feat(type_mapper): add expand_ref helper for local refs"
```

---

### Task 3: Update map_property_type_with_context signature

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/internal/graphql/type_mapper.gleam:240-275`

**Step 1: Add lexicon_id parameter and expand refs**

Replace the existing `map_property_type_with_context` function:

```gleam
/// Maps a lexicon Property to a GraphQL type, with parent context for union naming.
/// Handles arrays, refs, and unions with proper type resolution.
/// lexicon_id is used to expand local refs (e.g., "#replyRef" -> "app.bsky.feed.post#replyRef")
pub fn map_property_type_with_context(
  property: types.Property,
  object_types: Dict(String, schema.Type),
  parent_type_name: String,
  field_name: String,
  lexicon_id: String,
) -> schema.Type {
  case property.type_ {
    "array" -> {
      // Expand local refs in array items before lookup
      let expanded_items = case property.items {
        option.Some(types.ArrayItems(type_: item_type, ref: item_ref, refs: item_refs)) -> {
          option.Some(types.ArrayItems(
            type_: item_type,
            ref: option.map(item_ref, fn(r) { expand_ref(r, lexicon_id) }),
            refs: option.map(item_refs, fn(rs) {
              list.map(rs, fn(r) { expand_ref(r, lexicon_id) })
            }),
          ))
        }
        option.None -> option.None
      }
      map_array_type(expanded_items, object_types)
    }
    "ref" -> {
      case property.ref {
        option.Some(ref_str) -> {
          // Expand local ref before lookup
          let full_ref = expand_ref(ref_str, lexicon_id)
          case dict.get(object_types, full_ref) {
            Ok(obj_type) -> obj_type
            Error(_) -> schema.string_type()
          }
        }
        option.None -> schema.string_type()
      }
    }
    "union" -> {
      case property.refs {
        option.Some(refs) -> {
          // Expand local refs in union before lookup
          let expanded_refs = list.map(refs, fn(r) { expand_ref(r, lexicon_id) })
          build_property_union_type(
            expanded_refs,
            object_types,
            parent_type_name,
            field_name,
          )
        }
        option.None -> schema.string_type()
      }
    }
    _ -> map_type(property.type_)
  }
}
```

**Step 2: Add list import if not present**

Check the imports at the top of the file and add `import gleam/list` if not already there.

**Step 3: Run tests (expect failures from callers)**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`

Expected: Compile error - callers missing new `lexicon_id` argument

**Step 4: Commit (WIP)**

```bash
git add lexicon_graphql/src/lexicon_graphql/internal/graphql/type_mapper.gleam
git commit -m "feat(type_mapper): add lexicon_id param to expand local refs (WIP)"
```

---

### Task 4: Update map_property_type to pass empty lexicon_id

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/internal/graphql/type_mapper.gleam:231-238`

**Step 1: Update map_property_type wrapper**

Replace the existing `map_property_type` function:

```gleam
/// Maps a lexicon Property to a GraphQL type.
/// Handles arrays specially by looking at the items field.
/// Note: This version doesn't expand local refs. Use map_property_type_with_context
/// with lexicon_id for proper local ref resolution.
pub fn map_property_type(
  property: types.Property,
  object_types: Dict(String, schema.Type),
) -> schema.Type {
  map_property_type_with_context(property, object_types, "", "", "")
}
```

**Step 2: Run build**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`

Expected: Still compile errors from other callers

---

### Task 5: Update builder.gleam callers

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/schema/builder.gleam`

**Step 1: Update build_fields_with_context to accept and pass lexicon_id**

Find `build_fields_with_context` (around line 119) and update it:

```gleam
/// Build GraphQL fields from lexicon properties with parent type context
fn build_fields_with_context(
  properties: List(#(String, Property)),
  ref_object_types: dict.Dict(String, schema.Type),
  parent_type_name: String,
  lexicon_id: String,
) -> List(schema.Field) {
  // Add standard AT Proto fields
  let standard_fields = [
    schema.field("uri", schema.string_type(), "Record URI", fn(_ctx) {
      Ok(value.String("at://did:plc:example/collection/rkey"))
    }),
    schema.field("cid", schema.string_type(), "Record CID", fn(_ctx) {
      Ok(value.String("bafyreicid"))
    }),
    schema.field("did", schema.string_type(), "DID of record author", fn(_ctx) {
      Ok(value.String("did:plc:example"))
    }),
    schema.field(
      "indexedAt",
      schema.string_type(),
      "When record was indexed",
      fn(_ctx) { Ok(value.String("2024-01-01T00:00:00Z")) },
    ),
  ]

  // Build fields from lexicon properties with context
  let lexicon_fields =
    list.map(properties, fn(prop) {
      let #(name, property) = prop
      let graphql_type =
        type_mapper.map_property_type_with_context(
          property,
          ref_object_types,
          parent_type_name,
          name,
          lexicon_id,
        )

      schema.field(name, graphql_type, "Field from lexicon", fn(_ctx) {
        Ok(value.Null)
      })
    })

  // Combine standard and lexicon fields
  list.append(standard_fields, lexicon_fields)
}
```

**Step 2: Update parse_lexicon to pass lexicon_id**

Find `parse_lexicon` (around line 93) and update the call:

```gleam
/// Parse a single lexicon into a RecordType
fn parse_lexicon(
  lexicon: Lexicon,
  ref_object_types: dict.Dict(String, schema.Type),
) -> Result(RecordType, Nil) {
  case lexicon {
    types.Lexicon(
      id,
      types.Defs(option.Some(types.RecordDef("record", _, properties)), _),
    ) -> {
      let type_name = nsid.to_type_name(id)
      let field_name = nsid.to_field_name(id)
      let fields =
        build_fields_with_context(properties, ref_object_types, type_name, id)

      Ok(RecordType(
        nsid: id,
        type_name: type_name,
        field_name: field_name,
        fields: fields,
      ))
    }
    _ -> Error(Nil)
  }
}
```

**Step 3: Update build_object_type_fields to pass lexicon_id**

Find `build_object_type_fields` (around line 268) and update it:

```gleam
/// Build fields for object types (simplified - no standard AT Proto fields)
fn build_object_type_fields(
  properties: List(#(String, Property)),
  ref_object_types: dict.Dict(String, schema.Type),
  lexicon_id: String,
) -> List(schema.Field) {
  list.map(properties, fn(prop) {
    let #(name, property) = prop
    let graphql_type = type_mapper.map_property_type_with_context(
      property,
      ref_object_types,
      "",
      name,
      lexicon_id,
    )
    schema.field(name, graphql_type, "Field from lexicon", fn(_ctx) {
      Ok(value.Null)
    })
  })
}
```

**Step 4: Update extract_object_type_lexicons caller**

Find the call to `build_object_type_fields` in `extract_object_type_lexicons` (around line 257) and update it:

```gleam
fn extract_object_type_lexicons(
  lexicons: List(Lexicon),
  ref_object_types: dict.Dict(String, schema.Type),
) -> dict.Dict(String, schema.Type) {
  list.fold(lexicons, dict.new(), fn(acc, lexicon) {
    case lexicon {
      types.Lexicon(
        id,
        types.Defs(option.Some(types.RecordDef("object", _, properties)), _),
      ) -> {
        let type_name = nsid.to_type_name(id)
        let fields = build_object_type_fields(properties, ref_object_types, id)
        let object_type =
          schema.object_type(type_name, "Object type: " <> id, fields)
        dict.insert(acc, id, object_type)
      }
      _ -> acc
    }
  })
}
```

**Step 5: Run build**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`

Expected: Build succeeds

**Step 6: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/schema/builder.gleam
git commit -m "feat(builder): pass lexicon_id for local ref expansion"
```

---

### Task 6: Update type_mapper_test.gleam

**Files:**
- Modify: `lexicon_graphql/test/type_mapper_test.gleam`

**Step 1: Update map_property_union_type_test**

Find the test around line 153 and update the call to include lexicon_id:

```gleam
pub fn map_property_union_type_test() {
  // Create object types that the union will reference
  let images_type = schema.object_type("AppBskyEmbedImages", "Images embed", [])
  let video_type = schema.object_type("AppBskyEmbedVideo", "Video embed", [])

  let object_types =
    dict.new()
    |> dict.insert("app.bsky.embed.images", images_type)
    |> dict.insert("app.bsky.embed.video", video_type)

  let property =
    types.Property(
      type_: "union",
      required: False,
      format: option.None,
      ref: option.None,
      refs: option.Some([
        "app.bsky.embed.images",
        "app.bsky.embed.video",
      ]),
      items: option.None,
    )

  let result =
    type_mapper.map_property_type_with_context(
      property,
      object_types,
      "AppBskyFeedPost",
      "embed",
      "app.bsky.feed.post",
    )

  // Should be a union type, not String
  schema.type_name(result)
  |> should.equal("AppBskyFeedPostEmbed")
}
```

**Step 2: Run tests**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`

Expected: Existing tests pass

**Step 3: Commit**

```bash
git add lexicon_graphql/test/type_mapper_test.gleam
git commit -m "test(type_mapper): update test for new lexicon_id parameter"
```

---

### Task 7: Run full test suite and verify local ref tests pass

**Files:**
- None (verification only)

**Step 1: Run all tests**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`

Expected: All tests pass including the new `local_ref_test.gleam` tests

**Step 2: If tests fail, debug**

Check the SDL output to see what's generated. Add debug output if needed:

```gleam
// Temporary debug in test
io.println(serialized)
```

**Step 3: Commit final passing state**

```bash
git add -A
git commit -m "feat: resolve local refs (#replyRef) to object types

Local refs in AT Protocol lexicons (refs starting with #) now properly
resolve to their object type definitions instead of falling back to String.

- Add expand_ref helper to type_mapper
- Add lexicon_id parameter to map_property_type_with_context
- Update builder.gleam to pass lexicon_id through the call chain
- Handles both direct refs and refs within arrays

Fixes: reply field in app.bsky.feed.post now resolves to
AppBskyFeedPostReplyRef instead of String"
```

---

### Task 8: Verify with MCP introspection

**Files:**
- None (verification only)

**Step 1: Query the quickslice MCP to verify the fix**

After deploying/rebuilding, use the MCP to introspect the schema:

```graphql
{
  __type(name: "AppBskyFeedPost") {
    fields {
      name
      type {
        name
        kind
      }
    }
  }
}
```

Expected: `reply` field shows `AppBskyFeedPostReplyRef` (OBJECT), not `String` (SCALAR)

---

## Summary

| Task | Description | Files |
|------|-------------|-------|
| 1 | Add failing tests | `test/local_ref_test.gleam` |
| 2 | Add `expand_ref` helper | `type_mapper.gleam` |
| 3 | Update `map_property_type_with_context` signature | `type_mapper.gleam` |
| 4 | Update `map_property_type` wrapper | `type_mapper.gleam` |
| 5 | Update builder.gleam callers | `builder.gleam` |
| 6 | Update type_mapper tests | `type_mapper_test.gleam` |
| 7 | Verify all tests pass | - |
| 8 | Verify with MCP | - |
