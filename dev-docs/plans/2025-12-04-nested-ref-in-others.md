# Nested Refs in Others Object Types Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix refs within `defs.others` object types (like `facet#main.index` referencing `facet#byteSlice`) falling back to `String` instead of resolving to their proper object types.

**Architecture:** Update `extract_ref_object_types` in `builder.gleam` to use `map_property_type_with_context` instead of the simple `map_type`, and ensure proper build order so dependency types exist before types that reference them.

**Tech Stack:** Gleam, lexicon_graphql library, swell GraphQL library

---

## Background

When building object types from `defs.others`, the current code in `builder.gleam:extract_ref_object_types` uses `type_mapper.map_type(property.type_)` at line 199:

```gleam
let graphql_type = type_mapper.map_type(property.type_)
```

This doesn't resolve refs - it just returns `String` for `type: "ref"`. The result is that when `app.bsky.richtext.facet#main` has an `index` field referencing `#byteSlice`, it becomes `index: String` instead of `index: AppBskyRichtextFacetByteSlice`.

The fix requires:
1. Building object types from `others` in dependency order (types without refs first)
2. Using `map_property_type_with_context` to properly resolve refs

---

### Task 1: Add failing test for nested refs in others

**Files:**
- Modify: `lexicon_graphql/test/local_ref_test.gleam`

**Step 1: Write the failing test**

Add this test at the end of the file:

```gleam
/// Test that refs within "others" object types resolve correctly.
/// This tests the pattern where facet#main has an index field that refs #byteSlice.
pub fn nested_ref_in_others_resolves_test() {
  let lexicon =
    types.Lexicon(
      id: "app.bsky.richtext.facet",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "object", key: None, properties: [
            #(
              "index",
              types.Property(
                type_: "ref",
                required: True,
                format: None,
                ref: Some("#byteSlice"),
                refs: None,
                items: None,
              ),
            ),
            #(
              "features",
              types.Property(
                type_: "array",
                required: True,
                format: None,
                ref: None,
                refs: None,
                items: Some(types.ArrayItems(
                  type_: "string",
                  ref: None,
                  refs: None,
                )),
              ),
            ),
          ]),
        ),
        others: dict.from_list([
          #(
            "byteSlice",
            types.Object(
              types.ObjectDef(type_: "object", required_fields: [], properties: [
                #(
                  "byteStart",
                  types.Property(
                    type_: "integer",
                    required: True,
                    format: None,
                    ref: None,
                    refs: None,
                    items: None,
                  ),
                ),
                #(
                  "byteEnd",
                  types.Property(
                    type_: "integer",
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

      // The byteSlice type should exist
      string.contains(serialized, "type AppBskyRichtextFacetByteSlice")
      |> should.be_true

      // The index field should be AppBskyRichtextFacetByteSlice, NOT String
      string.contains(serialized, "index: AppBskyRichtextFacetByteSlice!")
      |> should.be_true
    }
    Error(_) -> should.fail()
  }
}

/// Test cross-lexicon ref from record to another lexicon's others type
pub fn cross_lexicon_ref_to_others_test() {
  // Post lexicon references facet lexicon's main type
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
                refs: None,
                items: None,
              ),
            ),
            #(
              "facets",
              types.Property(
                type_: "array",
                required: False,
                format: None,
                ref: None,
                refs: None,
                items: Some(types.ArrayItems(
                  type_: "ref",
                  ref: Some("app.bsky.richtext.facet"),
                  refs: None,
                )),
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let facet_lexicon =
    types.Lexicon(
      id: "app.bsky.richtext.facet",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "object", key: None, properties: [
            #(
              "index",
              types.Property(
                type_: "ref",
                required: True,
                format: None,
                ref: Some("#byteSlice"),
                refs: None,
                items: None,
              ),
            ),
          ]),
        ),
        others: dict.from_list([
          #(
            "byteSlice",
            types.Object(
              types.ObjectDef(type_: "object", required_fields: [], properties: [
                #(
                  "byteStart",
                  types.Property(
                    type_: "integer",
                    required: True,
                    format: None,
                    ref: None,
                    refs: None,
                    items: None,
                  ),
                ),
                #(
                  "byteEnd",
                  types.Property(
                    type_: "integer",
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

  let result = builder.build_schema([post_lexicon, facet_lexicon])
  should.be_ok(result)

  case result {
    Ok(schema_val) -> {
      let all_types = introspection.get_all_schema_types(schema_val)
      let serialized = sdl.print_types(all_types)

      // The facet type should exist
      string.contains(serialized, "type AppBskyRichtextFacet")
      |> should.be_true

      // The byteSlice type should exist
      string.contains(serialized, "type AppBskyRichtextFacetByteSlice")
      |> should.be_true

      // facets field should be array of AppBskyRichtextFacet
      string.contains(serialized, "facets: [AppBskyRichtextFacet!]")
      |> should.be_true

      // index field in facet should be byteSlice type, NOT String
      string.contains(serialized, "index: AppBskyRichtextFacetByteSlice!")
      |> should.be_true
    }
    Error(_) -> should.fail()
  }
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`

Expected: FAIL - `index: String!` instead of `index: AppBskyRichtextFacetByteSlice!`

**Step 3: Commit failing test**

```bash
git add lexicon_graphql/test/local_ref_test.gleam
git commit -m "test: add failing tests for nested refs in others object types"
```

---

### Task 2: Refactor extract_ref_object_types to build in dependency order

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/schema/builder.gleam:183-229`

**Step 1: Replace extract_ref_object_types function**

Replace the entire `extract_ref_object_types` function with this new implementation:

```gleam
/// Extract object types from lexicon "others" defs (e.g., #artist, #aspectRatio)
/// Returns a dict keyed by full ref like "fm.teal.alpha.feed.track#artist"
///
/// Builds types in two passes:
/// 1. First pass: build types that have no ref dependencies (leaf types)
/// 2. Second pass: build types that reference other types (can now resolve refs)
fn extract_ref_object_types(
  lexicons: List(Lexicon),
) -> dict.Dict(String, schema.Type) {
  // Collect all object defs with their metadata
  let all_defs =
    list.flat_map(lexicons, fn(lexicon) {
      let types.Lexicon(id, types.Defs(_, others)) = lexicon
      dict.to_list(others)
      |> list.filter_map(fn(entry) {
        let #(def_name, def) = entry
        case def {
          types.Object(obj_def) -> {
            let full_ref = id <> "#" <> def_name
            Ok(#(full_ref, id, obj_def))
          }
          types.Record(_) -> Error(Nil)
        }
      })
    })

  // Partition into types with refs and types without refs
  let #(with_refs, without_refs) =
    list.partition(all_defs, fn(entry) {
      let #(_, _, obj_def) = entry
      has_ref_properties(obj_def)
    })

  // First pass: build leaf types (no ref dependencies)
  let leaf_types =
    list.fold(without_refs, dict.new(), fn(acc, entry) {
      let #(full_ref, lexicon_id, obj_def) = entry
      let object_type = build_others_object_type(full_ref, lexicon_id, obj_def, acc)
      dict.insert(acc, full_ref, object_type)
    })

  // Second pass: build types that have refs (can now resolve to leaf types)
  list.fold(with_refs, leaf_types, fn(acc, entry) {
    let #(full_ref, lexicon_id, obj_def) = entry
    let object_type = build_others_object_type(full_ref, lexicon_id, obj_def, acc)
    dict.insert(acc, full_ref, object_type)
  })
}

/// Check if an ObjectDef has any ref properties
fn has_ref_properties(obj_def: types.ObjectDef) -> Bool {
  list.any(obj_def.properties, fn(prop) {
    let #(_, property) = prop
    property.type_ == "ref" || property.type_ == "union" || case property.items {
      option.Some(items) -> items.type_ == "ref" || option.is_some(items.refs)
      option.None -> False
    }
  })
}

/// Build a single object type from an ObjectDef in "others"
fn build_others_object_type(
  full_ref: String,
  lexicon_id: String,
  obj_def: types.ObjectDef,
  existing_types: dict.Dict(String, schema.Type),
) -> schema.Type {
  let type_name = nsid.to_type_name(string.replace(full_ref, "#", "."))

  let lexicon_fields =
    list.map(obj_def.properties, fn(prop) {
      let #(name, property) = prop
      let graphql_type =
        type_mapper.map_property_type_with_context(
          property,
          existing_types,
          type_name,
          name,
          lexicon_id,
        )

      // Apply required/non-null wrapper
      let field_type = case list.contains(obj_def.required_fields, name) {
        True -> schema.non_null(graphql_type)
        False -> graphql_type
      }

      schema.field(
        name,
        field_type,
        "Field from object def",
        fn(_ctx) { Ok(value.Null) },
      )
    })

  // GraphQL requires at least one field - add placeholder for empty objects
  let fields = case lexicon_fields {
    [] -> [
      schema.field(
        "_",
        schema.boolean_type(),
        "Placeholder field for empty object type",
        fn(_ctx) { Ok(value.Boolean(True)) },
      ),
    ]
    _ -> lexicon_fields
  }

  schema.object_type(type_name, "Object type: " <> full_ref, fields)
}
```

**Step 2: Add string import if not present**

Check imports at top of file and ensure `import gleam/string` is present.

**Step 3: Run build**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`

Expected: Build succeeds

**Step 4: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/schema/builder.gleam
git commit -m "feat(builder): resolve refs within others object types

Refactor extract_ref_object_types to:
1. Build leaf types (no ref dependencies) first
2. Build types with refs second (can now resolve)
3. Use map_property_type_with_context instead of map_type

This fixes nested refs like facet#main.index -> facet#byteSlice"
```

---

### Task 3: Run tests and verify fix

**Files:**
- None (verification only)

**Step 1: Run all tests**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`

Expected: All tests pass including new `nested_ref_in_others_resolves_test` and `cross_lexicon_ref_to_others_test`

**Step 2: If tests fail, add debug output**

If tests fail, temporarily add to the test:

```gleam
import gleam/io
// ... in test ...
io.println(serialized)
```

**Step 3: Commit passing tests**

```bash
git add -A
git commit -m "test: verify nested refs in others resolve correctly"
```

---

### Task 4: Verify with live GraphQL query

**Files:**
- None (verification only)

**Step 1: Rebuild the server**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`

**Step 2: Query the schema via MCP**

Use `mcp__quickslice__execute_query` to verify:

```graphql
query {
  appBskyFeedPost {
    edges {
      node {
        facets {
          index {
            byteStart
            byteEnd
          }
        }
      }
    }
  }
}
```

Expected: Query succeeds with `index` returning an object with `byteStart` and `byteEnd` fields (integers), not the raw JSON object.

**Step 3: Introspect the type**

```graphql
{
  __type(name: "AppBskyRichtextFacet") {
    fields {
      name
      type {
        name
        kind
        ofType {
          name
          kind
        }
      }
    }
  }
}
```

Expected: `index` field shows type `AppBskyRichtextFacetByteSlice` (OBJECT), not `String` (SCALAR)

---

## Summary

| Task | Description | Files |
|------|-------------|-------|
| 1 | Add failing tests for nested refs | `test/local_ref_test.gleam` |
| 2 | Refactor extract_ref_object_types | `builder.gleam` |
| 3 | Verify tests pass | - |
| 4 | Verify with live query | - |
