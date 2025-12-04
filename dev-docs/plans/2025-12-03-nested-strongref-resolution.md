# Nested StrongRef Resolution Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add `*Resolved` fields to nested object types containing strongRef/at-uri fields, enabling thread traversal via `reply.parentResolved`.

**Architecture:** Extend `object_builder.gleam` to scan object properties for strongRef/at-uri fields and generate corresponding `*Resolved` fields with resolvers that use the existing DataLoader infrastructure.

**Tech Stack:** Gleam, lexicon_graphql library, swell GraphQL library

---

## Background

Currently, forward join resolution (`*Resolved` fields) only works at the record level. When a record has a top-level field like `pinnedPost` (strongRef), we generate `pinnedPostResolved`.

For nested object types like `AppBskyFeedPostReplyRef`, the strongRef fields (`parent`, `root`) don't get `*Resolved` fields because:
1. `collection_meta.extract_metadata()` only scans top-level record properties
2. `build_forward_join_fields_with_types()` only runs on record types
3. `object_builder.build_object_type()` doesn't know about forward joins

**Current schema (broken):**
```graphql
type AppBskyFeedPostReplyRef {
  parent: String!  # Should be ComAtprotoRepoStrongRef!
  root: String!    # Should be ComAtprotoRepoStrongRef!
}
```

**Desired schema:**
```graphql
type AppBskyFeedPostReplyRef {
  parent: ComAtprotoRepoStrongRef!
  parentResolved: Record
  root: ComAtprotoRepoStrongRef!
  rootResolved: Record
}
```

---

## Prerequisites

This plan depends on the local ref resolution fix (`2025-12-03-local-ref-resolution.md`) being completed first. That fix ensures `#replyRef` resolves to `AppBskyFeedPostReplyRef` object type instead of `String`.

---

### Task 1: Add failing test for nested forward join resolution

**Files:**
- Create: `lexicon_graphql/test/nested_forward_join_test.gleam`

**Step 1: Write the failing test**

```gleam
/// Tests for nested forward join resolution in schema builder
///
/// Verifies that object types containing strongRef fields get *Resolved fields
import gleam/dict
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import lexicon_graphql/schema/builder
import lexicon_graphql/types
import swell/introspection
import swell/sdl

/// Test that nested strongRef fields get *Resolved fields
pub fn nested_strongref_gets_resolved_field_test() {
  // Create a lexicon with a record that has a nested object containing strongRef
  let lexicon =
    types.Lexicon(
      id: "app.bsky.feed.post",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: Some("tid"), properties: [
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
                ref: Some("#replyRef"),
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
              types.ObjectDef(type_: "object", required_fields: ["parent", "root"], properties: [
                #(
                  "parent",
                  types.Property(
                    type_: "ref",
                    required: True,
                    format: None,
                    ref: Some("com.atproto.repo.strongRef"),
                    refs: None,
                    items: None,
                  ),
                ),
                #(
                  "root",
                  types.Property(
                    type_: "ref",
                    required: True,
                    format: None,
                    ref: Some("com.atproto.repo.strongRef"),
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

  // Also need the strongRef lexicon
  let strong_ref_lexicon =
    types.Lexicon(
      id: "com.atproto.repo.strongRef",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "object", key: None, properties: [
            #(
              "uri",
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
              "cid",
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

  let result = builder.build_schema([lexicon, strong_ref_lexicon])
  should.be_ok(result)

  case result {
    Ok(schema_val) -> {
      let all_types = introspection.get_all_schema_types(schema_val)
      let serialized = sdl.print_types(all_types)

      // The replyRef object type should have parentResolved and rootResolved fields
      string.contains(serialized, "parentResolved")
      |> should.be_true

      string.contains(serialized, "rootResolved")
      |> should.be_true
    }
    Error(_) -> should.fail()
  }
}

/// Test that at-uri format fields in nested objects also get resolved
pub fn nested_at_uri_gets_resolved_field_test() {
  let lexicon =
    types.Lexicon(
      id: "test.record",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: Some("tid"), properties: [
            #(
              "reference",
              types.Property(
                type_: "ref",
                required: False,
                format: None,
                ref: Some("#refObject"),
                refs: None,
                items: None,
              ),
            ),
          ]),
        ),
        others: dict.from_list([
          #(
            "refObject",
            types.Object(
              types.ObjectDef(type_: "object", required_fields: ["target"], properties: [
                #(
                  "target",
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

      // The refObject type should have targetResolved field
      string.contains(serialized, "targetResolved")
      |> should.be_true
    }
    Error(_) -> should.fail()
  }
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`

Expected: FAIL - no `parentResolved` field exists on the nested object type

**Step 3: Commit failing test**

```bash
git add lexicon_graphql/test/nested_forward_join_test.gleam
git commit -m "test: add failing tests for nested strongRef resolution"
```

---

### Task 2: Add forward join field identification to object_builder

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam`

**Step 1: Add ForwardJoinField type and identification function**

Add after the imports (around line 17):

```gleam
/// Type of forward join field found in an object
pub type NestedForwardJoinField {
  NestedStrongRefField(name: String)
  NestedAtUriField(name: String)
}

/// Identify forward join fields in object properties
/// Returns list of fields that can be resolved to other records
pub fn identify_forward_join_fields(
  properties: List(#(String, types.Property)),
) -> List(NestedForwardJoinField) {
  list.filter_map(properties, fn(prop) {
    let #(name, property) = prop
    case property.type_, property.ref, property.format {
      // strongRef field
      "ref", option.Some(ref), _ if ref == "com.atproto.repo.strongRef" ->
        Ok(NestedStrongRefField(name))
      // at-uri string field
      "string", _, option.Some(fmt) if fmt == "at-uri" ->
        Ok(NestedAtUriField(name))
      _, _, _ -> Error(Nil)
    }
  })
}
```

**Step 2: Run build to verify syntax**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`

Expected: Build succeeds

**Step 3: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam
git commit -m "feat(object_builder): add forward join field identification"
```

---

### Task 3: Add parameters for forward join resolution to object_builder

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam`

**Step 1: Update build_object_type signature**

Update the `build_object_type` function (around line 38) to accept optional batch_fetcher and generic_record_type:

```gleam
/// Build a GraphQL object type from an ObjectDef
/// object_types_dict is used to resolve refs to other object types
/// batch_fetcher and generic_record_type are optional - when provided, *Resolved fields are added
pub fn build_object_type(
  obj_def: types.ObjectDef,
  type_name: String,
  lexicon_id: String,
  object_types_dict: Dict(String, schema.Type),
  batch_fetcher: option.Option(BatchFetcher),
  generic_record_type: option.Option(schema.Type),
) -> schema.Type {
  let lexicon_fields =
    build_object_fields(
      obj_def.properties,
      lexicon_id,
      object_types_dict,
      type_name,
    )

  // Build forward join fields if we have the necessary dependencies
  let forward_join_fields = case batch_fetcher, generic_record_type {
    option.Some(_fetcher), option.Some(record_type) -> {
      let join_fields = identify_forward_join_fields(obj_def.properties)
      build_nested_forward_join_fields(join_fields, record_type, batch_fetcher)
    }
    _, _ -> []
  }

  // Combine regular fields with forward join fields
  let all_fields = list.append(lexicon_fields, forward_join_fields)

  // GraphQL requires at least one field - add placeholder for empty objects
  let fields = case all_fields {
    [] -> [
      schema.field(
        "_",
        schema.boolean_type(),
        "Placeholder field for empty object type",
        fn(_ctx) { Ok(value.Boolean(True)) },
      ),
    ]
    _ -> all_fields
  }

  schema.object_type(type_name, "Object type from lexicon definition", fields)
}
```

**Step 2: Add BatchFetcher type alias and import**

Add near the top of the file after imports:

```gleam
import lexicon_graphql/query/dataloader

/// Batch fetcher type alias for convenience
pub type BatchFetcher = dataloader.BatchFetcher
```

**Step 3: Update build_all_object_types signature**

Update to accept and pass through the new parameters:

```gleam
/// Build a dict of all object types from the registry
/// When batch_fetcher and generic_record_type are provided, nested forward joins are enabled
pub fn build_all_object_types(
  registry: lexicon_registry.Registry,
  batch_fetcher: option.Option(BatchFetcher),
  generic_record_type: option.Option(schema.Type),
) -> Dict(String, schema.Type) {
  let object_refs = lexicon_registry.get_all_object_refs(registry)
  let sorted_refs = sort_refs_dependencies_first(object_refs)

  list.fold(sorted_refs, dict.new(), fn(acc, ref) {
    case lexicon_registry.get_object_def(registry, ref) {
      option.Some(obj_def) -> {
        let type_name = ref_to_type_name(ref)
        let lexicon_id = lexicon_registry.lexicon_id_from_ref(ref)
        let object_type = build_object_type(
          obj_def,
          type_name,
          lexicon_id,
          acc,
          batch_fetcher,
          generic_record_type,
        )
        dict.insert(acc, ref, object_type)
      }
      option.None -> acc
    }
  })
}
```

**Step 4: Update internal call site**

Find the call to `build_object_type` inside `build_all_object_types` (around line 163) and update it to pass the new parameters.

**Step 5: Run build (expect failures from callers)**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`

Expected: Compile errors from callers missing new arguments

**Step 6: Commit WIP**

```bash
git add lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam
git commit -m "feat(object_builder): add batch_fetcher and generic_record_type params (WIP)"
```

---

### Task 4: Implement build_nested_forward_join_fields

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam`

**Step 1: Add the forward join field builder function**

Add after `identify_forward_join_fields`:

```gleam
/// Build *Resolved fields for nested forward joins
fn build_nested_forward_join_fields(
  join_fields: List(NestedForwardJoinField),
  generic_record_type: schema.Type,
  batch_fetcher: option.Option(BatchFetcher),
) -> List(schema.Field) {
  list.map(join_fields, fn(join_field) {
    let field_name = case join_field {
      NestedStrongRefField(name) -> name
      NestedAtUriField(name) -> name
    }

    schema.field(
      field_name <> "Resolved",
      generic_record_type,
      "Forward join to referenced record",
      fn(ctx) {
        // Extract the field value from the parent object
        case ctx.data {
          option.Some(value.Object(fields)) -> {
            case list.key_find(fields, field_name) {
              Ok(field_value) -> {
                // Extract URI using uri_extractor
                case uri_extractor.extract_uri(value_to_dynamic(field_value)) {
                  option.Some(uri) -> {
                    // Use batch fetcher to resolve the record
                    case batch_fetcher {
                      option.Some(fetcher) -> {
                        case dataloader.batch_fetch_by_uri([uri], fetcher) {
                          Ok(results) -> {
                            case dict.get(results, uri) {
                              Ok(record) -> Ok(record)
                              Error(_) -> Ok(value.Null)
                            }
                          }
                          Error(_) -> Ok(value.Null)
                        }
                      }
                      option.None -> Ok(value.String(uri))
                    }
                  }
                  option.None -> Ok(value.Null)
                }
              }
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    )
  })
}

/// Convert a GraphQL Value to Dynamic for uri_extractor
fn value_to_dynamic(val: value.Value) -> dynamic.Dynamic {
  // Use the same pattern as dataloader.gleam
  unsafe_coerce_to_dynamic(val)
}

@external(erlang, "object_builder_ffi", "identity")
fn unsafe_coerce_to_dynamic(value: a) -> dynamic.Dynamic
```

**Step 2: Add required imports**

Add at the top of the file:

```gleam
import gleam/dynamic
import lexicon_graphql/internal/lexicon/uri_extractor
```

**Step 3: Create the FFI file**

Create `lexicon_graphql/src/object_builder_ffi.erl`:

```erlang
-module(object_builder_ffi).
-export([identity/1]).

identity(X) -> X.
```

**Step 4: Run build**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`

Expected: Still compile errors from callers, but object_builder.gleam should compile

**Step 5: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam
git add lexicon_graphql/src/object_builder_ffi.erl
git commit -m "feat(object_builder): implement nested forward join field builder"
```

---

### Task 5: Update builder.gleam callers

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/schema/builder.gleam`

**Step 1: Find calls to object_builder functions**

Search for `object_builder.build_all_object_types` and `object_builder.build_object_type` calls.

**Step 2: Update calls to pass None for new parameters**

For the basic schema builder (without database), pass `option.None` for both new parameters since there's no batch_fetcher available:

```gleam
// When calling build_all_object_types:
object_builder.build_all_object_types(registry, option.None, option.None)

// When calling build_object_type:
object_builder.build_object_type(obj_def, type_name, lexicon_id, acc, option.None, option.None)
```

**Step 3: Run build**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`

Expected: Build succeeds (or errors from database.gleam)

**Step 4: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/schema/builder.gleam
git commit -m "fix(builder): update object_builder calls with new parameters"
```

---

### Task 6: Update database.gleam with two-pass object type building

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/schema/database.gleam`

**Step 1: Identify where object types are built**

Find the section where `ref_object_types` or `object_builder.build_all_object_types` is called.

**Step 2: Implement two-pass build**

The pattern should be:

```gleam
// Pass 1: Build object types WITHOUT forward joins (no batch_fetcher, no Record union yet)
let basic_object_types = object_builder.build_all_object_types(
  registry,
  option.None,
  option.None,
)

// ... build record types and Record union ...

// Pass 2: Rebuild object types WITH forward joins (now we have batch_fetcher and Record union)
let complete_object_types = object_builder.build_all_object_types(
  registry,
  batch_fetcher,
  option.Some(record_union),
)
```

**Step 3: Update the schema building flow**

Integrate the two-pass approach into the existing multi-pass schema building in `build_schema_with_fetcher`.

**Step 4: Run build**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`

Expected: Build succeeds

**Step 5: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/schema/database.gleam
git commit -m "feat(database): implement two-pass object type building for nested forward joins"
```

---

### Task 7: Run tests and verify

**Files:**
- None (verification only)

**Step 1: Run all tests**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`

Expected: All tests pass including the new `nested_forward_join_test.gleam`

**Step 2: If tests fail, debug**

Check the SDL output to verify the schema has the expected fields:

```gleam
// Temporary debug in test
io.println(serialized)
```

Look for:
- `type AppBskyFeedPostReplyRef` exists
- Has fields: `parent`, `parentResolved`, `root`, `rootResolved`
- `parentResolved` returns `Record` type

**Step 3: Commit passing state**

```bash
git add -A
git commit -m "feat: add *Resolved fields to nested object types with strongRef

Nested object types containing strongRef or at-uri fields now get
corresponding *Resolved fields that resolve to the actual record.

This enables thread traversal via reply.parentResolved.

- Add identify_forward_join_fields to object_builder
- Add build_nested_forward_join_fields with resolver logic
- Implement two-pass object type building in database.gleam
- Add batch_fetcher and generic_record_type params to object_builder

Example query now works:
  reply {
    parentResolved {
      ... on AppBskyFeedPost { text }
    }
  }"
```

---

### Task 8: Add integration test for thread traversal

**Files:**
- Modify: `server/test/join_integration_test.gleam`

**Step 1: Add test for nested forward join resolution**

Add a new test that:
1. Creates posts with reply references
2. Queries `reply.parentResolved`
3. Verifies the parent post data is returned

```gleam
pub fn nested_forward_join_resolves_reply_parent_test() {
  // Create a root post
  // Create a reply post with reply.parent pointing to root
  // Query the reply with reply.parentResolved
  // Verify the root post data is returned
}
```

**Step 2: Run integration tests**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`

**Step 3: Commit**

```bash
git add server/test/join_integration_test.gleam
git commit -m "test: add integration test for nested forward join resolution"
```

---

### Task 9: Verify with MCP introspection

**Files:**
- None (verification only)

**Step 1: Query the schema**

Use the quickslice MCP to introspect:

```graphql
{
  __type(name: "AppBskyFeedPostReplyRef") {
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

Expected fields:
- `parent: ComAtprotoRepoStrongRef!`
- `parentResolved: Record`
- `root: ComAtprotoRepoStrongRef!`
- `rootResolved: Record`

**Step 2: Test actual resolution**

```graphql
{
  appBskyFeedPost(first: 1, where: { reply: { isNotNull: true } }) {
    edges {
      node {
        text
        reply {
          parent { uri cid }
          parentResolved {
            ... on AppBskyFeedPost {
              uri
              text
            }
          }
        }
      }
    }
  }
}
```

---

## Summary

| Task | Description | Files |
|------|-------------|-------|
| 1 | Add failing tests | `test/nested_forward_join_test.gleam` |
| 2 | Add field identification | `object_builder.gleam` |
| 3 | Add new parameters | `object_builder.gleam` |
| 4 | Implement field builder | `object_builder.gleam`, `object_builder_ffi.erl` |
| 5 | Update builder.gleam | `builder.gleam` |
| 6 | Two-pass build in database | `database.gleam` |
| 7 | Run tests and verify | - |
| 8 | Add integration test | `join_integration_test.gleam` |
| 9 | Verify with MCP | - |

## Dependencies

- Requires `2025-12-03-local-ref-resolution.md` to be completed first (so `#replyRef` resolves to object type)
