# Fix StrongRef Resolution in Nested Object Types

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix the bug where `com.atproto.repo.strongRef` refs in nested object types (like `#replyRef`) resolve to `String` instead of `ComAtprotoRepoStrongRef`.

**Architecture:** Reorder schema building phases so main-level object types (like `com.atproto.repo.strongRef`) are built BEFORE nested "others" object types (like `#replyRef`). This ensures strongRef is available in the type dictionary when processing refs inside nested objects.

**Tech Stack:** Gleam, lexicon_graphql library, swell GraphQL schema

---

### Task 1: Write Failing Test for StrongRef in Nested Object

**Files:**
- Create: `lexicon_graphql/test/strongref_nested_resolution_test.gleam`

**Step 1: Write the failing test**

Create a test that verifies strongRef fields inside nested "others" object types resolve to the object type, not String.

```gleam
/// Tests for strongRef resolution in nested object types
///
/// Verifies that com.atproto.repo.strongRef refs in "others" object definitions
/// (like #replyRef) resolve to ComAtprotoRepoStrongRef, not String
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import lexicon_graphql/schema/builder
import lexicon_graphql/types
import swell/schema

/// Test that strongRef fields in nested objects resolve to object type, not String
/// This reproduces the bug: app.bsky.feed.post#replyRef.parent should be
/// ComAtprotoRepoStrongRef, not String
pub fn strongref_in_nested_object_resolves_to_object_type_test() {
  // Create com.atproto.repo.strongRef lexicon (main-level object type)
  let strongref_lexicon =
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
                format: Some("cid"),
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

  // Create a post lexicon with #replyRef that references strongRef
  let post_lexicon =
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
              types.ObjectDef(
                type_: "object",
                required_fields: ["parent", "root"],
                properties: [
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
                ],
              ),
            ),
          ),
        ]),
      ),
    )

  // Build schema with both lexicons
  let result = builder.build_schema([strongref_lexicon, post_lexicon])
  should.be_ok(result)

  let assert Ok(built_schema) = result
  let query_type = schema.query_type(built_schema)
  let all_types = schema.get_all_types(built_schema)

  // Find the AppBskyFeedPostReplyRef type
  let reply_ref_type =
    list.find(all_types, fn(t) {
      schema.type_name(t) == "AppBskyFeedPostReplyRef"
    })
  should.be_ok(reply_ref_type)

  let assert Ok(reply_ref) = reply_ref_type
  let fields = schema.get_fields(reply_ref)

  // Find the "parent" field
  let parent_field =
    list.find(fields, fn(f) { schema.field_name(f) == "parent" })
  should.be_ok(parent_field)

  let assert Ok(parent) = parent_field
  let parent_type = schema.field_type(parent)

  // Unwrap NonNull wrapper to get inner type
  let inner_type = case schema.inner_type(parent_type) {
    Some(t) -> t
    None -> parent_type
  }

  // The type should be ComAtprotoRepoStrongRef, NOT String
  let type_name = schema.type_name(inner_type)
  should.not_equal(type_name, "String")
  should.equal(type_name, "ComAtprotoRepoStrongRef")
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test -- --only strongref_in_nested_object_resolves_to_object_type_test`

Expected: FAIL - the `parent` field type will be `String` instead of `ComAtprotoRepoStrongRef`

**Step 3: Commit the failing test**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql
git add test/strongref_nested_resolution_test.gleam
git commit -m "test: add failing test for strongRef resolution in nested objects"
```

---

### Task 2: Reorder Schema Building Phases

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/schema/builder.gleam:43-82`

**Step 1: Update build_schema function**

Change the order of operations so `extract_object_type_lexicons` runs FIRST (with an empty dict), then `extract_ref_object_types` runs SECOND (with the object type lexicons available).

Replace lines 43-82 in `builder.gleam`:

```gleam
pub fn build_schema(lexicons: List(Lexicon)) -> Result(schema.Schema, String) {
  case lexicons {
    [] -> Error("Cannot build schema from empty lexicon list")
    _ -> {
      // FIRST: Extract object-type lexicons (like com.atproto.repo.strongRef)
      // These have no dependencies on "others" types, so build them first
      let object_type_lexicons =
        extract_object_type_lexicons(lexicons, dict.new())

      // SECOND: Extract ref object types from lexicon "others" (e.g., #replyRef)
      // Now these can resolve refs to object-type lexicons like strongRef
      let ref_object_types =
        extract_ref_object_types_with_existing(lexicons, object_type_lexicons)

      // Merge object_type_lexicons with ref_object_types
      let all_object_types = dict.merge(object_type_lexicons, ref_object_types)

      // Extract record types from lexicons, passing all object types for field resolution
      let record_types = extract_record_types(lexicons, all_object_types)

      // Build object types dict including record types
      let record_object_types = build_object_types_dict(record_types)
      let object_types = dict.merge(all_object_types, record_object_types)

      // Build the query type with fields for each record (not object types)
      let query_type = build_query_type(record_types, object_types)

      // Build the mutation type with stub resolvers, using shared object types
      let mutation_type =
        mutation_builder.build_mutation_type(
          lexicons,
          object_types,
          option.None,
          option.None,
          option.None,
          option.None,
        )

      // Create the schema with queries and mutations
      Ok(schema.schema(query_type, option.Some(mutation_type)))
    }
  }
}
```

**Step 2: Run build to verify it fails (function doesn't exist yet)**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`

Expected: FAIL - `extract_ref_object_types_with_existing` is not defined

---

### Task 3: Create extract_ref_object_types_with_existing Function

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/schema/builder.gleam:188-231`

**Step 1: Add new function that accepts existing types**

Add this new function after `extract_ref_object_types` (around line 232):

```gleam
/// Extract object types from lexicon "others" defs, with pre-existing types available
/// This variant accepts an existing_types dict so refs to object-type lexicons
/// (like com.atproto.repo.strongRef) can be resolved
fn extract_ref_object_types_with_existing(
  lexicons: List(Lexicon),
  existing_types: dict.Dict(String, schema.Type),
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
  // Start with existing_types so we can resolve external refs
  let leaf_types =
    list.fold(without_refs, existing_types, fn(acc, entry) {
      let #(full_ref, lexicon_id, obj_def) = entry
      let object_type =
        build_others_object_type(full_ref, lexicon_id, obj_def, acc)
      dict.insert(acc, full_ref, object_type)
    })

  // Second pass: build types that have refs (can now resolve to leaf types AND existing types)
  let all_types =
    list.fold(with_refs, leaf_types, fn(acc, entry) {
      let #(full_ref, lexicon_id, obj_def) = entry
      let object_type =
        build_others_object_type(full_ref, lexicon_id, obj_def, acc)
      dict.insert(acc, full_ref, object_type)
    })

  // Return only the newly built types (exclude existing_types keys)
  dict.filter(all_types, fn(key, _value) {
    case dict.get(existing_types, key) {
      Ok(_) -> False
      Error(_) -> True
    }
  })
}
```

**Step 2: Run build to verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`

Expected: SUCCESS - should compile without errors

**Step 3: Run the test**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test -- --only strongref_in_nested_object_resolves_to_object_type_test`

Expected: PASS - the `parent` field should now be `ComAtprotoRepoStrongRef`

**Step 4: Run all tests to check for regressions**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`

Expected: All tests pass

**Step 5: Commit the fix**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql
git add src/lexicon_graphql/schema/builder.gleam
git commit -m "fix: resolve strongRef refs in nested object types

Reorder schema building phases so object-type lexicons (like
com.atproto.repo.strongRef) are built before 'others' object types
(like #replyRef). This ensures strongRef is available in the type
dictionary when processing refs inside nested objects.

Previously, #replyRef.parent would resolve to String because strongRef
wasn't built yet. Now it correctly resolves to ComAtprotoRepoStrongRef."
```

---

### Task 4: Verify with MCP Server

**Step 1: Run the quickslice server and verify the fix**

Use the quickslice MCP to introspect the schema and verify `AppBskyFeedPostReplyRef` now has correct types:

```graphql
{
  __type(name: "AppBskyFeedPostReplyRef") {
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

Expected: `parent` and `root` fields should have type `ComAtprotoRepoStrongRef` (not `String`)

**Step 2: Squash commits if desired**

```bash
git rebase -i HEAD~2
# Squash the test commit into the fix commit
```

---

## Summary

This fix changes the schema building order from:
1. `extract_ref_object_types()` - builds "others" ❌ strongRef not available
2. `extract_object_type_lexicons()` - builds main object types

To:
1. `extract_object_type_lexicons()` - builds main object types FIRST
2. `extract_ref_object_types_with_existing()` - builds "others" ✅ strongRef available

The key insight is that object-type lexicons (type: "object" at main level) have no dependencies on "others" types, so they can safely be built first. Then "others" types can resolve refs to both sibling "others" types AND main object types.
