# Object Type Build Order Fix Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix union type resolution in `AppBskyRichtextFacet.features` by ensuring `#fragment` object types are built before main object types that reference them.

**Architecture:** Sort object refs before building so that refs containing `#` (like `app.bsky.richtext.facet#mention`) are processed before refs without `#` (like `app.bsky.richtext.facet`). This ensures dependencies are built first.

**Tech Stack:** Gleam, lexicon_graphql library, swell GraphQL library

---

## Background

When `build_all_object_types` processes refs in arbitrary order (from `dict.keys`), a main object type like `app.bsky.richtext.facet` may be built before its `#fragment` dependencies (`#mention`, `#link`, `#tag`). When the main type's `features` field tries to resolve the union refs, they aren't in the dict yet, causing fallback to `String`.

**Current behavior:** `features: [String!]!`
**Expected behavior:** `features: [AppBskyRichtextFacetMentionOrAppBskyRichtextFacetLinkOrAppBskyRichtextFacetTag!]!`

---

### Task 1: Add failing test for object type build order

**Files:**
- Create: `lexicon_graphql/test/object_build_order_test.gleam`

**Step 1: Write the failing test**

```gleam
/// Tests for object type build order
///
/// Verifies that #fragment refs are built before main object types
/// so that unions in main types can resolve their member types
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import lexicon_graphql/internal/graphql/object_builder
import lexicon_graphql/internal/lexicon/registry
import lexicon_graphql/types
import swell/schema

/// Test that main object types with union array fields resolve correctly
/// when the union members are #fragment refs in the same lexicon
pub fn union_array_refs_resolve_to_object_types_test() {
  // Create a lexicon like app.bsky.richtext.facet with:
  // - main object type with features: array of union [#mention, #link]
  // - others: mention, link object definitions
  let lexicon =
    types.Lexicon(
      id: "app.bsky.richtext.facet",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "object", key: None, properties: [
            #(
              "features",
              types.Property(
                type_: "array",
                required: True,
                format: None,
                ref: None,
                refs: None,
                items: Some(types.ArrayItems(
                  type_: "union",
                  ref: None,
                  refs: Some(["#mention", "#link"]),
                )),
              ),
            ),
          ]),
        ),
        others: dict.from_list([
          #(
            "mention",
            types.Object(
              types.ObjectDef(type_: "object", required_fields: ["did"], properties: [
                #(
                  "did",
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
          ),
          #(
            "link",
            types.Object(
              types.ObjectDef(type_: "object", required_fields: ["uri"], properties: [
                #(
                  "uri",
                  types.Property(
                    type_: "string",
                    required: True,
                    format: Some("uri"),
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

  // Build registry and object types
  let reg = registry.from_lexicons([lexicon])
  let object_types = object_builder.build_all_object_types(reg)

  // The main type should exist
  let main_type_result = dict.get(object_types, "app.bsky.richtext.facet")
  should.be_ok(main_type_result)

  // The #fragment types should exist
  let mention_type_result =
    dict.get(object_types, "app.bsky.richtext.facet#mention")
  should.be_ok(mention_type_result)

  let link_type_result = dict.get(object_types, "app.bsky.richtext.facet#link")
  should.be_ok(link_type_result)

  // Check the main type's features field is a union, not String
  case main_type_result {
    Ok(main_type) -> {
      let type_name = schema.type_name(main_type)
      should.equal(type_name, "AppBskyRichtextFacet")

      // Get the fields and find "features"
      let fields = schema.get_fields(main_type)
      let features_field =
        list.find(fields, fn(f) { schema.field_name(f) == "features" })

      case features_field {
        Ok(field) -> {
          // The field type should be a list containing a union, not [String!]
          let field_type = schema.field_type(field)
          let inner_type_name = get_list_inner_type_name(field_type)

          // Should NOT be "String" - should be a union type name
          should.be_false(inner_type_name == "String")

          // Should contain "Mention" and "Link" in the union name
          should.be_true(string.contains(inner_type_name, "Mention"))
          should.be_true(string.contains(inner_type_name, "Link"))
        }
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

/// Helper to get the inner type name from a list type
/// [NonNull[Union]] -> "UnionName"
fn get_list_inner_type_name(t: schema.Type) -> String {
  // Unwrap List -> NonNull -> actual type
  case schema.unwrap_type(t) {
    Ok(inner) ->
      case schema.unwrap_type(inner) {
        Ok(innermost) -> schema.type_name(innermost)
        Error(_) -> schema.type_name(inner)
      }
    Error(_) -> schema.type_name(t)
  }
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test 2>&1 | grep -A 5 "union_array_refs_resolve"`

Expected: FAIL - the `features` field type name is `String` instead of containing `Mention` and `Link`

**Step 3: Commit failing test**

```bash
git add lexicon_graphql/test/object_build_order_test.gleam
git commit -m "test: add failing test for object type build order"
```

---

### Task 2: Add sort_refs_dependencies_first helper function

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam`

**Step 1: Add the helper function after the imports (around line 15)**

Add after the imports section:

```gleam
/// Sort refs so that #fragment refs come before main refs
/// This ensures dependencies are built first
/// e.g., "app.bsky.richtext.facet#mention" before "app.bsky.richtext.facet"
fn sort_refs_dependencies_first(refs: List(String)) -> List(String) {
  list.sort(refs, fn(a, b) {
    let a_has_hash = string.contains(a, "#")
    let b_has_hash = string.contains(b, "#")
    case a_has_hash, b_has_hash {
      // Both have # or both don't - sort alphabetically for determinism
      True, True -> string.compare(a, b)
      False, False -> string.compare(a, b)
      // # refs come first
      True, False -> order.Lt
      False, True -> order.Gt
    }
  })
}
```

**Step 2: Add the order import**

At line 7, add the import:

```gleam
import gleam/order
```

**Step 3: Run build to verify no syntax errors**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`

Expected: Build succeeds (function not yet used)

**Step 4: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam
git commit -m "feat(object_builder): add sort_refs_dependencies_first helper"
```

---

### Task 3: Use sorted refs in build_all_object_types

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam:115-123`

**Step 1: Update build_all_object_types to sort refs**

Replace the current implementation (around lines 115-123):

```gleam
pub fn build_all_object_types(
  registry: lexicon_registry.Registry,
) -> Dict(String, schema.Type) {
  let object_refs = lexicon_registry.get_all_object_refs(registry)

  // Sort refs so #fragment refs are built before main refs
  // This ensures union member types exist when main types reference them
  let sorted_refs = sort_refs_dependencies_first(object_refs)

  // Build all object types in dependency order
  list.fold(sorted_refs, dict.new(), fn(acc, ref) {
    case lexicon_registry.get_object_def(registry, ref) {
      option.Some(obj_def) -> {
        // Generate a GraphQL type name from the ref
        // e.g., "social.grain.defs#aspectRatio" -> "SocialGrainDefsAspectRatio"
        let type_name = ref_to_type_name(ref)
        let lexicon_id = lexicon_registry.lexicon_id_from_ref(ref)
        // Pass acc as the object_types_dict so we can resolve refs to previously built types
        let object_type = build_object_type(obj_def, type_name, lexicon_id, acc)
        dict.insert(acc, ref, object_type)
      }
      option.None -> acc
    }
  })
}
```

**Step 2: Run tests**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`

Expected: All tests pass including the new `union_array_refs_resolve_to_object_types_test`

**Step 3: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam
git commit -m "fix(object_builder): sort refs to build dependencies first

Refs containing # (like app.bsky.richtext.facet#mention) are now built
before refs without # (like app.bsky.richtext.facet). This ensures
union member types exist when main object types reference them.

Fixes: AppBskyRichtextFacet.features now resolves to union type instead of String"
```

---

### Task 4: Verify with MCP introspection

**Files:**
- None (verification only)

**Step 1: Query the quickslice MCP to verify the fix**

```graphql
{
  __type(name: "AppBskyRichtextFacet") {
    fields {
      name
      type {
        kind
        ofType {
          kind
          ofType {
            kind
            ofType {
              name
              kind
            }
          }
        }
      }
    }
  }
}
```

Expected: `features` field shows a UNION type (not SCALAR/String)

**Step 2: Verify union member types exist**

```graphql
{
  mention: __type(name: "AppBskyRichtextFacetMention") { name kind }
  link: __type(name: "AppBskyRichtextFacetLink") { name kind }
  tag: __type(name: "AppBskyRichtextFacetTag") { name kind }
}
```

Expected: All three types exist as OBJECT types

---

## Summary

| Task | Description | Files |
|------|-------------|-------|
| 1 | Add failing test | `test/object_build_order_test.gleam` |
| 2 | Add sort helper | `object_builder.gleam` |
| 3 | Use sorted refs | `object_builder.gleam` |
| 4 | Verify with MCP | - |
