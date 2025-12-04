# Array Fields in Object Types Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make array fields in object types (like `AppBskyEmbedImages.images`) resolve to proper array types instead of `String`.

**Architecture:** Pass lexicon ID context through object type building so shorthand refs (`#image`) can be expanded to fully-qualified refs (`app.bsky.embed.images#image`) before lookup in the object types dictionary.

**Tech Stack:** Gleam, swell (GraphQL library), lexicon_graphql package

---

### Task 1: Add ref expansion helper to object_builder

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam`

**Step 1: Write the helper function**

Add at the bottom of the file before the closing:

```gleam
/// Expand a shorthand ref to fully-qualified ref
/// "#image" with lexicon_id "app.bsky.embed.images" -> "app.bsky.embed.images#image"
fn expand_ref(ref: String, lexicon_id: String) -> String {
  case string.starts_with(ref, "#") {
    True -> lexicon_id <> ref
    False -> ref
  }
}
```

**Step 2: Add string import if not present**

Check imports at top of file, add if missing:
```gleam
import gleam/string
```

**Step 3: Run build to verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`
Expected: Compiles with no errors (warning about unused function is OK)

**Step 4: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam
git commit -m "feat(object_builder): add expand_ref helper for shorthand refs"
```

---

### Task 2: Add lexicon ID parser to registry

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/internal/lexicon/registry.gleam`

**Step 1: Add the helper function**

Add after the existing `parse_ref` function:

```gleam
/// Extract lexicon ID from a fully-qualified ref
/// "app.bsky.embed.images#image" -> "app.bsky.embed.images"
/// "app.bsky.embed.images" -> "app.bsky.embed.images"
pub fn lexicon_id_from_ref(ref: String) -> String {
  case string.split(ref, "#") {
    [lexicon_id, _] -> lexicon_id
    _ -> ref
  }
}
```

**Step 2: Run build to verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`
Expected: Compiles with no errors

**Step 3: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/internal/lexicon/registry.gleam
git commit -m "feat(registry): add lexicon_id_from_ref helper"
```

---

### Task 3: Add array items expansion helper to object_builder

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam`

**Step 1: Add the helper function**

Add after `expand_ref`:

```gleam
/// Expand shorthand refs in ArrayItems
fn expand_array_items(
  items: types.ArrayItems,
  lexicon_id: String,
) -> types.ArrayItems {
  types.ArrayItems(
    type_: items.type_,
    ref: option.map(items.ref, fn(r) { expand_ref(r, lexicon_id) }),
    refs: option.map(items.refs, fn(rs) {
      list.map(rs, fn(r) { expand_ref(r, lexicon_id) })
    }),
  )
}
```

**Step 2: Run build to verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`
Expected: Compiles with no errors (warning about unused function is OK)

**Step 3: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam
git commit -m "feat(object_builder): add expand_array_items helper"
```

---

### Task 4: Update build_object_fields to accept lexicon_id

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam`

**Step 1: Update function signature**

Change `build_object_fields` from:
```gleam
fn build_object_fields(
  properties: List(#(String, types.Property)),
  object_types_dict: Dict(String, schema.Type),
) -> List(schema.Field) {
```

To:
```gleam
fn build_object_fields(
  properties: List(#(String, types.Property)),
  lexicon_id: String,
  object_types_dict: Dict(String, schema.Type),
) -> List(schema.Field) {
```

**Step 2: Run build to see call site errors**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`
Expected: Error about missing argument in `build_object_type`

**Step 3: Commit (WIP)**

```bash
git add lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam
git commit -m "wip: update build_object_fields signature to accept lexicon_id"
```

---

### Task 5: Update build_object_type to pass lexicon_id

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam`

**Step 1: Update function signature**

Change `build_object_type` from:
```gleam
pub fn build_object_type(
  obj_def: types.ObjectDef,
  type_name: String,
  object_types_dict: Dict(String, schema.Type),
) -> schema.Type {
  let fields = build_object_fields(obj_def.properties, object_types_dict)
```

To:
```gleam
pub fn build_object_type(
  obj_def: types.ObjectDef,
  type_name: String,
  lexicon_id: String,
  object_types_dict: Dict(String, schema.Type),
) -> schema.Type {
  let fields = build_object_fields(obj_def.properties, lexicon_id, object_types_dict)
```

**Step 2: Run build to see call site errors**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`
Expected: Error about missing argument in `build_all_object_types`

**Step 3: Commit (WIP)**

```bash
git add lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam
git commit -m "wip: update build_object_type signature to accept lexicon_id"
```

---

### Task 6: Update build_all_object_types to extract and pass lexicon_id

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam`

**Step 1: Update the fold body**

Change in `build_all_object_types`:
```gleam
list.fold(object_refs, dict.new(), fn(acc, ref) {
  case lexicon_registry.get_object_def(registry, ref) {
    option.Some(obj_def) -> {
      let type_name = ref_to_type_name(ref)
      let object_type = build_object_type(obj_def, type_name, acc)
      dict.insert(acc, ref, object_type)
    }
    option.None -> acc
  }
})
```

To:
```gleam
list.fold(object_refs, dict.new(), fn(acc, ref) {
  case lexicon_registry.get_object_def(registry, ref) {
    option.Some(obj_def) -> {
      let type_name = ref_to_type_name(ref)
      let lexicon_id = lexicon_registry.lexicon_id_from_ref(ref)
      let object_type = build_object_type(obj_def, type_name, lexicon_id, acc)
      dict.insert(acc, ref, object_type)
    }
    option.None -> acc
  }
})
```

**Step 2: Run build to verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`
Expected: Compiles with no errors

**Step 3: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam
git commit -m "feat(object_builder): pass lexicon_id through build chain"
```

---

### Task 7: Handle array types in build_object_fields

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam`

**Step 1: Update the property mapping**

Change in `build_object_fields`:
```gleam
list.map(properties, fn(prop) {
  let #(name, types.Property(type_, required, format, ref, _refs, _items)) =
    prop

  // Map the type, using the object_types_dict to resolve refs
  let graphql_type =
    type_mapper.map_type_with_registry(type_, format, ref, object_types_dict)
```

To:
```gleam
list.map(properties, fn(prop) {
  let #(name, types.Property(type_, required, format, ref, _refs, items)) =
    prop

  // Map the type, handling arrays specially to resolve item refs
  let graphql_type = case type_ {
    "array" -> {
      let expanded_items = case items {
        option.Some(arr_items) ->
          option.Some(expand_array_items(arr_items, lexicon_id))
        option.None -> option.None
      }
      type_mapper.map_array_type(expanded_items, object_types_dict)
    }
    _ -> type_mapper.map_type_with_registry(type_, format, ref, object_types_dict)
  }
```

**Step 2: Run build to verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`
Expected: Compiles with no errors

**Step 3: Run tests**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`
Expected: All tests pass

**Step 4: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam
git commit -m "feat(object_builder): handle array types with expanded refs"
```

---

### Task 8: Rebuild server and verify fix

**Files:**
- None (verification only)

**Step 1: Rebuild server**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles with no errors

**Step 2: Restart server and test via MCP**

After restarting the server, run introspection query:
```graphql
{
  __type(name: "AppBskyEmbedImages") {
    fields {
      name
      type { kind name ofType { kind name } }
    }
  }
}
```

Expected: `images` field should have type `LIST` with `ofType` containing `AppBskyEmbedImagesImage`

**Step 3: Verify nested type exists**

```graphql
{
  __type(name: "AppBskyEmbedImagesImage") {
    name
    fields { name }
  }
}
```

Expected: Type exists with fields `alt`, `image`, `aspectRatio`

**Step 4: Final commit**

```bash
git add -A
git commit -m "feat: array fields in object types resolve to proper types

- Pass lexicon ID through object type building chain
- Expand shorthand refs (#image -> lexicon#image) at build time
- Handle array types by calling map_array_type with expanded items
- Enables querying embed images with proper nested types"
```

---

## Verification Query

After implementation, this query should work:

```graphql
{
  appBskyFeedPost {
    edges {
      node {
        embed {
          ... on AppBskyEmbedImages {
            images {
              alt
              aspectRatio
            }
          }
        }
      }
    }
  }
}
```
