# Union Type Naming Refactor Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Change array union type names from concatenated style (`AppBskyRichtextFacetMentionOrAppBskyRichtextFacetLinkOrAppBskyRichtextFacetTag`) to contextual style (`AppBskyRichtextFacetFeaturesUnion`).

**Architecture:** Modify `build_array_union_type` to accept parent type name and field name parameters, then update all callers to pass these values. The union name becomes `{ParentTypeName}{CapitalizedFieldName}Union`.

**Tech Stack:** Gleam, swell GraphQL library

---

### Task 1: Add parameters to build_array_union_type

**Files:**
- Modify: `src/lexicon_graphql/internal/graphql/type_mapper.gleam:180-183`

**Step 1: Update function signature**

Change the function signature from:

```gleam
fn build_array_union_type(
  refs: List(String),
  object_types: Dict(String, schema.Type),
) -> schema.Type {
```

To:

```gleam
fn build_array_union_type(
  refs: List(String),
  object_types: Dict(String, schema.Type),
  parent_type_name: String,
  field_name: String,
) -> schema.Type {
```

**Step 2: Update union name generation**

Replace lines 210-211:

```gleam
      // Build union name: "TypeAOrTypeBOrTypeC"
      let union_name = string.join(type_names, "Or")
```

With:

```gleam
      // Build union name: ParentTypeNameFieldNameUnion
      let capitalized_field = capitalize_first(field_name)
      let union_name = parent_type_name <> capitalized_field <> "Union"
```

**Step 3: Run tests to see failures**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test 2>&1 | head -50`
Expected: Compilation error - `build_array_union_type` called with wrong number of arguments

---

### Task 2: Update map_array_type to accept and pass context

**Files:**
- Modify: `src/lexicon_graphql/internal/graphql/type_mapper.gleam:117-167`

**Step 1: Update map_array_type signature**

Change from:

```gleam
pub fn map_array_type(
  items: Option(types.ArrayItems),
  object_types: Dict(String, schema.Type),
) -> schema.Type {
```

To:

```gleam
pub fn map_array_type(
  items: Option(types.ArrayItems),
  object_types: Dict(String, schema.Type),
  parent_type_name: String,
  field_name: String,
) -> schema.Type {
```

**Step 2: Update the union case to pass context**

Change lines 152-156:

```gleam
        "union" -> {
          case item_refs {
            option.Some(refs) -> {
              let union_type = build_array_union_type(refs, object_types)
              schema.list_type(schema.non_null(union_type))
```

To:

```gleam
        "union" -> {
          case item_refs {
            option.Some(refs) -> {
              let union_type = build_array_union_type(refs, object_types, parent_type_name, field_name)
              schema.list_type(schema.non_null(union_type))
```

**Step 3: Run tests to see failures**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build 2>&1 | head -50`
Expected: Compilation errors - callers of `map_array_type` need updating

---

### Task 3: Update map_property_type_with_context to pass context

**Files:**
- Modify: `src/lexicon_graphql/internal/graphql/type_mapper.gleam:305`

**Step 1: Update the map_array_type call**

Change line 305:

```gleam
      map_array_type(expanded_items, object_types)
```

To:

```gleam
      map_array_type(expanded_items, object_types, parent_type_name, field_name)
```

**Step 2: Run build to check**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build 2>&1 | head -50`
Expected: Compilation errors - object_builder.gleam still needs updating

---

### Task 4: Update object_builder.gleam to pass context

**Files:**
- Modify: `src/lexicon_graphql/internal/graphql/object_builder.gleam:37-61` (build_object_type)
- Modify: `src/lexicon_graphql/internal/graphql/object_builder.gleam:64-127` (build_object_fields)

**Step 1: Update build_object_fields to accept and pass type_name**

Change function signature from:

```gleam
fn build_object_fields(
  properties: List(#(String, types.Property)),
  lexicon_id: String,
  object_types_dict: Dict(String, schema.Type),
) -> List(schema.Field) {
```

To:

```gleam
fn build_object_fields(
  properties: List(#(String, types.Property)),
  lexicon_id: String,
  object_types_dict: Dict(String, schema.Type),
  parent_type_name: String,
) -> List(schema.Field) {
```

**Step 2: Update the array case in build_object_fields**

Change lines 74-82:

```gleam
    let graphql_type = case type_ {
      "array" -> {
        let expanded_items = case items {
          option.Some(arr_items) ->
            option.Some(expand_array_items(arr_items, lexicon_id))
          option.None -> option.None
        }
        type_mapper.map_array_type(expanded_items, object_types_dict)
      }
```

To:

```gleam
    let graphql_type = case type_ {
      "array" -> {
        let expanded_items = case items {
          option.Some(arr_items) ->
            option.Some(expand_array_items(arr_items, lexicon_id))
          option.None -> option.None
        }
        type_mapper.map_array_type(expanded_items, object_types_dict, parent_type_name, name)
      }
```

**Step 3: Update build_object_type to pass type_name to build_object_fields**

Change line 45-46:

```gleam
  let lexicon_fields =
    build_object_fields(obj_def.properties, lexicon_id, object_types_dict)
```

To:

```gleam
  let lexicon_fields =
    build_object_fields(obj_def.properties, lexicon_id, object_types_dict, type_name)
```

**Step 4: Run build to check**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build 2>&1 | head -50`
Expected: Compilation errors - tests need updating

---

### Task 5: Update type_mapper_test.gleam

**Files:**
- Modify: `test/type_mapper_test.gleam:81, 91, 98`

**Step 1: Update test calls to map_array_type**

Change line 81:

```gleam
  let result = type_mapper.map_array_type(option.Some(items), dict.new())
```

To:

```gleam
  let result = type_mapper.map_array_type(option.Some(items), dict.new(), "TestType", "testField")
```

Change line 91:

```gleam
  let result = type_mapper.map_array_type(option.Some(items), dict.new())
```

To:

```gleam
  let result = type_mapper.map_array_type(option.Some(items), dict.new(), "TestType", "testField")
```

Change line 98:

```gleam
  let result = type_mapper.map_array_type(option.None, dict.new())
```

To:

```gleam
  let result = type_mapper.map_array_type(option.None, dict.new(), "TestType", "testField")
```

**Step 2: Run tests**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test 2>&1 | tail -20`
Expected: All tests pass

---

### Task 6: Update union_resolver_test.gleam to verify new naming

**Files:**
- Modify: `test/union_resolver_test.gleam`

**Step 1: Update the test to check for new union name format**

The union type name should now be `AppBskyRichtextFacetFeaturesUnion` instead of `AppBskyRichtextFacetMentionOrAppBskyRichtextFacetLink`.

Find and update any assertions that check the union type name.

**Step 2: Run tests**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test 2>&1 | tail -20`
Expected: All tests pass

---

### Task 7: Verify schema changes via MCP

**Step 1: Restart the MCP server and introspect**

User restarts the server, then run introspection query to verify union names changed.

**Step 2: Verify union type names**

Check that:
- `AppBskyRichtextFacetMentionOrAppBskyRichtextFacetLinkOrAppBskyRichtextFacetTag` is now `AppBskyRichtextFacetFeaturesUnion`
- `AppBskyFeedThreadgateMentionRuleOrAppBskyFeedThreadgateFollowerRuleOrAppBskyFeedThreadgateFollowingRuleOrAppBskyFeedThreadgateListRule` is now `AppBskyFeedThreadgateAllowUnion`

---

### Task 8: Commit changes

**Step 1: Stage and commit**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql
git add src/lexicon_graphql/internal/graphql/type_mapper.gleam \
        src/lexicon_graphql/internal/graphql/object_builder.gleam \
        test/type_mapper_test.gleam \
        test/union_resolver_test.gleam
git commit -m "refactor: use {Parent}{Field}Union naming for array union types

Change array union type names from concatenated member names to
contextual names based on parent type and field name.

Before: AppBskyRichtextFacetMentionOrAppBskyRichtextFacetLinkOrAppBskyRichtextFacetTag
After:  AppBskyRichtextFacetFeaturesUnion

This makes union type names shorter, more readable, and consistent
with property union naming convention."
```

**Step 2: Verify clean state**

Run: `git status`
Expected: `nothing to commit, working tree clean`
