# RefField isNull Filter Support

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Allow filtering on `ref` type fields (like `reply` in `app.bsky.feed.post`) with the `isNull` operator.

**Architecture:** Create a separate `RefFieldCondition` GraphQL input type with only `isNull` (no eq, contains, etc.). Update schema generation to include RefFields in filterable fields and assign them this limited condition type.

**Tech Stack:** Gleam, GraphQL schema generation (swell), lexicon_graphql

---

## Task 1: Add RefFieldCondition Type Builder

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/input/connection.gleam`

**Step 1: Write the failing test**

Create test file `lexicon_graphql/test/ref_field_condition_test.gleam`:

```gleam
import gleam/list
import gleeunit
import gleeunit/should
import lexicon_graphql/input/connection
import swell/schema

pub fn main() {
  gleeunit.main()
}

pub fn ref_field_condition_has_only_is_null_test() {
  let condition_type = connection.build_ref_where_condition_input_type("Test")

  let fields = schema.get_input_fields(condition_type)

  // Should have exactly 1 field
  list.length(fields) |> should.equal(1)

  // That field should be isNull
  let has_is_null =
    fields
    |> list.any(fn(field) { schema.input_field_name(field) == "isNull" })

  has_is_null |> should.be_true
}

pub fn ref_field_condition_no_eq_operator_test() {
  let condition_type = connection.build_ref_where_condition_input_type("Test")

  let fields = schema.get_input_fields(condition_type)

  // Should NOT have eq field
  let has_eq =
    fields
    |> list.any(fn(field) { schema.input_field_name(field) == "eq" })

  has_eq |> should.be_false
}
```

**Step 2: Run test to verify it fails**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test -- --filter="ref_field_condition"
```

Expected: Compile error - `build_ref_where_condition_input_type` does not exist

**Step 3: Add the RefFieldCondition builder function**

In `lexicon_graphql/src/lexicon_graphql/input/connection.gleam`, add after `build_where_condition_input_type` (around line 121):

```gleam
/// Builds a WhereConditionInput type for ref fields (objects)
/// Only supports isNull operator - ref fields can only check for presence/absence
pub fn build_ref_where_condition_input_type(type_name: String) -> schema.Type {
  let condition_type_name = type_name <> "RefFieldCondition"

  schema.input_object_type(
    condition_type_name,
    "Filter for " <> type_name <> " reference fields (presence check only)",
    [
      schema.input_field(
        "isNull",
        schema.boolean_type(),
        "Filter for null (true) or non-null (false) values",
        None,
      ),
    ],
  )
}
```

**Step 4: Run test to verify it passes**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test -- --filter="ref_field_condition"
```

Expected: PASS

**Step 5: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/input/connection.gleam lexicon_graphql/test/ref_field_condition_test.gleam && git commit -m "feat(where): add RefFieldCondition type with isNull only"
```

---

## Task 2: Update build_where_input_type to Handle Field Types

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/input/connection.gleam`

**Step 1: Write the failing test**

Add to `lexicon_graphql/test/ref_field_condition_test.gleam`:

```gleam
pub fn where_input_with_ref_field_uses_ref_condition_test() {
  // Fields: text is primitive (False), reply is ref (True)
  let fields = [#("text", False), #("reply", True)]

  let where_input = connection.build_where_input_type_with_field_types("Test", fields)

  let input_fields = schema.get_input_fields(where_input)

  // Find the reply field
  let reply_field =
    list.find(input_fields, fn(field) {
      schema.input_field_name(field) == "reply"
    })

  case reply_field {
    Ok(field) -> {
      // The type name should contain "RefFieldCondition"
      let type_name = schema.get_type_name(schema.input_field_type(field))
      case type_name {
        Ok(name) -> should.be_true(string.contains(name, "RefFieldCondition"))
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}
```

Add import at top: `import gleam/string`

**Step 2: Run test to verify it fails**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test -- --filter="where_input_with_ref_field"
```

Expected: Compile error - `build_where_input_type_with_field_types` does not exist

**Step 3: Add the new function**

In `lexicon_graphql/src/lexicon_graphql/input/connection.gleam`, add after `build_ref_where_condition_input_type`:

```gleam
/// Builds a WhereInput type with support for different field types
/// field_info is a list of (field_name, is_ref_field) tuples
pub fn build_where_input_type_with_field_types(
  type_name: String,
  field_info: List(#(String, Bool)),
) -> schema.Type {
  let where_input_name = type_name <> "WhereInput"

  // Build the condition types
  let string_condition_type =
    build_where_condition_input_type(type_name, schema.string_type())
  let ref_condition_type = build_ref_where_condition_input_type(type_name)

  // Build input fields - use ref condition for ref fields, string condition otherwise
  let field_input_fields =
    list.map(field_info, fn(info) {
      let #(field_name, is_ref) = info
      let condition_type = case is_ref {
        True -> ref_condition_type
        False -> string_condition_type
      }
      schema.input_field(
        field_name,
        condition_type,
        "Filter by " <> field_name,
        None,
      )
    })

  // Create placeholder type for recursive reference
  let where_input_type =
    schema.input_object_type(
      where_input_name,
      "Filter conditions for " <> type_name,
      field_input_fields,
    )

  // Add AND/OR fields
  let logic_fields = [
    schema.input_field(
      "and",
      schema.list_type(schema.non_null(where_input_type)),
      "All conditions must match (AND logic)",
      None,
    ),
    schema.input_field(
      "or",
      schema.list_type(schema.non_null(where_input_type)),
      "Any condition must match (OR logic)",
      None,
    ),
  ]

  schema.input_object_type(
    where_input_name,
    "Filter conditions for " <> type_name <> " with nested AND/OR support",
    list.append(field_input_fields, logic_fields),
  )
}
```

**Step 4: Run test to verify it passes**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test -- --filter="where_input_with_ref_field"
```

Expected: PASS

**Step 5: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/input/connection.gleam lexicon_graphql/test/ref_field_condition_test.gleam && git commit -m "feat(where): add build_where_input_type_with_field_types for ref support"
```

---

## Task 3: Add is_filterable_property Function

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/schema/database.gleam`

**Step 1: Add is_filterable_property function**

In `lexicon_graphql/src/lexicon_graphql/schema/database.gleam`, add after `is_groupable_property` (around line 1328):

```gleam
/// Check if a lexicon property is filterable based on its type
/// Filterable types: string, integer, boolean, number, ref (primitives + refs for isNull)
/// Non-filterable types: blob, array (complex types)
fn is_filterable_property(property: types.Property) -> Bool {
  case property_to_field_type(property) {
    StringField | IntField | BoolField | NumberField | DateTimeField | RefField ->
      True
    BlobField | ArrayField -> False
  }
}
```

**Step 2: Verify it compiles**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build
```

Expected: Compiles successfully

**Step 3: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/schema/database.gleam && git commit -m "feat(where): add is_filterable_property including RefField"
```

---

## Task 4: Update get_filterable_field_names to Return Field Type Info

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/schema/database.gleam`

**Step 1: Update the function to return tuples**

Replace `get_filterable_field_names` (lines 1356-1381) with:

```gleam
/// Get all filterable field names for WHERE inputs with their type info
/// Returns list of (field_name, is_ref_field) tuples
/// Includes primitive fields, ref fields, and computed fields like actorHandle
fn get_filterable_field_names(record_type: RecordType) -> List(#(String, Bool)) {
  // Filter properties to only filterable types, return with is_ref flag
  let filterable_property_info =
    list.filter_map(record_type.properties, fn(prop) {
      let #(field_name, property) = prop
      case is_filterable_property(property) {
        True -> {
          let is_ref = property_to_field_type(property) == RefField
          Ok(#(field_name, is_ref))
        }
        False -> Error(Nil)
      }
    })

  // Add standard filterable fields from AT Protocol (all are non-ref)
  // Note: actorHandle IS included because WHERE clauses support filtering by it
  let standard_filterable_fields = [
    #("uri", False),
    #("cid", False),
    #("did", False),
    #("collection", False),
    #("indexedAt", False),
    #("actorHandle", False),
  ]
  list.append(standard_filterable_fields, filterable_property_info)
}
```

**Step 2: Verify it compiles (will fail - callers need update)**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build 2>&1 | head -30
```

Expected: Type mismatch errors at call sites

**Step 3: Commit (partial - will fix callers next)**

```bash
git add lexicon_graphql/src/lexicon_graphql/schema/database.gleam && git commit -m "feat(where): update get_filterable_field_names to return type info"
```

---

## Task 5: Update build_where_input_type Caller

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/schema/database.gleam`

**Step 1: Update the local build_where_input_type function**

Replace `build_where_input_type` (lines 1480-1489) with:

```gleam
/// Build a WhereInput type for a record type with all its filterable fields
/// Includes primitive fields (string, integer, boolean, number) and ref fields
/// Excludes complex types (blob, array) but includes computed fields like actorHandle
fn build_where_input_type(record_type: RecordType) -> schema.Type {
  // Get filterable field info (includes type info for ref vs primitive)
  let field_info = get_filterable_field_names(record_type)

  // Use the connection module to build the where input type with field types
  lexicon_connection.build_where_input_type_with_field_types(
    record_type.type_name,
    field_info,
  )
}
```

**Step 2: Run tests to verify**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test
```

Expected: All tests pass (may need to accept birdie snapshots)

**Step 3: Accept any snapshot updates**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam run -m birdie -- accept-all
```

**Step 4: Run tests again**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test
```

Expected: All tests pass

**Step 5: Commit**

```bash
git add lexicon_graphql/ && git commit -m "feat(where): wire up RefField support in schema generation"
```

---

## Task 6: Full Test Suite Verification

**Step 1: Run lexicon_graphql tests**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test
```

Expected: All tests pass

**Step 2: Run server tests**

```bash
cd /Users/chadmiller/code/quickslice/server && gleam test
```

Expected: All tests pass

**Step 3: Build both projects**

```bash
cd /Users/chadmiller/code/quickslice/server && gleam build
cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build
```

Expected: Both compile successfully

---

## Files Modified Summary

1. `lexicon_graphql/src/lexicon_graphql/input/connection.gleam`
   - Added `build_ref_where_condition_input_type`
   - Added `build_where_input_type_with_field_types`

2. `lexicon_graphql/src/lexicon_graphql/schema/database.gleam`
   - Added `is_filterable_property`
   - Updated `get_filterable_field_names` to return `List(#(String, Bool))`
   - Updated `build_where_input_type` to use new function

3. `lexicon_graphql/test/ref_field_condition_test.gleam` (new)
   - Tests for RefFieldCondition type
