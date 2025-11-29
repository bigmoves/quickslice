# Array Type Mapping Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix lexicon arrays to map to proper GraphQL list types instead of String.

**Architecture:** Add `ArrayItems` type to carry array item metadata, modify parser to extract `items` field, update type mapper to handle arrays with `schema.list_type()`, pre-scan properties to build union types upfront.

**Tech Stack:** Gleam, swell/schema GraphQL library, gleeunit for testing

---

## Task 1: Add ArrayItems Type to types.gleam

**Files:**
- Modify: `src/lexicon_graphql/types.gleam:44-52`
- Test: `test/lexicon_parser_test.gleam`

**Step 1: Add ArrayItems type definition**

Add after line 52 in `src/lexicon_graphql/types.gleam`:

```gleam
/// Array items definition (for array properties)
pub type ArrayItems {
  ArrayItems(
    type_: String,
    ref: Option(String),
    refs: Option(List(String)),
  )
}
```

**Step 2: Update Property type to include items field**

Replace the existing `Property` type (lines 44-52) with:

```gleam
/// Property definition
pub type Property {
  Property(
    type_: String,
    required: Bool,
    format: Option(String),
    ref: Option(String),
    items: Option(ArrayItems),
  )
}
```

**Step 3: Run build to see compilation errors**

Run: `gleam build`

Expected: Compilation errors in parser.gleam and anywhere Property is constructed (missing `items` argument)

**Step 4: Commit types change**

```bash
git add src/lexicon_graphql/types.gleam
git commit -m "feat: add ArrayItems type and items field to Property"
```

---

## Task 2: Update Parser to Extract Array Items

**Files:**
- Modify: `src/lexicon_graphql/internal/lexicon/parser.gleam:93-100, 138-144, 150-173`
- Test: `test/lexicon_parser_test.gleam`

**Step 1: Write failing test for parsing array with string items**

Add to `test/lexicon_parser_test.gleam`:

```gleam
// Test parsing lexicon with array property containing string items
pub fn parse_array_with_string_items_test() {
  let json =
    "{
      \"lexicon\": 1,
      \"id\": \"fm.teal.alpha.feed.track\",
      \"defs\": {
        \"main\": {
          \"type\": \"record\",
          \"record\": {
            \"type\": \"object\",
            \"properties\": {
              \"artistNames\": {
                \"type\": \"array\",
                \"items\": {
                  \"type\": \"string\"
                }
              }
            }
          }
        }
      }
    }"

  let result = lexicon_parser.parse_lexicon(json)
  should.be_ok(result)

  case result {
    Ok(lexicon) -> {
      case lexicon.defs.main {
        option.Some(types.RecordDef(type_: _, key: _, properties: props)) -> {
          // Find artistNames property
          case list.find(props, fn(p) { p.0 == "artistNames" }) {
            Ok(#(_, prop)) -> {
              should.equal(prop.type_, "array")
              case prop.items {
                option.Some(items) -> {
                  should.equal(items.type_, "string")
                  should.equal(items.ref, option.None)
                  should.equal(items.refs, option.None)
                }
                option.None -> should.fail()
              }
            }
            Error(_) -> should.fail()
          }
        }
        option.None -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}
```

**Step 2: Run test to verify it fails**

Run: `gleam test -- --testfilter=parse_array_with_string_items`

Expected: FAIL (compilation error due to missing items field)

**Step 3: Add array_items_decoder function**

Add after `decode_property` function (around line 173) in `src/lexicon_graphql/internal/lexicon/parser.gleam`:

```gleam
/// Decode array items (type, ref, refs)
fn decode_array_items(
  dyn: decode.Dynamic,
) -> Result(types.ArrayItems, List(decode.DecodeError)) {
  let items_decoder = {
    use type_ <- decode.field("type", decode.string)
    use ref <- decode.optional_field(
      "ref",
      None,
      decode.optional(decode.string),
    )
    use refs <- decode.optional_field(
      "refs",
      None,
      decode.optional(decode.list(decode.string)),
    )
    decode.success(types.ArrayItems(type_:, ref:, refs:))
  }
  decode.run(dyn, items_decoder)
}
```

**Step 4: Update decode_property to extract items**

Replace the entire `decode_property` function (lines 150-173) with:

```gleam
/// Decode a property's type, format, ref, and items fields
fn decode_property(
  dyn: decode.Dynamic,
) -> Result(
  #(String, option.Option(String), option.Option(String), option.Option(types.ArrayItems)),
  List(decode.DecodeError),
) {
  let property_decoder = {
    use type_ <- decode.field("type", decode.string)
    use format <- decode.optional_field(
      "format",
      None,
      decode.optional(decode.string),
    )
    use ref <- decode.optional_field(
      "ref",
      None,
      decode.optional(decode.string),
    )
    use items_dyn <- decode.optional_field(
      "items",
      None,
      decode.optional(decode.dynamic),
    )

    // Decode items if present
    let items = case items_dyn {
      option.Some(dyn) ->
        case decode_array_items(dyn) {
          Ok(arr_items) -> option.Some(arr_items)
          Error(_) -> None
        }
      None -> None
    }

    decode.success(#(type_, format, ref, items))
  }
  decode.run(dyn, property_decoder)
}
```

**Step 5: Update call sites to use new decode_property signature**

In `decode_object_def_inner` (around line 93-100), update:

```gleam
        let #(prop_type, prop_format, prop_ref, prop_items) = case
          decode_property(prop_dyn)
        {
          Ok(#(t, f, r, i)) -> #(t, f, r, i)
          Error(_) -> #("string", None, None, None)
        }

        #(name, types.Property(prop_type, is_required, prop_format, prop_ref, prop_items))
```

In `decode_record_object` (around line 138-144), update:

```gleam
      let #(prop_type, prop_format, prop_ref, prop_items) = case decode_property(prop_dyn) {
        Ok(#(t, f, r, i)) -> #(t, f, r, i)
        Error(_) -> #("string", None, None, None)
      }

      #(name, types.Property(prop_type, is_required, prop_format, prop_ref, prop_items))
```

**Step 6: Run test to verify it passes**

Run: `gleam test -- --testfilter=parse_array_with_string_items`

Expected: PASS

**Step 7: Run all parser tests**

Run: `gleam test -- --testfilter=lexicon_parser`

Expected: All pass

**Step 8: Commit parser changes**

```bash
git add src/lexicon_graphql/internal/lexicon/parser.gleam test/lexicon_parser_test.gleam
git commit -m "feat: parse array items from lexicon properties"
```

---

## Task 3: Add Tests for Array Ref and Union Items Parsing

**Files:**
- Test: `test/lexicon_parser_test.gleam`

**Step 1: Write test for parsing array with ref items**

Add to `test/lexicon_parser_test.gleam`:

```gleam
// Test parsing lexicon with array property containing ref items
pub fn parse_array_with_ref_items_test() {
  let json =
    "{
      \"lexicon\": 1,
      \"id\": \"fm.teal.alpha.feed.track\",
      \"defs\": {
        \"main\": {
          \"type\": \"record\",
          \"record\": {
            \"type\": \"object\",
            \"properties\": {
              \"artists\": {
                \"type\": \"array\",
                \"items\": {
                  \"type\": \"ref\",
                  \"ref\": \"fm.teal.alpha.feed.defs#artist\"
                }
              }
            }
          }
        }
      }
    }"

  let result = lexicon_parser.parse_lexicon(json)
  should.be_ok(result)

  case result {
    Ok(lexicon) -> {
      case lexicon.defs.main {
        option.Some(types.RecordDef(type_: _, key: _, properties: props)) -> {
          case list.find(props, fn(p) { p.0 == "artists" }) {
            Ok(#(_, prop)) -> {
              should.equal(prop.type_, "array")
              case prop.items {
                option.Some(items) -> {
                  should.equal(items.type_, "ref")
                  should.equal(items.ref, option.Some("fm.teal.alpha.feed.defs#artist"))
                }
                option.None -> should.fail()
              }
            }
            Error(_) -> should.fail()
          }
        }
        option.None -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}
```

**Step 2: Run test to verify it passes**

Run: `gleam test -- --testfilter=parse_array_with_ref_items`

Expected: PASS

**Step 3: Write test for parsing array with union items**

Add to `test/lexicon_parser_test.gleam`:

```gleam
// Test parsing lexicon with array property containing union items
pub fn parse_array_with_union_items_test() {
  let json =
    "{
      \"lexicon\": 1,
      \"id\": \"fm.teal.alpha.feed.track\",
      \"defs\": {
        \"main\": {
          \"type\": \"record\",
          \"record\": {
            \"type\": \"object\",
            \"properties\": {
              \"creators\": {
                \"type\": \"array\",
                \"items\": {
                  \"type\": \"union\",
                  \"refs\": [
                    \"fm.teal.alpha.feed.defs#artist\",
                    \"fm.teal.alpha.feed.defs#band\"
                  ]
                }
              }
            }
          }
        }
      }
    }"

  let result = lexicon_parser.parse_lexicon(json)
  should.be_ok(result)

  case result {
    Ok(lexicon) -> {
      case lexicon.defs.main {
        option.Some(types.RecordDef(type_: _, key: _, properties: props)) -> {
          case list.find(props, fn(p) { p.0 == "creators" }) {
            Ok(#(_, prop)) -> {
              should.equal(prop.type_, "array")
              case prop.items {
                option.Some(items) -> {
                  should.equal(items.type_, "union")
                  should.equal(items.refs, option.Some([
                    "fm.teal.alpha.feed.defs#artist",
                    "fm.teal.alpha.feed.defs#band",
                  ]))
                }
                option.None -> should.fail()
              }
            }
            Error(_) -> should.fail()
          }
        }
        option.None -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}
```

**Step 4: Run test to verify it passes**

Run: `gleam test -- --testfilter=parse_array_with_union_items`

Expected: PASS

**Step 5: Run all tests**

Run: `gleam test`

Expected: All pass

**Step 6: Commit test additions**

```bash
git add test/lexicon_parser_test.gleam
git commit -m "test: add parser tests for array ref and union items"
```

---

## Task 4: Add map_array_type Function to Type Mapper

**Files:**
- Modify: `src/lexicon_graphql/internal/graphql/type_mapper.gleam`
- Test: `test/type_mapper_test.gleam`

**Step 1: Write failing test for array of strings**

Add to `test/type_mapper_test.gleam`:

```gleam
import gleam/dict
import gleam/option
import lexicon_graphql/types

pub fn map_array_of_strings_test() {
  let items = types.ArrayItems(type_: "string", ref: option.None, refs: option.None)
  let result = type_mapper.map_array_type(option.Some(items), dict.new())

  // Should be [String!] - list of non-null strings
  schema.type_name(result)
  |> should.equal("[String!]")
}
```

**Step 2: Run test to verify it fails**

Run: `gleam test -- --testfilter=map_array_of_strings`

Expected: FAIL (function doesn't exist)

**Step 3: Add imports to type_mapper.gleam**

At the top of `src/lexicon_graphql/internal/graphql/type_mapper.gleam`, update imports:

```gleam
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option}
import gleam/string
import lexicon_graphql/internal/lexicon/nsid
import lexicon_graphql/scalar/blob as blob_type
import lexicon_graphql/types
import swell/schema
```

**Step 4: Add map_array_type function**

Add after `map_type_with_registry` function (after line 108):

```gleam
/// Maps array items to a GraphQL list type.
/// Returns [ItemType!] where ItemType depends on the items definition.
pub fn map_array_type(
  items: Option(types.ArrayItems),
  object_types: Dict(String, schema.Type),
) -> schema.Type {
  case items {
    option.None ->
      // Fallback: array without items info -> [String!]
      schema.list_type(schema.non_null(schema.string_type()))

    option.Some(types.ArrayItems(type_: item_type, ref: item_ref, refs: item_refs)) ->
      case item_type {
        "string" -> schema.list_type(schema.non_null(schema.string_type()))
        "integer" -> schema.list_type(schema.non_null(schema.int_type()))
        "boolean" -> schema.list_type(schema.non_null(schema.boolean_type()))
        "number" -> schema.list_type(schema.non_null(schema.float_type()))

        "ref" -> {
          case item_ref {
            option.Some(ref_str) -> {
              let type_name = ref_to_type_name(ref_str)
              case dict.get(object_types, type_name) {
                Ok(obj_type) -> schema.list_type(schema.non_null(obj_type))
                Error(_) -> schema.list_type(schema.non_null(schema.string_type()))
              }
            }
            option.None -> schema.list_type(schema.non_null(schema.string_type()))
          }
        }

        "union" -> {
          case item_refs {
            option.Some(refs) -> {
              let union_type = build_array_union_type(refs, object_types)
              schema.list_type(schema.non_null(union_type))
            }
            option.None -> schema.list_type(schema.non_null(schema.string_type()))
          }
        }

        // Fallback for unknown item types
        _ -> schema.list_type(schema.non_null(schema.string_type()))
      }
  }
}

/// Convert a ref string to a type name.
/// "fm.teal.alpha.feed.defs#artist" -> "FmTealAlphaFeedDefsArtist"
pub fn ref_to_type_name(ref: String) -> String {
  ref
  |> string.replace("#", ".")
  |> nsid.to_type_name()
}

/// Build a union type from a list of refs.
/// Returns a GraphQL union with name like "FmTealAlphaFeedDefsArtistOrFmTealAlphaFeedDefsTrack"
fn build_array_union_type(
  refs: List(String),
  object_types: Dict(String, schema.Type),
) -> schema.Type {
  // Convert refs to type names
  let type_names = list.map(refs, ref_to_type_name)

  // Build union name: "TypeAOrTypeBOrTypeC"
  let union_name = string.join(type_names, "Or")

  // Look up member types from object_types dict
  let member_types = list.filter_map(type_names, fn(name) {
    case dict.get(object_types, name) {
      Ok(t) -> Ok(t)
      Error(_) -> Error(Nil)
    }
  })

  // Type resolver - inspect context to determine concrete type
  let type_resolver = fn(_ctx: schema.Context) -> Result(String, String) {
    // For now, return first type - proper implementation needs __typename or type field
    case type_names {
      [first, ..] -> Ok(first)
      [] -> Error("No types in union")
    }
  }

  schema.union_type(union_name, "Union of array item types", member_types, type_resolver)
}
```

**Step 5: Run test to verify it passes**

Run: `gleam test -- --testfilter=map_array_of_strings`

Expected: PASS

**Step 6: Commit type mapper changes**

```bash
git add src/lexicon_graphql/internal/graphql/type_mapper.gleam test/type_mapper_test.gleam
git commit -m "feat: add map_array_type function for array->list mapping"
```

---

## Task 5: Add More Type Mapper Tests

**Files:**
- Test: `test/type_mapper_test.gleam`

**Step 1: Add test for array of integers**

Add to `test/type_mapper_test.gleam`:

```gleam
pub fn map_array_of_integers_test() {
  let items = types.ArrayItems(type_: "integer", ref: option.None, refs: option.None)
  let result = type_mapper.map_array_type(option.Some(items), dict.new())

  schema.type_name(result)
  |> should.equal("[Int!]")
}
```

**Step 2: Add test for array without items (fallback)**

```gleam
pub fn map_array_without_items_test() {
  let result = type_mapper.map_array_type(option.None, dict.new())

  // Should fallback to [String!]
  schema.type_name(result)
  |> should.equal("[String!]")
}
```

**Step 3: Add test for ref_to_type_name**

```gleam
pub fn ref_to_type_name_test() {
  type_mapper.ref_to_type_name("fm.teal.alpha.feed.defs#artist")
  |> should.equal("FmTealAlphaFeedDefsArtist")
}

pub fn ref_to_type_name_simple_test() {
  type_mapper.ref_to_type_name("app.bsky.feed.post")
  |> should.equal("AppBskyFeedPost")
}
```

**Step 4: Run all type mapper tests**

Run: `gleam test -- --testfilter=type_mapper`

Expected: All pass

**Step 5: Commit test additions**

```bash
git add test/type_mapper_test.gleam
git commit -m "test: add comprehensive tests for array type mapping"
```

---

## Task 6: Add map_property_type Function

**Files:**
- Modify: `src/lexicon_graphql/internal/graphql/type_mapper.gleam`
- Test: `test/type_mapper_test.gleam`

**Step 1: Write failing test for map_property_type with array**

Add to `test/type_mapper_test.gleam`:

```gleam
pub fn map_property_type_array_test() {
  let items = types.ArrayItems(type_: "string", ref: option.None, refs: option.None)
  let property = types.Property(
    type_: "array",
    required: True,
    format: option.None,
    ref: option.None,
    items: option.Some(items),
  )

  let result = type_mapper.map_property_type(property, dict.new())

  schema.type_name(result)
  |> should.equal("[String!]")
}
```

**Step 2: Run test to verify it fails**

Run: `gleam test -- --testfilter=map_property_type_array`

Expected: FAIL (function doesn't exist)

**Step 3: Add map_property_type function**

Add to `src/lexicon_graphql/internal/graphql/type_mapper.gleam`:

```gleam
/// Maps a lexicon Property to a GraphQL type.
/// Handles arrays specially by looking at the items field.
pub fn map_property_type(
  property: types.Property,
  object_types: Dict(String, schema.Type),
) -> schema.Type {
  case property.type_ {
    "array" -> map_array_type(property.items, object_types)
    "ref" -> {
      case property.ref {
        option.Some(ref_str) -> {
          case dict.get(object_types, ref_str) {
            Ok(obj_type) -> obj_type
            Error(_) -> schema.string_type()
          }
        }
        option.None -> schema.string_type()
      }
    }
    _ -> map_type(property.type_)
  }
}
```

**Step 4: Run test to verify it passes**

Run: `gleam test -- --testfilter=map_property_type_array`

Expected: PASS

**Step 5: Add test for non-array property**

```gleam
pub fn map_property_type_string_test() {
  let property = types.Property(
    type_: "string",
    required: True,
    format: option.None,
    ref: option.None,
    items: option.None,
  )

  let result = type_mapper.map_property_type(property, dict.new())

  result
  |> should.equal(schema.string_type())
}
```

**Step 6: Run all tests**

Run: `gleam test`

Expected: All pass

**Step 7: Commit map_property_type**

```bash
git add src/lexicon_graphql/internal/graphql/type_mapper.gleam test/type_mapper_test.gleam
git commit -m "feat: add map_property_type for unified property->type mapping"
```

---

## Task 7: Update database.gleam to Use map_property_type

**Files:**
- Modify: `src/lexicon_graphql/schema/database.gleam:1240-1244`

**Step 1: Update the build_record_fields call site**

In `src/lexicon_graphql/schema/database.gleam`, find line 1240-1244:

```gleam
  let lexicon_fields =
    list.map(properties, fn(prop) {
      let #(name, types.Property(type_, _required, format, ref)) = prop
      // Use map_type_with_registry to resolve refs to object types
      let graphql_type =
        type_mapper.map_type_with_registry(type_, format, ref, ref_object_types)
```

Replace with:

```gleam
  let lexicon_fields =
    list.map(properties, fn(prop) {
      let #(name, property) = prop
      let types.Property(type_, _required, _format, _ref, _items) = property
      // Use map_property_type to handle arrays, refs, and primitives
      let graphql_type =
        type_mapper.map_property_type(property, ref_object_types)
```

**Step 2: Run build to check for errors**

Run: `gleam build`

Expected: Success

**Step 3: Run all tests**

Run: `gleam test`

Expected: All pass

**Step 4: Commit integration**

```bash
git add src/lexicon_graphql/schema/database.gleam
git commit -m "feat: integrate map_property_type in database schema builder"
```

---

## Task 8: Add Integration Test for Array Fields in Schema

**Files:**
- Test: `test/schema_builder_test.gleam` or new `test/array_type_test.gleam`

**Step 1: Write integration test for array field in generated schema**

Create `test/array_type_test.gleam`:

```gleam
/// Tests for array type mapping in schema generation
import gleam/list
import gleam/option
import gleam/string
import gleeunit/should
import lexicon_graphql/schema/database
import lexicon_graphql/types
import swell/introspection
import swell/sdl

pub fn array_field_generates_list_type_test() {
  let lexicon =
    types.Lexicon(
      id: "fm.teal.alpha.feed.track",
      defs: types.Defs(
        main: option.Some(
          types.RecordDef(
            type_: "record",
            key: option.None,
            properties: [
              #(
                "artistNames",
                types.Property(
                  type_: "array",
                  required: False,
                  format: option.None,
                  ref: option.None,
                  items: option.Some(
                    types.ArrayItems(
                      type_: "string",
                      ref: option.None,
                      refs: option.None,
                    ),
                  ),
                ),
              ),
            ],
          ),
        ),
        others: dict.new(),
      ),
    )

  let test_schema = database.build_schema([lexicon])
  let all_types = introspection.get_all_schema_types(test_schema)
  let serialized = sdl.print_types(all_types)

  // Should contain artistNames field with list type
  string.contains(serialized, "artistNames: [String!]")
  |> should.be_true
}
```

**Step 2: Run test**

Run: `gleam test -- --testfilter=array_field_generates_list_type`

Expected: PASS

**Step 3: Commit integration test**

```bash
git add test/array_type_test.gleam
git commit -m "test: add integration test for array field schema generation"
```

---

## Task 9: Run Full Test Suite and Verify

**Files:** None (verification only)

**Step 1: Run full test suite**

Run: `gleam test`

Expected: All tests pass

**Step 2: Build the project**

Run: `gleam build`

Expected: Success with no warnings

**Step 3: Final commit if needed**

If any cleanup was needed:

```bash
git add -A
git commit -m "chore: cleanup and fix any remaining issues"
```

---

## Summary

After completing all tasks:

1. `types.gleam` has `ArrayItems` type and `Property` includes `items` field
2. `parser.gleam` extracts `items` from lexicon JSON for array properties
3. `type_mapper.gleam` has `map_array_type`, `map_property_type`, `ref_to_type_name`, and `build_array_union_type`
4. `database.gleam` uses `map_property_type` for field type resolution
5. Full test coverage for parsing and type mapping

Arrays now correctly map to:
- `[String!]`, `[Int!]`, `[Boolean!]`, `[Float!]` for primitives
- `[ObjectType!]` for refs
- `[UnionType!]` for unions (with fully qualified names like `FmTealAlphaFeedDefsArtistOrFmTealAlphaFeedDefsTrack`)
