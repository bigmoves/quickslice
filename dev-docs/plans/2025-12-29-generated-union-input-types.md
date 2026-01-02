# Generated Union Input Types Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Generate type-safe GraphQL input types for AT Protocol union fields, with proper discriminator handling for both single and multi-variant unions, and transformation to AT Protocol format.

**Architecture:** Build input types from lexicon ObjectDefs at schema build time. For single-variant unions, use the variant's input type directly. For multi-variant unions, generate a discriminated union input with a `type` enum field and optional fields for each variant. Build a registry mapping field paths to their union refs during schema generation, pass it to the mutation context, and use it for dynamic `$type` resolution.

**Tech Stack:** Gleam, swell GraphQL library, lexicon_graphql type mapper, lexicon registry

---

## Background

AT Protocol unions require a `$type` discriminator in JSON:
```json
{
  "$type": "com.atproto.label.defs#selfLabels",
  "values": [{ "val": "sexual" }]
}
```

GraphQL can't use `$type` (reserved character), so we:
1. Generate input types matching the union variant structure
2. For single-variant unions: use the variant type directly
3. For multi-variant unions: add a `type` enum field as discriminator
4. Build a registry of union field -> refs during schema generation
5. Transform server-side using the registry to add the correct `$type` field

**Key Design: Dynamic Union Resolution**

Since Quickslice works with arbitrary lexicons, we can't hardcode union variants. Instead:
- During schema generation, build a `UnionFieldRegistry`: `Dict(String, List(String))`
- Key: `"collection.fieldName"` (e.g., `"social.grain.gallery.labels"`)
- Value: list of variant refs (e.g., `["com.atproto.label.defs#selfLabels"]`)
- Pass this registry to mutation resolvers
- Use it at transformation time to look up the correct `$type`

**Example schema for single-variant union (labels):**
```graphql
input SelfLabelInput {
  val: String!
}

input SelfLabelsInput {
  values: [SelfLabelInput!]!
}

input SocialGrainGalleryInput {
  title: String!
  images: [BlobInput!]!
  labels: SelfLabelsInput  # Direct variant type
}
```

**Example schema for multi-variant union (embed):**
```graphql
enum AppBskyFeedPostEmbedType {
  IMAGES
  EXTERNAL
  RECORD
  RECORD_WITH_MEDIA
}

input AppBskyFeedPostEmbedInput {
  type: AppBskyFeedPostEmbedType!  # Discriminator
  images: AppBskyEmbedImagesInput
  external: AppBskyEmbedExternalInput
  record: AppBskyEmbedRecordInput
  recordWithMedia: AppBskyEmbedRecordWithMediaInput
}
```

---

### Task 1: Create Union Input Builder Module

**Files:**
- Create: `lexicon_graphql/src/lexicon_graphql/internal/graphql/union_input_builder.gleam`

**Step 1: Create the union input builder module**

```gleam
/// Union Input Builder
///
/// Builds GraphQL input types for AT Protocol union fields.
/// Generates input types from lexicon ObjectDefs, handling nested refs.
///
/// For single-variant unions: returns the variant's input type directly
/// For multi-variant unions: creates a discriminated input type with:
///   - A `type` enum field for selecting the variant
///   - Optional fields for each variant's data
///
/// Also builds a UnionFieldRegistry for dynamic $type resolution at runtime.
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option}
import gleam/string
import lexicon_graphql/internal/graphql/type_mapper
import lexicon_graphql/internal/lexicon/nsid
import lexicon_graphql/internal/lexicon/registry as lexicon_registry
import lexicon_graphql/types
import swell/schema

/// Registry of generated union input types
/// Maps fully-qualified ref (e.g., "com.atproto.label.defs#selfLabels") to input type
pub type UnionInputRegistry =
  Dict(String, schema.Type)

/// Mapping of union field paths to their variant refs
/// Key: "collection.fieldName" (e.g., "social.grain.gallery.labels")
/// Value: list of variant refs (e.g., ["com.atproto.label.defs#selfLabels"])
/// Used for dynamic $type resolution during mutation transformation
pub type UnionFieldRegistry =
  Dict(String, List(String))

/// Combined registry for union inputs
pub type UnionRegistry {
  UnionRegistry(
    input_types: UnionInputRegistry,
    field_variants: UnionFieldRegistry,
  )
}

/// Build input types for all object defs in the registry
/// Returns a registry with input types (field_variants populated separately)
pub fn build_union_input_types(
  registry: lexicon_registry.Registry,
) -> UnionRegistry {
  let object_defs = lexicon_registry.get_all_object_defs(registry)

  // Build input types in dependency order (simple types first)
  // Do two passes: first build all without refs, then with refs
  let first_pass =
    dict.fold(object_defs, dict.new(), fn(acc, ref, obj_def) {
      let input_type = build_input_type_from_object_def(ref, obj_def, acc)
      dict.insert(acc, ref, input_type)
    })

  // Second pass to resolve any remaining refs
  let input_types =
    dict.fold(object_defs, first_pass, fn(acc, ref, obj_def) {
      let input_type = build_input_type_from_object_def(ref, obj_def, acc)
      dict.insert(acc, ref, input_type)
    })

  // field_variants is populated during build_input_type in mutation builder
  UnionRegistry(input_types: input_types, field_variants: dict.new())
}

/// Add a union field entry to the registry
pub fn register_union_field(
  registry: UnionRegistry,
  collection: String,
  field_name: String,
  refs: List(String),
) -> UnionRegistry {
  let key = collection <> "." <> field_name
  UnionRegistry(
    input_types: registry.input_types,
    field_variants: dict.insert(registry.field_variants, key, refs),
  )
}

/// Look up union refs for a field
pub fn get_union_refs(
  registry: UnionRegistry,
  collection: String,
  field_name: String,
) -> Option(List(String)) {
  let key = collection <> "." <> field_name
  dict.get(registry.field_variants, key) |> option.from_result
}

/// Convert a ref like "com.atproto.label.defs#selfLabels" to "SelfLabelsInput"
pub fn ref_to_input_type_name(ref: String) -> String {
  let base_name = nsid.to_type_name(string.replace(ref, "#", "."))
  base_name <> "Input"
}

/// Convert a ref to a short variant name for enum values
/// "com.atproto.label.defs#selfLabels" -> "SELF_LABELS"
pub fn ref_to_variant_enum_value(ref: String) -> String {
  // Extract the part after # or the last segment
  let short_name = case string.split(ref, "#") {
    [_, name] -> name
    _ -> {
      case string.split(ref, ".") |> list.last {
        Ok(name) -> name
        Error(_) -> ref
      }
    }
  }
  // Convert camelCase to SCREAMING_SNAKE_CASE
  camel_to_screaming_snake(short_name)
}

/// Convert camelCase to SCREAMING_SNAKE_CASE
fn camel_to_screaming_snake(s: String) -> String {
  s
  |> string.to_graphemes
  |> list.fold(#("", False), fn(acc, char) {
    let #(result, prev_was_lower) = acc
    let is_upper = string.uppercase(char) == char && string.lowercase(char) != char
    case is_upper, prev_was_lower {
      True, True -> #(result <> "_" <> char, False)
      _, _ -> #(result <> string.uppercase(char), !is_upper)
    }
  })
  |> fn(pair) { pair.0 }
}

/// Convert SCREAMING_SNAKE_CASE back to the original short name
/// "SELF_LABELS" -> "selfLabels"
pub fn enum_value_to_short_name(enum_value: String) -> String {
  let parts = string.split(enum_value, "_")
  case parts {
    [first, ..rest] -> {
      let lower_first = string.lowercase(first)
      let capitalized_rest = list.map(rest, fn(part) {
        case string.pop_grapheme(string.lowercase(part)) {
          Ok(#(first_char, remaining)) -> string.uppercase(first_char) <> remaining
          Error(_) -> part
        }
      })
      lower_first <> string.join(capitalized_rest, "")
    }
    [] -> enum_value
  }
}

/// Convert a ref to a camelCase field name for variant fields
/// "com.atproto.label.defs#selfLabels" -> "selfLabels"
pub fn ref_to_variant_field_name(ref: String) -> String {
  case string.split(ref, "#") {
    [_, name] -> name
    _ -> {
      case string.split(ref, ".") |> list.last {
        Ok(name) -> name
        Error(_) -> ref
      }
    }
  }
}

/// Build a GraphQL input type from an ObjectDef
fn build_input_type_from_object_def(
  ref: String,
  obj_def: types.ObjectDef,
  existing_input_types: UnionInputRegistry,
) -> schema.Type {
  let type_name = ref_to_input_type_name(ref)
  let required_fields = obj_def.required_fields

  let input_fields =
    list.map(obj_def.properties, fn(prop) {
      let #(name, property) = prop
      let is_required = list.contains(required_fields, name)

      // Get the input type for this property
      let field_type = map_property_to_input_type(property, existing_input_types)

      // Wrap in non_null if required
      let final_type = case is_required {
        True -> schema.non_null(field_type)
        False -> field_type
      }

      schema.input_field(name, final_type, "Input for " <> name, option.None)
    })

  schema.input_object_type(type_name, "Input type for " <> ref, input_fields)
}

/// Build a discriminated union input type for multi-variant unions
/// Creates an input with a `type` enum and optional fields for each variant
pub fn build_multi_variant_union_input(
  parent_type_name: String,
  field_name: String,
  refs: List(String),
  existing_input_types: UnionInputRegistry,
) -> schema.Type {
  let union_name = parent_type_name <> capitalize_first(field_name) <> "Input"
  let enum_name = parent_type_name <> capitalize_first(field_name) <> "Type"

  // Build the type enum with variant names
  let enum_values =
    list.map(refs, fn(ref) {
      let value_name = ref_to_variant_enum_value(ref)
      schema.enum_value(value_name, "Select " <> ref <> " variant")
    })

  let type_enum = schema.enum_type(
    enum_name,
    "Discriminator for " <> field_name <> " union variants",
    enum_values,
  )

  // Build input fields: required type discriminator + optional variant fields
  let type_field = schema.input_field(
    "type",
    schema.non_null(type_enum),
    "Select which variant to use",
    option.None,
  )

  let variant_fields =
    list.map(refs, fn(ref) {
      let variant_field_name = ref_to_variant_field_name(ref)
      let field_type = case dict.get(existing_input_types, ref) {
        Ok(input_type) -> input_type
        Error(_) -> schema.string_type()
      }
      schema.input_field(
        variant_field_name,
        field_type,  // Optional - user provides data for selected variant
        "Data for " <> ref <> " variant",
        option.None,
      )
    })

  let all_fields = [type_field, ..variant_fields]

  schema.input_object_type(
    union_name,
    "Union input for " <> field_name <> " - use type to select variant",
    all_fields,
  )
}

/// Map a lexicon property to a GraphQL input type
fn map_property_to_input_type(
  property: types.Property,
  existing_input_types: UnionInputRegistry,
) -> schema.Type {
  case property.type_ {
    // For refs, check if we have a generated input type
    "ref" -> {
      case property.ref {
        option.Some(ref) -> {
          case dict.get(existing_input_types, ref) {
            Ok(input_type) -> input_type
            // Fall back to basic type mapping if no input type exists
            Error(_) -> type_mapper.map_input_type("ref")
          }
        }
        option.None -> type_mapper.map_input_type("ref")
      }
    }

    // For arrays, handle items
    "array" -> {
      case property.items {
        option.Some(types.ArrayItems(item_type, item_ref, _item_refs)) -> {
          let item_input_type = case item_type {
            "ref" -> {
              case item_ref {
                option.Some(ref) -> {
                  case dict.get(existing_input_types, ref) {
                    Ok(input_type) -> input_type
                    Error(_) -> type_mapper.map_input_type("ref")
                  }
                }
                option.None -> type_mapper.map_input_type("ref")
              }
            }
            _ -> type_mapper.map_input_type(item_type)
          }
          schema.list_type(schema.non_null(item_input_type))
        }
        option.None -> schema.list_type(schema.non_null(schema.string_type()))
      }
    }

    // For unions - handled differently based on variant count
    // Single-variant: use variant type directly
    // Multi-variant: caller should use build_multi_variant_union_input
    "union" -> {
      case property.refs {
        option.Some([single_ref]) -> {
          // Single-variant union: use that variant's input type directly
          case dict.get(existing_input_types, single_ref) {
            Ok(input_type) -> input_type
            Error(_) -> type_mapper.map_input_type("union")
          }
        }
        option.Some(_multiple_refs) -> {
          // Multi-variant: return placeholder, caller handles this
          type_mapper.map_input_type("union")
        }
        _ -> type_mapper.map_input_type("union")
      }
    }

    // Default: use type_mapper
    other -> type_mapper.map_input_type(other)
  }
}

/// Capitalize the first letter of a string
fn capitalize_first(s: String) -> String {
  case string.pop_grapheme(s) {
    Ok(#(first, rest)) -> string.uppercase(first) <> rest
    Error(_) -> s
  }
}

/// Get an input type from the registry by ref
pub fn get_input_type(
  registry: UnionRegistry,
  ref: String,
) -> Option(schema.Type) {
  dict.get(registry.input_types, ref) |> option.from_result
}

/// Check if a union has multiple variants
pub fn is_multi_variant_union(refs: Option(List(String))) -> Bool {
  case refs {
    option.Some([_, _, ..]) -> True
    _ -> False
  }
}

/// Find the ref that matches an enum value
pub fn enum_value_to_ref(enum_value: String, refs: List(String)) -> Option(String) {
  list.find(refs, fn(ref) {
    ref_to_variant_enum_value(ref) == enum_value
  })
  |> option.from_result
}
```

**Step 2: Verify compilation**

Run: `cd lexicon_graphql && gleam build`
Expected: Build succeeds (may have warnings about unused functions)

**Step 3: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/internal/graphql/union_input_builder.gleam
git commit -m "feat: add union input type builder with multi-variant support and field registry"
```

---

### Task 2: Add get_all_object_defs to Registry

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/internal/lexicon/registry.gleam`

**Step 1: Add function to get all object defs**

Add this function after `get_object_def`:

```gleam
/// Get all object definitions from the registry
pub fn get_all_object_defs(registry: Registry) -> Dict(String, types.ObjectDef) {
  registry.object_defs
}
```

**Step 2: Verify compilation**

Run: `cd lexicon_graphql && gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/internal/lexicon/registry.gleam
git commit -m "feat: add get_all_object_defs to lexicon registry"
```

---

### Task 3: Update Type Mapper for Union Input Types

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/internal/graphql/type_mapper.gleam`

**Step 1: Add imports**

Add after the existing imports:

```gleam
import gleam/dict.{type Dict}
import lexicon_graphql/types
```

**Step 2: Add new function for union input type resolution**

Add this new function after `map_input_type`:

```gleam
/// Context needed for building union input types
pub type UnionInputContext {
  UnionInputContext(
    input_types: Dict(String, schema.Type),
    parent_type_name: String,
  )
}

/// Maps a lexicon property to a GraphQL input type, with union input registry lookup
/// This version can resolve union refs to their generated input types
/// For multi-variant unions, builds discriminated union input types on demand
pub fn map_input_type_with_unions(
  property: types.Property,
  field_name: String,
  ctx: UnionInputContext,
) -> schema.Type {
  case property.type_ {
    // For unions, handle single vs multi-variant
    "union" -> {
      case property.refs {
        // Single-variant: use variant's input type directly
        option.Some([single_ref]) -> {
          case dict.get(ctx.input_types, single_ref) {
            Ok(input_type) -> input_type
            Error(_) -> schema.string_type()
          }
        }
        // Multi-variant: build discriminated union input
        option.Some(refs) if list.length(refs) > 1 -> {
          build_multi_variant_union_input(
            ctx.parent_type_name,
            field_name,
            refs,
            ctx.input_types,
          )
        }
        _ -> schema.string_type()
      }
    }

    // For refs, check the registry
    "ref" -> {
      case property.ref {
        option.Some(ref) -> {
          case dict.get(ctx.input_types, ref) {
            Ok(input_type) -> input_type
            Error(_) -> map_input_type("ref")
          }
        }
        option.None -> map_input_type("ref")
      }
    }

    // For arrays with ref/union items
    "array" -> {
      case property.items {
        option.Some(types.ArrayItems(item_type, item_ref, item_refs)) -> {
          case item_type {
            "ref" -> {
              case item_ref {
                option.Some(ref) -> {
                  case dict.get(ctx.input_types, ref) {
                    Ok(input_type) ->
                      schema.list_type(schema.non_null(input_type))
                    Error(_) ->
                      schema.list_type(schema.non_null(schema.string_type()))
                  }
                }
                option.None ->
                  schema.list_type(schema.non_null(schema.string_type()))
              }
            }
            "union" -> {
              case item_refs {
                // Single-variant array items
                option.Some([single_ref]) -> {
                  case dict.get(ctx.input_types, single_ref) {
                    Ok(input_type) ->
                      schema.list_type(schema.non_null(input_type))
                    Error(_) ->
                      schema.list_type(schema.non_null(schema.string_type()))
                  }
                }
                // Multi-variant array items
                option.Some(refs) if list.length(refs) > 1 -> {
                  let item_union = build_multi_variant_union_input(
                    ctx.parent_type_name,
                    field_name <> "Item",
                    refs,
                    ctx.input_types,
                  )
                  schema.list_type(schema.non_null(item_union))
                }
                _ -> schema.list_type(schema.non_null(schema.string_type()))
              }
            }
            _ -> map_array_type(property.items, dict.new(), "", "")
          }
        }
        option.None -> schema.list_type(schema.non_null(schema.string_type()))
      }
    }

    // Default to regular input type mapping
    _ -> map_input_type(property.type_)
  }
}

/// Build a discriminated union input type for multi-variant unions
fn build_multi_variant_union_input(
  parent_type_name: String,
  field_name: String,
  refs: List(String),
  existing_input_types: Dict(String, schema.Type),
) -> schema.Type {
  let union_name = parent_type_name <> capitalize_first(field_name) <> "Input"
  let enum_name = parent_type_name <> capitalize_first(field_name) <> "Type"

  // Build the type enum with variant names
  let enum_values =
    list.map(refs, fn(ref) {
      let value_name = ref_to_variant_enum_value(ref)
      schema.enum_value(value_name, "Select " <> ref <> " variant")
    })

  let type_enum = schema.enum_type(
    enum_name,
    "Discriminator for " <> field_name <> " union variants",
    enum_values,
  )

  // Build input fields: required type discriminator + optional variant fields
  let type_field = schema.input_field(
    "type",
    schema.non_null(type_enum),
    "Select which variant to use",
    option.None,
  )

  let variant_fields =
    list.map(refs, fn(ref) {
      let variant_field_name = ref_to_variant_field_name(ref)
      let field_type = case dict.get(existing_input_types, ref) {
        Ok(input_type) -> input_type
        Error(_) -> schema.string_type()
      }
      schema.input_field(
        variant_field_name,
        field_type,  // Optional - user provides data for selected variant
        "Data for " <> ref <> " variant",
        option.None,
      )
    })

  let all_fields = [type_field, ..variant_fields]

  schema.input_object_type(
    union_name,
    "Union input for " <> field_name <> " - use type field to select variant",
    all_fields,
  )
}

/// Convert a ref to SCREAMING_SNAKE_CASE enum value
fn ref_to_variant_enum_value(ref: String) -> String {
  let short_name = case string.split(ref, "#") {
    [_, name] -> name
    _ -> {
      case string.split(ref, ".") |> list.last {
        Ok(name) -> name
        Error(_) -> ref
      }
    }
  }
  camel_to_screaming_snake(short_name)
}

/// Convert camelCase to SCREAMING_SNAKE_CASE
fn camel_to_screaming_snake(s: String) -> String {
  s
  |> string.to_graphemes
  |> list.fold(#("", False), fn(acc, char) {
    let #(result, prev_was_lower) = acc
    let is_upper = string.uppercase(char) == char && string.lowercase(char) != char
    case is_upper, prev_was_lower {
      True, True -> #(result <> "_" <> char, False)
      _, _ -> #(result <> string.uppercase(char), !is_upper)
    }
  })
  |> fn(pair) { pair.0 }
}

/// Convert a ref to a camelCase field name
fn ref_to_variant_field_name(ref: String) -> String {
  case string.split(ref, "#") {
    [_, name] -> name
    _ -> {
      case string.split(ref, ".") |> list.last {
        Ok(name) -> name
        Error(_) -> ref
      }
    }
  }
}

/// Capitalize the first letter
fn capitalize_first(s: String) -> String {
  case string.pop_grapheme(s) {
    Ok(#(first, rest)) -> string.uppercase(first) <> rest
    Error(_) -> s
  }
}
```

**Step 3: Verify compilation**

Run: `cd lexicon_graphql && gleam build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/internal/graphql/type_mapper.gleam
git commit -m "feat: add map_input_type_with_unions with multi-variant support"
```

---

### Task 4: Update Mutation Builder to Build and Return Union Registry

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/mutation/builder.gleam`

**Step 1: Add imports**

Add to imports:

```gleam
import lexicon_graphql/internal/graphql/union_input_builder
import lexicon_graphql/internal/lexicon/registry as lexicon_registry
```

**Step 2: Create new return type that includes the union registry**

Add after the existing type definitions:

```gleam
/// Result of building mutation type - includes the type and union field registry
pub type MutationBuildResult {
  MutationBuildResult(
    mutation_type: schema.Type,
    union_registry: union_input_builder.UnionRegistry,
  )
}
```

**Step 3: Update build_mutation_type to return MutationBuildResult**

```gleam
/// Build a GraphQL Mutation type from lexicon definitions
/// Returns both the mutation type and a union registry for transformation
pub fn build_mutation_type(
  lexicons: List(types.Lexicon),
  object_types: dict.Dict(String, schema.Type),
  create_factory: option.Option(ResolverFactory),
  update_factory: option.Option(ResolverFactory),
  delete_factory: option.Option(ResolverFactory),
  upload_blob_factory: option.Option(UploadBlobResolverFactory),
  custom_fields: option.Option(List(schema.Field)),
  registry: option.Option(lexicon_registry.Registry),
) -> MutationBuildResult {
  // Build union input types if registry is provided
  let initial_union_registry = case registry {
    option.Some(reg) -> union_input_builder.build_union_input_types(reg)
    option.None -> union_input_builder.UnionRegistry(
      input_types: dict.new(),
      field_variants: dict.new(),
    )
  }

  // Extract record types
  let record_types = extract_record_types(lexicons)

  // Build mutation fields for each record type, accumulating union field registrations
  let #(record_mutation_fields, final_union_registry) =
    list.fold(record_types, #([], initial_union_registry), fn(acc, record) {
      let #(fields_acc, registry_acc) = acc
      let #(new_fields, updated_registry) = build_mutations_for_record(
        record,
        object_types,
        create_factory,
        update_factory,
        delete_factory,
        registry_acc,
      )
      #(list.append(fields_acc, new_fields), updated_registry)
    })

  // Add uploadBlob mutation if factory is provided
  let with_upload_blob = case upload_blob_factory {
    option.Some(factory) -> {
      let upload_blob_mutation = build_upload_blob_mutation(factory)
      [upload_blob_mutation, ..record_mutation_fields]
    }
    option.None -> record_mutation_fields
  }

  // Add custom fields if provided
  let all_mutation_fields = case custom_fields {
    option.Some(fields) -> list.append(with_upload_blob, fields)
    option.None -> with_upload_blob
  }

  // Build the Mutation object type
  let mutation_type = schema.object_type("Mutation", "Root mutation type", all_mutation_fields)

  MutationBuildResult(
    mutation_type: mutation_type,
    union_registry: final_union_registry,
  )
}
```

**Step 4: Update build_mutations_for_record to return updated registry**

```gleam
fn build_mutations_for_record(
  record: RecordInfo,
  object_types: dict.Dict(String, schema.Type),
  create_factory: option.Option(ResolverFactory),
  update_factory: option.Option(ResolverFactory),
  delete_factory: option.Option(ResolverFactory),
  union_registry: union_input_builder.UnionRegistry,
) -> #(List(schema.Field), union_input_builder.UnionRegistry) {
  // Build input type and get updated registry with union field mappings
  let #(input_type, updated_registry) = build_input_type_with_registry(
    record.type_name <> "Input",
    record.nsid,
    record.properties,
    union_registry,
  )

  let create_mutation =
    build_create_mutation(record, object_types, create_factory, input_type)
  let update_mutation =
    build_update_mutation(record, object_types, update_factory, input_type)
  let delete_mutation = build_delete_mutation(record, delete_factory)

  #([create_mutation, update_mutation, delete_mutation], updated_registry)
}
```

**Step 5: Update build_input_type to register union fields**

```gleam
/// Build an InputObjectType and register union fields
fn build_input_type_with_registry(
  type_name: String,
  collection: String,
  properties: List(#(String, types.Property)),
  union_registry: union_input_builder.UnionRegistry,
) -> #(schema.Type, union_input_builder.UnionRegistry) {
  // Create context for union input type resolution
  let ctx = type_mapper.UnionInputContext(
    input_types: union_registry.input_types,
    parent_type_name: type_name,
  )

  // Build fields and register union fields
  let #(input_fields, final_registry) =
    list.fold(properties, #([], union_registry), fn(acc, prop) {
      let #(fields_acc, registry_acc) = acc
      let #(name, types.Property(type_, required, _, ref, refs, items)) = prop

      // Build property for type mapping
      let property = types.Property(type_, required, option.None, ref, refs, items)

      // Use union-aware type mapping with field name for multi-variant naming
      let graphql_type = type_mapper.map_input_type_with_unions(property, name, ctx)

      // Register union fields for later transformation
      let updated_registry = case type_, refs {
        "union", option.Some(ref_list) -> {
          union_input_builder.register_union_field(registry_acc, collection, name, ref_list)
        }
        _, _ -> registry_acc
      }

      // Make required fields non-null
      let field_type = case required {
        True -> schema.non_null(graphql_type)
        False -> graphql_type
      }

      let input_field = schema.input_field(
        name,
        field_type,
        "Input field for " <> name,
        option.None,
      )

      #([input_field, ..fields_acc], updated_registry)
    })

  let reversed_fields = list.reverse(input_fields)
  let input_type = schema.input_object_type(
    type_name,
    "Input type for " <> type_name,
    reversed_fields,
  )

  #(input_type, final_registry)
}
```

**Step 6: Update create and update mutation builders to accept input_type directly**

```gleam
fn build_create_mutation(
  record: RecordInfo,
  object_types: dict.Dict(String, schema.Type),
  factory: option.Option(ResolverFactory),
  input_type: schema.Type,
) -> schema.Field {
  let mutation_name = "create" <> record.type_name

  // Get the complete object type from the dict (includes all join fields)
  let assert Ok(return_type) = dict.get(object_types, record.nsid)

  // Create arguments
  let arguments = [
    schema.argument(
      "input",
      schema.non_null(input_type),
      "Record data",
      option.None,
    ),
    schema.argument(
      "rkey",
      schema.string_type(),
      "Optional record key (defaults to TID)",
      option.None,
    ),
  ]

  // Get resolver - either from factory or use stub
  let collection = record.nsid
  let resolver = case factory {
    option.Some(factory_fn) -> factory_fn(collection)
    option.None -> fn(_resolver_ctx) {
      Error("Create mutation for " <> collection <> " not yet implemented.")
    }
  }

  schema.field_with_args(
    mutation_name,
    return_type,
    "Create a new " <> record.nsid <> " record",
    arguments,
    resolver,
  )
}
```

(Same pattern for `build_update_mutation`)

**Step 7: Verify compilation**

Run: `cd lexicon_graphql && gleam build`
Expected: Build succeeds

**Step 8: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/mutation/builder.gleam
git commit -m "feat: mutation builder returns union registry for dynamic transformation"
```

---

### Task 5: Update Server Schema to Use MutationBuildResult

**Files:**
- Modify: `server/src/graphql/lexicon/schema.gleam`

**Step 1: Update to use MutationBuildResult**

Where mutation type is built, extract the union registry:

```gleam
// Build mutation type and get union registry
let mutation_build_result = mutation_builder.build_mutation_type(
  lexicons,
  object_types,
  create_factory,
  update_factory,
  delete_factory,
  upload_blob_factory,
  custom_mutation_fields,
  option.Some(registry),
)

let mutation_type = mutation_build_result.mutation_type
let union_registry = mutation_build_result.union_registry
```

**Step 2: Pass union_registry to mutation context**

The union_registry needs to be available to mutation resolvers. Update the schema building to include it in the resolver factories or mutation context.

**Step 3: Verify compilation**

Run: `cd server && gleam build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add server/src/graphql/lexicon/schema.gleam
git commit -m "feat: extract union registry from mutation builder"
```

---

### Task 6: Add Union Input Transformation in Mutations

**Files:**
- Modify: `server/src/graphql/lexicon/mutations.gleam`

**Step 1: Add union registry to MutationContext**

Update MutationContext to include the union registry:

```gleam
import lexicon_graphql/internal/graphql/union_input_builder

pub type MutationContext {
  MutationContext(
    db: Executor,
    did_cache: Subject(did_cache.Message),
    signing_key: option.Option(String),
    atp_client_id: String,
    plc_url: String,
    collection_ids: List(String),
    external_collection_ids: List(String),
    union_registry: union_input_builder.UnionRegistry,  // Add this
  )
}
```

**Step 2: Add dynamic union transformation helpers**

```gleam
/// Transform union input using registry-based lookup
/// For single-variant: adds $type based on the single ref
/// For multi-variant: reads "type" enum field, maps to ref, extracts variant data
fn transform_union_input(
  collection: String,
  field_name: String,
  fields: List(#(String, value.Value)),
  union_registry: union_input_builder.UnionRegistry,
) -> value.Value {
  // Look up the refs for this field
  case union_input_builder.get_union_refs(union_registry, collection, field_name) {
    option.Some([single_ref]) -> {
      // Single-variant: just add $type
      let with_type = [#("$type", value.String(single_ref)), ..fields]
      value.Object(with_type)
    }
    option.Some(refs) -> {
      // Multi-variant: read "type" field and extract variant data
      transform_multi_variant_union(fields, refs)
    }
    option.None -> {
      // Not a registered union field, pass through
      value.Object(fields)
    }
  }
}

/// Transform multi-variant union input
/// Input: { type: "SELF_LABELS", selfLabels: { values: [...] } }
/// Output: { "$type": "com.atproto.label.defs#selfLabels", "values": [...] }
fn transform_multi_variant_union(
  fields: List(#(String, value.Value)),
  refs: List(String),
) -> value.Value {
  // Extract the type discriminator
  let type_value = case list.key_find(fields, "type") {
    Ok(value.String(v)) -> option.Some(v)
    Ok(value.Enum(v)) -> option.Some(v)
    _ -> option.None
  }

  case type_value {
    option.Some(enum_value) -> {
      // Find which ref matches this enum value
      case union_input_builder.enum_value_to_ref(enum_value, refs) {
        option.Some(type_ref) -> {
          // Get the variant field name
          let variant_field_name = union_input_builder.ref_to_variant_field_name(type_ref)

          // Extract variant data
          case list.key_find(fields, variant_field_name) {
            Ok(value.Object(variant_fields)) -> {
              // Build AT Protocol format
              let with_type = [#("$type", value.String(type_ref)), ..variant_fields]
              value.Object(with_type)
            }
            _ -> value.Object(fields)  // No variant data
          }
        }
        option.None -> value.Object(fields)  // Unknown enum value
      }
    }
    option.None -> value.Object(fields)  // No type field
  }
}

/// Check if this field is a union field in the registry
fn is_registered_union_field(
  collection: String,
  field_name: String,
  union_registry: union_input_builder.UnionRegistry,
) -> Bool {
  case union_input_builder.get_union_refs(union_registry, collection, field_name) {
    option.Some(_) -> True
    option.None -> False
  }
}
```

**Step 3: Update transform_record_value to use registry**

```gleam
fn transform_record_value(
  val: value.Value,
  field_name: String,
  collection: String,
  union_registry: union_input_builder.UnionRegistry,
) -> value.Value {
  case val {
    value.Object(fields) -> {
      // Check if this is a registered union field
      case is_registered_union_field(collection, field_name, union_registry) {
        True -> transform_union_input(collection, field_name, fields, union_registry)
        False -> {
          // Check for blob pattern
          case is_blob_input(fields) {
            True -> transform_blob_object(fields)
            False -> {
              // Recursively transform nested objects
              value.Object(
                list.map(fields, fn(pair) {
                  let #(name, v) = pair
                  #(name, transform_record_value(v, name, collection, union_registry))
                }),
              )
            }
          }
        }
      }
    }
    value.List(items) ->
      value.List(list.map(items, fn(item) {
        transform_record_value(item, field_name, collection, union_registry)
      }))
    _ -> val
  }
}

/// Check if an object looks like a blob input
fn is_blob_input(fields: List(#(String, value.Value))) -> Bool {
  case list.key_find(fields, "ref"), list.key_find(fields, "mimeType") {
    Ok(_), Ok(_) -> True
    _, _ -> False
  }
}
```

**Step 4: Update mutation resolvers to pass union_registry**

In the create/update resolver factories, pass the union_registry to transform_record_value:

```gleam
// In create_resolver_factory or similar:
let transformed_input = transform_record_value(
  input_value,
  "input",
  collection,
  ctx.union_registry,
)
```

**Step 5: Verify compilation**

Run: `cd server && gleam build`
Expected: Build succeeds

**Step 6: Commit**

```bash
git add server/src/graphql/lexicon/mutations.gleam
git commit -m "feat: dynamic union transformation using registry lookup"
```

---

### Task 7: Write Unit Tests

**Files:**
- Create: `lexicon_graphql/test/union_input_builder_test.gleam`

**Step 1: Create test file**

```gleam
import gleam/dict
import gleam/option
import gleeunit/should
import lexicon_graphql/internal/graphql/union_input_builder
import lexicon_graphql/internal/graphql/type_mapper
import lexicon_graphql/internal/lexicon/registry as lexicon_registry
import lexicon_graphql/types
import swell/schema

pub fn ref_to_input_type_name_test() {
  union_input_builder.ref_to_input_type_name("com.atproto.label.defs#selfLabels")
  |> should.equal("ComAtprotoLabelDefsSelfLabelsInput")
}

pub fn ref_to_variant_enum_value_test() {
  union_input_builder.ref_to_variant_enum_value("com.atproto.label.defs#selfLabels")
  |> should.equal("SELF_LABELS")
}

pub fn ref_to_variant_enum_value_camel_case_test() {
  union_input_builder.ref_to_variant_enum_value("app.bsky.embed.recordWithMedia")
  |> should.equal("RECORD_WITH_MEDIA")
}

pub fn ref_to_variant_field_name_test() {
  union_input_builder.ref_to_variant_field_name("com.atproto.label.defs#selfLabels")
  |> should.equal("selfLabels")
}

pub fn enum_value_to_short_name_test() {
  union_input_builder.enum_value_to_short_name("SELF_LABELS")
  |> should.equal("selfLabels")
}

pub fn enum_value_to_short_name_single_word_test() {
  union_input_builder.enum_value_to_short_name("IMAGES")
  |> should.equal("images")
}

pub fn is_multi_variant_union_single_test() {
  union_input_builder.is_multi_variant_union(option.Some(["ref1"]))
  |> should.be_false()
}

pub fn is_multi_variant_union_multiple_test() {
  union_input_builder.is_multi_variant_union(option.Some(["ref1", "ref2"]))
  |> should.be_true()
}

pub fn register_and_get_union_field_test() {
  let registry = union_input_builder.UnionRegistry(
    input_types: dict.new(),
    field_variants: dict.new(),
  )

  let refs = ["com.atproto.label.defs#selfLabels"]
  let updated = union_input_builder.register_union_field(
    registry,
    "social.grain.gallery",
    "labels",
    refs,
  )

  case union_input_builder.get_union_refs(updated, "social.grain.gallery", "labels") {
    option.Some(found_refs) -> found_refs |> should.equal(refs)
    option.None -> should.fail()
  }
}

pub fn enum_value_to_ref_test() {
  let refs = [
    "com.atproto.label.defs#selfLabels",
    "app.bsky.embed.images",
  ]

  case union_input_builder.enum_value_to_ref("SELF_LABELS", refs) {
    option.Some(ref) -> ref |> should.equal("com.atproto.label.defs#selfLabels")
    option.None -> should.fail()
  }

  case union_input_builder.enum_value_to_ref("IMAGES", refs) {
    option.Some(ref) -> ref |> should.equal("app.bsky.embed.images")
    option.None -> should.fail()
  }
}

pub fn build_union_input_types_creates_input_types_test() {
  let self_label_def = types.ObjectDef(
    type_: "object",
    required_fields: ["val"],
    properties: [
      #("val", types.Property("string", True, option.None, option.None, option.None, option.None)),
    ],
  )

  let self_labels_def = types.ObjectDef(
    type_: "object",
    required_fields: ["values"],
    properties: [
      #("values", types.Property(
        "array",
        True,
        option.None,
        option.None,
        option.None,
        option.Some(types.ArrayItems("ref", option.Some("com.atproto.label.defs#selfLabel"), option.None)),
      )),
    ],
  )

  let object_defs = dict.from_list([
    #("com.atproto.label.defs#selfLabel", self_label_def),
    #("com.atproto.label.defs#selfLabels", self_labels_def),
  ])

  let registry = lexicon_registry.Registry(
    lexicons: dict.new(),
    object_defs: object_defs,
  )

  let result = union_input_builder.build_union_input_types(registry)

  dict.size(result.input_types) |> should.equal(2)

  case dict.get(result.input_types, "com.atproto.label.defs#selfLabel") {
    Ok(input_type) -> {
      schema.type_name(input_type)
      |> should.equal("ComAtprotoLabelDefsSelfLabelInput")
    }
    Error(_) -> should.fail()
  }
}

pub fn multi_variant_union_input_has_type_enum_test() {
  let input_types = dict.from_list([
    #("app.bsky.embed.images", schema.input_object_type("ImagesInput", "Images", [])),
    #("app.bsky.embed.external", schema.input_object_type("ExternalInput", "External", [])),
  ])

  let ctx = type_mapper.UnionInputContext(
    input_types: input_types,
    parent_type_name: "Post",
  )

  let property = types.Property(
    "union",
    False,
    option.None,
    option.None,
    option.Some(["app.bsky.embed.images", "app.bsky.embed.external"]),
    option.None,
  )

  let result = type_mapper.map_input_type_with_unions(property, "embed", ctx)

  schema.type_name(result) |> should.equal("PostEmbedInput")
  schema.is_input_object(result) |> should.be_true()
}
```

**Step 2: Run tests**

Run: `cd lexicon_graphql && gleam test`
Expected: Tests pass

**Step 3: Commit**

```bash
git add lexicon_graphql/test/union_input_builder_test.gleam
git commit -m "test: add unit tests for union input builder with registry"
```

---

### Task 8: Integration Test

**Step 1: Start server and test**

Follow manual testing steps from original plan.

**Step 2: Commit fixes**

---

### Task 9: Final Verification

**Step 1: Run all tests**

**Step 2: Final commit**

---

## Usage Examples

### Single-Variant Union (selfLabels)

```graphql
mutation {
  createSocialGrainGallery(input: {
    title: "My Gallery"
    images: []
    labels: {
      values: [{ val: "art" }, { val: "photography" }]
    }
  }) {
    uri
  }
}
```

### Multi-Variant Union (embed)

```graphql
mutation {
  createAppBskyFeedPost(input: {
    text: "Check this out!"
    embed: {
      type: IMAGES  # Discriminator selects variant
      images: {     # Provide data for selected variant
        images: [{ alt: "Photo", image: { ref: "...", mimeType: "image/jpeg", size: 1234 } }]
      }
    }
  }) {
    uri
  }
}
```

---

## Notes

- **No hardcoded union variants** - everything is derived from lexicons
- Union field registry is built during schema generation from lexicon definitions
- Registry maps `"collection.fieldName"` -> `[refs]` for dynamic lookup
- Single-variant unions use the variant type directly
- Multi-variant unions get a `type` enum field + optional variant fields
- Server transformation uses registry lookup, not hardcoded patterns
