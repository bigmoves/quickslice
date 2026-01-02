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
import lexicon_graphql/input/union as union_input
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
  union_input.ref_to_enum_value(ref)
}

/// Convert SCREAMING_SNAKE_CASE back to the original short name
/// "SELF_LABELS" -> "selfLabels"
pub fn enum_value_to_short_name(enum_value: String) -> String {
  union_input.screaming_snake_to_camel(enum_value)
}

/// Convert a ref to a camelCase field name for variant fields
/// "com.atproto.label.defs#selfLabels" -> "selfLabels"
pub fn ref_to_variant_field_name(ref: String) -> String {
  union_input.ref_to_short_name(ref)
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
      let field_type =
        map_property_to_input_type(property, existing_input_types)

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

  let type_enum =
    schema.enum_type(
      enum_name,
      "Discriminator for " <> field_name <> " union variants",
      enum_values,
    )

  // Build input fields: required type discriminator + optional variant fields
  let type_field =
    schema.input_field(
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
        field_type,
        // Optional - user provides data for selected variant
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
pub fn enum_value_to_ref(
  enum_value: String,
  refs: List(String),
) -> Option(String) {
  list.find(refs, fn(ref) { ref_to_variant_enum_value(ref) == enum_value })
  |> option.from_result
}
