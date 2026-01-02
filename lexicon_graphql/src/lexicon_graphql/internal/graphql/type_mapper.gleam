/// Lexicon Type Mapper
///
/// Maps AT Protocol lexicon types to GraphQL types.
/// Simplified MVP version - handles basic types only.
///
/// Based on the Elixir implementation but adapted for the pure Gleam GraphQL library.
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option}
import gleam/string
import lexicon_graphql/internal/lexicon/nsid
import lexicon_graphql/scalar/blob as blob_type
import lexicon_graphql/types
import swell/schema
import swell/value

/// Maps a lexicon type string to a GraphQL output Type.
///
/// ## Examples
///
/// ```gleam
/// map_type("string")   // schema.string_type()
/// map_type("integer")  // schema.int_type()
/// map_type("boolean")  // schema.boolean_type()
/// ```
pub fn map_type(lexicon_type: String) -> schema.Type {
  case lexicon_type {
    // Primitive types
    "string" -> schema.string_type()
    "integer" -> schema.int_type()
    "boolean" -> schema.boolean_type()
    "number" -> schema.float_type()

    // Binary/blob types
    "blob" -> blob_type.create_blob_type()
    "bytes" -> schema.string_type()
    "cid-link" -> schema.string_type()

    // Complex types - simplified to String for MVP
    "unknown" -> schema.string_type()
    "ref" -> schema.string_type()
    "union" -> schema.string_type()

    // Default fallback for any unknown type
    _ -> schema.string_type()
  }
}

/// Maps a lexicon type string to a GraphQL input Type.
/// For most types this is the same as output types, but for complex types
/// like blob, we need to return input object types instead.
///
/// ## Examples
///
/// ```gleam
/// map_input_type("string")   // schema.string_type()
/// map_input_type("blob")     // BlobInput input object type
/// ```
pub fn map_input_type(lexicon_type: String) -> schema.Type {
  case lexicon_type {
    // Primitive types (same as output)
    "string" -> schema.string_type()
    "integer" -> schema.int_type()
    "boolean" -> schema.boolean_type()
    "number" -> schema.float_type()

    // Binary/blob types - use input type for blob
    "blob" -> blob_type.create_blob_input_type()
    "bytes" -> schema.string_type()
    "cid-link" -> schema.string_type()

    // Complex types - simplified to String for MVP
    "unknown" -> schema.string_type()
    "ref" -> schema.string_type()
    "union" -> schema.string_type()

    // Default fallback for any unknown type
    _ -> schema.string_type()
  }
}

/// Get the Blob output type (for mutations and queries)
pub fn get_blob_type() -> schema.Type {
  blob_type.create_blob_type()
}

/// Maps a lexicon type to a GraphQL type, resolving refs using a registry
/// and object types dict.
///
/// This function handles:
/// - Regular types: maps using map_type()
/// - Refs: looks up the ref in object_types_dict to get the actual object type
///
/// Used by object_type_builder to build nested object types.
pub fn map_type_with_registry(
  lexicon_type: String,
  _format: Option(String),
  ref: Option(String),
  object_types_dict: Dict(String, schema.Type),
) -> schema.Type {
  case lexicon_type {
    "ref" ->
      case ref {
        option.Some(ref_str) ->
          case dict.get(object_types_dict, ref_str) {
            Ok(object_type) -> object_type
            Error(_) -> schema.string_type()
          }
        option.None -> schema.string_type()
      }
    _ -> map_type(lexicon_type)
  }
}

/// Maps array items to a GraphQL list type.
/// Returns [ItemType!] where ItemType depends on the items definition.
pub fn map_array_type(
  items: Option(types.ArrayItems),
  object_types: Dict(String, schema.Type),
  parent_type_name: String,
  field_name: String,
) -> schema.Type {
  case items {
    option.None ->
      // Fallback: array without items info -> [String!]
      schema.list_type(schema.non_null(schema.string_type()))

    option.Some(types.ArrayItems(
      type_: item_type,
      ref: item_ref,
      refs: item_refs,
    )) ->
      case item_type {
        "string" -> schema.list_type(schema.non_null(schema.string_type()))
        "integer" -> schema.list_type(schema.non_null(schema.int_type()))
        "boolean" -> schema.list_type(schema.non_null(schema.boolean_type()))
        "number" -> schema.list_type(schema.non_null(schema.float_type()))

        "ref" -> {
          case item_ref {
            option.Some(ref_str) -> {
              // Use raw ref string for lookup - object_types dict is keyed by raw refs
              case dict.get(object_types, ref_str) {
                Ok(obj_type) -> schema.list_type(schema.non_null(obj_type))
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
            option.Some(refs) -> {
              let union_type =
                build_array_union_type(
                  refs,
                  object_types,
                  parent_type_name,
                  field_name,
                )
              schema.list_type(schema.non_null(union_type))
            }
            option.None ->
              schema.list_type(schema.non_null(schema.string_type()))
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
/// Returns a GraphQL union with name like "ParentTypeFieldUnion"
/// Returns String type if no member types can be resolved (to avoid invalid unions)
fn build_array_union_type(
  refs: List(String),
  object_types: Dict(String, schema.Type),
  parent_type_name: String,
  field_name: String,
) -> schema.Type {
  // Look up member types from object_types dict using raw refs (dict is keyed by raw refs)
  let member_types =
    list.filter_map(refs, fn(ref) {
      case dict.get(object_types, ref) {
        Ok(t) -> Ok(t)
        Error(_) -> Error(Nil)
      }
    })

  // If no member types could be resolved, fall back to String type
  // This prevents invalid unions with 0 members
  case member_types {
    [] -> schema.string_type()
    _ -> {
      // Get only the refs that were successfully resolved
      let resolved_refs =
        list.filter(refs, fn(ref) {
          case dict.get(object_types, ref) {
            Ok(_) -> True
            Error(_) -> False
          }
        })

      // Convert resolved refs to type names
      let type_names = list.map(resolved_refs, ref_to_type_name)

      // Build union name: ParentTypeNameFieldNameUnion
      let capitalized_field = capitalize_first(field_name)
      let union_name = parent_type_name <> capitalized_field <> "Union"

      // Build a mapping from $type values to GraphQL type names
      // AT Protocol $type values are like "app.bsky.richtext.facet#mention"
      // GraphQL type names are like "AppBskyRichtextFacetMention"
      let ref_to_name_map =
        list.zip(resolved_refs, type_names)
        |> dict.from_list()

      // Type resolver - inspect $type field in context to determine concrete type
      let type_resolver = fn(ctx: schema.Context) -> Result(String, String) {
        case ctx.data {
          option.Some(value.Object(fields)) -> {
            // Look for $type field in the data
            case list.key_find(fields, "$type") {
              Ok(value.String(type_value)) -> {
                // Map the $type value to a GraphQL type name
                case dict.get(ref_to_name_map, type_value) {
                  Ok(type_name) -> Ok(type_name)
                  Error(_) -> {
                    // Fallback: try to convert the $type value directly
                    Ok(ref_to_type_name(type_value))
                  }
                }
              }
              _ -> {
                // No $type field, fall back to first type
                case type_names {
                  [first, ..] -> Ok(first)
                  [] -> Error("No types in union")
                }
              }
            }
          }
          _ -> {
            // No object data, fall back to first type
            case type_names {
              [first, ..] -> Ok(first)
              [] -> Error("No types in union")
            }
          }
        }
      }

      schema.union_type(
        union_name,
        "Union of array item types",
        member_types,
        type_resolver,
      )
    }
  }
}

/// Maps a lexicon Property to a GraphQL type.
/// Handles arrays specially by looking at the items field.
/// Note: This version doesn't expand local refs. Use map_property_type_with_context
/// with lexicon_id for proper local ref resolution.
pub fn map_property_type(
  property: types.Property,
  object_types: Dict(String, schema.Type),
) -> schema.Type {
  map_property_type_with_context(property, object_types, "", "", "")
}

/// Maps a lexicon Property to a GraphQL type, with parent context for union naming.
/// Handles arrays, refs, and unions with proper type resolution.
/// lexicon_id is used to expand local refs (e.g., "#replyRef" -> "app.bsky.feed.post#replyRef")
pub fn map_property_type_with_context(
  property: types.Property,
  object_types: Dict(String, schema.Type),
  parent_type_name: String,
  field_name: String,
  lexicon_id: String,
) -> schema.Type {
  case property.type_ {
    "array" -> {
      // Expand local refs in array items before lookup
      let expanded_items = case property.items {
        option.Some(types.ArrayItems(
          type_: item_type,
          ref: item_ref,
          refs: item_refs,
        )) -> {
          option.Some(types.ArrayItems(
            type_: item_type,
            ref: option.map(item_ref, fn(r) { expand_ref(r, lexicon_id) }),
            refs: option.map(item_refs, fn(rs) {
              list.map(rs, fn(r) { expand_ref(r, lexicon_id) })
            }),
          ))
        }
        option.None -> option.None
      }
      map_array_type(expanded_items, object_types, parent_type_name, field_name)
    }
    "ref" -> {
      case property.ref {
        option.Some(ref_str) -> {
          // Expand local ref before lookup
          let full_ref = expand_ref(ref_str, lexicon_id)
          case dict.get(object_types, full_ref) {
            Ok(obj_type) -> obj_type
            Error(_) -> schema.string_type()
          }
        }
        option.None -> schema.string_type()
      }
    }
    "union" -> {
      case property.refs {
        option.Some(refs) -> {
          // Expand local refs in union before lookup
          let expanded_refs =
            list.map(refs, fn(r) { expand_ref(r, lexicon_id) })
          build_property_union_type(
            expanded_refs,
            object_types,
            parent_type_name,
            field_name,
          )
        }
        option.None -> schema.string_type()
      }
    }
    _ -> map_type(property.type_)
  }
}

/// Build a union type for a property field.
/// Names the union as ParentTypeNameFieldName (e.g., AppBskyFeedPostEmbed)
/// Returns String type if no member types can be resolved (to avoid invalid unions)
fn build_property_union_type(
  refs: List(String),
  object_types: Dict(String, schema.Type),
  parent_type_name: String,
  field_name: String,
) -> schema.Type {
  // Look up member types from object_types dict
  let member_types =
    list.filter_map(refs, fn(ref) {
      case dict.get(object_types, ref) {
        Ok(t) -> Ok(t)
        Error(_) -> Error(Nil)
      }
    })

  // If no member types could be resolved, fall back to String type
  // This prevents invalid unions with 0 members
  case member_types {
    [] -> schema.string_type()
    _ -> {
      // Build union name: ParentTypeNameFieldName (capitalize field name)
      let capitalized_field = capitalize_first(field_name)
      let union_name = parent_type_name <> capitalized_field

      // Type resolver - inspect $type field to determine concrete type
      // Only include refs that were successfully resolved to types
      let resolved_refs =
        list.filter(refs, fn(ref) {
          case dict.get(object_types, ref) {
            Ok(_) -> True
            Error(_) -> False
          }
        })

      // Convert resolved refs to type names
      let type_names = list.map(resolved_refs, ref_to_type_name)

      // Build a mapping from $type values to GraphQL type names
      let ref_to_name_map =
        list.zip(resolved_refs, type_names)
        |> dict.from_list()

      // Type resolver - inspect $type field in context to determine concrete type
      let type_resolver = fn(ctx: schema.Context) -> Result(String, String) {
        case ctx.data {
          option.Some(value.Object(fields)) -> {
            // Look for $type field in the data
            case list.key_find(fields, "$type") {
              Ok(value.String(type_value)) -> {
                // Map the $type value to a GraphQL type name
                case dict.get(ref_to_name_map, type_value) {
                  Ok(type_name) -> Ok(type_name)
                  Error(_) -> {
                    // Fallback: try to convert the $type value directly
                    Ok(ref_to_type_name(type_value))
                  }
                }
              }
              _ -> {
                // No $type field, fall back to first type
                case type_names {
                  [first, ..] -> Ok(first)
                  [] -> Error("No types in union")
                }
              }
            }
          }
          _ -> {
            // No object data, fall back to first type
            case type_names {
              [first, ..] -> Ok(first)
              [] -> Error("No types in union")
            }
          }
        }
      }

      schema.union_type(
        union_name,
        "Union type for " <> field_name,
        member_types,
        type_resolver,
      )
    }
  }
}

/// Capitalize the first letter of a string
fn capitalize_first(s: String) -> String {
  case string.pop_grapheme(s) {
    Ok(#(first, rest)) -> string.uppercase(first) <> rest
    Error(_) -> s
  }
}

/// Expand a local ref to a fully-qualified ref
/// "#replyRef" with lexicon_id "app.bsky.feed.post" -> "app.bsky.feed.post#replyRef"
/// External refs pass through unchanged
pub fn expand_ref(ref: String, lexicon_id: String) -> String {
  case string.starts_with(ref, "#") {
    True -> lexicon_id <> ref
    False -> ref
  }
}

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
        // Multi-variant: build discriminated union input (2 or more refs)
        option.Some([first, second, ..rest]) -> {
          let refs = [first, second, ..rest]
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
                // Multi-variant array items (2 or more refs)
                option.Some([first, second, ..rest]) -> {
                  let refs = [first, second, ..rest]
                  let item_union =
                    build_multi_variant_union_input(
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
    let is_upper =
      string.uppercase(char) == char && string.lowercase(char) != char
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
