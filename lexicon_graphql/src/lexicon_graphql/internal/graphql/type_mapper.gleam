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
              let union_type = build_array_union_type(refs, object_types)
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
/// Returns a GraphQL union with name like "FmTealAlphaFeedDefsArtistOrFmTealAlphaFeedDefsTrack"
fn build_array_union_type(
  refs: List(String),
  object_types: Dict(String, schema.Type),
) -> schema.Type {
  // Convert refs to type names
  let type_names = list.map(refs, ref_to_type_name)

  // Build union name: "TypeAOrTypeBOrTypeC"
  let union_name = string.join(type_names, "Or")

  // Look up member types from object_types dict using raw refs (dict is keyed by raw refs)
  let member_types =
    list.filter_map(refs, fn(ref) {
      case dict.get(object_types, ref) {
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

  schema.union_type(
    union_name,
    "Union of array item types",
    member_types,
    type_resolver,
  )
}

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
