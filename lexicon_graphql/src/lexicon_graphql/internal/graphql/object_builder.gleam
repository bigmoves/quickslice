/// Object Type Builder
///
/// Builds GraphQL object types from ObjectDef definitions.
/// Used for nested object types like aspectRatio that are defined
/// as refs in lexicons (e.g., "social.grain.defs#aspectRatio")
import gleam/dict.{type Dict}
import gleam/list
import gleam/option
import gleam/string
import lexicon_graphql/internal/graphql/type_mapper
import lexicon_graphql/internal/lexicon/nsid
import lexicon_graphql/internal/lexicon/registry as lexicon_registry
import lexicon_graphql/types
import swell/schema
import swell/value

/// Build a GraphQL object type from an ObjectDef
/// object_types_dict is used to resolve refs to other object types
pub fn build_object_type(
  obj_def: types.ObjectDef,
  type_name: String,
  object_types_dict: Dict(String, schema.Type),
) -> schema.Type {
  let fields = build_object_fields(obj_def.properties, object_types_dict)

  schema.object_type(type_name, "Object type from lexicon definition", fields)
}

/// Build GraphQL fields from object properties
fn build_object_fields(
  properties: List(#(String, types.Property)),
  object_types_dict: Dict(String, schema.Type),
) -> List(schema.Field) {
  list.map(properties, fn(prop) {
    let #(name, types.Property(type_, required, format, ref)) = prop

    // Map the type, using the object_types_dict to resolve refs
    let graphql_type =
      type_mapper.map_type_with_registry(type_, format, ref, object_types_dict)

    // Make required fields non-null
    let field_type = case required {
      True -> schema.non_null(graphql_type)
      False -> graphql_type
    }

    // Create field with a resolver that extracts the value from context
    schema.field(name, field_type, "Field from object definition", fn(ctx) {
      case ctx.data {
        option.Some(value.Object(fields)) -> {
          case list.key_find(fields, name) {
            Ok(val) -> Ok(val)
            Error(_) -> Ok(value.Null)
          }
        }
        _ -> Ok(value.Null)
      }
    })
  })
}

/// Build a dict of all object types from the registry
/// Keys are the fully-qualified refs (e.g., "social.grain.defs#aspectRatio")
///
/// Note: This builds types recursively. Object types that reference other object types
/// will have those refs resolved using the same dict (which gets built incrementally).
pub fn build_all_object_types(
  registry: lexicon_registry.Registry,
) -> Dict(String, schema.Type) {
  let object_refs = lexicon_registry.get_all_object_refs(registry)

  // Build all object types in a single pass
  // For simple cases (no circular refs), this works fine
  // TODO: Handle circular refs if needed
  list.fold(object_refs, dict.new(), fn(acc, ref) {
    case lexicon_registry.get_object_def(registry, ref) {
      option.Some(obj_def) -> {
        // Generate a GraphQL type name from the ref
        // e.g., "social.grain.defs#aspectRatio" -> "SocialGrainDefsAspectRatio"
        let type_name = ref_to_type_name(ref)
        // Pass acc as the object_types_dict so we can resolve refs to previously built types
        let object_type = build_object_type(obj_def, type_name, acc)
        dict.insert(acc, ref, object_type)
      }
      option.None -> acc
    }
  })
}

/// Convert a ref to a GraphQL type name
/// Example: "social.grain.defs#aspectRatio" -> "SocialGrainDefsAspectRatio"
fn ref_to_type_name(ref: String) -> String {
  // Replace # with . first, then convert to PascalCase
  let normalized = string.replace(ref, "#", ".")
  nsid.to_type_name(normalized)
}
