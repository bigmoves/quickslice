/// Lexicon Registry
///
/// Provides cross-lexicon ref resolution for object types.
/// This allows looking up definitions like "social.grain.defs#aspectRatio"
/// across all loaded lexicons.
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option}
import gleam/string
import lexicon_graphql/types

/// Registry that holds all lexicons and allows ref lookups
pub type Registry {
  Registry(
    /// All lexicons indexed by ID
    lexicons: Dict(String, types.Lexicon),
    /// All object definitions indexed by fully-qualified ref (e.g., "social.grain.defs#aspectRatio")
    object_defs: Dict(String, types.ObjectDef),
  )
}

/// Create a registry from a list of lexicons
pub fn from_lexicons(lexicons: List(types.Lexicon)) -> Registry {
  // Build lexicons dict
  let lexicons_dict =
    lexicons
    |> list.map(fn(lex) { #(lex.id, lex) })
    |> dict.from_list

  // Build object defs dict by extracting all object definitions from all lexicons
  // This includes both:
  // 1. Object definitions in "others" (e.g., defs#aspectRatio)
  // 2. Main-level object types (e.g., app.bsky.embed.images where main.type == "object")
  let others_object_defs =
    lexicons
    |> list.flat_map(fn(lex) {
      // Extract all object definitions from this lexicon's "others" dict
      lex.defs.others
      |> dict.to_list
      |> list.filter_map(fn(entry) {
        let #(name, def) = entry
        case def {
          types.Object(obj_def) -> {
            // Create fully-qualified ref: "lexicon.id#defName"
            let ref = lex.id <> "#" <> name
            Ok(#(ref, obj_def))
          }
          types.Record(_) -> Error(Nil)
        }
      })
    })

  // Extract main-level object types (like app.bsky.embed.images)
  let main_object_defs =
    lexicons
    |> list.filter_map(fn(lex) {
      case lex.defs.main {
        option.Some(types.RecordDef(type_: "object", key: _, properties: props)) -> {
          // Convert RecordDef to ObjectDef for main-level object types
          let obj_def =
            types.ObjectDef(
              type_: "object",
              required_fields: [],
              properties: props,
            )
          // Use lexicon id as key (no # fragment for main-level types)
          Ok(#(lex.id, obj_def))
        }
        _ -> Error(Nil)
      }
    })

  // Merge both sources of object definitions
  let object_defs_dict =
    list.append(others_object_defs, main_object_defs)
    |> dict.from_list

  Registry(lexicons: lexicons_dict, object_defs: object_defs_dict)
}

/// Look up an object definition by ref (e.g., "social.grain.defs#aspectRatio")
pub fn get_object_def(
  registry: Registry,
  ref: String,
) -> Option(types.ObjectDef) {
  dict.get(registry.object_defs, ref)
  |> option.from_result
}

/// Look up a lexicon by ID
pub fn get_lexicon(registry: Registry, id: String) -> Option(types.Lexicon) {
  dict.get(registry.lexicons, id)
  |> option.from_result
}

/// Parse a ref into lexicon ID and definition name
/// Example: "social.grain.defs#aspectRatio" -> #("social.grain.defs", "aspectRatio")
pub fn parse_ref(ref: String) -> Option(#(String, String)) {
  case string.split(ref, "#") {
    [lexicon_id, def_name] -> option.Some(#(lexicon_id, def_name))
    _ -> option.None
  }
}

/// Extract lexicon ID from a fully-qualified ref
/// "app.bsky.embed.images#image" -> "app.bsky.embed.images"
/// "app.bsky.embed.images" -> "app.bsky.embed.images"
pub fn lexicon_id_from_ref(ref: String) -> String {
  case string.split(ref, "#") {
    [lexicon_id, _] -> lexicon_id
    _ -> ref
  }
}

/// Get all object definition refs from the registry
pub fn get_all_object_refs(registry: Registry) -> List(String) {
  registry.object_defs
  |> dict.keys
}
