/// Object Type Builder
///
/// Builds GraphQL object types from ObjectDef definitions.
/// Used for nested object types like aspectRatio that are defined
/// as refs in lexicons (e.g., "social.grain.defs#aspectRatio")
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/option
import gleam/order
import gleam/string
import lexicon_graphql/internal/graphql/type_mapper
import lexicon_graphql/internal/lexicon/nsid
import lexicon_graphql/internal/lexicon/registry as lexicon_registry
import lexicon_graphql/internal/lexicon/uri_extractor
import lexicon_graphql/query/dataloader
import lexicon_graphql/types
import swell/schema
import swell/value

/// Batch fetcher type alias for convenience
pub type BatchFetcher =
  dataloader.BatchFetcher

/// Type of forward join field found in an object
pub type NestedForwardJoinField {
  NestedStrongRefField(name: String)
  NestedAtUriField(name: String)
}

/// Identify forward join fields in object properties
/// Returns list of fields that can be resolved to other records
pub fn identify_forward_join_fields(
  properties: List(#(String, types.Property)),
) -> List(NestedForwardJoinField) {
  list.filter_map(properties, fn(prop) {
    let #(name, property) = prop
    case property.type_, property.ref, property.format {
      // strongRef field
      "ref", option.Some(ref), _ if ref == "com.atproto.repo.strongRef" ->
        Ok(NestedStrongRefField(name))
      // at-uri string field
      "string", _, option.Some(fmt) if fmt == "at-uri" ->
        Ok(NestedAtUriField(name))
      _, _, _ -> Error(Nil)
    }
  })
}

/// Build *Resolved fields for nested forward joins
fn build_nested_forward_join_fields(
  join_fields: List(NestedForwardJoinField),
  generic_record_type: schema.Type,
  batch_fetcher: option.Option(BatchFetcher),
) -> List(schema.Field) {
  list.map(join_fields, fn(join_field) {
    let field_name = case join_field {
      NestedStrongRefField(name) -> name
      NestedAtUriField(name) -> name
    }

    schema.field(
      field_name <> "Resolved",
      generic_record_type,
      "Forward join to referenced record",
      fn(ctx) {
        // Extract the field value from the parent object
        case ctx.data {
          option.Some(value.Object(fields)) -> {
            case list.key_find(fields, field_name) {
              Ok(field_value) -> {
                // Extract URI using uri_extractor
                case uri_extractor.extract_uri(value_to_dynamic(field_value)) {
                  option.Some(uri) -> {
                    // Use batch fetcher to resolve the record
                    case batch_fetcher {
                      option.Some(fetcher) -> {
                        case dataloader.batch_fetch_by_uri([uri], fetcher) {
                          Ok(results) -> {
                            case dict.get(results, uri) {
                              Ok(record) -> Ok(record)
                              Error(_) -> Ok(value.Null)
                            }
                          }
                          Error(_) -> Ok(value.Null)
                        }
                      }
                      option.None -> Ok(value.String(uri))
                    }
                  }
                  option.None -> Ok(value.Null)
                }
              }
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    )
  })
}

/// Convert a GraphQL Value to Dynamic for uri_extractor
fn value_to_dynamic(val: value.Value) -> Dynamic {
  case val {
    value.String(s) -> unsafe_coerce_to_dynamic(s)
    value.Int(i) -> unsafe_coerce_to_dynamic(i)
    value.Float(f) -> unsafe_coerce_to_dynamic(f)
    value.Boolean(b) -> unsafe_coerce_to_dynamic(b)
    value.Null -> unsafe_coerce_to_dynamic(option.None)
    value.Object(fields) -> {
      let field_map =
        list.fold(fields, dict.new(), fn(acc, field) {
          let #(key, field_val) = field
          dict.insert(acc, key, value_to_dynamic(field_val))
        })
      unsafe_coerce_to_dynamic(field_map)
    }
    value.List(items) -> {
      let converted = list.map(items, value_to_dynamic)
      unsafe_coerce_to_dynamic(converted)
    }
    value.Enum(name) -> unsafe_coerce_to_dynamic(name)
  }
}

@external(erlang, "object_builder_ffi", "identity")
fn unsafe_coerce_to_dynamic(value: a) -> Dynamic

/// Sort refs so that #fragment refs come before main refs
/// This ensures dependencies are built first
/// e.g., "app.bsky.richtext.facet#mention" before "app.bsky.richtext.facet"
fn sort_refs_dependencies_first(refs: List(String)) -> List(String) {
  list.sort(refs, fn(a, b) {
    let a_has_hash = string.contains(a, "#")
    let b_has_hash = string.contains(b, "#")
    case a_has_hash, b_has_hash {
      // Both have # or both don't - sort alphabetically for determinism
      True, True -> string.compare(a, b)
      False, False -> string.compare(a, b)
      // # refs come first
      True, False -> order.Lt
      False, True -> order.Gt
    }
  })
}

/// Build a GraphQL object type from an ObjectDef
/// object_types_dict is used to resolve refs to other object types
/// batch_fetcher and generic_record_type are optional - when provided, *Resolved fields are added
pub fn build_object_type(
  obj_def: types.ObjectDef,
  type_name: String,
  lexicon_id: String,
  object_types_dict: Dict(String, schema.Type),
  batch_fetcher: option.Option(BatchFetcher),
  generic_record_type: option.Option(schema.Type),
) -> schema.Type {
  let lexicon_fields =
    build_object_fields(
      obj_def.properties,
      lexicon_id,
      object_types_dict,
      type_name,
    )

  // Build forward join fields if we have the necessary dependencies
  let forward_join_fields = case batch_fetcher, generic_record_type {
    option.Some(_fetcher), option.Some(record_type) -> {
      let join_fields = identify_forward_join_fields(obj_def.properties)
      build_nested_forward_join_fields(join_fields, record_type, batch_fetcher)
    }
    _, _ -> []
  }

  // Combine regular fields with forward join fields
  let all_fields = list.append(lexicon_fields, forward_join_fields)

  // GraphQL requires at least one field - add placeholder for empty objects
  let fields = case all_fields {
    [] -> [
      schema.field(
        "_",
        schema.boolean_type(),
        "Placeholder field for empty object type",
        fn(_ctx) { Ok(value.Boolean(True)) },
      ),
    ]
    _ -> all_fields
  }

  schema.object_type(type_name, "Object type from lexicon definition", fields)
}

/// Build GraphQL fields from object properties
fn build_object_fields(
  properties: List(#(String, types.Property)),
  lexicon_id: String,
  object_types_dict: Dict(String, schema.Type),
  parent_type_name: String,
) -> List(schema.Field) {
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
        type_mapper.map_array_type(
          expanded_items,
          object_types_dict,
          parent_type_name,
          name,
        )
      }
      _ -> {
        // Expand local refs (e.g., "#byteSlice" -> "app.bsky.richtext.facet#byteSlice")
        let expanded_ref = option.map(ref, fn(r) { expand_ref(r, lexicon_id) })
        type_mapper.map_type_with_registry(
          type_,
          format,
          expanded_ref,
          object_types_dict,
        )
      }
    }

    // Make required fields non-null
    let field_type = case required {
      True -> schema.non_null(graphql_type)
      False -> graphql_type
    }

    // Create field with a resolver that extracts the value from context
    // For blob fields, enrich with did; for nested objects, propagate did
    schema.field(name, field_type, "Field from object definition", fn(ctx) {
      case ctx.data {
        option.Some(value.Object(fields)) -> {
          // Get did from parent if available (propagated from record level)
          let parent_did = case list.key_find(fields, "did") {
            Ok(value.String(d)) -> option.Some(d)
            _ -> option.None
          }

          case list.key_find(fields, name) {
            Ok(val) -> {
              case type_, parent_did {
                // For blob fields, ensure did is injected
                "blob", option.Some(did) -> Ok(enrich_blob_with_did(val, did))
                // For nested objects/arrays, propagate did
                _, option.Some(did) -> Ok(propagate_did(val, did))
                // No did available, return as-is
                _, option.None -> Ok(val)
              }
            }
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
/// When batch_fetcher and generic_record_type are provided, nested forward joins are enabled
pub fn build_all_object_types(
  registry: lexicon_registry.Registry,
  batch_fetcher: option.Option(BatchFetcher),
  generic_record_type: option.Option(schema.Type),
) -> Dict(String, schema.Type) {
  let object_refs = lexicon_registry.get_all_object_refs(registry)

  // Sort refs so #fragment refs are built before main refs
  // This ensures union member types exist when main types reference them
  let sorted_refs = sort_refs_dependencies_first(object_refs)

  // Build all object types in dependency order
  list.fold(sorted_refs, dict.new(), fn(acc, ref) {
    case lexicon_registry.get_object_def(registry, ref) {
      option.Some(obj_def) -> {
        // Generate a GraphQL type name from the ref
        // e.g., "social.grain.defs#aspectRatio" -> "SocialGrainDefsAspectRatio"
        let type_name = ref_to_type_name(ref)
        let lexicon_id = lexicon_registry.lexicon_id_from_ref(ref)
        // Pass acc as the object_types_dict so we can resolve refs to previously built types
        let object_type =
          build_object_type(
            obj_def,
            type_name,
            lexicon_id,
            acc,
            batch_fetcher,
            generic_record_type,
          )
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

/// Expand a shorthand ref to fully-qualified ref
/// "#image" with lexicon_id "app.bsky.embed.images" -> "app.bsky.embed.images#image"
fn expand_ref(ref: String, lexicon_id: String) -> String {
  case string.starts_with(ref, "#") {
    True -> lexicon_id <> ref
    False -> ref
  }
}

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

/// Enrich a blob value with did for URL generation
/// Handles both raw blob format and AT Protocol $link format
fn enrich_blob_with_did(val: value.Value, did: String) -> value.Value {
  case val {
    value.Object(fields) -> {
      // Extract ref - handle nested $link format from AT Protocol
      let ref = case list.key_find(fields, "ref") {
        Ok(value.Object(ref_obj)) -> {
          case list.key_find(ref_obj, "$link") {
            Ok(value.String(cid)) -> cid
            _ -> ""
          }
        }
        Ok(value.String(cid)) -> cid
        _ -> ""
      }

      let mime_type = case list.key_find(fields, "mimeType") {
        Ok(value.String(mt)) -> mt
        _ -> "image/jpeg"
      }

      let size = case list.key_find(fields, "size") {
        Ok(value.Int(s)) -> s
        _ -> 0
      }

      value.Object([
        #("ref", value.String(ref)),
        #("mime_type", value.String(mime_type)),
        #("size", value.Int(size)),
        #("did", value.String(did)),
      ])
    }
    _ -> val
  }
}

/// Propagate did through nested objects and arrays
fn propagate_did(val: value.Value, did: String) -> value.Value {
  case val {
    value.Object(fields) -> {
      let has_did = list.any(fields, fn(f) { f.0 == "did" })
      case has_did {
        True -> val
        False ->
          value.Object(list.append(fields, [#("did", value.String(did))]))
      }
    }
    value.List(items) -> {
      value.List(list.map(items, fn(item) { propagate_did(item, did) }))
    }
    _ -> val
  }
}
