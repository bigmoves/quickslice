/// Schema Builder
///
/// Builds GraphQL schemas from AT Protocol lexicon definitions.
/// Simplified MVP version - handles basic record types only.
import gleam/dict
import gleam/list
import gleam/option
import gleam/string
import lexicon_graphql/internal/graphql/type_mapper
import lexicon_graphql/internal/lexicon/nsid
import lexicon_graphql/mutation/builder as mutation_builder
import lexicon_graphql/types
import swell/schema
import swell/value

/// Re-export types for backwards compatibility
pub type Lexicon =
  types.Lexicon

pub type Defs =
  types.Defs

pub type RecordDef =
  types.RecordDef

pub type Property =
  types.Property

/// Record type metadata
type RecordType {
  RecordType(
    nsid: String,
    type_name: String,
    field_name: String,
    fields: List(schema.Field),
  )
}

/// Builds a GraphQL schema from a list of lexicon definitions.
///
/// Returns a Schema that can be used for query execution.
/// Includes mutations with stub resolvers (no actual implementation).
pub fn build_schema(lexicons: List(Lexicon)) -> Result(schema.Schema, String) {
  case lexicons {
    [] -> Error("Cannot build schema from empty lexicon list")
    _ -> {
      // First extract ref object types from lexicon "others" (e.g., #artist, #aspectRatio)
      let ref_object_types = extract_ref_object_types(lexicons)

      // Extract object-type lexicons (like embed types)
      let object_type_lexicons =
        extract_object_type_lexicons(lexicons, ref_object_types)

      // Merge ref_object_types with object_type_lexicons
      let all_object_types = dict.merge(ref_object_types, object_type_lexicons)

      // Extract record types from lexicons, passing all object types for field resolution
      let record_types = extract_record_types(lexicons, all_object_types)

      // Build object types dict including record types
      let record_object_types = build_object_types_dict(record_types)
      let object_types = dict.merge(all_object_types, record_object_types)

      // Build the query type with fields for each record (not object types)
      let query_type = build_query_type(record_types, object_types)

      // Build the mutation type with stub resolvers, using shared object types
      let mutation_type =
        mutation_builder.build_mutation_type(
          lexicons,
          object_types,
          option.None,
          option.None,
          option.None,
          option.None,
        )

      // Create the schema with queries and mutations
      Ok(schema.schema(query_type, option.Some(mutation_type)))
    }
  }
}

/// Extract record types from lexicon definitions
fn extract_record_types(
  lexicons: List(Lexicon),
  ref_object_types: dict.Dict(String, schema.Type),
) -> List(RecordType) {
  lexicons
  |> list.filter_map(fn(lex) { parse_lexicon(lex, ref_object_types) })
}

/// Parse a single lexicon into a RecordType
fn parse_lexicon(
  lexicon: Lexicon,
  ref_object_types: dict.Dict(String, schema.Type),
) -> Result(RecordType, Nil) {
  case lexicon {
    types.Lexicon(
      id,
      types.Defs(option.Some(types.RecordDef("record", _, properties)), _),
    ) -> {
      let type_name = nsid.to_type_name(id)
      let field_name = nsid.to_field_name(id)
      let fields =
        build_fields_with_context(properties, ref_object_types, type_name, id)

      Ok(RecordType(
        nsid: id,
        type_name: type_name,
        field_name: field_name,
        fields: fields,
      ))
    }
    _ -> Error(Nil)
  }
}

/// Build GraphQL fields from lexicon properties with parent type context
fn build_fields_with_context(
  properties: List(#(String, Property)),
  ref_object_types: dict.Dict(String, schema.Type),
  parent_type_name: String,
  lexicon_id: String,
) -> List(schema.Field) {
  // Add standard AT Proto fields
  let standard_fields = [
    schema.field("uri", schema.string_type(), "Record URI", fn(_ctx) {
      Ok(value.String("at://did:plc:example/collection/rkey"))
    }),
    schema.field("cid", schema.string_type(), "Record CID", fn(_ctx) {
      Ok(value.String("bafyreicid"))
    }),
    schema.field("did", schema.string_type(), "DID of record author", fn(_ctx) {
      Ok(value.String("did:plc:example"))
    }),
    schema.field(
      "indexedAt",
      schema.string_type(),
      "When record was indexed",
      fn(_ctx) { Ok(value.String("2024-01-01T00:00:00Z")) },
    ),
  ]

  // Build fields from lexicon properties with context
  let lexicon_fields =
    list.map(properties, fn(prop) {
      let #(name, property) = prop
      let graphql_type =
        type_mapper.map_property_type_with_context(
          property,
          ref_object_types,
          parent_type_name,
          name,
          lexicon_id,
        )

      schema.field(name, graphql_type, "Field from lexicon", fn(_ctx) {
        Ok(value.Null)
      })
    })

  // Combine standard and lexicon fields
  list.append(standard_fields, lexicon_fields)
}

/// Build a dict of object types keyed by nsid
fn build_object_types_dict(
  record_types: List(RecordType),
) -> dict.Dict(String, schema.Type) {
  list.fold(record_types, dict.new(), fn(acc, record_type) {
    let object_type =
      schema.object_type(
        record_type.type_name,
        "Record type: " <> record_type.nsid,
        record_type.fields,
      )
    dict.insert(acc, record_type.nsid, object_type)
  })
}

/// Extract object types from lexicon "others" defs (e.g., #artist, #aspectRatio)
/// Returns a dict keyed by full ref like "fm.teal.alpha.feed.track#artist"
///
/// Builds types in two passes:
/// 1. First pass: build types that have no ref dependencies (leaf types)
/// 2. Second pass: build types that reference other types (can now resolve refs)
fn extract_ref_object_types(
  lexicons: List(Lexicon),
) -> dict.Dict(String, schema.Type) {
  // Collect all object defs with their metadata
  let all_defs =
    list.flat_map(lexicons, fn(lexicon) {
      let types.Lexicon(id, types.Defs(_, others)) = lexicon
      dict.to_list(others)
      |> list.filter_map(fn(entry) {
        let #(def_name, def) = entry
        case def {
          types.Object(obj_def) -> {
            let full_ref = id <> "#" <> def_name
            Ok(#(full_ref, id, obj_def))
          }
          types.Record(_) -> Error(Nil)
        }
      })
    })

  // Partition into types with refs and types without refs
  let #(with_refs, without_refs) =
    list.partition(all_defs, fn(entry) {
      let #(_, _, obj_def) = entry
      has_ref_properties(obj_def)
    })

  // First pass: build leaf types (no ref dependencies)
  let leaf_types =
    list.fold(without_refs, dict.new(), fn(acc, entry) {
      let #(full_ref, lexicon_id, obj_def) = entry
      let object_type =
        build_others_object_type(full_ref, lexicon_id, obj_def, acc)
      dict.insert(acc, full_ref, object_type)
    })

  // Second pass: build types that have refs (can now resolve to leaf types)
  list.fold(with_refs, leaf_types, fn(acc, entry) {
    let #(full_ref, lexicon_id, obj_def) = entry
    let object_type =
      build_others_object_type(full_ref, lexicon_id, obj_def, acc)
    dict.insert(acc, full_ref, object_type)
  })
}

/// Check if an ObjectDef has any ref properties
fn has_ref_properties(obj_def: types.ObjectDef) -> Bool {
  list.any(obj_def.properties, fn(prop) {
    let #(_, property) = prop
    property.type_ == "ref"
    || property.type_ == "union"
    || case property.items {
      option.Some(items) -> items.type_ == "ref" || option.is_some(items.refs)
      option.None -> False
    }
  })
}

/// Build a single object type from an ObjectDef in "others"
fn build_others_object_type(
  full_ref: String,
  lexicon_id: String,
  obj_def: types.ObjectDef,
  existing_types: dict.Dict(String, schema.Type),
) -> schema.Type {
  let type_name = nsid.to_type_name(string.replace(full_ref, "#", "."))

  let lexicon_fields =
    list.map(obj_def.properties, fn(prop) {
      let #(name, property) = prop
      let graphql_type =
        type_mapper.map_property_type_with_context(
          property,
          existing_types,
          type_name,
          name,
          lexicon_id,
        )

      // Apply required/non-null wrapper
      let field_type = case list.contains(obj_def.required_fields, name) {
        True -> schema.non_null(graphql_type)
        False -> graphql_type
      }

      schema.field(name, field_type, "Field from object def", fn(_ctx) {
        Ok(value.Null)
      })
    })

  // GraphQL requires at least one field - add placeholder for empty objects
  let fields = case lexicon_fields {
    [] -> [
      schema.field(
        "_",
        schema.boolean_type(),
        "Placeholder field for empty object type",
        fn(_ctx) { Ok(value.Boolean(True)) },
      ),
    ]
    _ -> lexicon_fields
  }

  schema.object_type(type_name, "Object type: " <> full_ref, fields)
}

/// Build the root Query type with fields for each record type
fn build_query_type(
  record_types: List(RecordType),
  object_types: dict.Dict(String, schema.Type),
) -> schema.Type {
  let query_fields =
    list.map(record_types, fn(record_type) {
      // Get the pre-built object type from dict
      let assert Ok(object_type) = dict.get(object_types, record_type.nsid)

      // Create a list type for the query field
      let list_type = schema.list_type(object_type)

      // Create query field that returns a list of this record type
      schema.field(
        record_type.field_name,
        list_type,
        "Query " <> record_type.nsid,
        fn(_ctx) {
          // For now, return empty list - database resolver will be added later
          Ok(value.List([]))
        },
      )
    })

  schema.object_type("Query", "Root query type", query_fields)
}

/// Extract object types from object-type lexicons (type: "object" at main level)
/// These are NOT record types - they don't get query fields, just exist as types
fn extract_object_type_lexicons(
  lexicons: List(Lexicon),
  ref_object_types: dict.Dict(String, schema.Type),
) -> dict.Dict(String, schema.Type) {
  list.fold(lexicons, dict.new(), fn(acc, lexicon) {
    case lexicon {
      types.Lexicon(
        id,
        types.Defs(option.Some(types.RecordDef("object", _, properties)), _),
      ) -> {
        let type_name = nsid.to_type_name(id)
        let fields = build_object_type_fields(properties, ref_object_types, id)
        let object_type =
          schema.object_type(type_name, "Object type: " <> id, fields)
        dict.insert(acc, id, object_type)
      }
      _ -> acc
    }
  })
}

/// Build fields for object types (simplified - no standard AT Proto fields)
fn build_object_type_fields(
  properties: List(#(String, Property)),
  ref_object_types: dict.Dict(String, schema.Type),
  lexicon_id: String,
) -> List(schema.Field) {
  list.map(properties, fn(prop) {
    let #(name, property) = prop
    let graphql_type =
      type_mapper.map_property_type_with_context(
        property,
        ref_object_types,
        "",
        name,
        lexicon_id,
      )
    schema.field(name, graphql_type, "Field from lexicon", fn(_ctx) {
      Ok(value.Null)
    })
  })
}
