/// Schema Builder
///
/// Builds GraphQL schemas from AT Protocol lexicon definitions.
/// Simplified MVP version - handles basic record types only.
import gleam/dict
import gleam/list
import gleam/option
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
fn extract_ref_object_types(
  lexicons: List(Lexicon),
) -> dict.Dict(String, schema.Type) {
  list.fold(lexicons, dict.new(), fn(acc, lexicon) {
    let types.Lexicon(id, types.Defs(_, others)) = lexicon
    dict.fold(others, acc, fn(inner_acc, def_name, def) {
      case def {
        types.Object(types.ObjectDef(_, _, properties)) -> {
          // Build full ref: "fm.teal.alpha.feed.track#artist"
          let full_ref = id <> "#" <> def_name
          let type_name = nsid.to_type_name(id <> "." <> def_name)

          // Build fields for the object type
          let lexicon_fields =
            list.map(properties, fn(prop) {
              let #(name, property) = prop
              let graphql_type = type_mapper.map_type(property.type_)
              schema.field(
                name,
                graphql_type,
                "Field from object def",
                fn(_ctx) { Ok(value.Null) },
              )
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

          let object_type =
            schema.object_type(type_name, "Object type: " <> full_ref, fields)
          dict.insert(inner_acc, full_ref, object_type)
        }
        types.Record(_) -> inner_acc
      }
    })
  })
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
