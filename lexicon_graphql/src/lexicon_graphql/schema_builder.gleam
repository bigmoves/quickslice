/// Schema Builder
///
/// Builds GraphQL schemas from AT Protocol lexicon definitions.
/// Simplified MVP version - handles basic record types only.
import gleam/dict
import gleam/list
import gleam/option
import lexicon_graphql/mutation_builder
import lexicon_graphql/nsid
import lexicon_graphql/type_mapper
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
      // Extract record types from lexicons
      let record_types = extract_record_types(lexicons)

      // Build object types dict for sharing between queries and mutations
      let object_types = build_object_types_dict(record_types)

      // Build the query type with fields for each record
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
fn extract_record_types(lexicons: List(Lexicon)) -> List(RecordType) {
  lexicons
  |> list.filter_map(parse_lexicon)
}

/// Parse a single lexicon into a RecordType
fn parse_lexicon(lexicon: Lexicon) -> Result(RecordType, Nil) {
  case lexicon {
    types.Lexicon(
      id,
      types.Defs(option.Some(types.RecordDef("record", _, properties)), _),
    ) -> {
      let type_name = nsid.to_type_name(id)
      let field_name = nsid.to_field_name(id)
      let fields = build_fields(properties)

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

/// Build GraphQL fields from lexicon properties
fn build_fields(properties: List(#(String, Property))) -> List(schema.Field) {
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

  // Build fields from lexicon properties
  let lexicon_fields =
    list.map(properties, fn(prop) {
      let #(name, types.Property(type_, _required, _, _)) = prop
      let graphql_type = type_mapper.map_type(type_)

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
