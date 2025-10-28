/// Database Schema Builder
///
/// Builds GraphQL schemas from AT Protocol lexicon definitions with database-backed resolvers.
/// This extends the base schema_builder with actual data resolution.
import gleam/list
import gleam/option
import gleam/result
import graphql/schema
import graphql/value
import lexicon_graphql/nsid
import lexicon_graphql/schema_builder
import lexicon_graphql/type_mapper

/// Record type metadata with database resolver info
type RecordType {
  RecordType(
    nsid: String,
    type_name: String,
    field_name: String,
    fields: List(schema.Field),
  )
}

/// Type for a database record fetcher function
/// Takes a collection NSID and returns a list of records as GraphQL values
pub type RecordFetcher =
  fn(String) -> Result(List(value.Value), String)

/// Build a GraphQL schema from lexicons with database-backed resolvers
///
/// The fetcher parameter should be a function that queries the database for records
pub fn build_schema_with_fetcher(
  lexicons: List(schema_builder.Lexicon),
  fetcher: RecordFetcher,
) -> Result(schema.Schema, String) {
  case lexicons {
    [] -> Error("Cannot build schema from empty lexicon list")
    _ -> {
      // Extract record types from lexicons
      let record_types = extract_record_types(lexicons)

      // Build the query type with fields for each record
      let query_type = build_query_type(record_types, fetcher)

      // Create the schema
      Ok(schema.schema(query_type, option.None))
    }
  }
}

/// Extract record types from lexicon definitions
fn extract_record_types(
  lexicons: List(schema_builder.Lexicon),
) -> List(RecordType) {
  lexicons
  |> list.filter_map(parse_lexicon)
}

/// Parse a single lexicon into a RecordType
fn parse_lexicon(lexicon: schema_builder.Lexicon) -> Result(RecordType, Nil) {
  case lexicon {
    schema_builder.Lexicon(
      id,
      schema_builder.Defs(schema_builder.RecordDef("record", properties)),
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
fn build_fields(
  properties: List(#(String, schema_builder.Property)),
) -> List(schema.Field) {
  // Add standard AT Proto fields
  let standard_fields = [
    schema.field(
      "uri",
      schema.string_type(),
      "Record URI",
      fn(ctx: schema.Context) {
        // Extract URI from the record data
        case get_field_from_context(ctx, "uri") {
          Ok(uri) -> Ok(value.String(uri))
          Error(_) -> Ok(value.String(""))
        }
      },
    ),
    schema.field("cid", schema.string_type(), "Record CID", fn(ctx) {
      case get_field_from_context(ctx, "cid") {
        Ok(cid) -> Ok(value.String(cid))
        Error(_) -> Ok(value.String(""))
      }
    }),
    schema.field("did", schema.string_type(), "DID of record author", fn(ctx) {
      case get_field_from_context(ctx, "did") {
        Ok(did) -> Ok(value.String(did))
        Error(_) -> Ok(value.String(""))
      }
    }),
    schema.field("collection", schema.string_type(), "Collection name", fn(ctx) {
      case get_field_from_context(ctx, "collection") {
        Ok(collection) -> Ok(value.String(collection))
        Error(_) -> Ok(value.String(""))
      }
    }),
    schema.field(
      "indexedAt",
      schema.string_type(),
      "When record was indexed",
      fn(ctx) {
        case get_field_from_context(ctx, "indexedAt") {
          Ok(indexed_at) -> Ok(value.String(indexed_at))
          Error(_) -> Ok(value.String(""))
        }
      },
    ),
  ]

  // Build fields from lexicon properties
  let lexicon_fields =
    list.map(properties, fn(prop) {
      let #(name, schema_builder.Property(type_, _required)) = prop
      let graphql_type = type_mapper.map_type(type_)

      schema.field(name, graphql_type, "Field from lexicon", fn(ctx) {
        // Try to extract field from the value object in context
        case get_nested_field_from_context(ctx, "value", name) {
          Ok(val) -> Ok(value.String(val))
          Error(_) -> Ok(value.Null)
        }
      })
    })

  // Combine standard and lexicon fields
  list.append(standard_fields, lexicon_fields)
}

/// Build the root Query type with fields for each record type
fn build_query_type(
  record_types: List(RecordType),
  fetcher: RecordFetcher,
) -> schema.Type {
  let query_fields =
    list.map(record_types, fn(record_type) {
      // Build the object type for this record
      let object_type =
        schema.object_type(
          record_type.type_name,
          "Record type: " <> record_type.nsid,
          record_type.fields,
        )

      // Create a list type for the query field
      let list_type = schema.list_type(object_type)

      // Create query field that returns a list of this record type
      // Capture the nsid and fetcher in the closure
      let collection_nsid = record_type.nsid
      schema.field(
        record_type.field_name,
        list_type,
        "Query " <> record_type.nsid,
        fn(_ctx: schema.Context) {
          // Call the fetcher function to get records from database
          fetcher(collection_nsid)
          |> result.map(fn(records) { value.List(records) })
        },
      )
    })

  schema.object_type("Query", "Root query type", query_fields)
}

/// Helper to extract a field value from resolver context
fn get_field_from_context(
  ctx: schema.Context,
  field_name: String,
) -> Result(String, Nil) {
  case ctx.data {
    option.Some(value.Object(fields)) -> {
      case list.key_find(fields, field_name) {
        Ok(value.String(val)) -> Ok(val)
        _ -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

/// Helper to extract a nested field value from resolver context
fn get_nested_field_from_context(
  ctx: schema.Context,
  parent_field: String,
  field_name: String,
) -> Result(String, Nil) {
  case ctx.data {
    option.Some(value.Object(fields)) -> {
      case list.key_find(fields, parent_field) {
        Ok(value.Object(nested_fields)) -> {
          case list.key_find(nested_fields, field_name) {
            Ok(value.String(val)) -> Ok(val)
            _ -> Error(Nil)
          }
        }
        _ -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}
