/// Mutation Builder
///
/// Builds GraphQL mutation types from AT Protocol lexicon definitions.
/// Generates create, update, and delete mutations for each record type.
///
/// Note: The resolvers are currently stubs. Actual mutation logic should be
/// implemented in the server layer by extracting data from the GraphQL context.
import gleam/list
import gleam/option
import graphql/schema
import graphql/value
import lexicon_graphql/nsid
import lexicon_graphql/type_mapper
import lexicon_graphql/types

/// Resolver factory function type
/// Takes collection name and returns a resolver function
pub type ResolverFactory =
  fn(String) -> schema.Resolver

/// Build a GraphQL Mutation type from lexicon definitions
///
/// For each record type, generates:
/// - create{TypeName}(input: {TypeName}Input!, rkey: String): {TypeName}
/// - update{TypeName}(rkey: String!, input: {TypeName}Input!): {TypeName}
/// - delete{TypeName}(rkey: String!): {TypeName}
///
/// Resolver factories are optional - if None, mutations will return errors
pub fn build_mutation_type(
  lexicons: List(types.Lexicon),
  create_factory: option.Option(ResolverFactory),
  update_factory: option.Option(ResolverFactory),
  delete_factory: option.Option(ResolverFactory),
) -> schema.Type {
  // Extract record types
  let record_types = extract_record_types(lexicons)

  // Build mutation fields for each record type
  let mutation_fields =
    list.flat_map(record_types, fn(record) {
      build_mutations_for_record(
        record,
        create_factory,
        update_factory,
        delete_factory,
      )
    })

  // Build the Mutation object type
  schema.object_type("Mutation", "Root mutation type", mutation_fields)
}

/// Record type info needed for building mutations
type RecordInfo {
  RecordInfo(
    nsid: String,
    type_name: String,
    properties: List(#(String, types.Property)),
  )
}

/// Extract record information from lexicons
fn extract_record_types(
  lexicons: List(types.Lexicon),
) -> List(RecordInfo) {
  lexicons
  |> list.filter_map(fn(lexicon) {
    case lexicon {
      types.Lexicon(
        id,
        types.Defs(types.RecordDef("record", properties)),
      ) -> {
        let type_name = nsid.to_type_name(id)
        Ok(RecordInfo(nsid: id, type_name: type_name, properties: properties))
      }
      _ -> Error(Nil)
    }
  })
}

/// Build all three mutations (create, update, delete) for a record type
fn build_mutations_for_record(
  record: RecordInfo,
  create_factory: option.Option(ResolverFactory),
  update_factory: option.Option(ResolverFactory),
  delete_factory: option.Option(ResolverFactory),
) -> List(schema.Field) {
  let create_mutation = build_create_mutation(record, create_factory)
  let update_mutation = build_update_mutation(record, update_factory)
  let delete_mutation = build_delete_mutation(record, delete_factory)

  [create_mutation, update_mutation, delete_mutation]
}

/// Build create mutation for a record type
/// Signature: create{TypeName}(input: {TypeName}Input!, rkey: String): {TypeName}
fn build_create_mutation(
  record: RecordInfo,
  factory: option.Option(ResolverFactory),
) -> schema.Field {
  let mutation_name = "create" <> record.type_name
  let input_type_name = record.type_name <> "Input"

  // Build the input type
  let input_type = build_input_type(input_type_name, record.properties)

  // Build the return type (the record object type)
  let return_type = build_record_object_type(record)

  // Create arguments
  let arguments = [
    schema.argument(
      "input",
      schema.non_null(input_type),
      "Record data",
      option.None,
    ),
    schema.argument(
      "rkey",
      schema.string_type(),
      "Optional record key (defaults to TID)",
      option.None,
    ),
  ]

  // Get resolver - either from factory or use stub
  let collection = record.nsid
  let resolver = case factory {
    option.Some(factory_fn) -> factory_fn(collection)
    option.None ->
      fn(_resolver_ctx) {
        Error(
          "Create mutation for "
          <> collection
          <> " not yet implemented. Use XRPC endpoint instead.",
        )
      }
  }

  // Create the mutation field with arguments
  schema.field_with_args(
    mutation_name,
    return_type,
    "Create a new " <> record.nsid <> " record",
    arguments,
    resolver,
  )
}

/// Build update mutation for a record type
/// Signature: update{TypeName}(rkey: String!, input: {TypeName}Input!): {TypeName}
fn build_update_mutation(
  record: RecordInfo,
  factory: option.Option(ResolverFactory),
) -> schema.Field {
  let mutation_name = "update" <> record.type_name
  let input_type_name = record.type_name <> "Input"

  // Build the input type
  let input_type = build_input_type(input_type_name, record.properties)

  // Build the return type (the record object type)
  let return_type = build_record_object_type(record)

  // Create arguments
  let arguments = [
    schema.argument(
      "rkey",
      schema.non_null(schema.string_type()),
      "Record key to update",
      option.None,
    ),
    schema.argument(
      "input",
      schema.non_null(input_type),
      "Updated record data",
      option.None,
    ),
  ]

  // Get resolver - either from factory or use stub
  let collection = record.nsid
  let resolver = case factory {
    option.Some(factory_fn) -> factory_fn(collection)
    option.None ->
      fn(_resolver_ctx) {
        Error(
          "Update mutation for "
          <> collection
          <> " not yet implemented. Use XRPC endpoint instead.",
        )
      }
  }

  // Create the mutation field with arguments
  schema.field_with_args(
    mutation_name,
    return_type,
    "Update an existing " <> record.nsid <> " record",
    arguments,
    resolver,
  )
}

/// Build delete mutation for a record type
/// Signature: delete{TypeName}(rkey: String!): DeleteResult
fn build_delete_mutation(
  record: RecordInfo,
  factory: option.Option(ResolverFactory),
) -> schema.Field {
  let mutation_name = "delete" <> record.type_name

  // Build the return type (simple deletion result with just URI)
  let return_type = build_delete_result_type()

  // Create arguments
  let arguments = [
    schema.argument(
      "rkey",
      schema.non_null(schema.string_type()),
      "Record key to delete",
      option.None,
    ),
  ]

  // Get resolver - either from factory or use stub
  let collection = record.nsid
  let resolver = case factory {
    option.Some(factory_fn) -> factory_fn(collection)
    option.None ->
      fn(_resolver_ctx) {
        Error(
          "Delete mutation for "
          <> collection
          <> " not yet implemented. Use XRPC endpoint instead.",
        )
      }
  }

  // Create the mutation field with arguments
  schema.field_with_args(
    mutation_name,
    return_type,
    "Delete a " <> record.nsid <> " record",
    arguments,
    resolver,
  )
}

/// Build an InputObjectType from lexicon properties
fn build_input_type(
  type_name: String,
  properties: List(#(String, types.Property)),
) -> schema.Type {
  let input_fields =
    list.map(properties, fn(prop) {
      let #(name, types.Property(type_, required)) = prop
      // Use map_input_type to get input-compatible types (e.g., BlobInput instead of Blob)
      let graphql_type = type_mapper.map_input_type(type_)

      // Make required fields non-null
      let field_type = case required {
        True -> schema.non_null(graphql_type)
        False -> graphql_type
      }

      schema.input_field(name, field_type, "Input field for " <> name, option.None)
    })

  schema.input_object_type(
    type_name,
    "Input type for " <> type_name,
    input_fields,
  )
}

/// Build a simple deletion result type that only contains URI
fn build_delete_result_type() -> schema.Type {
  let fields = [
    schema.field("uri", schema.string_type(), "URI of deleted record", fn(ctx) {
      case ctx.data {
        option.Some(value.Object(fields)) -> {
          case list.key_find(fields, "uri") {
            Ok(val) -> Ok(val)
            Error(_) -> Ok(value.Null)
          }
        }
        _ -> Ok(value.Null)
      }
    }),
  ]

  schema.object_type("DeleteResult", "Result of a delete mutation", fields)
}

/// Build an ObjectType representing a record
fn build_record_object_type(record: RecordInfo) -> schema.Type {
  // Build standard AT Proto fields that extract data from parent context
  let standard_fields = [
    schema.field("uri", schema.string_type(), "Record URI", fn(ctx) {
      // Extract from parent object data
      case ctx.data {
        option.Some(value.Object(fields)) -> {
          case list.key_find(fields, "uri") {
            Ok(val) -> Ok(val)
            Error(_) -> Ok(value.String("at://did:plc:example/collection/rkey"))
          }
        }
        _ -> Ok(value.String("at://did:plc:example/collection/rkey"))
      }
    }),
    schema.field("cid", schema.string_type(), "Record CID", fn(ctx) {
      case ctx.data {
        option.Some(value.Object(fields)) -> {
          case list.key_find(fields, "cid") {
            Ok(val) -> Ok(val)
            Error(_) -> Ok(value.String("bafyreicid"))
          }
        }
        _ -> Ok(value.String("bafyreicid"))
      }
    }),
    schema.field("did", schema.string_type(), "DID of record author", fn(ctx) {
      case ctx.data {
        option.Some(value.Object(fields)) -> {
          case list.key_find(fields, "did") {
            Ok(val) -> Ok(val)
            Error(_) -> Ok(value.String("did:plc:example"))
          }
        }
        _ -> Ok(value.String("did:plc:example"))
      }
    }),
    schema.field(
      "indexedAt",
      schema.string_type(),
      "When record was indexed",
      fn(ctx) {
        case ctx.data {
          option.Some(value.Object(fields)) -> {
            case list.key_find(fields, "indexedAt") {
              Ok(val) -> Ok(val)
              Error(_) -> Ok(value.String("2024-01-01T00:00:00Z"))
            }
          }
          _ -> Ok(value.String("2024-01-01T00:00:00Z"))
        }
      },
    ),
    schema.field("collection", schema.string_type(), "Collection name", fn(ctx) {
      case ctx.data {
        option.Some(value.Object(fields)) -> {
          case list.key_find(fields, "collection") {
            Ok(val) -> Ok(val)
            Error(_) -> Ok(value.Null)
          }
        }
        _ -> Ok(value.Null)
      }
    }),
  ]

  // Build fields from lexicon properties
  let lexicon_fields =
    list.map(record.properties, fn(prop) {
      let #(name, types.Property(type_, _required)) = prop
      let graphql_type = type_mapper.map_type(type_)

      schema.field(name, graphql_type, "Field from lexicon", fn(_ctx) {
        Ok(value.Null)
      })
    })

  // Combine all fields
  let all_fields = list.append(standard_fields, lexicon_fields)

  schema.object_type(
    record.type_name,
    "Record type: " <> record.nsid,
    all_fields,
  )
}
