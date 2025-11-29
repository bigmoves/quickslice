/// Mutation Builder
///
/// Builds GraphQL mutation types from AT Protocol lexicon definitions.
/// Generates create, update, and delete mutations for each record type.
///
/// Note: The resolvers are currently stubs. Actual mutation logic should be
/// implemented in the server layer by extracting data from the GraphQL context.
import gleam/dict
import gleam/list
import gleam/option
import lexicon_graphql/internal/graphql/type_mapper
import lexicon_graphql/internal/lexicon/nsid
import lexicon_graphql/types
import swell/schema
import swell/value

/// Resolver factory function type
/// Takes collection name and returns a resolver function
pub type ResolverFactory =
  fn(String) -> schema.Resolver

/// Upload blob resolver factory type
/// Doesn't take a collection name (not collection-specific)
pub type UploadBlobResolverFactory =
  fn() -> schema.Resolver

/// Build a GraphQL Mutation type from lexicon definitions
///
/// For each record type, generates:
/// - create{TypeName}(input: {TypeName}Input!, rkey: String): {TypeName}
/// - update{TypeName}(rkey: String!, input: {TypeName}Input!): {TypeName}
/// - delete{TypeName}(rkey: String!): {TypeName}
///
/// Also adds uploadBlob mutation if upload_blob_factory is provided
///
/// Resolver factories are optional - if None, mutations will return errors
pub fn build_mutation_type(
  lexicons: List(types.Lexicon),
  object_types: dict.Dict(String, schema.Type),
  create_factory: option.Option(ResolverFactory),
  update_factory: option.Option(ResolverFactory),
  delete_factory: option.Option(ResolverFactory),
  upload_blob_factory: option.Option(UploadBlobResolverFactory),
) -> schema.Type {
  // Extract record types
  let record_types = extract_record_types(lexicons)

  // Build mutation fields for each record type using complete object types
  let record_mutation_fields =
    list.flat_map(record_types, fn(record) {
      build_mutations_for_record(
        record,
        object_types,
        create_factory,
        update_factory,
        delete_factory,
      )
    })

  // Add uploadBlob mutation if factory is provided
  let all_mutation_fields = case upload_blob_factory {
    option.Some(factory) -> {
      let upload_blob_mutation = build_upload_blob_mutation(factory)
      [upload_blob_mutation, ..record_mutation_fields]
    }
    option.None -> record_mutation_fields
  }

  // Build the Mutation object type
  schema.object_type("Mutation", "Root mutation type", all_mutation_fields)
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
fn extract_record_types(lexicons: List(types.Lexicon)) -> List(RecordInfo) {
  lexicons
  |> list.filter_map(fn(lexicon) {
    case lexicon {
      types.Lexicon(
        id,
        types.Defs(option.Some(types.RecordDef("record", _, props)), _others),
      ) -> {
        let type_name = nsid.to_type_name(id)
        Ok(RecordInfo(nsid: id, type_name: type_name, properties: props))
      }
      _ -> Error(Nil)
    }
  })
}

/// Build all three mutations (create, update, delete) for a record type
fn build_mutations_for_record(
  record: RecordInfo,
  object_types: dict.Dict(String, schema.Type),
  create_factory: option.Option(ResolverFactory),
  update_factory: option.Option(ResolverFactory),
  delete_factory: option.Option(ResolverFactory),
) -> List(schema.Field) {
  let create_mutation =
    build_create_mutation(record, object_types, create_factory)
  let update_mutation =
    build_update_mutation(record, object_types, update_factory)
  let delete_mutation = build_delete_mutation(record, delete_factory)

  [create_mutation, update_mutation, delete_mutation]
}

/// Build create mutation for a record type
/// Signature: create{TypeName}(input: {TypeName}Input!, rkey: String): {TypeName}
fn build_create_mutation(
  record: RecordInfo,
  object_types: dict.Dict(String, schema.Type),
  factory: option.Option(ResolverFactory),
) -> schema.Field {
  let mutation_name = "create" <> record.type_name
  let input_type_name = record.type_name <> "Input"

  // Build the input type
  let input_type = build_input_type(input_type_name, record.properties)

  // Get the complete object type from the dict (includes all join fields)
  let assert Ok(return_type) = dict.get(object_types, record.nsid)

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
    option.None -> fn(_resolver_ctx) {
      Error("Create mutation for " <> collection <> " not yet implemented.")
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
  object_types: dict.Dict(String, schema.Type),
  factory: option.Option(ResolverFactory),
) -> schema.Field {
  let mutation_name = "update" <> record.type_name
  let input_type_name = record.type_name <> "Input"

  // Build the input type
  let input_type = build_input_type(input_type_name, record.properties)

  // Get the complete object type from the dict (includes all join fields)
  let assert Ok(return_type) = dict.get(object_types, record.nsid)

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
    option.None -> fn(_resolver_ctx) {
      Error("Update mutation for " <> collection <> " not yet implemented.")
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
    option.None -> fn(_resolver_ctx) {
      Error("Delete mutation for " <> collection <> " not yet implemented.")
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
      let #(name, types.Property(type_, required, _, _, _)) = prop
      // Use map_input_type to get input-compatible types (e.g., BlobInput instead of Blob)
      let graphql_type = type_mapper.map_input_type(type_)

      // Make required fields non-null
      let field_type = case required {
        True -> schema.non_null(graphql_type)
        False -> graphql_type
      }

      schema.input_field(
        name,
        field_type,
        "Input field for " <> name,
        option.None,
      )
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

/// Build BlobUploadResponse type
/// Returns: { ref: String!, mimeType: String!, size: Int! }
fn build_blob_upload_response_type() -> schema.Type {
  let fields = [
    // ref field - CID reference
    schema.field(
      "ref",
      schema.non_null(schema.string_type()),
      "CID reference to the blob",
      fn(ctx) {
        case ctx.data {
          option.Some(value.Object(fields)) -> {
            case list.key_find(fields, "ref") {
              Ok(value.String(ref)) -> Ok(value.String(ref))
              _ -> Error("Missing ref field")
            }
          }
          _ -> Error("Missing blob data")
        }
      },
    ),
    // mimeType field
    schema.field(
      "mimeType",
      schema.non_null(schema.string_type()),
      "MIME type of the blob",
      fn(ctx) {
        case ctx.data {
          option.Some(value.Object(fields)) -> {
            case list.key_find(fields, "mime_type") {
              Ok(value.String(mime_type)) -> Ok(value.String(mime_type))
              _ -> Error("Missing mime_type field")
            }
          }
          _ -> Error("Missing blob data")
        }
      },
    ),
    // size field
    schema.field(
      "size",
      schema.non_null(schema.int_type()),
      "Size in bytes",
      fn(ctx) {
        case ctx.data {
          option.Some(value.Object(fields)) -> {
            case list.key_find(fields, "size") {
              Ok(value.Int(size)) -> Ok(value.Int(size))
              _ -> Error("Missing size field")
            }
          }
          _ -> Error("Missing blob data")
        }
      },
    ),
  ]

  schema.object_type(
    "BlobUploadResponse",
    "Response from uploading a blob",
    fields,
  )
}

/// Build uploadBlob mutation
/// Signature: uploadBlob(data: String!, mimeType: String!): BlobUploadResponse!
fn build_upload_blob_mutation(
  factory: UploadBlobResolverFactory,
) -> schema.Field {
  // Create the BlobUploadResponse type
  let response_type = build_blob_upload_response_type()

  // Create arguments
  let arguments = [
    schema.argument(
      "data",
      schema.non_null(schema.string_type()),
      "Base64 encoded blob data",
      option.None,
    ),
    schema.argument(
      "mimeType",
      schema.non_null(schema.string_type()),
      "MIME type of the blob",
      option.None,
    ),
  ]

  // Get resolver from factory
  let resolver = factory()

  // Create the mutation field with arguments
  schema.field_with_args(
    "uploadBlob",
    schema.non_null(response_type),
    "Upload a blob to the PDS",
    arguments,
    resolver,
  )
}
