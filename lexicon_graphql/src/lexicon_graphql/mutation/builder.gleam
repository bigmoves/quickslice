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
import lexicon_graphql/internal/graphql/union_input_builder
import lexicon_graphql/internal/lexicon/nsid
import lexicon_graphql/internal/lexicon/registry as lexicon_registry
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

/// Result of building mutation type - includes the type and union field registry
///
/// The union_registry is used during schema generation to create proper GraphQL
/// input types for AT Protocol union fields. At runtime, mutation resolvers use
/// lexicon JSON directly to transform union inputs (matching the blob transformation
/// pattern), so union_registry is not needed after schema building.
pub type MutationBuildResult {
  MutationBuildResult(
    mutation_type: schema.Type,
    union_registry: union_input_builder.UnionRegistry,
  )
}

/// Build a GraphQL Mutation type from lexicon definitions
///
/// For each record type, generates:
/// - create{TypeName}(input: {TypeName}Input!, rkey: String): {TypeName}
/// - update{TypeName}(rkey: String!, input: {TypeName}Input!): {TypeName}
/// - delete{TypeName}(rkey: String!): {TypeName}
///
/// Also adds uploadBlob mutation if upload_blob_factory is provided
/// Custom fields can be passed to add additional mutations beyond collection-based ones
///
/// Resolver factories are optional - if None, mutations will return errors
/// Registry parameter enables union input type generation
pub fn build_mutation_type(
  lexicons: List(types.Lexicon),
  object_types: dict.Dict(String, schema.Type),
  create_factory: option.Option(ResolverFactory),
  update_factory: option.Option(ResolverFactory),
  delete_factory: option.Option(ResolverFactory),
  upload_blob_factory: option.Option(UploadBlobResolverFactory),
  custom_fields: option.Option(List(schema.Field)),
  registry: option.Option(lexicon_registry.Registry),
) -> MutationBuildResult {
  // Build union input types if registry is provided
  let initial_union_registry = case registry {
    option.Some(reg) -> union_input_builder.build_union_input_types(reg)
    option.None ->
      union_input_builder.UnionRegistry(
        input_types: dict.new(),
        field_variants: dict.new(),
      )
  }

  // Extract record types
  let record_types = extract_record_types(lexicons)

  // Build mutation fields for each record type, accumulating union field registrations
  let #(record_mutation_fields, final_union_registry) =
    list.fold(record_types, #([], initial_union_registry), fn(acc, record) {
      let #(fields_acc, registry_acc) = acc
      let #(new_fields, updated_registry) =
        build_mutations_for_record(
          record,
          object_types,
          create_factory,
          update_factory,
          delete_factory,
          registry_acc,
        )
      #(list.append(fields_acc, new_fields), updated_registry)
    })

  // Add uploadBlob mutation if factory is provided
  let with_upload_blob = case upload_blob_factory {
    option.Some(factory) -> {
      let upload_blob_mutation = build_upload_blob_mutation(factory)
      [upload_blob_mutation, ..record_mutation_fields]
    }
    option.None -> record_mutation_fields
  }

  // Add custom fields if provided
  let all_mutation_fields = case custom_fields {
    option.Some(fields) -> list.append(with_upload_blob, fields)
    option.None -> with_upload_blob
  }

  // Build the Mutation object type
  let mutation_type =
    schema.object_type("Mutation", "Root mutation type", all_mutation_fields)

  MutationBuildResult(
    mutation_type: mutation_type,
    union_registry: final_union_registry,
  )
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
  union_registry: union_input_builder.UnionRegistry,
) -> #(List(schema.Field), union_input_builder.UnionRegistry) {
  // Build input type and get updated registry with union field mappings
  let #(input_type, updated_registry) =
    build_input_type_with_registry(
      record.type_name <> "Input",
      record.nsid,
      record.properties,
      union_registry,
    )

  let create_mutation =
    build_create_mutation(record, object_types, create_factory, input_type)
  let update_mutation =
    build_update_mutation(record, object_types, update_factory, input_type)
  let delete_mutation = build_delete_mutation(record, delete_factory)

  #([create_mutation, update_mutation, delete_mutation], updated_registry)
}

/// Build create mutation for a record type
/// Signature: create{TypeName}(input: {TypeName}Input!, rkey: String): {TypeName}
fn build_create_mutation(
  record: RecordInfo,
  object_types: dict.Dict(String, schema.Type),
  factory: option.Option(ResolverFactory),
  input_type: schema.Type,
) -> schema.Field {
  let mutation_name = "create" <> record.type_name

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
  input_type: schema.Type,
) -> schema.Field {
  let mutation_name = "update" <> record.type_name

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

/// Build an InputObjectType and register union fields
fn build_input_type_with_registry(
  type_name: String,
  collection: String,
  properties: List(#(String, types.Property)),
  union_registry: union_input_builder.UnionRegistry,
) -> #(schema.Type, union_input_builder.UnionRegistry) {
  // Create context for union input type resolution
  let ctx =
    type_mapper.UnionInputContext(
      input_types: union_registry.input_types,
      parent_type_name: type_name,
    )

  // Build fields and register union fields
  let #(input_fields, final_registry) =
    list.fold(properties, #([], union_registry), fn(acc, prop) {
      let #(fields_acc, registry_acc) = acc
      let #(name, types.Property(type_, required, _, ref, refs, items)) = prop

      // Build property for type mapping
      let property =
        types.Property(type_, required, option.None, ref, refs, items)

      // Use union-aware type mapping with field name for multi-variant naming
      let graphql_type =
        type_mapper.map_input_type_with_unions(property, name, ctx)

      // Register union fields for later transformation
      let updated_registry = case type_, refs {
        "union", option.Some(ref_list) -> {
          union_input_builder.register_union_field(
            registry_acc,
            collection,
            name,
            ref_list,
          )
        }
        _, _ -> registry_acc
      }

      // Make required fields non-null
      let field_type = case required {
        True -> schema.non_null(graphql_type)
        False -> graphql_type
      }

      let input_field =
        schema.input_field(
          name,
          field_type,
          "Input field for " <> name,
          option.None,
        )

      #([input_field, ..fields_acc], updated_registry)
    })

  let reversed_fields = list.reverse(input_fields)
  let input_type =
    schema.input_object_type(
      type_name,
      "Input type for " <> type_name,
      reversed_fields,
    )

  #(input_type, final_registry)
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
