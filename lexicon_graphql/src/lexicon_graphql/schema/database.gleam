/// Database Schema Builder
///
/// Builds GraphQL schemas from AT Protocol lexicon definitions with database-backed resolvers.
/// This extends the base schema_builder with actual data resolution.
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import lexicon_graphql/input/aggregate as aggregate_input
import lexicon_graphql/output/aggregate as aggregate_types
import lexicon_graphql/internal/lexicon/collection_meta
import lexicon_graphql/input/connection as lexicon_connection
import lexicon_graphql/query/dataloader
import lexicon_graphql/internal/lexicon/registry as lexicon_registry
import lexicon_graphql/mutation/builder as mutation_builder
import lexicon_graphql/internal/lexicon/nsid
import lexicon_graphql/internal/graphql/object_builder as object_type_builder
import lexicon_graphql/internal/graphql/type_mapper
import lexicon_graphql/types
import lexicon_graphql/internal/lexicon/uri_extractor
import lexicon_graphql/input/where as where_input
import swell/connection
import swell/schema
import swell/value

/// Represents the type of a field based on lexicon schema
/// Used for validation (e.g., ensuring date intervals are only applied to datetime fields)
pub type FieldType {
  StringField
  IntField
  BoolField
  NumberField
  DateTimeField
  BlobField
  RefField
  ArrayField
}

/// Convert a lexicon property to a FieldType
/// Checks format field for datetime, otherwise uses type_ field
pub fn property_to_field_type(property: types.Property) -> FieldType {
  // Check format first for datetime
  case property.format {
    option.Some("datetime") -> DateTimeField
    _ ->
      case property.type_ {
        "string" -> StringField
        "integer" -> IntField
        "boolean" -> BoolField
        "number" -> NumberField
        "array" -> ArrayField
        "blob" -> BlobField
        "ref" -> RefField
        _ -> StringField
        // Default to string for unknown types
      }
  }
}

/// Represents a reverse join relationship discovered from lexicon analysis
type ReverseJoinRelationship {
  ReverseJoinRelationship(
    /// Collection that has the reference field (e.g., "app.bsky.feed.like")
    source_collection: String,
    /// Type name of the source collection (e.g., "AppBskyFeedLike")
    source_type_name: String,
    /// Field name in source that references target (e.g., "subject")
    source_field: String,
  )
}

/// Record type metadata with database resolver info
type RecordType {
  RecordType(
    nsid: String,
    type_name: String,
    field_name: String,
    fields: List(schema.Field),
    /// Original lexicon properties for this record (used for filtering sortable fields)
    properties: List(#(String, types.Property)),
    /// Metadata extracted from lexicon for join field generation
    meta: collection_meta.CollectionMeta,
    /// Reverse join relationships where this type is the target
    reverse_joins: List(ReverseJoinRelationship),
  )
}

/// Type for a database record fetcher function with pagination support
/// Takes a collection NSID and pagination params, returns Connection data
/// Returns: (records_with_cursors, end_cursor, has_next_page, has_previous_page, total_count)
pub type RecordFetcher =
  fn(String, dataloader.PaginationParams) ->
    Result(
      #(
        List(#(value.Value, String)),
        option.Option(String),
        Bool,
        Bool,
        option.Option(Int),
      ),
      String,
    )

/// Type for aggregate query parameters
pub type AggregateParams {
  AggregateParams(
    group_by: List(aggregate_input.GroupByFieldInput),
    where: option.Option(where_input.WhereClause),
    order_by_desc: Bool,
    limit: Int,
  )
}

/// Type for a database aggregate fetcher function
/// Takes a collection NSID and aggregate params, returns aggregate results
pub type AggregateFetcher =
  fn(String, AggregateParams) ->
    Result(List(aggregate_types.AggregateResult), String)

/// Build a GraphQL schema from lexicons with database-backed resolvers
///
/// The fetcher parameter should be a function that queries the database for records with pagination
/// The batch_fetcher parameter is used for join operations (forward and reverse joins)
/// The mutation resolver factories are optional - if None, mutations will return stub errors
pub fn build_schema_with_fetcher(
  lexicons: List(types.Lexicon),
  fetcher: RecordFetcher,
  batch_fetcher: option.Option(dataloader.BatchFetcher),
  paginated_batch_fetcher: option.Option(dataloader.PaginatedBatchFetcher),
  create_factory: option.Option(mutation_builder.ResolverFactory),
  update_factory: option.Option(mutation_builder.ResolverFactory),
  delete_factory: option.Option(mutation_builder.ResolverFactory),
  upload_blob_factory: option.Option(mutation_builder.UploadBlobResolverFactory),
) -> Result(schema.Schema, String) {
  case lexicons {
    [] -> Error("Cannot build schema from empty lexicon list")
    _ -> {
      // Extract record types and object types from lexicons with batch fetcher for joins
      let #(record_types, object_types, _field_type_registry) =
        extract_record_types_and_object_types(
          lexicons,
          batch_fetcher,
          paginated_batch_fetcher,
        )

      // Build the query type with fields for each record using shared object types
      let query_type =
        build_query_type(
          record_types,
          object_types,
          fetcher,
          option.None,
          dict.new(),
        )

      // Build the mutation type with provided resolver factories
      // Pass the complete object types so mutations use the same types as queries
      let mutation_type =
        mutation_builder.build_mutation_type(
          lexicons,
          object_types,
          create_factory,
          update_factory,
          delete_factory,
          upload_blob_factory,
        )

      // Create the schema with both queries and mutations
      Ok(schema.schema(query_type, option.Some(mutation_type)))
    }
  }
}

/// Build a GraphQL schema with subscriptions from lexicons
///
/// This extends build_schema_with_fetcher to also generate subscription fields
/// for each record type: {collection}Created, {collection}Updated, {collection}Deleted
pub fn build_schema_with_subscriptions(
  lexicons: List(types.Lexicon),
  fetcher: RecordFetcher,
  batch_fetcher: option.Option(dataloader.BatchFetcher),
  paginated_batch_fetcher: option.Option(dataloader.PaginatedBatchFetcher),
  create_factory: option.Option(mutation_builder.ResolverFactory),
  update_factory: option.Option(mutation_builder.ResolverFactory),
  delete_factory: option.Option(mutation_builder.ResolverFactory),
  upload_blob_factory: option.Option(mutation_builder.UploadBlobResolverFactory),
  aggregate_fetcher: option.Option(AggregateFetcher),
) -> Result(schema.Schema, String) {
  case lexicons {
    [] -> Error("Cannot build schema from empty lexicon list")
    _ -> {
      // Extract record types and object types from lexicons
      let #(record_types, object_types, field_type_registry) =
        extract_record_types_and_object_types(
          lexicons,
          batch_fetcher,
          paginated_batch_fetcher,
        )

      // Build the query type (pass field_type_registry for aggregation validation)
      let query_type =
        build_query_type(
          record_types,
          object_types,
          fetcher,
          aggregate_fetcher,
          field_type_registry,
        )

      // Build the mutation type
      let mutation_type =
        mutation_builder.build_mutation_type(
          lexicons,
          object_types,
          create_factory,
          update_factory,
          delete_factory,
          upload_blob_factory,
        )

      // Build the subscription type
      let subscription_type =
        build_subscription_type(record_types, object_types)

      // Create the schema with queries, mutations, and subscriptions
      Ok(schema.schema_with_subscriptions(
        query_type,
        option.Some(mutation_type),
        option.Some(subscription_type),
      ))
    }
  }
}

/// Extract record types and object types from lexicon definitions
///
/// NEW 3-PASS ARCHITECTURE:
/// Pass 0: Build object types from lexicon defs (e.g., aspectRatio)
/// Pass 1: Extract metadata and build basic types (base fields only)
/// Pass 2: Build complete RecordTypes with ALL join fields
/// Pass 3: Rebuild ONLY Connection fields using final types
///
/// Returns: #(record_types, object_types, field_type_registry)
fn extract_record_types_and_object_types(
  lexicons: List(types.Lexicon),
  batch_fetcher: option.Option(dataloader.BatchFetcher),
  paginated_batch_fetcher: option.Option(dataloader.PaginatedBatchFetcher),
) -> #(
  List(RecordType),
  dict.Dict(String, schema.Type),
  dict.Dict(String, dict.Dict(String, FieldType)),
) {
  // =============================================================================
  // PASS 0: Build object types from lexicon defs
  // =============================================================================

  // Create a registry from all lexicons
  let registry = lexicon_registry.from_lexicons(lexicons)

  // Build all object types from defs (e.g., social.grain.defs#aspectRatio)
  let ref_object_types = object_type_builder.build_all_object_types(registry)

  // =============================================================================
  // PASS 1: Extract metadata and build basic types
  // =============================================================================

  // Extract metadata from all lexicons
  let metadata_list =
    list.filter_map(lexicons, fn(lex) {
      case lex {
        types.Lexicon(
          id,
          types.Defs(option.Some(types.RecordDef("record", _, _)), _),
        ) -> {
          let meta = collection_meta.extract_metadata(lex)
          Ok(#(id, meta))
        }
        _ -> Error(Nil)
      }
    })

  // Build reverse join map: target_nsid -> List(ReverseJoinRelationship)
  let reverse_join_map = build_reverse_join_map(metadata_list)

  // Build DID join map: source_nsid -> List(#(target_nsid, target_meta))
  let did_join_map = build_did_join_map(metadata_list)

  // Parse lexicons to create basic RecordTypes (base fields only, no joins yet)
  let basic_record_types_without_forward_joins =
    lexicons
    |> list.filter_map(fn(lex) {
      parse_lexicon_with_reverse_joins(lex, [], batch_fetcher, ref_object_types)
    })

  // Build basic object types WITHOUT forward joins (needed as generic type for forward joins)
  let basic_object_types_without_forward_joins =
    list.fold(
      basic_record_types_without_forward_joins,
      dict.new(),
      fn(acc, record_type) {
        let object_type =
          schema.object_type(
            record_type.type_name,
            "Record type: " <> record_type.nsid,
            record_type.fields,
          )
        dict.insert(acc, record_type.nsid, object_type)
      },
    )

  // Build Record union for forward joins to reference
  let basic_possible_types =
    dict.values(basic_object_types_without_forward_joins)
  let basic_record_union = build_record_union(basic_possible_types)
  let basic_object_types_with_generic =
    dict.insert(
      basic_object_types_without_forward_joins,
      "_generic_record",
      basic_record_union,
    )

  // Now add forward join fields to basic types
  // This ensures Connection types built in Pass 2 reference types with forward joins
  let basic_record_types =
    list.map(basic_record_types_without_forward_joins, fn(record_type) {
      let forward_join_fields =
        build_forward_join_fields_with_types(
          record_type.meta,
          batch_fetcher,
          basic_object_types_with_generic,
        )

      let all_fields = list.flatten([record_type.fields, forward_join_fields])

      RecordType(..record_type, fields: all_fields)
    })

  // Build basic object types WITH forward joins
  // These are the types that Pass 2 Connections will reference
  let basic_object_types_without_generic =
    list.fold(basic_record_types, dict.new(), fn(acc, record_type) {
      let object_type =
        schema.object_type(
          record_type.type_name,
          "Record type: " <> record_type.nsid,
          record_type.fields,
        )
      dict.insert(acc, record_type.nsid, object_type)
    })

  // Rebuild Record union with complete basic types
  let basic_possible_types_complete =
    dict.values(basic_object_types_without_generic)
  let basic_record_union_complete =
    build_record_union(basic_possible_types_complete)
  let basic_object_types =
    dict.insert(
      basic_object_types_without_generic,
      "_generic_record",
      basic_record_union_complete,
    )

  // Build sort field enums dict BEFORE Pass 2 - create each enum once and reuse
  let sort_field_enums: dict.Dict(String, schema.Type) =
    list.fold(basic_record_types, dict.new(), fn(acc, rt: RecordType) {
      let enum = build_sort_field_enum(rt)
      dict.insert(acc, rt.type_name, enum)
    })

  // Build field type registry for validation (collection_nsid -> field_name -> FieldType)
  let field_type_registry: dict.Dict(String, dict.Dict(String, FieldType)) =
    list.fold(basic_record_types, dict.new(), fn(acc, rt: RecordType) {
      let field_types = build_field_type_map(rt.properties)
      dict.insert(acc, rt.nsid, field_types)
    })

  // =============================================================================
  // PASS 2: Build complete RecordTypes with ALL join fields
  // =============================================================================

  let complete_record_types =
    list.map(basic_record_types, fn(record_type) {
      let reverse_joins =
        dict.get(reverse_join_map, record_type.nsid) |> result.unwrap([])
      let did_join_targets =
        dict.get(did_join_map, record_type.nsid) |> result.unwrap([])

      // Build ALL join fields using basic object types
      // These Connection types will reference basic types initially
      let forward_join_fields =
        build_forward_join_fields_with_types(
          record_type.meta,
          batch_fetcher,
          basic_object_types,
        )

      let reverse_join_fields =
        build_reverse_join_fields_with_types(
          reverse_joins,
          paginated_batch_fetcher,
          basic_object_types,
          basic_record_types,
          sort_field_enums,
        )

      let did_join_fields =
        build_did_join_fields_with_types(
          did_join_targets,
          batch_fetcher,
          paginated_batch_fetcher,
          basic_object_types,
          basic_record_types,
          sort_field_enums,
        )

      // Combine all fields (base + forward + reverse + DID joins)
      let all_fields =
        list.flatten([
          record_type.fields,
          forward_join_fields,
          reverse_join_fields,
          did_join_fields,
        ])

      RecordType(
        ..record_type,
        fields: all_fields,
        reverse_joins: reverse_joins,
      )
    })

  // Build complete object types from complete RecordTypes
  let complete_object_types_without_generic: dict.Dict(String, schema.Type) =
    list.fold(complete_record_types, dict.new(), fn(acc, rt: RecordType) {
      let object_type =
        schema.object_type(rt.type_name, "Record type: " <> rt.nsid, rt.fields)
      dict.insert(acc, rt.nsid, object_type)
    })

  // Build Record union with complete object types
  let complete_possible_types =
    dict.values(complete_object_types_without_generic)
  let complete_record_union = build_record_union(complete_possible_types)
  let complete_object_types =
    dict.insert(
      complete_object_types_without_generic,
      "_generic_record",
      complete_record_union,
    )

  // =============================================================================
  // PASS 3: Rebuild join fields using COMPLETE object types
  // =============================================================================
  // This fixes the issue where DID/reverse join fields reference incomplete types.
  // Even though basic_object_types have forward joins now, they don't have reverse/did joins.
  // So we need to rebuild all Connections to reference complete_object_types.

  let final_record_types =
    list.map(basic_record_types, fn(record_type) {
      // Rebuild ALL join fields with complete types
      let reverse_joins =
        dict.get(reverse_join_map, record_type.nsid) |> result.unwrap([])
      let did_join_targets =
        dict.get(did_join_map, record_type.nsid) |> result.unwrap([])

      let forward_join_fields =
        build_forward_join_fields_with_types(
          record_type.meta,
          batch_fetcher,
          complete_object_types,
        )

      let reverse_join_fields =
        build_reverse_join_fields_with_types(
          reverse_joins,
          paginated_batch_fetcher,
          complete_object_types,
          complete_record_types,
          sort_field_enums,
        )

      let did_join_fields =
        build_did_join_fields_with_types(
          did_join_targets,
          batch_fetcher,
          paginated_batch_fetcher,
          complete_object_types,
          complete_record_types,
          sort_field_enums,
        )

      // Combine all fields
      let all_fields =
        list.flatten([
          record_type.fields,
          forward_join_fields,
          reverse_join_fields,
          did_join_fields,
        ])

      RecordType(
        ..record_type,
        fields: all_fields,
        reverse_joins: reverse_joins,
      )
    })

  // Rebuild final object types with all fields
  let final_object_types_without_generic =
    list.fold(final_record_types, dict.new(), fn(acc, record_type) {
      let object_type =
        schema.object_type(
          record_type.type_name,
          "Record type: " <> record_type.nsid,
          record_type.fields,
        )

      dict.insert(acc, record_type.nsid, object_type)
    })

  // Rebuild Record union with final object types
  let final_possible_types = dict.values(final_object_types_without_generic)
  let final_record_union = build_record_union(final_possible_types)
  let final_object_types =
    dict.insert(
      final_object_types_without_generic,
      "_generic_record",
      final_record_union,
    )

  // Merge ref_object_types (from lexicon defs) into final_object_types
  // This makes object types like "social.grain.defs#aspectRatio" available for ref resolution
  let final_object_types_with_refs =
    dict.fold(ref_object_types, final_object_types, fn(acc, ref, obj_type) {
      dict.insert(acc, ref, obj_type)
    })

  #(final_record_types, final_object_types_with_refs, field_type_registry)
}

/// Build a map of reverse join relationships from metadata
/// Returns: Dict(target_nsid, List(ReverseJoinRelationship))
fn build_reverse_join_map(
  metadata_list: List(#(String, collection_meta.CollectionMeta)),
) -> Dict(String, List(ReverseJoinRelationship)) {
  // For each collection with forward join fields, create reverse join entries
  let result =
    list.fold(metadata_list, dict.new(), fn(acc, meta_pair) {
      let #(source_nsid, source_meta) = meta_pair

      // For each forward join field in the source collection
      list.fold(source_meta.forward_join_fields, acc, fn(map_acc, join_field) {
        let field_name = case join_field {
          collection_meta.StrongRefField(name) -> name
          collection_meta.AtUriField(name) -> name
        }

        let relationship =
          ReverseJoinRelationship(
            source_collection: source_nsid,
            source_type_name: source_meta.type_name,
            source_field: field_name,
          )

        // Since at-uri and strongRef can reference ANY collection,
        // we add this relationship to ALL other collections as potential targets
        // This is a conservative approach - in production you might want to be more selective
        list.fold(metadata_list, map_acc, fn(target_acc, target_pair) {
          let #(target_nsid, _target_meta) = target_pair

          // Don't create self-referencing reverse joins (for now)
          case target_nsid == source_nsid {
            True -> target_acc
            False -> {
              let existing =
                dict.get(target_acc, target_nsid) |> result.unwrap([])
              dict.insert(target_acc, target_nsid, [relationship, ..existing])
            }
          }
        })
      })
    })

  result
}

/// Build a map of DID-based join relationships from metadata
/// Every collection can join to every other collection by DID
/// Returns: Dict(source_nsid, List(#(target_nsid, target_meta)))
fn build_did_join_map(
  metadata_list: List(#(String, collection_meta.CollectionMeta)),
) -> Dict(String, List(#(String, collection_meta.CollectionMeta))) {
  // For each collection, create DID join entries to all other collections
  list.fold(metadata_list, dict.new(), fn(acc, source_pair) {
    let #(source_nsid, _source_meta) = source_pair

    // Build list of target collections (all collections except self)
    let targets =
      list.filter_map(metadata_list, fn(target_pair) {
        let #(target_nsid, target_meta) = target_pair
        case target_nsid == source_nsid {
          True -> Error(Nil)
          // Don't join to self
          False -> Ok(#(target_nsid, target_meta))
        }
      })

    dict.insert(acc, source_nsid, targets)
  })
}

/// Parse a single lexicon into a RecordType with all fields including reverse joins
fn parse_lexicon_with_reverse_joins(
  lexicon: types.Lexicon,
  reverse_joins: List(ReverseJoinRelationship),
  batch_fetcher: option.Option(dataloader.BatchFetcher),
  ref_object_types: Dict(String, schema.Type),
) -> Result(RecordType, Nil) {
  case lexicon {
    types.Lexicon(
      id,
      types.Defs(option.Some(types.RecordDef("record", _, properties)), _),
    ) -> {
      let type_name = nsid.to_type_name(id)
      let field_name = nsid.to_field_name(id)

      // Extract metadata for join field generation
      let meta = collection_meta.extract_metadata(lexicon)

      // Build regular and forward join fields
      let base_fields =
        build_fields_with_meta(
          properties,
          meta,
          batch_fetcher,
          ref_object_types,
        )

      // Note: Reverse join fields are NOT built here - they will be added later
      // once object types exist (see build_reverse_join_fields_with_types)

      // Use only base fields for initial schema building
      let all_fields = base_fields

      Ok(RecordType(
        nsid: id,
        type_name: type_name,
        field_name: field_name,
        fields: all_fields,
        properties: properties,
        meta: meta,
        reverse_joins: reverse_joins,
      ))
    }
    _ -> Error(Nil)
  }
}

/// Build GraphQL fields from lexicon properties (WITHOUT forward or reverse joins)
/// Join fields are added later once object types exist
fn build_fields_with_meta(
  properties: List(#(String, types.Property)),
  _meta: collection_meta.CollectionMeta,
  _batch_fetcher: option.Option(dataloader.BatchFetcher),
  ref_object_types: Dict(String, schema.Type),
) -> List(schema.Field) {
  let regular_fields = build_fields(properties, ref_object_types)
  // Note: Forward join fields are NOT built here - they need object types first

  regular_fields
}

/// Build forward join fields from collection metadata with proper types
/// These fields resolve to the referenced records using the DataLoader
fn build_forward_join_fields_with_types(
  meta: collection_meta.CollectionMeta,
  batch_fetcher: option.Option(dataloader.BatchFetcher),
  object_types: dict.Dict(String, schema.Type),
) -> List(schema.Field) {
  // Get the generic record type
  let generic_type = case dict.get(object_types, "_generic_record") {
    Ok(t) -> t
    Error(_) -> schema.string_type()
    // Fallback
  }

  list.map(meta.forward_join_fields, fn(join_field) {
    let field_name = case join_field {
      collection_meta.StrongRefField(name) -> name
      collection_meta.AtUriField(name) -> name
    }

    schema.field(
      field_name <> "Resolved",
      generic_type,
      "Forward join to referenced record",
      fn(ctx) {
        // Extract the field value from the parent record
        case get_nested_field_from_context_dynamic(ctx, "value", field_name) {
          Ok(field_value) -> {
            // Extract URI using uri_extractor
            case uri_extractor.extract_uri(field_value) {
              option.Some(uri) -> {
                // Use batch fetcher if available to resolve the record
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
                  option.None -> {
                    // No batch fetcher - return URI as string
                    Ok(value.String(uri))
                  }
                }
              }
              option.None -> Ok(value.Null)
            }
          }
          Error(_) -> Ok(value.Null)
        }
      },
    )
  })
}

/// Build reverse join fields with proper object type references
/// These fields return lists of records that reference the current record
fn build_reverse_join_fields_with_types(
  reverse_joins: List(ReverseJoinRelationship),
  paginated_batch_fetcher: option.Option(dataloader.PaginatedBatchFetcher),
  object_types: dict.Dict(String, schema.Type),
  record_types: List(RecordType),
  sort_field_enums: dict.Dict(String, schema.Type),
) -> List(schema.Field) {
  list.map(reverse_joins, fn(relationship) {
    // Generate field name: <sourceTypeName>Via<FieldName>
    // Example: appBskyFeedLikeViaSubject (camelCase)
    let field_name =
      lowercase_first(relationship.source_type_name)
      <> "Via"
      <> capitalize_first(relationship.source_field)

    // Get the source object type from the dict
    let source_object_type = case
      dict.get(object_types, relationship.source_collection)
    {
      Ok(obj_type) -> obj_type
      Error(_) -> schema.string_type()
      // Fallback to string type if not found
    }

    // Create Connection types for this join
    let edge_type =
      connection.edge_type(relationship.source_type_name, source_object_type)
    let connection_type =
      connection.connection_type(relationship.source_type_name, edge_type)

    // Find the RecordType for the source collection to build sortBy and where args
    let source_record_type =
      list.find(record_types, fn(rt) {
        rt.nsid == relationship.source_collection
      })

    // Build connection args with sortBy and where support
    let connection_args = case source_record_type {
      Ok(record_type) -> {
        // Look up pre-built sort field enum from dict
        let sort_field_enum = case
          dict.get(sort_field_enums, record_type.type_name)
        {
          Ok(enum) -> enum
          Error(_) -> build_sort_field_enum(record_type)
          // Fallback: build if not found (shouldn't happen)
        }
        let where_input_type = build_where_input_type(record_type)

        lexicon_connection.lexicon_connection_args_with_field_enum_and_where(
          record_type.type_name,
          sort_field_enum,
          where_input_type,
        )
      }
      Error(_) -> {
        // Fallback to basic pagination args if record type not found
        list.flatten([
          connection.forward_pagination_args(),
          connection.backward_pagination_args(),
        ])
      }
    }

    schema.field_with_args(
      field_name,
      connection_type,
      "Reverse join: records in "
        <> relationship.source_collection
        <> " that reference this record via "
        <> relationship.source_field,
      connection_args,
      fn(ctx) {
        // Extract the current record's URI
        case get_field_from_context(ctx, "uri") {
          Ok(parent_uri) -> {
            case paginated_batch_fetcher {
              option.Some(fetcher) -> {
                // Extract pagination params from context (includes sortBy and where)
                let pagination_params = extract_pagination_params(ctx)

                // Use paginated DataLoader to fetch records that reference this URI
                case
                  dataloader.batch_fetch_by_reverse_join_paginated(
                    parent_uri,
                    relationship.source_collection,
                    relationship.source_field,
                    pagination_params,
                    fetcher,
                  )
                {
                  Ok(batch_result) -> {
                    // Build edges from records with their cursors
                    let edges =
                      list.map(batch_result.edges, fn(edge_tuple) {
                        let #(record_value, record_cursor) = edge_tuple
                        connection.Edge(
                          node: record_value,
                          cursor: record_cursor,
                        )
                      })

                    // Build PageInfo
                    let page_info =
                      connection.PageInfo(
                        has_next_page: batch_result.has_next_page,
                        has_previous_page: batch_result.has_previous_page,
                        start_cursor: case list.first(edges) {
                          Ok(edge) -> option.Some(edge.cursor)
                          Error(_) -> option.None
                        },
                        end_cursor: case list.last(edges) {
                          Ok(edge) -> option.Some(edge.cursor)
                          Error(_) -> option.None
                        },
                      )

                    // Build Connection
                    let conn =
                      connection.Connection(
                        edges: edges,
                        page_info: page_info,
                        total_count: batch_result.total_count,
                      )

                    Ok(connection.connection_to_value(conn))
                  }
                  Error(_) -> {
                    // Return empty connection on error
                    Ok(empty_connection_value())
                  }
                }
              }
              option.None -> {
                // No paginated batch fetcher - return empty connection
                Ok(empty_connection_value())
              }
            }
          }
          Error(_) -> {
            // Can't get parent URI - return empty connection
            Ok(empty_connection_value())
          }
        }
      },
    )
  })
}

/// Build DID join fields from metadata with proper types
/// These fields allow joining from any record to related records that share the same DID
fn build_did_join_fields_with_types(
  did_join_targets: List(#(String, collection_meta.CollectionMeta)),
  batch_fetcher: option.Option(dataloader.BatchFetcher),
  paginated_batch_fetcher: option.Option(dataloader.PaginatedBatchFetcher),
  object_types: dict.Dict(String, schema.Type),
  record_types: List(RecordType),
  sort_field_enums: dict.Dict(String, schema.Type),
) -> List(schema.Field) {
  list.map(did_join_targets, fn(target_pair) {
    let #(target_nsid, target_meta) = target_pair

    // Generate field name: <targetTypeName>ByDid
    // Example: appBskyActorProfileByDid (camelCase with first letter lowercase)
    let field_name = lowercase_first(target_meta.type_name) <> "ByDid"

    // Get the target object type from the dict
    let target_object_type = case dict.get(object_types, target_nsid) {
      Ok(obj_type) -> obj_type
      Error(_) -> schema.string_type()
      // Fallback
    }

    // Determine field type and resolver based on cardinality
    // If has_unique_did is true, return single nullable object (no pagination)
    // Otherwise, return connection (with pagination)
    case target_meta.has_unique_did {
      True -> {
        // Unique DID join - returns single nullable object (e.g., profile)
        schema.field(
          field_name,
          target_object_type,
          "DID join: record in "
            <> target_nsid
            <> " that shares the same DID as this record",
          fn(ctx) {
            // Extract the DID from the current record's URI
            case get_field_from_context(ctx, "uri") {
              Ok(uri_value) -> {
                case extract_did_from_uri(uri_value) {
                  option.Some(did) -> {
                    case batch_fetcher {
                      option.Some(fetcher) -> {
                        // Use DataLoader to fetch records by DID
                        case
                          dataloader.batch_fetch_by_did(
                            [did],
                            target_nsid,
                            fetcher,
                          )
                        {
                          Ok(results) -> {
                            case dict.get(results, did) {
                              Ok(records) -> {
                                // Return single nullable object
                                case records {
                                  [first, ..] -> Ok(first)
                                  [] -> Ok(value.Null)
                                }
                              }
                              Error(_) -> Ok(value.Null)
                            }
                          }
                          Error(_) -> Ok(value.Null)
                        }
                      }
                      option.None -> Ok(value.Null)
                    }
                  }
                  option.None -> Ok(value.Null)
                }
              }
              Error(_) -> Ok(value.Null)
            }
          },
        )
      }
      False -> {
        // Non-unique DID join - returns connection (e.g., posts)
        // Create Connection types for this join
        let edge_type =
          connection.edge_type(target_meta.type_name, target_object_type)
        let connection_type =
          connection.connection_type(target_meta.type_name, edge_type)

        // Find the RecordType for the target collection to build sortBy and where args
        let target_record_type =
          list.find(record_types, fn(rt) { rt.nsid == target_nsid })

        // Build connection args with sortBy and where support
        let connection_args = case target_record_type {
          Ok(record_type) -> {
            // Look up pre-built sort field enum from dict
            let sort_field_enum = case
              dict.get(sort_field_enums, record_type.type_name)
            {
              Ok(enum) -> enum
              Error(_) -> build_sort_field_enum(record_type)
              // Fallback: build if not found (shouldn't happen)
            }
            let where_input_type = build_where_input_type(record_type)

            lexicon_connection.lexicon_connection_args_with_field_enum_and_where(
              record_type.type_name,
              sort_field_enum,
              where_input_type,
            )
          }
          Error(_) -> {
            // Fallback to basic pagination args if record type not found
            list.flatten([
              connection.forward_pagination_args(),
              connection.backward_pagination_args(),
            ])
          }
        }

        schema.field_with_args(
          field_name,
          connection_type,
          "DID join: records in "
            <> target_nsid
            <> " that share the same DID as this record",
          connection_args,
          fn(ctx) {
            // Extract the DID from the current record's URI
            case get_field_from_context(ctx, "uri") {
              Ok(uri_value) -> {
                case extract_did_from_uri(uri_value) {
                  option.Some(did) -> {
                    case paginated_batch_fetcher {
                      option.Some(fetcher) -> {
                        // Extract pagination params from context (includes sortBy and where)
                        let pagination_params = extract_pagination_params(ctx)

                        // Use paginated DataLoader to fetch records by DID
                        case
                          dataloader.batch_fetch_by_did_paginated(
                            did,
                            target_nsid,
                            pagination_params,
                            fetcher,
                          )
                        {
                          Ok(batch_result) -> {
                            // Build edges from records with their cursors
                            let edges =
                              list.map(batch_result.edges, fn(edge_tuple) {
                                let #(record_value, record_cursor) = edge_tuple
                                connection.Edge(
                                  node: record_value,
                                  cursor: record_cursor,
                                )
                              })

                            // Build PageInfo
                            let page_info =
                              connection.PageInfo(
                                has_next_page: batch_result.has_next_page,
                                has_previous_page: batch_result.has_previous_page,
                                start_cursor: case list.first(edges) {
                                  Ok(edge) -> option.Some(edge.cursor)
                                  Error(_) -> option.None
                                },
                                end_cursor: case list.last(edges) {
                                  Ok(edge) -> option.Some(edge.cursor)
                                  Error(_) -> option.None
                                },
                              )

                            // Build Connection
                            let conn =
                              connection.Connection(
                                edges: edges,
                                page_info: page_info,
                                total_count: batch_result.total_count,
                              )

                            Ok(connection.connection_to_value(conn))
                          }
                          Error(_) -> {
                            // Return empty connection on error
                            Ok(empty_connection_value())
                          }
                        }
                      }
                      option.None -> {
                        // No paginated batch fetcher - return empty connection
                        Ok(empty_connection_value())
                      }
                    }
                  }
                  option.None -> {
                    // Can't extract DID - return empty connection
                    Ok(empty_connection_value())
                  }
                }
              }
              Error(_) -> {
                // Can't get URI - return empty connection
                Ok(empty_connection_value())
              }
            }
          },
        )
      }
    }
  })
}

/// Extract DID from an AT Protocol URI
/// Format: at://did:plc:abc123/collection.nsid/rkey
/// Returns: Some("did:plc:abc123")
fn extract_did_from_uri(uri: String) -> option.Option(String) {
  case string.starts_with(uri, "at://") {
    True -> {
      let without_prefix = string.drop_start(uri, 5)
      // Remove "at://"
      case string.split(without_prefix, "/") {
        [did, ..] -> option.Some(did)
        _ -> option.None
      }
    }
    False -> option.None
  }
}

/// Capitalize the first letter of a string
fn capitalize_first(s: String) -> String {
  case string.pop_grapheme(s) {
    Ok(#(first, rest)) -> string.uppercase(first) <> rest
    Error(_) -> ""
  }
}

/// Lowercase the first letter of a string (for camelCase field names)
fn lowercase_first(s: String) -> String {
  case string.pop_grapheme(s) {
    Ok(#(first, rest)) -> string.lowercase(first) <> rest
    Error(_) -> ""
  }
}

/// Create an empty connection value with no edges
/// Helper function to reduce code duplication across error handling paths
/// Returns a GraphQL Value representing an empty connection
fn empty_connection_value() -> value.Value {
  // Create empty connection structure directly as a Value
  // This avoids the need for type parameters while maintaining the connection structure
  value.Object([
    #("edges", value.List([])),
    #(
      "pageInfo",
      value.Object([
        #("hasNextPage", value.Boolean(False)),
        #("hasPreviousPage", value.Boolean(False)),
        #("startCursor", value.Null),
        #("endCursor", value.Null),
      ]),
    ),
    #("totalCount", value.Null),
  ])
}

/// Build a Record union that can represent any record
/// This is used for forward joins which can point to any collection
fn build_record_union(possible_types: List(schema.Type)) -> schema.Type {
  // Type resolver examines the "collection" field to determine the concrete type
  let type_resolver = fn(ctx: schema.Context) -> Result(String, String) {
    case get_field_from_context(ctx, "collection") {
      Ok(collection_nsid) -> {
        // Convert NSID to type name (e.g., "app.bsky.feed.post" -> "AppBskyFeedPost")
        Ok(nsid.to_type_name(collection_nsid))
      }
      Error(_) ->
        Error("Could not determine record type: missing 'collection' field")
    }
  }

  schema.union_type(
    "Record",
    "A record from any collection",
    possible_types,
    type_resolver,
  )
}

/// Build GraphQL fields from lexicon properties
fn build_fields(
  properties: List(#(String, types.Property)),
  ref_object_types: Dict(String, schema.Type),
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
    schema.field(
      "actorHandle",
      schema.string_type(),
      "Handle of the actor who created this record",
      fn(ctx) {
        case get_field_from_context(ctx, "actorHandle") {
          Ok(handle) -> Ok(value.String(handle))
          Error(_) -> Ok(value.Null)
        }
      },
    ),
  ]

  // Build fields from lexicon properties
  let lexicon_fields =
    list.map(properties, fn(prop) {
      let #(name, types.Property(type_, _required, format, ref)) = prop
      // Use map_type_with_registry to resolve refs to object types
      let graphql_type =
        type_mapper.map_type_with_registry(type_, format, ref, ref_object_types)

      schema.field(name, graphql_type, "Field from lexicon", fn(ctx) {
        // Special handling for blob fields
        case type_ {
          "blob" -> {
            // Extract blob data from AT Protocol format and convert to Blob type format
            case extract_blob_data(ctx, name) {
              Ok(blob_value) -> Ok(blob_value)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> {
            // Try to extract field from the value object in context
            // Use the type-safe version that preserves Int, Float, Boolean types
            case get_nested_field_value_from_context(ctx, "value", name) {
              Ok(val) -> Ok(val)
              Error(_) -> Ok(value.Null)
            }
          }
        }
      })
    })

  // Combine standard and lexicon fields
  list.append(standard_fields, lexicon_fields)
}

/// Check if a lexicon property is sortable based on its type
/// Sortable types: string, integer, boolean, number (primitive types)
/// Non-sortable types: blob, ref, array (complex types)
fn is_sortable_property(property: types.Property) -> Bool {
  case property_to_field_type(property) {
    StringField | IntField | BoolField | NumberField | DateTimeField -> True
    BlobField | RefField | ArrayField -> False
  }
}

/// Check if a lexicon property is groupable based on its type (for aggregation)
/// Groupable types: string, integer, boolean, number, array
/// Non-groupable types: blob, ref (complex types)
fn is_groupable_property(property: types.Property) -> Bool {
  case property_to_field_type(property) {
    StringField | IntField | BoolField | NumberField | DateTimeField | ArrayField ->
      True
    BlobField | RefField -> False
  }
}

/// Get all sortable field names for sort enums
/// Returns only database-sortable fields (excludes computed fields like actorHandle)
fn get_sortable_field_names_for_sorting(record_type: RecordType) -> List(String) {
  // Filter properties to only sortable types, then get their field names
  let sortable_property_names =
    list.filter_map(record_type.properties, fn(prop) {
      let #(field_name, property) = prop
      case is_sortable_property(property) {
        True -> Ok(field_name)
        False -> Error(Nil)
      }
    })

  // Add standard sortable fields from AT Protocol
  // Note: actorHandle is NOT included because it's a computed field that requires a join
  // and can't be used directly in ORDER BY clauses
  let standard_sortable_fields = [
    "uri",
    "cid",
    "did",
    "collection",
    "indexedAt",
  ]
  list.append(standard_sortable_fields, sortable_property_names)
}

/// Get all filterable field names for WHERE inputs
/// Returns all primitive fields including computed fields like actorHandle
fn get_filterable_field_names(record_type: RecordType) -> List(String) {
  // Filter properties to only sortable types, then get their field names
  let filterable_property_names =
    list.filter_map(record_type.properties, fn(prop) {
      let #(field_name, property) = prop
      case is_sortable_property(property) {
        True -> Ok(field_name)
        False -> Error(Nil)
      }
    })

  // Add standard filterable fields from AT Protocol
  // Note: actorHandle IS included because WHERE clauses support filtering by it
  // via a join with the actor table
  let standard_filterable_fields = [
    "uri",
    "cid",
    "did",
    "collection",
    "indexedAt",
    "actorHandle",
  ]
  list.append(standard_filterable_fields, filterable_property_names)
}

/// Get all groupable field names for aggregation GROUP BY
/// Returns all groupable fields including arrays and computed fields like actorHandle
fn get_groupable_field_names(record_type: RecordType) -> List(String) {
  // Filter properties to only groupable types (includes arrays), then get their field names
  let groupable_property_names =
    list.filter_map(record_type.properties, fn(prop) {
      let #(field_name, property) = prop
      case is_groupable_property(property) {
        True -> Ok(field_name)
        False -> Error(Nil)
      }
    })

  // Add standard groupable fields from AT Protocol
  // Note: actorHandle IS included for grouping by actor
  let standard_groupable_fields = [
    "uri",
    "cid",
    "did",
    "collection",
    "indexedAt",
    "actorHandle",
  ]
  list.append(standard_groupable_fields, groupable_property_names)
}

/// Build a map of field names to their FieldTypes for validation
/// Includes both lexicon properties and standard AT Protocol fields
pub fn build_field_type_map(
  properties: List(#(String, types.Property)),
) -> dict.Dict(String, FieldType) {
  // Build map from lexicon properties
  let property_types =
    list.fold(properties, dict.new(), fn(acc, prop) {
      let #(field_name, property) = prop
      let field_type = property_to_field_type(property)
      dict.insert(acc, field_name, field_type)
    })

  // Add standard AT Protocol fields
  let standard_fields = [
    #("uri", StringField),
    #("cid", StringField),
    #("did", StringField),
    #("collection", StringField),
    #("indexedAt", DateTimeField),
    #("indexed_at", DateTimeField),
    #("actorHandle", StringField),
  ]

  list.fold(standard_fields, property_types, fn(acc, field) {
    let #(name, field_type) = field
    dict.insert(acc, name, field_type)
  })
}

/// Build a SortFieldEnum for a record type with all its sortable fields
/// Only includes primitive fields (string, integer, boolean, number) from the original lexicon
/// Excludes complex types (blob, ref), join fields, and computed fields like actorHandle
fn build_sort_field_enum(record_type: RecordType) -> schema.Type {
  // Get all sortable field names (excludes actorHandle and other computed fields)
  let sortable_fields = get_sortable_field_names_for_sorting(record_type)

  // Convert field names to enum values
  let enum_values =
    list.map(sortable_fields, fn(field_name) {
      schema.enum_value(field_name, "Sort by " <> field_name)
    })

  schema.enum_type(
    record_type.type_name <> "SortField",
    "Available sort fields for " <> record_type.type_name,
    enum_values,
  )
}

/// Build a GroupByFieldEnum for a record type with all its groupable fields
/// Includes primitive fields (string, integer, boolean, number, array) from the original lexicon
/// Includes computed fields like actorHandle (useful for grouping by actor)
/// Excludes complex types (blob, ref)
fn build_groupable_field_enum(record_type: RecordType) -> schema.Type {
  // Get all groupable field names (includes arrays and actorHandle)
  let groupable_fields = get_groupable_field_names(record_type)

  // Convert field names to enum values
  let enum_values =
    list.map(groupable_fields, fn(field_name) {
      schema.enum_value(field_name, "Group by " <> field_name)
    })

  schema.enum_type(
    record_type.type_name <> "GroupByField",
    "Available groupBy fields for " <> record_type.type_name,
    enum_values,
  )
}

/// Build a WhereInput type for a record type with all its filterable fields
/// Only includes primitive fields (string, integer, boolean, number) from the original lexicon
/// Excludes complex types (blob, ref) and join fields, but includes computed fields like actorHandle
fn build_where_input_type(record_type: RecordType) -> schema.Type {
  // Get filterable field names (includes actorHandle and other filterable computed fields)
  let field_names = get_filterable_field_names(record_type)

  // Use the connection module to build the where input type
  lexicon_connection.build_where_input_type(record_type.type_name, field_names)
}

/// Build the root Query type with fields for each record type
fn build_query_type(
  record_types: List(RecordType),
  object_types: dict.Dict(String, schema.Type),
  fetcher: RecordFetcher,
  aggregate_fetcher: option.Option(AggregateFetcher),
  field_type_registry: dict.Dict(String, dict.Dict(String, FieldType)),
) -> schema.Type {
  // Build regular query fields
  let query_fields =
    list.map(record_types, fn(record_type) {
      // Get the pre-built object type
      let assert Ok(object_type) = dict.get(object_types, record_type.nsid)

      // Create Connection types
      let edge_type = connection.edge_type(record_type.type_name, object_type)
      let connection_type =
        connection.connection_type(record_type.type_name, edge_type)

      // Build custom SortFieldEnum for this record type
      let sort_field_enum = build_sort_field_enum(record_type)

      // Build custom WhereInput type for this record type
      let where_input_type = build_where_input_type(record_type)

      // Build custom connection args with type-specific sort field enum and where input
      let connection_args =
        lexicon_connection.lexicon_connection_args_with_field_enum_and_where(
          record_type.type_name,
          sort_field_enum,
          where_input_type,
        )

      // Create query field that returns a Connection of this record type
      // Capture the nsid and fetcher in the closure
      let collection_nsid = record_type.nsid
      schema.field_with_args(
        record_type.field_name,
        connection_type,
        "Query " <> record_type.nsid <> " with cursor pagination and sorting",
        connection_args,
        fn(ctx: schema.Context) {
          // Extract pagination arguments from context
          let pagination_params = extract_pagination_params(ctx)

          // Call the fetcher function to get records with cursors from database
          use
            #(
              records_with_cursors,
              end_cursor,
              has_next_page,
              has_previous_page,
              total_count,
            )
          <- result.try(fetcher(collection_nsid, pagination_params))

          // Build edges from records with their cursors
          let edges =
            list.map(records_with_cursors, fn(record_tuple) {
              let #(record_value, record_cursor) = record_tuple
              connection.Edge(node: record_value, cursor: record_cursor)
            })

          // Build PageInfo
          let page_info =
            connection.PageInfo(
              has_next_page: has_next_page,
              has_previous_page: has_previous_page,
              start_cursor: case list.first(edges) {
                Ok(edge) -> option.Some(edge.cursor)
                Error(_) -> option.None
              },
              end_cursor: end_cursor,
            )

          // Build Connection
          let conn =
            connection.Connection(
              edges: edges,
              page_info: page_info,
              total_count: total_count,
            )

          Ok(connection.connection_to_value(conn))
        },
      )
    })

  // Build aggregated query fields if aggregate_fetcher is provided
  let aggregate_query_fields = case aggregate_fetcher {
    option.Some(agg_fetcher) -> {
      list.flat_map(record_types, fn(record_type) {
        build_aggregated_query_field(
          record_type,
          agg_fetcher,
          field_type_registry,
        )
      })
    }
    option.None -> []
  }

  // Combine regular and aggregate query fields
  let all_query_fields = list.append(query_fields, aggregate_query_fields)

  schema.object_type("Query", "Root query type", all_query_fields)
}

/// Extract pagination parameters from GraphQL context
fn extract_pagination_params(ctx: schema.Context) -> dataloader.PaginationParams {
  // Extract sortBy argument
  let sort_by = case schema.get_argument(ctx, "sortBy") {
    option.Some(value.List(items)) -> {
      // Convert list of sort objects to list of tuples
      let sort_tuples =
        list.filter_map(items, fn(item) {
          case item {
            value.Object(fields) -> {
              // Extract field and direction from the object
              case
                list.key_find(fields, "field"),
                list.key_find(fields, "direction")
              {
                Ok(value.String(field)), Ok(value.String(direction)) -> {
                  // Convert direction to lowercase for consistency
                  let dir = case direction {
                    "ASC" -> "asc"
                    "DESC" -> "desc"
                    _ -> "desc"
                  }
                  Ok(#(field, dir))
                }
                _, _ -> Error(Nil)
              }
            }
            _ -> Error(Nil)
          }
        })

      case sort_tuples {
        [] -> option.Some([#("indexed_at", "desc")])
        _ -> option.Some(sort_tuples)
      }
    }
    _ -> option.Some([#("indexed_at", "desc")])
  }

  // Extract first/after/last/before arguments
  let first = case schema.get_argument(ctx, "first") {
    option.Some(value.Int(n)) -> option.Some(n)
    option.Some(value.String(s)) -> {
      case int.parse(s) {
        Ok(n) -> option.Some(n)
        Error(_) -> option.None
      }
    }
    _ -> option.None
  }

  let after = case schema.get_argument(ctx, "after") {
    option.Some(value.String(s)) -> option.Some(s)
    _ -> option.None
  }

  let last = case schema.get_argument(ctx, "last") {
    option.Some(value.Int(n)) -> option.Some(n)
    option.Some(value.String(s)) -> {
      case int.parse(s) {
        Ok(n) -> option.Some(n)
        Error(_) -> option.None
      }
    }
    _ -> option.None
  }

  let before = case schema.get_argument(ctx, "before") {
    option.Some(value.String(s)) -> option.Some(s)
    _ -> option.None
  }

  // Extract where argument
  let where = case schema.get_argument(ctx, "where") {
    option.Some(where_value) -> {
      let parsed = where_input.parse_where_clause(where_value)
      case where_input.is_clause_empty(parsed) {
        True -> option.None
        False -> option.Some(parsed)
      }
    }
    _ -> option.None
  }

  dataloader.PaginationParams(
    first: first,
    after: after,
    last: last,
    before: before,
    sort_by: sort_by,
    where: where,
  )
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
/// Returns the value as a GraphQL value, handling all types (String, Int, Float, Boolean)
fn get_nested_field_value_from_context(
  ctx: schema.Context,
  parent_field: String,
  field_name: String,
) -> Result(value.Value, Nil) {
  case ctx.data {
    option.Some(value.Object(fields)) -> {
      case list.key_find(fields, parent_field) {
        Ok(value.Object(nested_fields)) -> {
          case list.key_find(nested_fields, field_name) {
            Ok(val) -> Ok(val)
            _ -> Error(Nil)
          }
        }
        _ -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

/// Helper to extract a nested field value as Dynamic from resolver context
/// This version returns the raw Dynamic value for processing by uri_extractor
fn get_nested_field_from_context_dynamic(
  ctx: schema.Context,
  parent_field: String,
  field_name: String,
) -> Result(Dynamic, Nil) {
  case ctx.data {
    option.Some(value.Object(fields)) -> {
      case list.key_find(fields, parent_field) {
        Ok(value.Object(nested_fields)) -> {
          case list.key_find(nested_fields, field_name) {
            Ok(field_value) -> {
              // Convert the GraphQL Value to Dynamic for uri_extractor
              Ok(value_to_dynamic(field_value))
            }
            _ -> Error(Nil)
          }
        }
        _ -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

/// Convert a GraphQL Value to Dynamic
fn value_to_dynamic(v: value.Value) -> Dynamic {
  case v {
    value.String(s) -> unsafe_coerce_to_dynamic(s)
    value.Int(i) -> unsafe_coerce_to_dynamic(i)
    value.Float(f) -> unsafe_coerce_to_dynamic(f)
    value.Boolean(b) -> unsafe_coerce_to_dynamic(b)
    value.Null -> unsafe_coerce_to_dynamic(option.None)
    value.Object(fields) -> {
      // Convert object fields to a dict-like structure
      let field_map =
        list.fold(fields, dict.new(), fn(acc, field) {
          let #(key, val) = field
          dict.insert(acc, key, value_to_dynamic(val))
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

@external(erlang, "dataloader_ffi", "identity")
fn unsafe_coerce_to_dynamic(value: a) -> Dynamic

/// Extract blob data from AT Protocol format and convert to Blob type format
/// AT Protocol blob format:
/// {
///   "ref": {"$link": "bafyrei..."},
///   "mimeType": "image/jpeg",
///   "size": 12345
/// }
/// Blob type expects:
/// {
///   "ref": "bafyrei...",
///   "mime_type": "image/jpeg",
///   "size": 12345,
///   "did": "did:plc:..."
/// }
fn extract_blob_data(
  ctx: schema.Context,
  field_name: String,
) -> Result(value.Value, Nil) {
  case ctx.data {
    option.Some(value.Object(fields)) -> {
      // First get the DID from the top-level context
      let did = case list.key_find(fields, "did") {
        Ok(value.String(d)) -> d
        _ -> ""
      }

      // Then get the blob object from value.{field_name}
      case list.key_find(fields, "value") {
        Ok(value.Object(nested_fields)) -> {
          case list.key_find(nested_fields, field_name) {
            Ok(value.Object(blob_fields)) -> {
              // Extract ref from {"$link": "cid..."}
              let ref = case list.key_find(blob_fields, "ref") {
                Ok(value.Object(ref_obj)) -> {
                  case list.key_find(ref_obj, "$link") {
                    Ok(value.String(cid)) -> cid
                    _ -> ""
                  }
                }
                _ -> ""
              }

              // Extract mimeType
              let mime_type = case list.key_find(blob_fields, "mimeType") {
                Ok(value.String(mt)) -> mt
                _ -> "image/jpeg"
              }

              // Extract size
              let size = case list.key_find(blob_fields, "size") {
                Ok(value.Int(s)) -> s
                _ -> 0
              }

              // Return blob data in format expected by Blob type resolvers
              Ok(
                value.Object([
                  #("ref", value.String(ref)),
                  #("mime_type", value.String(mime_type)),
                  #("size", value.Int(size)),
                  #("did", value.String(did)),
                ]),
              )
            }
            _ -> Error(Nil)
          }
        }
        _ -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

/// Build subscription type with fields for each record type
///
/// Generates three subscription fields per record:
/// - {collection}Created: Returns the full record type
/// - {collection}Updated: Returns the full record type
/// - {collection}Deleted: Returns String! (just the URI)
fn build_subscription_type(
  record_types: List(RecordType),
  object_types: dict.Dict(String, schema.Type),
) -> schema.Type {
  let subscription_fields =
    list.flat_map(record_types, fn(record_type) {
      // Get the pre-built object type for Created/Updated
      let assert Ok(object_type) = dict.get(object_types, record_type.nsid)

      // Convert collection name to camelCase field name base
      // e.g., "app.bsky.feed.post" -> "feedPost"
      let field_base = record_type.field_name

      // Created subscription - returns full record
      let created_field =
        schema.field(
          field_base <> "Created",
          schema.non_null(object_type),
          "Emitted when a new " <> record_type.nsid <> " record is created",
          fn(ctx) {
            // For subscriptions, the event data is passed via ctx.data
            // Return it directly without additional processing
            case ctx.data {
              option.Some(data) -> Ok(data)
              option.None ->
                Error("Subscription resolver called without event data")
            }
          },
        )

      // Updated subscription - returns full record
      let updated_field =
        schema.field(
          field_base <> "Updated",
          schema.non_null(object_type),
          "Emitted when a " <> record_type.nsid <> " record is updated",
          fn(ctx) {
            case ctx.data {
              option.Some(data) -> Ok(data)
              option.None ->
                Error("Subscription resolver called without event data")
            }
          },
        )

      // Deleted subscription
      let deleted_field =
        schema.field(
          field_base <> "Deleted",
          schema.non_null(object_type),
          "Emitted when a " <> record_type.nsid <> " record is deleted",
          fn(ctx) {
            case ctx.data {
              option.Some(data) -> Ok(data)
              option.None ->
                Error("Subscription resolver called without event data")
            }
          },
        )

      [created_field, updated_field, deleted_field]
    })

  schema.object_type(
    "Subscription",
    "GraphQL subscription root",
    subscription_fields,
  )
}

// ===== Aggregated Query Support =====

/// Build an aggregated query field for a record type
fn build_aggregated_query_field(
  record_type: RecordType,
  agg_fetcher: AggregateFetcher,
  field_type_registry: dict.Dict(String, dict.Dict(String, FieldType)),
) -> List(schema.Field) {
  // Create aggregated type name (e.g., XyzStatusphereStatusAggregated)
  let aggregated_type_name = record_type.type_name <> "Aggregated"

  // Create field name (e.g., xyzStatusphereStatusAggregated)
  let field_name = record_type.field_name <> "Aggregated"

  // Build the aggregated output type
  let aggregated_type =
    build_aggregated_output_type(record_type, aggregated_type_name)

  // Build collection-specific GroupByField enum
  let group_by_field_enum = build_groupable_field_enum(record_type)

  // Build collection-specific GroupByFieldInput with the field enum
  let group_by_input =
    build_group_by_field_input(record_type.type_name, group_by_field_enum)

  let aggregation_order_by_input = build_aggregation_order_by_input()

  // Build WhereInput type for this record type (reuse existing)
  let where_input_type = build_where_input_type(record_type)

  // Build arguments for aggregated query
  let args = [
    schema.argument(
      "groupBy",
      schema.list_type(schema.non_null(group_by_input)),
      "Fields to group by (required)",
      option.None,
    ),
    schema.argument(
      "where",
      where_input_type,
      "Filter records before aggregation",
      option.None,
    ),
    schema.argument(
      "orderBy",
      aggregation_order_by_input,
      "Order by count (default: desc)",
      option.None,
    ),
    schema.argument(
      "limit",
      schema.int_type(),
      "Maximum number of results (default 50, max 1000)",
      option.Some(value.Int(50)),
    ),
  ]

  // Create the query field
  let collection_nsid = record_type.nsid
  let field =
    schema.field_with_args(
      field_name,
      schema.list_type(schema.non_null(aggregated_type)),
      "Aggregated query for " <> record_type.nsid,
      args,
      fn(ctx: schema.Context) {
        // Extract and parse arguments
        case extract_aggregate_params(ctx) {
          Ok(params) -> {
            // Get field types for this collection
            let field_types =
              dict.get(field_type_registry, collection_nsid)
              |> result.unwrap(dict.new())

            // Validate that interval is only applied to datetime fields
            case validate_interval_fields(params.group_by, field_types) {
              Ok(_) -> {
                // Extract field names from groupBy
                let field_names = list.map(params.group_by, fn(gb) { gb.field })

                // Call the aggregate fetcher
                case agg_fetcher(collection_nsid, params) {
                  Ok(results) -> {
                    // Convert results to GraphQL values with field name mapping
                    let result_values =
                      list.map(results, fn(result) {
                        aggregate_result_to_value(result, field_names)
                      })
                    Ok(value.List(result_values))
                  }
                  Error(err) -> Error(err)
                }
              }
              Error(validation_err) -> Error(validation_err)
            }
          }
          Error(err) -> Error(err)
        }
      },
    )

  [field]
}

/// Build the aggregated output type for a record type
fn build_aggregated_output_type(
  record_type: RecordType,
  type_name: String,
) -> schema.Type {
  // Get all field names from the record type
  let field_names =
    list.map(record_type.properties, fn(prop) {
      let #(name, _) = prop
      name
    })

  // Add table column fields
  let all_field_names =
    list.append(["uri", "cid", "did", "collection", "indexed_at"], field_names)

  // Build fields: all as nullable JSON (using string_type as proxy) + count as Int!
  // Resolvers extract values from parent object
  let fields =
    list.map(all_field_names, fn(field_name) {
      schema.field(
        field_name,
        schema.string_type(),
        "Grouped field value",
        fn(ctx: schema.Context) {
          case ctx.data {
            option.Some(value.Object(fields)) -> {
              case list.key_find(fields, field_name) {
                Ok(v) -> Ok(v)
                Error(_) -> Ok(value.Null)
              }
            }
            _ -> Ok(value.Null)
          }
        },
      )
    })

  // Add count field
  let fields_with_count =
    list.append(fields, [
      schema.field(
        "count",
        schema.non_null(schema.int_type()),
        "Count of records in this group",
        fn(ctx: schema.Context) {
          case ctx.data {
            option.Some(value.Object(fields)) -> {
              case list.key_find(fields, "count") {
                Ok(v) -> Ok(v)
                Error(_) -> Ok(value.Int(0))
              }
            }
            _ -> Ok(value.Int(0))
          }
        },
      ),
    ])

  schema.object_type(
    type_name,
    "Aggregated results for " <> record_type.nsid,
    fields_with_count,
  )
}

/// Build GroupByFieldInput input type with collection-specific field enum
fn build_group_by_field_input(
  type_name: String,
  field_enum: schema.Type,
) -> schema.Type {
  let date_interval_enum = build_date_interval_enum()

  schema.input_object_type(
    type_name <> "GroupByFieldInput",
    "Specifies a field to group by with optional date truncation",
    [
      schema.input_field(
        "field",
        schema.non_null(field_enum),
        "Field name to group by",
        option.None,
      ),
      schema.input_field(
        "interval",
        date_interval_enum,
        "Date truncation interval (for datetime fields)",
        option.None,
      ),
    ],
  )
}

/// Build DateInterval enum type
fn build_date_interval_enum() -> schema.Type {
  schema.enum_type("DateInterval", "Date truncation intervals for aggregation", [
    schema.enum_value("HOUR", "Truncate to hour"),
    schema.enum_value("DAY", "Truncate to day"),
    schema.enum_value("WEEK", "Truncate to week"),
    schema.enum_value("MONTH", "Truncate to month"),
  ])
}

/// Build AggregationOrderBy input type
fn build_aggregation_order_by_input() -> schema.Type {
  schema.input_object_type(
    "AggregationOrderBy",
    "Order aggregation results by count",
    [
      schema.input_field(
        "count",
        lexicon_connection.sort_direction_enum(),
        "Order by count (asc or desc)",
        option.None,
      ),
    ],
  )
}

/// Extract aggregate parameters from GraphQL context
fn extract_aggregate_params(
  ctx: schema.Context,
) -> Result(AggregateParams, String) {
  // Extract groupBy argument (required)
  let group_by_result = case schema.get_argument(ctx, "groupBy") {
    option.Some(value.List(items)) -> {
      list.try_map(items, fn(item) {
        case item {
          value.Object(fields) -> {
            case list.key_find(fields, "field") {
              Ok(value.String(field)) -> {
                let interval = case list.key_find(fields, "interval") {
                  Ok(value.String(int_str)) -> {
                    case aggregate_input.parse_date_interval(int_str) {
                      Ok(interval) -> option.Some(interval)
                      Error(_) -> option.None
                    }
                  }
                  _ -> option.None
                }
                Ok(aggregate_input.GroupByFieldInput(field, interval))
              }
              _ -> Error("Missing or invalid 'field' in groupBy")
            }
          }
          _ -> Error("groupBy must be a list of objects")
        }
      })
    }
    option.Some(_) -> Error("groupBy must be a list")
    option.None -> Error("groupBy argument is required")
  }

  use group_by <- result.try(group_by_result)

  // Validate query complexity
  use _ <- result.try(validate_query_complexity(group_by))

  // Extract where argument (optional)
  let where_clause = case schema.get_argument(ctx, "where") {
    option.Some(where_value) -> {
      let wc = where_input.parse_where_clause(where_value)
      option.Some(wc)
    }
    option.None -> option.None
  }

  // Extract orderBy argument (optional, default to desc)
  let order_by_desc = case schema.get_argument(ctx, "orderBy") {
    option.Some(value.Object(fields)) -> {
      case list.key_find(fields, "count") {
        Ok(value.String("ASC")) -> False
        Ok(value.String("DESC")) -> True
        _ -> True
        // default desc
      }
    }
    _ -> True
    // default desc
  }

  // Extract limit argument (optional, default 50, max 1000)
  let limit = case schema.get_argument(ctx, "limit") {
    option.Some(value.Int(n)) -> {
      case n {
        _ if n > 1000 -> 1000
        _ if n < 1 -> 50
        _ -> n
      }
    }
    _ -> 50
  }

  Ok(AggregateParams(
    group_by: group_by,
    where: where_clause,
    order_by_desc: order_by_desc,
    limit: limit,
  ))
}

/// Validate query complexity to prevent resource exhaustion
/// Returns Ok(Nil) if valid, Error(String) if too complex
pub fn validate_query_complexity(
  group_by: List(aggregate_input.GroupByFieldInput),
) -> Result(Nil, String) {
  let field_count = list.length(group_by)
  case field_count {
    n if n > 5 ->
      Error(
        "Query too complex: maximum 5 group by fields allowed (got "
        <> int.to_string(n)
        <> ")",
      )
    n if n < 1 -> Error("Query must include at least 1 group by field")
    _ -> Ok(Nil)
  }
}

/// Validate that interval is only applied to datetime fields
/// Returns Ok(Nil) if valid, Error(String) with descriptive message if invalid
pub fn validate_interval_fields(
  group_by: List(aggregate_input.GroupByFieldInput),
  field_types: dict.Dict(String, FieldType),
) -> Result(Nil, String) {
  list.try_each(group_by, fn(gb) {
    case gb.interval {
      option.Some(_interval) -> {
        // Has interval, must be datetime
        case dict.get(field_types, gb.field) {
          Ok(DateTimeField) -> Ok(Nil)
          Ok(StringField) ->
            Error(
              "Cannot apply date interval to field '"
              <> gb.field
              <> "': field is string, not datetime",
            )
          Ok(IntField) ->
            Error(
              "Cannot apply date interval to field '"
              <> gb.field
              <> "': field is integer, not datetime",
            )
          Ok(BoolField) ->
            Error(
              "Cannot apply date interval to field '"
              <> gb.field
              <> "': field is boolean, not datetime",
            )
          Ok(NumberField) ->
            Error(
              "Cannot apply date interval to field '"
              <> gb.field
              <> "': field is number, not datetime",
            )
          Ok(ArrayField) ->
            Error(
              "Cannot apply date interval to field '"
              <> gb.field
              <> "': field is array, not datetime",
            )
          Ok(BlobField) ->
            Error(
              "Cannot apply date interval to field '"
              <> gb.field
              <> "': field is blob, not datetime",
            )
          Ok(RefField) ->
            Error(
              "Cannot apply date interval to field '"
              <> gb.field
              <> "': field is ref, not datetime",
            )
          Error(_) -> Error("Field '" <> gb.field <> "' not found in schema")
        }
      }
      option.None -> Ok(Nil)
      // No interval, any groupable type is fine
    }
  })
}

/// Convert AggregateResult to GraphQL value
/// Maps field_0, field_1, etc. to actual field names
fn aggregate_result_to_value(
  result: aggregate_types.AggregateResult,
  field_names: List(String),
) -> value.Value {
  // Map field_0, field_1, etc. to actual field names
  let group_fields =
    list.index_map(field_names, fn(field_name, index) {
      let field_key = "field_" <> int.to_string(index)
      case dict.get(result.group_values, field_key) {
        Ok(dynamic_value) -> #(
          field_name,
          dynamic_to_graphql_value(dynamic_value),
        )
        Error(_) -> #(field_name, value.Null)
      }
    })

  let all_fields =
    list.append(group_fields, [#("count", value.Int(result.count))])

  value.Object(all_fields)
}

/// Convert dynamic value to GraphQL value
/// Detects and parses JSON strings (arrays/objects) into proper GraphQL values
fn dynamic_to_graphql_value(dyn: dynamic.Dynamic) -> value.Value {
  // Try different decoders
  case decode.run(dyn, decode.string) {
    Ok(s) -> {
      // Check if this is a JSON string (starts with [ or {)
      case string.starts_with(s, "[") || string.starts_with(s, "{") {
        True -> {
          // Try to parse as JSON
          case json.parse(s, decode.dynamic) {
            Ok(parsed_json) -> {
              // Recursively convert the parsed JSON to GraphQL value
              json_dynamic_to_graphql_value(parsed_json)
            }
            Error(_) -> {
              // JSON parse failed, return as string
              value.String(s)
            }
          }
        }
        False -> value.String(s)
      }
    }
    Error(_) ->
      case decode.run(dyn, decode.int) {
        Ok(i) -> value.Int(i)
        Error(_) ->
          case decode.run(dyn, decode.float) {
            Ok(f) -> value.Float(f)
            Error(_) ->
              case decode.run(dyn, decode.bool) {
                Ok(b) -> value.Boolean(b)
                Error(_) -> value.Null
              }
          }
      }
  }
}

/// Convert a JSON dynamic value to GraphQL value recursively
/// Handles arrays and objects properly
fn json_dynamic_to_graphql_value(dyn: dynamic.Dynamic) -> value.Value {
  // Try to decode as different types
  case decode.run(dyn, decode.string) {
    Ok(s) -> value.String(s)
    Error(_) ->
      case decode.run(dyn, decode.int) {
        Ok(i) -> value.Int(i)
        Error(_) ->
          case decode.run(dyn, decode.float) {
            Ok(f) -> value.Float(f)
            Error(_) ->
              case decode.run(dyn, decode.bool) {
                Ok(b) -> value.Boolean(b)
                Error(_) ->
                  case decode.run(dyn, decode.list(decode.dynamic)) {
                    Ok(list_items) -> {
                      // Recursively convert array items
                      let graphql_items =
                        list.map(list_items, json_dynamic_to_graphql_value)
                      value.List(graphql_items)
                    }
                    Error(_) ->
                      case
                        decode.run(
                          dyn,
                          decode.dict(decode.string, decode.dynamic),
                        )
                      {
                        Ok(dict_items) -> {
                          // Convert dict to list of tuples for GraphQL Object
                          let graphql_fields =
                            dict.to_list(dict_items)
                            |> list.map(fn(pair) {
                              let #(key, val) = pair
                              #(key, json_dynamic_to_graphql_value(val))
                            })
                          value.Object(graphql_fields)
                        }
                        Error(_) -> value.Null
                      }
                  }
              }
          }
      }
  }
}
