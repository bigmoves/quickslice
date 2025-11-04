/// Database Schema Builder
///
/// Builds GraphQL schemas from AT Protocol lexicon definitions with database-backed resolvers.
/// This extends the base schema_builder with actual data resolution.
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import graphql/connection
import graphql/schema
import graphql/value
import lexicon_graphql/collection_meta
import lexicon_graphql/connection as lexicon_connection
import lexicon_graphql/dataloader
import lexicon_graphql/lexicon_registry
import lexicon_graphql/mutation_builder
import lexicon_graphql/nsid
import lexicon_graphql/object_type_builder
import lexicon_graphql/type_mapper
import lexicon_graphql/types
import lexicon_graphql/uri_extractor
import lexicon_graphql/where_input

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
      let #(record_types, object_types) =
        extract_record_types_and_object_types(
          lexicons,
          batch_fetcher,
          paginated_batch_fetcher,
        )

      // Build the query type with fields for each record using shared object types
      let query_type = build_query_type(record_types, object_types, fetcher)

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

/// Extract record types and object types from lexicon definitions
///
/// NEW 3-PASS ARCHITECTURE:
/// Pass 0: Build object types from lexicon defs (e.g., aspectRatio)
/// Pass 1: Extract metadata and build basic types (base fields only)
/// Pass 2: Build complete RecordTypes with ALL join fields
/// Pass 3: Rebuild ONLY Connection fields using final types
fn extract_record_types_and_object_types(
  lexicons: List(types.Lexicon),
  batch_fetcher: option.Option(dataloader.BatchFetcher),
  paginated_batch_fetcher: option.Option(dataloader.PaginatedBatchFetcher),
) -> #(List(RecordType), dict.Dict(String, schema.Type)) {
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
        types.Lexicon(id, types.Defs(option.Some(types.RecordDef("record", _, _)), _)) -> {
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
    list.fold(basic_record_types_without_forward_joins, dict.new(), fn(acc, record_type) {
      let object_type =
        schema.object_type(
          record_type.type_name,
          "Record type: " <> record_type.nsid,
          record_type.fields,
        )
      dict.insert(acc, record_type.nsid, object_type)
    })

  // Build Record union for forward joins to reference
  let basic_possible_types = dict.values(basic_object_types_without_forward_joins)
  let basic_record_union = build_record_union(basic_possible_types)
  let basic_object_types_with_generic =
    dict.insert(basic_object_types_without_forward_joins, "_generic_record", basic_record_union)

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

      let all_fields =
        list.flatten([record_type.fields, forward_join_fields])

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
  let basic_possible_types_complete = dict.values(basic_object_types_without_generic)
  let basic_record_union_complete = build_record_union(basic_possible_types_complete)
  let basic_object_types =
    dict.insert(basic_object_types_without_generic, "_generic_record", basic_record_union_complete)

  // Build sort field enums dict BEFORE Pass 2 - create each enum once and reuse
  let sort_field_enums: dict.Dict(String, schema.Type) =
    list.fold(basic_record_types, dict.new(), fn(acc, rt: RecordType) {
      let enum = build_sort_field_enum(rt)
      dict.insert(acc, rt.type_name, enum)
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

      RecordType(..record_type, fields: all_fields, reverse_joins: reverse_joins)
    })

  // Build complete object types from complete RecordTypes
  let complete_object_types_without_generic: dict.Dict(String, schema.Type) =
    list.fold(complete_record_types, dict.new(), fn(acc, rt: RecordType) {
      let object_type =
        schema.object_type(
          rt.type_name,
          "Record type: " <> rt.nsid,
          rt.fields,
        )
      dict.insert(acc, rt.nsid, object_type)
    })

  // Build Record union with complete object types
  let complete_possible_types = dict.values(complete_object_types_without_generic)
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

      RecordType(..record_type, fields: all_fields, reverse_joins: reverse_joins)
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

  #(final_record_types, final_object_types_with_refs)
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
    types.Lexicon(id, types.Defs(option.Some(types.RecordDef("record", _, properties)), _)) -> {
      let type_name = nsid.to_type_name(id)
      let field_name = nsid.to_field_name(id)

      // Extract metadata for join field generation
      let meta = collection_meta.extract_metadata(lexicon)

      // Build regular and forward join fields
      let base_fields =
        build_fields_with_meta(properties, meta, batch_fetcher, ref_object_types)

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
/// Non-sortable types: blob, ref (complex types)
fn is_sortable_property(property: types.Property) -> Bool {
  case property.type_, property.format {
    // Primitive types are sortable
    "string", _ -> True
    "integer", _ -> True
    "boolean", _ -> True
    "number", _ -> True
    // Blob and ref types are not sortable
    "blob", _ -> False
    "ref", _ -> False
    // Default to non-sortable for unknown types
    _, _ -> False
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
  let standard_sortable_fields = ["uri", "cid", "did", "collection", "indexedAt"]
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
) -> schema.Type {
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

  schema.object_type("Query", "Root query type", query_fields)
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
