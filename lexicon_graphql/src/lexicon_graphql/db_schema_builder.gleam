/// Database Schema Builder
///
/// Builds GraphQL schemas from AT Protocol lexicon definitions with database-backed resolvers.
/// This extends the base schema_builder with actual data resolution.
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import graphql/connection
import graphql/schema
import graphql/value
import lexicon_graphql/connection as lexicon_connection
import lexicon_graphql/mutation_builder
import lexicon_graphql/nsid
import lexicon_graphql/type_mapper
import lexicon_graphql/types
import lexicon_graphql/where_input

/// Record type metadata with database resolver info
type RecordType {
  RecordType(
    nsid: String,
    type_name: String,
    field_name: String,
    fields: List(schema.Field),
  )
}

/// Pagination parameters for connection queries
pub type PaginationParams {
  PaginationParams(
    first: option.Option(Int),
    after: option.Option(String),
    last: option.Option(Int),
    before: option.Option(String),
    sort_by: option.Option(List(#(String, String))),
    where: option.Option(where_input.WhereClause),
  )
}

/// Type for a database record fetcher function with pagination support
/// Takes a collection NSID and pagination params, returns Connection data
/// Returns: (records_with_cursors, end_cursor, has_next_page, has_previous_page, total_count)
pub type RecordFetcher =
  fn(String, PaginationParams) ->
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
/// The fetcher parameter should be a function that queries the database for records
/// The mutation resolver factories are optional - if None, mutations will return stub errors
pub fn build_schema_with_fetcher(
  lexicons: List(types.Lexicon),
  fetcher: RecordFetcher,
  create_factory: option.Option(mutation_builder.ResolverFactory),
  update_factory: option.Option(mutation_builder.ResolverFactory),
  delete_factory: option.Option(mutation_builder.ResolverFactory),
  upload_blob_factory: option.Option(mutation_builder.UploadBlobResolverFactory),
) -> Result(schema.Schema, String) {
  case lexicons {
    [] -> Error("Cannot build schema from empty lexicon list")
    _ -> {
      // Extract record types from lexicons
      let record_types = extract_record_types(lexicons)

      // Build the query type with fields for each record
      let query_type = build_query_type(record_types, fetcher)

      // Build the mutation type with provided resolver factories
      let mutation_type =
        mutation_builder.build_mutation_type(
          lexicons,
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

/// Extract record types from lexicon definitions
fn extract_record_types(lexicons: List(types.Lexicon)) -> List(RecordType) {
  lexicons
  |> list.filter_map(parse_lexicon)
}

/// Parse a single lexicon into a RecordType
fn parse_lexicon(lexicon: types.Lexicon) -> Result(RecordType, Nil) {
  case lexicon {
    types.Lexicon(id, types.Defs(types.RecordDef("record", properties))) -> {
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
  properties: List(#(String, types.Property)),
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
      let #(name, types.Property(type_, _required)) = prop
      let graphql_type = type_mapper.map_type(type_)

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
            case get_nested_field_from_context(ctx, "value", name) {
              Ok(val) -> Ok(value.String(val))
              Error(_) -> Ok(value.Null)
            }
          }
        }
      })
    })

  // Combine standard and lexicon fields
  list.append(standard_fields, lexicon_fields)
}

/// Build a SortFieldEnum for a record type with all its sortable fields
fn build_sort_field_enum(record_type: RecordType) -> schema.Type {
  // Get field names from the record type, excluding non-sortable fields
  let field_names =
    list.map(record_type.fields, fn(field) { schema.field_name(field) })
    |> list.filter(fn(name) { name != "actorHandle" })

  // Convert field names to enum values
  let enum_values =
    list.map(field_names, fn(field_name) {
      schema.enum_value(field_name, "Sort by " <> field_name)
    })

  schema.enum_type(
    record_type.type_name <> "SortField",
    "Available sort fields for " <> record_type.type_name,
    enum_values,
  )
}

/// Build a WhereInput type for a record type with all its filterable fields
fn build_where_input_type(record_type: RecordType) -> schema.Type {
  // Get field names from the record type
  let field_names =
    list.map(record_type.fields, fn(field) { schema.field_name(field) })

  // Use the connection module to build the where input type
  lexicon_connection.build_where_input_type(record_type.type_name, field_names)
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
fn extract_pagination_params(ctx: schema.Context) -> PaginationParams {
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
    option.Some(value.String(s)) -> {
      case int.parse(s) {
        Ok(n) -> option.Some(n)
        Error(_) -> option.Some(50)
      }
    }
    _ -> option.Some(50)
  }

  let after = case schema.get_argument(ctx, "after") {
    option.Some(value.String(s)) -> option.Some(s)
    _ -> option.None
  }

  let last = case schema.get_argument(ctx, "last") {
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

  PaginationParams(
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
