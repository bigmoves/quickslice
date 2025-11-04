/// DataLoader for batching database queries
///
/// This module provides batch query functions for join operations to prevent N+1 queries.
/// It works with the existing RecordFetcher pattern to batch URI lookups.
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import graphql/value
import lexicon_graphql/collection_meta
import lexicon_graphql/uri_extractor
import lexicon_graphql/where_input.{type WhereClause}

/// Result of a batch query: maps URIs to their records
pub type BatchResult =
  Dict(String, List(value.Value))

/// Batch query function type - takes a list of URIs and field constraints,
/// returns records grouped by the URI they match
pub type BatchFetcher =
  fn(List(String), String, Option(String)) -> Result(BatchResult, String)

/// Pagination parameters for join queries
/// Re-exported from db_schema_builder to avoid circular dependency
pub type PaginationParams {
  PaginationParams(
    first: Option(Int),
    after: Option(String),
    last: Option(Int),
    before: Option(String),
    sort_by: Option(List(#(String, String))),
    where: Option(WhereClause),
  )
}

/// Result of a paginated batch query
pub type PaginatedBatchResult {
  PaginatedBatchResult(
    /// Records with their cursors
    edges: List(#(value.Value, String)),
    /// Whether there are more records after this page
    has_next_page: Bool,
    /// Whether there are more records before this page
    has_previous_page: Bool,
    /// Total count of records (if available)
    total_count: Option(Int),
  )
}

/// Paginated batch query function type - takes a parent key, collection, field,
/// and pagination params, returns paginated records
pub type PaginatedBatchFetcher =
  fn(String, String, Option(String), PaginationParams) ->
    Result(PaginatedBatchResult, String)

/// Key for forward join batching - identifies a unique batch request
pub type ForwardJoinKey {
  ForwardJoinKey(
    /// Target collection to fetch
    target_collection: String,
    /// URIs to fetch
    uris: List(String),
  )
}

/// Key for reverse join batching - identifies records that reference parent URIs
pub type ReverseJoinKey {
  ReverseJoinKey(
    /// Collection to search in
    collection: String,
    /// Field name that contains the reference (e.g., "subject", "reply")
    reference_field: String,
    /// Parent URIs to find references to
    parent_uris: List(String),
  )
}

/// Extract the collection name from an AT URI
/// Format: at://did:plc:abc123/app.bsky.feed.post/rkey
/// Returns: app.bsky.feed.post
pub fn uri_to_collection(uri: String) -> Option(String) {
  case string.split(uri, "/") {
    ["at:", "", _did, collection, _rkey] -> Some(collection)
    _ -> None
  }
}

/// Batch fetch records by URI (forward joins)
///
/// Given a list of URIs, fetches all target records in a single query.
/// Returns a Dict mapping each URI to its record (if found).
pub fn batch_fetch_by_uri(
  uris: List(String),
  fetcher: BatchFetcher,
) -> Result(Dict(String, value.Value), String) {
  // Group URIs by collection for optimal batching
  let grouped = group_uris_by_collection(uris)

  // Fetch each collection's records in a batch
  list.try_fold(grouped, dict.new(), fn(acc, group) {
    let #(collection, collection_uris) = group

    case fetcher(collection_uris, collection, None) {
      Ok(batch_result) -> {
        // Merge results into accumulator
        // For forward joins, we expect single records per URI
        let merged =
          dict.fold(batch_result, acc, fn(result_acc, uri, records) {
            case records {
              [first, ..] -> dict.insert(result_acc, uri, first)
              [] -> result_acc
            }
          })
        Ok(merged)
      }
      Error(e) -> Error(e)
    }
  })
}

/// Batch fetch records by reverse join (records that reference parent URIs)
///
/// Given parent URIs and a field name, finds all records whose field references those URIs.
/// Returns a Dict mapping each parent URI to the list of records that reference it.
pub fn batch_fetch_by_reverse_join(
  parent_uris: List(String),
  collection: String,
  reference_field: String,
  fetcher: BatchFetcher,
) -> Result(Dict(String, List(value.Value)), String) {
  // Fetch all records that reference any of the parent URIs
  fetcher(parent_uris, collection, Some(reference_field))
}

/// Batch fetch records by DID (records that share the same DID)
///
/// Given a list of DIDs and a target collection, fetches all records in that collection
/// that belong to those DIDs.
/// Returns a Dict mapping each DID to the list of records (or single record for unique collections).
pub fn batch_fetch_by_did(
  dids: List(String),
  target_collection: String,
  fetcher: BatchFetcher,
) -> Result(Dict(String, List(value.Value)), String) {
  // Use the fetcher to get records by DID
  // The fetcher will need to be updated to handle DID-based queries
  // For now, we pass the DIDs as URIs and use None for the field
  // The actual database layer will interpret this correctly
  fetcher(dids, target_collection, None)
}

/// Batch fetch records by reverse join with pagination
///
/// Given a parent URI, field name, and pagination params, finds all records whose field
/// references that URI, with cursor-based pagination.
pub fn batch_fetch_by_reverse_join_paginated(
  parent_uri: String,
  collection: String,
  reference_field: String,
  pagination: PaginationParams,
  fetcher: PaginatedBatchFetcher,
) -> Result(PaginatedBatchResult, String) {
  // Fetch paginated records that reference the parent URI
  fetcher(parent_uri, collection, Some(reference_field), pagination)
}

/// Batch fetch records by DID with pagination
///
/// Given a DID, target collection, and pagination params, fetches all records in that collection
/// that belong to that DID, with cursor-based pagination.
pub fn batch_fetch_by_did_paginated(
  did: String,
  target_collection: String,
  pagination: PaginationParams,
  fetcher: PaginatedBatchFetcher,
) -> Result(PaginatedBatchResult, String) {
  // Fetch paginated records by DID
  fetcher(did, target_collection, None, pagination)
}

/// Group URIs by their collection for batching
fn group_uris_by_collection(uris: List(String)) -> List(#(String, List(String))) {
  // Group URIs by collection
  let grouped =
    list.fold(uris, dict.new(), fn(acc, uri) {
      case uri_to_collection(uri) {
        Some(collection) -> {
          let existing = dict.get(acc, collection) |> result.unwrap([])
          dict.insert(acc, collection, [uri, ..existing])
        }
        None -> acc
      }
    })

  dict.to_list(grouped)
}

/// Extract URIs from a list of records based on field metadata
///
/// This is used to collect URIs from parent records that need to be resolved.
/// For example, extracting all "subject" URIs from a list of Like records.
pub fn extract_uris_from_records(
  records: List(value.Value),
  field_name: String,
  _meta: collection_meta.CollectionMeta,
) -> List(String) {
  list.filter_map(records, fn(record) {
    // Extract the field value from the record
    case extract_field_value(record, field_name) {
      Some(field_value) -> {
        // Use uri_extractor to get the URI (handles both strongRef and at-uri)
        case uri_extractor.extract_uri(field_value) {
          Some(uri) -> Ok(uri)
          None -> Error(Nil)
        }
      }
      None -> Error(Nil)
    }
  })
}

/// Extract a field value from a GraphQL Value
fn extract_field_value(
  value: value.Value,
  field_name: String,
) -> Option(Dynamic) {
  case value {
    value.Object(fields) -> {
      // fields is a List(#(String, value.Value)), find the matching field
      list.find(fields, fn(pair) { pair.0 == field_name })
      |> result.map(fn(pair) { value_to_dynamic(pair.1) })
      |> option.from_result
    }
    _ -> None
  }
}

/// Convert a GraphQL Value to Dynamic for uri_extractor
/// Properly extracts the underlying value from GraphQL Value types
fn value_to_dynamic(v: value.Value) -> Dynamic {
  case v {
    value.String(s) -> unsafe_coerce_to_dynamic(s)
    value.Int(i) -> unsafe_coerce_to_dynamic(i)
    value.Float(f) -> unsafe_coerce_to_dynamic(f)
    value.Boolean(b) -> unsafe_coerce_to_dynamic(b)
    value.Null -> unsafe_coerce_to_dynamic(None)
    value.Object(fields) -> {
      // Convert object fields to a format uri_extractor can work with
      // Create a dict-like structure for the object
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
