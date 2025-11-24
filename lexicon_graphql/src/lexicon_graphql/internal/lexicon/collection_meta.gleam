/// Collection Metadata Extraction
///
/// Extracts metadata from lexicons to identify fields that can be used for joins.
/// This enables dynamic forward and reverse join field generation.
import gleam/list
import gleam/option.{type Option, None, Some}
import lexicon_graphql/internal/lexicon/nsid
import lexicon_graphql/types

/// Metadata about a collection extracted from its lexicon
pub type CollectionMeta {
  CollectionMeta(
    /// The NSID of the collection (e.g., "app.bsky.feed.post")
    nsid: String,
    /// The GraphQL type name (e.g., "AppBskyFeedPost")
    type_name: String,
    /// Record key type: "tid", "literal:self", or "any"
    key_type: String,
    /// Whether this collection has only one record per DID (e.g., profiles)
    has_unique_did: Bool,
    /// Fields that can be used for forward joins (following references)
    forward_join_fields: List(ForwardJoinField),
    /// Fields that can be used for reverse joins (fields with at-uri format)
    reverse_join_fields: List(String),
  )
}

/// A field that can be used for forward joins
pub type ForwardJoinField {
  /// A strongRef field - references a specific version of a record (URI + CID)
  StrongRefField(name: String)
  /// An at-uri field - references the latest version of a record (URI only)
  AtUriField(name: String)
}

/// Extract metadata from a lexicon
pub fn extract_metadata(lexicon: types.Lexicon) -> CollectionMeta {
  let type_name = nsid.to_type_name(lexicon.id)

  case lexicon.defs.main {
    Some(main_def) -> {
      // Extract key type from lexicon (default to "tid" if not specified)
      let key_type = case main_def.key {
        Some(k) -> k
        None -> "tid"
      }

      // Determine if this collection has only one record per DID
      // Collections with key="literal:self" have a unique record per DID (e.g., profiles)
      let has_unique_did = key_type == "literal:self"

      let #(forward_fields, reverse_fields) =
        scan_properties(main_def.properties)

      CollectionMeta(
        nsid: lexicon.id,
        type_name: type_name,
        key_type: key_type,
        has_unique_did: has_unique_did,
        forward_join_fields: forward_fields,
        reverse_join_fields: reverse_fields,
      )
    }
    None -> {
      // No main definition, return empty metadata
      CollectionMeta(
        nsid: lexicon.id,
        type_name: type_name,
        key_type: "tid",
        has_unique_did: False,
        forward_join_fields: [],
        reverse_join_fields: [],
      )
    }
  }
}

/// Scan properties to identify forward and reverse join fields
fn scan_properties(
  properties: List(#(String, types.Property)),
) -> #(List(ForwardJoinField), List(String)) {
  list.fold(properties, #([], []), fn(acc, prop) {
    let #(forward_fields, reverse_fields) = acc
    let #(name, property) = prop

    // Check if this is a forward join field
    let new_forward = case is_forward_join_field(property) {
      Some(field_type) -> [field_type(name), ..forward_fields]
      None -> forward_fields
    }

    // Check if this is a reverse join field (at-uri format)
    let new_reverse = case is_reverse_join_field(property) {
      True -> [name, ..reverse_fields]
      False -> reverse_fields
    }

    #(new_forward, new_reverse)
  })
}

/// Check if a property is a forward join field
/// Returns Some(constructor) if it is, None otherwise
fn is_forward_join_field(
  property: types.Property,
) -> Option(fn(String) -> ForwardJoinField) {
  // Case 1: strongRef field
  case property.type_, property.ref {
    "ref", Some(ref_target) if ref_target == "com.atproto.repo.strongRef" ->
      Some(StrongRefField)
    _, _ ->
      // Case 2: at-uri string field
      case property.type_, property.format {
        "string", Some(fmt) if fmt == "at-uri" -> Some(AtUriField)
        _, _ -> None
      }
  }
}

/// Check if a property is a reverse join field (has at-uri format)
fn is_reverse_join_field(property: types.Property) -> Bool {
  case property.format {
    Some(fmt) if fmt == "at-uri" -> True
    _ -> False
  }
}

/// Get all forward join field names from metadata
pub fn get_forward_join_field_names(meta: CollectionMeta) -> List(String) {
  list.map(meta.forward_join_fields, fn(field) {
    case field {
      StrongRefField(name) -> name
      AtUriField(name) -> name
    }
  })
}

/// Check if a field is a strongRef field
pub fn is_strong_ref_field(meta: CollectionMeta, field_name: String) -> Bool {
  list.any(meta.forward_join_fields, fn(field) {
    case field {
      StrongRefField(name) if name == field_name -> True
      _ -> False
    }
  })
}

/// Check if a field is an at-uri field
pub fn is_at_uri_field(meta: CollectionMeta, field_name: String) -> Bool {
  list.any(meta.forward_join_fields, fn(field) {
    case field {
      AtUriField(name) if name == field_name -> True
      _ -> False
    }
  })
}
