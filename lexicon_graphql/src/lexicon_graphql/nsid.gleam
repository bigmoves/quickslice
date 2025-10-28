/// NSID (Namespaced Identifier) utilities
///
/// NSIDs are used throughout AT Protocol to identify lexicons, collections,
/// and other namespaced resources. They follow the format: "domain.name.thing"
///
/// This module provides utilities for converting NSIDs to GraphQL-friendly names.
import gleam/list
import gleam/result
import gleam/string

/// Converts an NSID to a GraphQL type name (PascalCase).
///
/// ## Examples
///
/// ```gleam
/// to_type_name("xyz.statusphere.status")  // "XyzStatusphereStatus"
/// to_type_name("app.bsky.feed.post")     // "AppBskyFeedPost"
/// ```
pub fn to_type_name(nsid: String) -> String {
  nsid
  |> string.split(".")
  |> list.map(capitalize_first)
  |> string.join("")
}

/// Converts an NSID to a GraphQL field name (camelCase).
///
/// ## Examples
///
/// ```gleam
/// to_field_name("xyz.statusphere.status")  // "xyzStatusphereStatus"
/// to_field_name("app.bsky.feed.post")      // "appBskyFeedPost"
/// ```
pub fn to_field_name(nsid: String) -> String {
  case string.split(nsid, ".") {
    [] -> nsid
    [first, ..rest] -> {
      let capitalized_rest = list.map(rest, capitalize_first)
      string.join([first, ..capitalized_rest], "")
    }
  }
}

/// Extracts the collection name from an NSID (last segment).
///
/// ## Examples
///
/// ```gleam
/// to_collection_name("xyz.statusphere.status")  // "status"
/// to_collection_name("app.bsky.feed.post")      // "post"
/// ```
pub fn to_collection_name(nsid: String) -> String {
  nsid
  |> string.split(".")
  |> list.last
  |> result.unwrap("")
}

/// Capitalizes the first letter of a string.
fn capitalize_first(s: String) -> String {
  case string.pop_grapheme(s) {
    Ok(#(first, rest)) -> string.uppercase(first) <> rest
    Error(_) -> s
  }
}
