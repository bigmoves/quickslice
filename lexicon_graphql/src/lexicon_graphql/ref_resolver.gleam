/// Lexicon Reference Resolver
///
/// Resolves ref types in lexicon definitions to their actual types.
/// Handles both local references (within same lexicon) and external references.
///
/// Reference URI format:
/// - "nsid" - references the main definition of that NSID
/// - "nsid#fragment" - references a specific definition within that NSID
import gleam/list
import gleam/string
import lexicon_graphql/types

/// Parse a reference URI into NSID and definition name
///
/// ## Examples
///
/// ```gleam
/// parse_ref_uri("xyz.statusphere.profile")
/// // #("xyz.statusphere.profile", "main")
///
/// parse_ref_uri("xyz.statusphere.post#embed")
/// // #("xyz.statusphere.post", "embed")
/// ```
pub fn parse_ref_uri(ref_uri: String) -> #(String, String) {
  case string.split(ref_uri, "#") {
    [nsid] -> #(nsid, "main")
    [nsid, fragment] -> #(nsid, fragment)
    _ -> #(ref_uri, "main")
  }
}

/// Resolves a reference URI to the actual lexicon definition
///
/// Returns the NSID of the referenced type if found, Error if not found
pub fn resolve_ref(
  ref_uri: String,
  lexicons: List(types.Lexicon),
) -> Result(String, String) {
  let #(nsid, _fragment) = parse_ref_uri(ref_uri)

  // Look for the lexicon with this NSID
  case find_lexicon(nsid, lexicons) {
    Ok(_lexicon) -> Ok(nsid)
    Error(Nil) -> Error("Reference not found: " <> ref_uri)
  }
}

/// Find a lexicon by its NSID
fn find_lexicon(
  nsid: String,
  lexicons: List(types.Lexicon),
) -> Result(types.Lexicon, Nil) {
  lexicons
  |> list.find(fn(lex) { lex.id == nsid })
}
