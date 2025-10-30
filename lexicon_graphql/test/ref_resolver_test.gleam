/// Tests for Lexicon Reference Resolver
///
/// Resolves ref types in lexicon definitions to their actual types
import gleeunit/should
import lexicon_graphql/ref_resolver
import lexicon_graphql/types

// Test resolving a local reference (within same lexicon)
pub fn resolve_local_ref_test() {
  // Lexicon with a main record that references another type in the same lexicon
  let lexicon =
    types.Lexicon(
      id: "xyz.statusphere.post",
      defs: types.Defs(
        main: types.RecordDef(type_: "record", properties: [
          #("text", types.Property("string", True)),
          #("embed", types.Property("ref", False)),
        ]),
      ),
    )

  // Reference should resolve to a type within the same lexicon
  let result = ref_resolver.resolve_ref("xyz.statusphere.post#embed", [lexicon])

  should.be_ok(result)
}

// Test resolving an external reference (different lexicon)
pub fn resolve_external_ref_test() {
  let post_lexicon =
    types.Lexicon(
      id: "xyz.statusphere.post",
      defs: types.Defs(
        main: types.RecordDef(type_: "record", properties: [
          #("text", types.Property("string", True)),
          #("author", types.Property("ref", False)),
        ]),
      ),
    )

  let profile_lexicon =
    types.Lexicon(
      id: "xyz.statusphere.profile",
      defs: types.Defs(
        main: types.RecordDef(type_: "record", properties: [
          #("displayName", types.Property("string", True)),
        ]),
      ),
    )

  // Reference to different lexicon
  let result =
    ref_resolver.resolve_ref("xyz.statusphere.profile", [
      post_lexicon,
      profile_lexicon,
    ])

  should.be_ok(result)
}

// Test error when reference not found
pub fn resolve_nonexistent_ref_test() {
  let lexicon =
    types.Lexicon(
      id: "xyz.statusphere.post",
      defs: types.Defs(
        main: types.RecordDef(type_: "record", properties: [
          #("text", types.Property("string", True)),
        ]),
      ),
    )

  // Try to resolve a reference that doesn't exist
  let result =
    ref_resolver.resolve_ref("xyz.statusphere.nonexistent", [lexicon])

  should.be_error(result)
}

// Test parsing ref URI format
pub fn parse_ref_uri_test() {
  // Test parsing full NSID
  ref_resolver.parse_ref_uri("xyz.statusphere.profile")
  |> should.equal(#("xyz.statusphere.profile", "main"))

  // Test parsing NSID with fragment
  ref_resolver.parse_ref_uri("xyz.statusphere.post#embed")
  |> should.equal(#("xyz.statusphere.post", "embed"))
}
