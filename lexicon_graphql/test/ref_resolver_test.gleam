/// Tests for Lexicon Reference Resolver
///
/// Resolves ref types in lexicon definitions to their actual types
import gleeunit/should
import lexicon_graphql/ref_resolver
import lexicon_graphql/schema_builder

// Test resolving a local reference (within same lexicon)
pub fn resolve_local_ref_test() {
  // Lexicon with a main record that references another type in the same lexicon
  let lexicon =
    schema_builder.Lexicon(
      id: "xyz.statusphere.post",
      defs: schema_builder.Defs(
        main: schema_builder.RecordDef(type_: "record", properties: [
          #("text", schema_builder.Property("string", True)),
          #("embed", schema_builder.Property("ref", False)),
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
    schema_builder.Lexicon(
      id: "xyz.statusphere.post",
      defs: schema_builder.Defs(
        main: schema_builder.RecordDef(type_: "record", properties: [
          #("text", schema_builder.Property("string", True)),
          #("author", schema_builder.Property("ref", False)),
        ]),
      ),
    )

  let profile_lexicon =
    schema_builder.Lexicon(
      id: "xyz.statusphere.profile",
      defs: schema_builder.Defs(
        main: schema_builder.RecordDef(type_: "record", properties: [
          #("displayName", schema_builder.Property("string", True)),
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
    schema_builder.Lexicon(
      id: "xyz.statusphere.post",
      defs: schema_builder.Defs(
        main: schema_builder.RecordDef(type_: "record", properties: [
          #("text", schema_builder.Property("string", True)),
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
