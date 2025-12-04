/// Tests for Lexicon Registry utilities
import gleeunit/should
import lexicon_graphql/internal/lexicon/registry

pub fn lexicon_id_from_ref_with_fragment_test() {
  // Ref with fragment should return the lexicon ID part
  registry.lexicon_id_from_ref("app.bsky.embed.images#image")
  |> should.equal("app.bsky.embed.images")
}

pub fn lexicon_id_from_ref_without_fragment_test() {
  // Ref without fragment should return as-is
  registry.lexicon_id_from_ref("app.bsky.embed.images")
  |> should.equal("app.bsky.embed.images")
}

pub fn lexicon_id_from_ref_with_defs_fragment_test() {
  // Standard defs ref pattern
  registry.lexicon_id_from_ref("social.grain.defs#aspectRatio")
  |> should.equal("social.grain.defs")
}

pub fn parse_ref_with_fragment_test() {
  // Parse ref should split into lexicon ID and def name
  registry.parse_ref("app.bsky.embed.images#image")
  |> should.be_some
  |> should.equal(#("app.bsky.embed.images", "image"))
}

pub fn parse_ref_without_fragment_test() {
  // Ref without fragment should return None
  registry.parse_ref("app.bsky.embed.images")
  |> should.be_none
}
