/// Tests for NSID (Namespaced Identifier) utilities
///
/// NSIDs are used in AT Protocol to identify lexicons and collections
import gleeunit/should
import lexicon_graphql/internal/lexicon/nsid as nsid

pub fn nsid_to_type_name_test() {
  nsid.to_type_name("xyz.statusphere.status")
  |> should.equal("XyzStatusphereStatus")

  nsid.to_type_name("app.bsky.feed.post")
  |> should.equal("AppBskyFeedPost")

  nsid.to_type_name("com.atproto.repo.createRecord")
  |> should.equal("ComAtprotoRepoCreateRecord")
}

pub fn nsid_to_field_name_test() {
  nsid.to_field_name("xyz.statusphere.status")
  |> should.equal("xyzStatusphereStatus")

  nsid.to_field_name("app.bsky.feed.post")
  |> should.equal("appBskyFeedPost")

  nsid.to_field_name("com.atproto.repo.createRecord")
  |> should.equal("comAtprotoRepoCreateRecord")
}

pub fn nsid_to_collection_name_test() {
  // Collection names are just the last part of the NSID
  nsid.to_collection_name("xyz.statusphere.status")
  |> should.equal("status")

  nsid.to_collection_name("app.bsky.feed.post")
  |> should.equal("post")

  nsid.to_collection_name("com.atproto.repo.createRecord")
  |> should.equal("createRecord")
}
