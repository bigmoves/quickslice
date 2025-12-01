import gleeunit/should
import handlers/oauth/metadata

pub fn metadata_uses_provided_scopes_test() {
  let scopes = ["atproto", "repo:*", "account:email"]
  let meta = metadata.generate_metadata("https://example.com", scopes)

  meta.scopes_supported |> should.equal(scopes)
}

pub fn metadata_default_scopes_work_test() {
  let scopes = ["atproto", "transition:generic"]
  let meta = metadata.generate_metadata("https://example.com", scopes)

  meta.scopes_supported |> should.equal(["atproto", "transition:generic"])
}
