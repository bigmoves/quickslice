import gleam/option.{None}
import gleeunit/should
import handlers/oauth/client_metadata

pub fn generate_metadata_test() {
  let metadata =
    client_metadata.generate_metadata(
      "https://example.com/oauth-client-metadata.json",
      "Test Client",
      ["https://example.com/callback"],
      "atproto transition:generic",
      None,
      None,
    )

  metadata.client_id
  |> should.equal("https://example.com/oauth-client-metadata.json")
  metadata.client_name |> should.equal("Test Client")
  metadata.scope |> should.equal("atproto transition:generic")
  metadata.dpop_bound_access_tokens |> should.be_true
}

pub fn scopes_passed_through_test() {
  let metadata =
    client_metadata.generate_metadata(
      "https://example.com",
      "Test",
      [],
      "atproto transition:generic transition:chat.bsky",
      None,
      None,
    )

  // Scopes should be passed through as-is (validated at config save time)
  metadata.scope
  |> should.equal("atproto transition:generic transition:chat.bsky")
}
