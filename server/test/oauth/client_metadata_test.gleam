import gleam/option.{None}
import gleeunit/should
import handlers/oauth/client_metadata

pub fn generate_metadata_test() {
  let metadata =
    client_metadata.generate_metadata(
      "https://example.com/oauth-client-metadata.json",
      "Test Client",
      ["https://example.com/callback"],
      "atproto openid profile",
      None,
      None,
    )

  metadata.client_id
  |> should.equal("https://example.com/oauth-client-metadata.json")
  metadata.client_name |> should.equal("Test Client")
  metadata.scope |> should.equal("atproto")
  metadata.dpop_bound_access_tokens |> should.be_true
}

pub fn filter_atproto_scopes_test() {
  let metadata =
    client_metadata.generate_metadata(
      "https://example.com",
      "Test",
      [],
      "atproto transition:generic openid profile email",
      None,
      None,
    )

  // Should only keep atproto and transition:generic
  metadata.scope |> should.equal("atproto transition:generic")
}
