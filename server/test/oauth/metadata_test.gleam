import gleeunit/should
import handlers/oauth/metadata

pub fn generate_metadata_test() {
  let meta = metadata.generate_metadata("https://example.com")

  meta.issuer |> should.equal("https://example.com")
  meta.authorization_endpoint
  |> should.equal("https://example.com/oauth/authorize")
  meta.token_endpoint |> should.equal("https://example.com/oauth/token")
  meta.jwks_uri |> should.equal("https://example.com/.well-known/jwks.json")
  meta.registration_endpoint
  |> should.equal("https://example.com/oauth/register")
  meta.pushed_authorization_request_endpoint
  |> should.equal("https://example.com/oauth/par")
}

pub fn encode_metadata_test() {
  let meta = metadata.generate_metadata("https://example.com")
  let json = metadata.encode_metadata(meta)

  // Verify it encodes without error (JSON encoding doesn't fail)
  let _ = json
  Nil
}
