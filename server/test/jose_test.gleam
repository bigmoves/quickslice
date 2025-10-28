import gleam/option
import gleeunit
import gleeunit/should
import jose_wrapper

pub fn main() {
  gleeunit.main()
}

pub fn generate_dpop_proof_test() {
  // Fake test JWK (not a real key - for testing only)
  let jwk_json =
    "{\"kid\":\"did:key:zFAKEKEYFORTESTINGONLY123456789\",\"alg\":\"ES256\",\"use\":\"sig\",\"kty\":\"EC\",\"crv\":\"P-256\",\"x\":\"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"y\":\"BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB\",\"d\":\"CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC\"}"

  let method = "POST"
  let url =
    "https://leccinum.us-west.host.bsky.network/xrpc/com.atproto.repo.createRecord"
  let access_token = "test_token_12345"

  case
    jose_wrapper.generate_dpop_proof_with_nonce(
      method,
      url,
      access_token,
      jwk_json,
      option.None,
    )
  {
    Ok(dpop_proof) -> {
      // DPoP proof should be a JWT (three parts separated by dots)
      should.not_equal(dpop_proof, "")

      // Should start with eyJ (base64 encoded JSON header)
      should.be_true(case dpop_proof {
        "eyJ" <> _ -> True
        _ -> False
      })
    }
    Error(_err) -> {
      should.fail()
    }
  }
}
