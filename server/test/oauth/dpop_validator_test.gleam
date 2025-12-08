import gleam/option
import gleeunit/should
import lib/oauth/dpop/validator

// Note: Full DPoP proof verification tests require real P-256 keys which are
// tested through integration tests. These unit tests focus on the header
// extraction logic which is the core of this module.

pub fn get_dpop_header_test() {
  let headers = [
    #("content-type", "application/json"),
    #("dpop", "eyJhbGciOiJFUzI1NiJ9.test"),
    #("authorization", "Bearer token"),
  ]

  let result = validator.get_dpop_header(headers)
  should.equal(result, option.Some("eyJhbGciOiJFUzI1NiJ9.test"))
}

pub fn get_dpop_header_case_insensitive_test() {
  let headers = [
    #("content-type", "application/json"),
    #("DPoP", "eyJhbGciOiJFUzI1NiJ9.test"),
  ]

  let result = validator.get_dpop_header(headers)
  should.equal(result, option.Some("eyJhbGciOiJFUzI1NiJ9.test"))
}

pub fn get_dpop_header_missing_test() {
  let headers = [
    #("content-type", "application/json"),
    #("authorization", "Bearer token"),
  ]

  let result = validator.get_dpop_header(headers)
  should.equal(result, option.None)
}
