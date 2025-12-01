import gleeunit/should
import lib/oauth/scopes/validator

pub fn validate_scopes_supported_all_in_list_test() {
  let supported = ["atproto", "transition:generic", "repo:*"]
  validator.validate_scopes_supported("atproto repo:*", supported)
  |> should.be_ok
}

pub fn validate_scopes_supported_unsupported_scope_test() {
  let supported = ["atproto", "transition:generic"]
  validator.validate_scopes_supported("atproto account:email", supported)
  |> should.be_error
}

pub fn validate_scopes_supported_empty_request_test() {
  let supported = ["atproto", "transition:generic"]
  validator.validate_scopes_supported("", supported)
  |> should.be_ok
}

pub fn validate_scopes_supported_invalid_format_test() {
  let supported = ["atproto", "transition:generic"]
  validator.validate_scopes_supported("invalid:::", supported)
  |> should.be_error
}
