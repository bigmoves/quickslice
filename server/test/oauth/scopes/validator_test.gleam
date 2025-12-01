import gleeunit/should
import lib/oauth/scopes/validator

pub fn validate_scope_format_valid_test() {
  validator.validate_scope_format("atproto repo:* account:email")
  |> should.be_ok
}

pub fn validate_scope_format_invalid_test() {
  validator.validate_scope_format("atproto invalid:::")
  |> should.be_error
}

pub fn validate_scope_format_empty_test() {
  validator.validate_scope_format("")
  |> should.be_ok
}
