import gleeunit/should
import lib/oauth/scopes/parse_error

pub fn invalid_scope_format_to_string_test() {
  parse_error.InvalidScopeFormat("repo:", "missing collection")
  |> parse_error.to_string
  |> should.equal("Invalid scope format: 'repo:' - missing collection")
}

pub fn invalid_action_to_string_test() {
  parse_error.InvalidAction("foo")
  |> parse_error.to_string
  |> should.equal(
    "Unknown action 'foo', expected: create, update, delete, read, manage",
  )
}

pub fn invalid_rpc_scope_to_string_test() {
  parse_error.InvalidRpcScope("wildcard method requires specific audience")
  |> parse_error.to_string
  |> should.equal(
    "Invalid RPC scope: wildcard method requires specific audience",
  )
}
