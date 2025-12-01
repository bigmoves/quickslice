/// OAuth scope validation
import gleam/list
import gleam/result
import gleam/string
import lib/oauth/scopes/parse_error
import lib/oauth/scopes/parser
import lib/oauth/scopes/types.{type Scope}
import lib/oauth/types/error.{type OAuthError, InvalidScope}

/// Validate scope string format and parse into structured scopes
pub fn validate_scope_format(
  scope_string: String,
) -> Result(List(Scope), OAuthError) {
  parser.parse_scopes(scope_string)
  |> result.map_error(fn(e) { InvalidScope(parse_error.to_string(e)) })
}

/// Validate that all requested scopes are in the supported scopes list
pub fn validate_scopes_supported(
  requested: String,
  supported: List(String),
) -> Result(List(Scope), OAuthError) {
  // First validate format
  use parsed <- result.try(validate_scope_format(requested))

  // Extract requested scope tokens
  let requested_tokens =
    requested
    |> string.split(" ")
    |> list.map(string.trim)
    |> list.filter(fn(s) { !string.is_empty(s) })

  // Find any unsupported scope
  let unsupported =
    list.find(requested_tokens, fn(token) { !list.contains(supported, token) })

  case unsupported {
    Ok(scope) -> Error(InvalidScope("Unsupported scope: " <> scope))
    Error(Nil) -> Ok(parsed)
  }
}
