/// OAuth request validation utilities
import gleam/list
import gleam/option.{type Option}
import gleam/string
import gleam/uri
import lib/oauth/types/error.{type OAuthError, InvalidRequest, InvalidScope}

/// Validate redirect URI format
pub fn validate_redirect_uri(uri_string: String) -> Result(Nil, OAuthError) {
  case uri.parse(uri_string) {
    Ok(parsed) -> {
      case parsed.scheme {
        option.Some(scheme) -> {
          case scheme {
            "https" -> Ok(Nil)
            "http" -> {
              case parsed.host {
                option.Some("localhost") -> Ok(Nil)
                option.Some("127.0.0.1") -> Ok(Nil)
                option.Some("[::1]") -> Ok(Nil)
                _ ->
                  Error(InvalidRequest(
                    "HTTP redirect URIs only allowed for localhost",
                  ))
              }
            }
            _ -> {
              case parsed.fragment {
                option.None -> Ok(Nil)
                option.Some(_) ->
                  Error(InvalidRequest("Redirect URI must not contain fragment"))
              }
            }
          }
        }
        option.None -> Error(InvalidRequest("Redirect URI must have a scheme"))
      }
    }
    Error(_) -> Error(InvalidRequest("Invalid redirect URI format"))
  }
}

/// Check if redirect URI matches registered URI
pub fn matches_redirect_uri(
  requested: String,
  registered: String,
  require_exact: Bool,
) -> Bool {
  case require_exact {
    True -> requested == registered
    False -> string.starts_with(requested, registered)
  }
}

/// Validate that redirect URI matches one of the registered URIs
pub fn validate_redirect_uri_match(
  requested: String,
  registered_uris: List(String),
  require_exact: Bool,
) -> Result(Nil, OAuthError) {
  let matches =
    list.any(registered_uris, fn(registered) {
      matches_redirect_uri(requested, registered, require_exact)
    })

  case matches {
    True -> Ok(Nil)
    False ->
      Error(InvalidRequest("Redirect URI does not match any registered URIs"))
  }
}

/// Validate PKCE code challenge method
pub fn validate_code_challenge_method(method: String) -> Result(Nil, OAuthError) {
  case method {
    "S256" -> Ok(Nil)
    "plain" ->
      Error(InvalidRequest("PKCE method 'plain' is not allowed, use S256"))
    _ -> Error(InvalidRequest("Invalid code_challenge_method: " <> method))
  }
}

/// Validate scope format and allowed scopes
pub fn validate_scope(
  requested: Option(String),
  allowed: Option(String),
) -> Result(Nil, OAuthError) {
  case requested {
    option.None -> Ok(Nil)
    option.Some(req) -> {
      case string.is_empty(req) {
        True -> Error(InvalidScope("Scope cannot be empty string"))
        False -> {
          case allowed {
            option.None ->
              Error(InvalidScope("No scopes allowed for this client"))
            option.Some(allow) -> {
              let req_scopes = string.split(req, " ")
              let allow_scopes = string.split(allow, " ")

              let all_allowed =
                list.all(req_scopes, fn(scope) {
                  list.contains(allow_scopes, scope)
                })

              case all_allowed {
                True -> Ok(Nil)
                False -> Error(InvalidScope("Requested scope not allowed"))
              }
            }
          }
        }
      }
    }
  }
}
