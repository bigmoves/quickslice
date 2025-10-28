import gleam/dynamic/decode
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/option.{None, Some}
import gleam/result
import gleam/string

/// UserInfo response from OAuth provider
pub type UserInfo {
  UserInfo(sub: String, did: String)
}

/// ATProto session data from AIP
pub type AtprotoSession {
  AtprotoSession(pds_endpoint: String, access_token: String, dpop_jwk: String)
}

/// Error type for authentication operations
pub type AuthError {
  MissingAuthHeader
  InvalidAuthHeader
  UnauthorizedToken
  NetworkError
  ParseError
}

/// Extract bearer token from Authorization header
///
/// # Example
/// ```gleam
/// extract_bearer_token(request.headers)
/// // Ok("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...")
/// ```
pub fn extract_bearer_token(
  from headers: List(#(String, String)),
) -> Result(String, AuthError) {
  headers
  |> list_find(one_that: fn(header) {
    string.lowercase(header.0) == "authorization"
  })
  |> result.map(fn(header) { header.1 })
  |> result.replace_error(MissingAuthHeader)
  |> result.try(fn(auth_value) {
    case string.starts_with(auth_value, "Bearer ") {
      True -> {
        auth_value
        |> string.drop_start(7)
        |> Ok
      }
      False -> Error(InvalidAuthHeader)
    }
  })
}

/// Helper function to find in list
fn list_find(
  in list: List(a),
  one_that predicate: fn(a) -> Bool,
) -> Result(a, Nil) {
  case list {
    [] -> Error(Nil)
    [first, ..rest] ->
      case predicate(first) {
        True -> Ok(first)
        False -> list_find(rest, predicate)
      }
  }
}

/// Verify OAuth token with authorization server
///
/// Makes a request to the `/oauth/userinfo` endpoint to validate the token
/// and retrieve user information.
pub fn verify_oauth_token(
  token: String,
  auth_base_url: String,
) -> Result(UserInfo, AuthError) {
  let url = auth_base_url <> "/oauth/userinfo"

  case request.to(url) {
    Error(_) -> Error(NetworkError)
    Ok(req) -> {
      let req =
        req
        |> request.set_header("authorization", "Bearer " <> token)

      case httpc.send(req) {
        Error(_) -> Error(NetworkError)
        Ok(resp) -> {
          case resp.status {
            200 -> parse_userinfo(resp.body)
            _ -> Error(UnauthorizedToken)
          }
        }
      }
    }
  }
}

/// Parse userinfo response JSON
fn parse_userinfo(body: String) -> Result(UserInfo, AuthError) {
  let decoder = {
    use sub <- decode.field("sub", decode.string)
    use did_opt <- decode.field("did", decode.optional(decode.string))

    let did = case did_opt {
      Some(d) -> d
      None -> ""
    }

    decode.success(UserInfo(sub: sub, did: did))
  }

  body
  |> json.parse(decoder)
  |> result.replace_error(ParseError)
}

/// Get ATProto session from authorization server
///
/// Makes a request to the `/api/atprotocol/session` endpoint to retrieve
/// the user's PDS endpoint, access token, and DPoP JWK.
pub fn get_atproto_session(
  token: String,
  auth_base_url: String,
) -> Result(AtprotoSession, AuthError) {
  let url = auth_base_url <> "/api/atprotocol/session"

  case request.to(url) {
    Error(_) -> Error(NetworkError)
    Ok(req) -> {
      let req =
        req
        |> request.set_header("authorization", "Bearer " <> token)

      case httpc.send(req) {
        Error(_) -> Error(NetworkError)
        Ok(resp) -> {
          case resp.status {
            200 -> parse_session(resp.body)
            _ -> Error(UnauthorizedToken)
          }
        }
      }
    }
  }
}

/// Parse ATProto session response JSON
fn parse_session(body: String) -> Result(AtprotoSession, AuthError) {
  // Extract dpop_jwk field from the JSON response
  // It should be a JSON object, so we need to extract it and re-stringify it
  let dpop_jwk_json = case extract_dpop_jwk(body) {
    Ok(jwk) -> jwk
    Error(_) -> "{}"
  }

  let decoder = {
    use pds_endpoint <- decode.field("pds_endpoint", decode.string)
    use access_token <- decode.field("access_token", decode.string)

    decode.success(AtprotoSession(
      pds_endpoint: pds_endpoint,
      access_token: access_token,
      dpop_jwk: dpop_jwk_json,
    ))
  }

  body
  |> json.parse(decoder)
  |> result.replace_error(ParseError)
}

/// Extract the dpop_jwk field from the session response and convert it to a JSON string
fn extract_dpop_jwk(body: String) -> Result(String, Nil) {
  // Find the dpop_jwk field in the JSON
  case string.split(body, "\"dpop_jwk\":") {
    [_, rest] -> {
      // Find the matching closing brace for the dpop_jwk object
      case find_json_object(rest) {
        Ok(jwk_json) -> Ok(jwk_json)
        Error(_) -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

/// Extract a JSON object from a string starting after the opening brace
fn find_json_object(str: String) -> Result(String, Nil) {
  // Skip whitespace and find the opening brace
  let trimmed = string.trim_start(str)
  case string.starts_with(trimmed, "{") {
    False -> Error(Nil)
    True -> {
      // Count braces to find the matching closing brace
      let chars = string.to_graphemes(trimmed)
      case find_matching_brace(chars, 0, 0, "") {
        Ok(json) -> Ok(json)
        Error(_) -> Error(Nil)
      }
    }
  }
}

/// Find the matching closing brace and extract the JSON object
fn find_matching_brace(
  chars: List(String),
  depth: Int,
  pos: Int,
  acc: String,
) -> Result(String, Nil) {
  case chars {
    [] -> Error(Nil)
    [char, ..rest] -> {
      let new_acc = acc <> char
      let new_depth = case char {
        "{" -> depth + 1
        "}" -> depth - 1
        _ -> depth
      }

      case new_depth {
        0 if pos > 0 -> Ok(new_acc)
        _ -> find_matching_brace(rest, new_depth, pos + 1, new_acc)
      }
    }
  }
}
