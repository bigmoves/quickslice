import atproto_auth.{type AtprotoSession}
import gleam/bit_array
import gleam/http.{type Method, Delete, Get, Head, Options, Patch, Post, Put}
import gleam/http/request
import gleam/http/response.{type Response}
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import jose_wrapper
import lib/http_client

/// Make an authenticated DPoP request to a PDS with nonce retry support
///
/// This function creates a DPoP proof token and makes an authenticated
/// request with the appropriate headers. If the server responds with
/// use_dpop_nonce error, it retries with the provided nonce.
pub fn make_dpop_request(
  method: String,
  url: String,
  session: AtprotoSession,
  body: String,
) -> Result(Response(String), String) {
  // First attempt without nonce
  case make_dpop_request_with_nonce(method, url, session, body, None) {
    Ok(resp) -> {
      // Check if response is 401 with use_dpop_nonce error
      case resp.status {
        401 -> {
          // Check if body contains use_dpop_nonce error
          case string.contains(resp.body, "use_dpop_nonce") {
            True -> {
              // Extract DPoP-Nonce header and retry
              case get_dpop_nonce_header(resp.headers) {
                Some(nonce) -> {
                  make_dpop_request_with_nonce(
                    method,
                    url,
                    session,
                    body,
                    Some(nonce),
                  )
                }
                None -> Ok(resp)
              }
            }
            False -> Ok(resp)
          }
        }
        _ -> Ok(resp)
      }
    }
    Error(err) -> Error(err)
  }
}

/// Make DPoP request with optional nonce
fn make_dpop_request_with_nonce(
  method: String,
  url: String,
  session: AtprotoSession,
  body: String,
  nonce: option.Option(String),
) -> Result(Response(String), String) {
  // Generate DPoP proof token with optional nonce
  case
    jose_wrapper.generate_dpop_proof_with_nonce(
      method,
      url,
      session.access_token,
      session.dpop_jwk,
      nonce,
    )
  {
    Error(err) ->
      Error("Failed to generate DPoP proof: " <> string.inspect(err))
    Ok(dpop_proof) -> {
      // Create the HTTP request with DPoP headers
      case request.to(url) {
        Error(_) -> Error("Failed to create request")
        Ok(req) -> {
          let req =
            req
            |> request.set_method(parse_method(method))
            |> request.set_header(
              "authorization",
              "DPoP " <> session.access_token,
            )
            |> request.set_header("dpop", dpop_proof)
            |> request.set_header("content-type", "application/json")
            |> request.set_body(body)

          case http_client.send(req) {
            Error(_) -> Error("Request failed")
            Ok(resp) -> Ok(resp)
          }
        }
      }
    }
  }
}

/// Extract DPoP-Nonce header from response headers
fn get_dpop_nonce_header(
  headers: List(#(String, String)),
) -> option.Option(String) {
  case
    list.find(headers, fn(header) { string.lowercase(header.0) == "dpop-nonce" })
  {
    Ok(#(_, nonce)) -> Some(nonce)
    Error(_) -> None
  }
}

/// Make an authenticated DPoP request with binary body
pub fn make_dpop_request_with_binary(
  method: String,
  url: String,
  session: AtprotoSession,
  body: BitArray,
  content_type: String,
) -> Result(Response(String), String) {
  // First attempt without nonce
  case
    make_dpop_request_with_binary_and_nonce(
      method,
      url,
      session,
      body,
      content_type,
      None,
    )
  {
    Ok(resp) -> {
      // Check if response is 401 with use_dpop_nonce error
      case resp.status {
        401 -> {
          // Check if body contains use_dpop_nonce error
          case string.contains(resp.body, "use_dpop_nonce") {
            True -> {
              // Extract DPoP-Nonce header and retry
              case get_dpop_nonce_header(resp.headers) {
                Some(nonce) -> {
                  make_dpop_request_with_binary_and_nonce(
                    method,
                    url,
                    session,
                    body,
                    content_type,
                    Some(nonce),
                  )
                }
                None -> Ok(resp)
              }
            }
            False -> Ok(resp)
          }
        }
        _ -> Ok(resp)
      }
    }
    Error(err) -> Error(err)
  }
}

/// Make DPoP request with binary body and optional nonce
fn make_dpop_request_with_binary_and_nonce(
  method: String,
  url: String,
  session: AtprotoSession,
  body: BitArray,
  content_type: String,
  nonce: option.Option(String),
) -> Result(Response(String), String) {
  // Generate DPoP proof token with optional nonce
  case
    jose_wrapper.generate_dpop_proof_with_nonce(
      method,
      url,
      session.access_token,
      session.dpop_jwk,
      nonce,
    )
  {
    Error(err) ->
      Error("Failed to generate DPoP proof: " <> string.inspect(err))
    Ok(dpop_proof) -> {
      // Create the HTTP request with DPoP headers
      case request.to(url) {
        Error(_) -> Error("Failed to create request")
        Ok(req) -> {
          let req =
            req
            |> request.set_method(parse_method(method))
            |> request.set_header(
              "authorization",
              "DPoP " <> session.access_token,
            )
            |> request.set_header("dpop", dpop_proof)
            |> request.set_header("content-type", content_type)
            |> request.set_body(body)

          case http_client.send_bits(req) {
            Error(_) -> Error("Request failed")
            Ok(resp) -> {
              // Convert BitArray response body to String
              case bit_array.to_string(resp.body) {
                Ok(body_string) ->
                  Ok(response.Response(..resp, body: body_string))
                Error(_) -> Error("Failed to decode response body")
              }
            }
          }
        }
      }
    }
  }
}

/// Helper to parse HTTP method string
fn parse_method(method: String) -> Method {
  case string.uppercase(method) {
    "GET" -> Get
    "POST" -> Post
    "PUT" -> Put
    "DELETE" -> Delete
    "PATCH" -> Patch
    "HEAD" -> Head
    "OPTIONS" -> Options
    _ -> Post
  }
}
