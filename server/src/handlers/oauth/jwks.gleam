/// JWKS (JSON Web Key Set) endpoint handler
/// GET /.well-known/jwks.json
import gleam/json
import gleam/option.{type Option, None, Some}
import wisp

/// Empty JWKS response (no keys configured)
fn empty_jwks() -> json.Json {
  json.object([#("keys", json.array([], fn(_) { json.null() }))])
}

/// Handle GET /.well-known/jwks.json
/// Returns the server's public signing keys
pub fn handle(signing_key: Option(String)) -> wisp.Response {
  let json_response = case signing_key {
    None -> empty_jwks()
    Some(private_key_multibase) -> {
      // Derive public key from private key and build JWKS
      case derive_public_key_jwks(private_key_multibase) {
        Ok(jwks) -> jwks
        Error(_) -> empty_jwks()
      }
    }
  }

  wisp.response(200)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text(json.to_string(json_response)))
}

/// Derive JWKS from private key multibase string
fn derive_public_key_jwks(private_key: String) -> Result(json.Json, Nil) {
  use public_did_key <- result_try(erlang_derive_public_did_key(private_key))
  use #(x, y) <- result_try(erlang_extract_public_key_coords(private_key))

  Ok(
    json.object([
      #(
        "keys",
        json.array(
          [
            json.object([
              #("kid", json.string(public_did_key)),
              #("kty", json.string("EC")),
              #("crv", json.string("P-256")),
              #("x", json.string(x)),
              #("y", json.string(y)),
              #("use", json.string("sig")),
              #("alg", json.string("ES256")),
            ]),
          ],
          fn(k) { k },
        ),
      ),
    ]),
  )
}

fn result_try(
  result: Result(a, e),
  next: fn(a) -> Result(b, Nil),
) -> Result(b, Nil) {
  case result {
    Ok(a) -> next(a)
    Error(_) -> Error(Nil)
  }
}

/// Erlang FFI: Derive public did:key from private key
@external(erlang, "jwt_ffi", "derive_public_did_key")
fn erlang_derive_public_did_key(private_key: String) -> Result(String, Nil)

/// Erlang FFI: Extract public key coordinates from private key
@external(erlang, "jwt_ffi", "extract_public_key_coords")
fn erlang_extract_public_key_coords(
  private_key: String,
) -> Result(#(String, String), Nil)
