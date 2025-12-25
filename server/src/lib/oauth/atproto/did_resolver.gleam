/// DID resolution for ATProtocol
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/http/request
import gleam/json
import gleam/list
import gleam/option.{type Option, None}
import gleam/result
import gleam/string
import lib/http_client
import lib/oauth/atproto/types.{type ATProtoError}
import lib/oauth/did_cache

/// DID Document structure
pub type DIDDocument {
  DIDDocument(id: String, also_known_as: List(String), service: List(Service))
}

pub type Service {
  Service(id: String, service_type: String, service_endpoint: String)
}

/// Resolve a DID to its document, using cache
pub fn resolve_did_with_cache(
  cache: Subject(did_cache.Message),
  did: String,
  invalidate_first: Bool,
) -> Result(DIDDocument, ATProtoError) {
  // Invalidate cache if requested
  case invalidate_first {
    True -> did_cache.invalidate(cache, did)
    False -> Nil
  }

  // Try cache first
  case did_cache.get(cache, did) {
    Ok(doc_json) -> {
      case parse_did_document(doc_json) {
        Ok(doc) -> Ok(doc)
        Error(_) -> resolve_did_fresh(cache, did)
      }
    }
    Error(_) -> resolve_did_fresh(cache, did)
  }
}

/// Resolve DID without cache
fn resolve_did_fresh(
  cache: Subject(did_cache.Message),
  did: String,
) -> Result(DIDDocument, ATProtoError) {
  use doc <- result.try(resolve_did(did))

  // Cache the result
  let doc_json = encode_did_document(doc)
  did_cache.put(cache, did, doc_json, 3600)
  // 1 hour TTL

  Ok(doc)
}

/// Resolve a DID directly (no cache)
pub fn resolve_did(did: String) -> Result(DIDDocument, ATProtoError) {
  case string.starts_with(did, "did:plc:") {
    True -> resolve_plc_did(did)
    False -> {
      case string.starts_with(did, "did:web:") {
        True -> resolve_web_did(did)
        False -> Error(types.DIDResolutionFailed("Unsupported DID method"))
      }
    }
  }
}

/// Resolve a did:plc DID
fn resolve_plc_did(did: String) -> Result(DIDDocument, ATProtoError) {
  let url = "https://plc.directory/" <> did

  use req <- result.try(
    request.to(url)
    |> result.map_error(fn(_) { types.HTTPError("Invalid URL") }),
  )

  use resp <- result.try(
    http_client.send(req)
    |> result.map_error(fn(_) { types.HTTPError("Request failed") }),
  )

  case resp.status {
    200 -> parse_did_document(resp.body)
    404 -> Error(types.DIDResolutionFailed("DID not found"))
    status ->
      Error(types.HTTPError(
        "PLC resolution failed with status " <> string.inspect(status),
      ))
  }
}

/// Resolve a did:web DID
fn resolve_web_did(did: String) -> Result(DIDDocument, ATProtoError) {
  // Extract domain from did:web:domain
  let domain = string.drop_start(did, 8)
  let url = "https://" <> domain <> "/.well-known/did.json"

  use req <- result.try(
    request.to(url)
    |> result.map_error(fn(_) { types.HTTPError("Invalid URL") }),
  )

  use resp <- result.try(
    http_client.send(req)
    |> result.map_error(fn(_) { types.HTTPError("Request failed") }),
  )

  case resp.status {
    200 -> parse_did_document(resp.body)
    404 -> Error(types.DIDResolutionFailed("DID not found"))
    status ->
      Error(types.HTTPError(
        "Web DID resolution failed with status " <> string.inspect(status),
      ))
  }
}

/// Resolve a handle to a DID
pub fn resolve_handle_to_did(handle: String) -> Result(String, ATProtoError) {
  let url =
    "https://bsky.social/xrpc/com.atproto.identity.resolveHandle?handle="
    <> handle

  use req <- result.try(
    request.to(url)
    |> result.map_error(fn(_) { types.HTTPError("Invalid URL") }),
  )

  use resp <- result.try(
    http_client.send(req)
    |> result.map_error(fn(_) { types.HTTPError("Request failed") }),
  )

  case resp.status {
    200 -> {
      let decoder = decode.at(["did"], decode.string)
      case json.parse(resp.body, decoder) {
        Ok(did) -> Ok(did)
        Error(_) -> Error(types.DIDResolutionFailed("Invalid response format"))
      }
    }
    _ -> Error(types.DIDResolutionFailed("Handle resolution failed"))
  }
}

/// Get PDS endpoint from DID document
pub fn get_pds_endpoint(doc: DIDDocument) -> Option(String) {
  doc.service
  |> list.find(fn(s) { s.service_type == "AtprotoPersonalDataServer" })
  |> result.map(fn(s) { s.service_endpoint })
  |> option.from_result
}

/// Parse DID document from JSON
fn parse_did_document(json_str: String) -> Result(DIDDocument, ATProtoError) {
  let decoder = {
    use id <- decode.field("id", decode.string)
    use also_known_as_opt <- decode.optional_field(
      "alsoKnownAs",
      None,
      decode.optional(decode.list(decode.string)),
    )
    use service <- decode.field("service", decode.list(service_decoder()))
    let also_known_as = option.unwrap(also_known_as_opt, [])
    decode.success(DIDDocument(
      id: id,
      also_known_as: also_known_as,
      service: service,
    ))
  }

  json.parse(json_str, decoder)
  |> result.map_error(fn(_) { types.InvalidDIDDocument("Parse failed") })
}

fn service_decoder() -> decode.Decoder(Service) {
  use id <- decode.field("id", decode.string)
  use service_type <- decode.field("type", decode.string)
  use endpoint <- decode.field("serviceEndpoint", decode.string)
  decode.success(Service(
    id: id,
    service_type: service_type,
    service_endpoint: endpoint,
  ))
}

/// Encode DID document to JSON
fn encode_did_document(doc: DIDDocument) -> String {
  let services =
    doc.service
    |> list.map(fn(s) {
      json.object([
        #("id", json.string(s.id)),
        #("type", json.string(s.service_type)),
        #("serviceEndpoint", json.string(s.service_endpoint)),
      ])
    })

  json.object([
    #("id", json.string(doc.id)),
    #("alsoKnownAs", json.array(doc.also_known_as, json.string)),
    #("service", json.preprocessed_array(services)),
  ])
  |> json.to_string
}

/// Extract handle from DID document's alsoKnownAs field
/// Returns the first at:// URI stripped of the at:// prefix
pub fn get_handle(doc: DIDDocument) -> Option(String) {
  doc.also_known_as
  |> list.find(fn(aka) { string.starts_with(aka, "at://") })
  |> result.map(fn(aka) { string.drop_start(aka, 5) })
  |> option.from_result
}
