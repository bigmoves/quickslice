# OAuth Handlers Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add remaining OAuth handler modules to the server, mirroring the oauth_server endpoints using the repositories created in the previous plan.

**Architecture:** Handlers are stateless functions that process HTTP requests and return responses. Each handler uses repository modules for data access. Complex handlers (authorize, token, atp_callback) delegate to existing oauth_server modules for business logic.

**Tech Stack:** Gleam, SQLite (sqlight), wisp HTTP framework, gleeunit for tests

---

## Task 13: Create jwks handler

**Files:**
- Create: `server/src/handlers/oauth/jwks.gleam`

**Step 1: Create the handler file**

```gleam
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

  Ok(json.object([
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
  ]))
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
@external(erlang, "jose_ffi", "derive_public_did_key")
fn erlang_derive_public_did_key(private_key: String) -> Result(String, Nil)

/// Erlang FFI: Extract public key coordinates from private key
@external(erlang, "jose_ffi", "extract_public_key_coords")
fn erlang_extract_public_key_coords(
  private_key: String,
) -> Result(#(String, String), Nil)
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/handlers/oauth/jwks.gleam
git commit -m "feat: add jwks handler"
```

---

## Task 14: Create jwks handler test

**Files:**
- Create: `server/test/oauth/jwks_test.gleam`

**Step 1: Create the test file**

```gleam
import gleeunit/should
import gleam/option.{None}
import handlers/oauth/jwks

pub fn handle_no_signing_key_test() {
  let response = jwks.handle(None)

  // Should return 200 with empty JWKS
  response.status |> should.equal(200)
}
```

**Step 2: Run the tests**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: Tests pass

**Step 3: Commit**

```bash
git add server/test/oauth/jwks_test.gleam
git commit -m "test: add jwks handler test"
```

---

## Task 15: Create dpop_nonce handler

**Files:**
- Create: `server/src/handlers/oauth/dpop_nonce.gleam`

**Step 1: Create the handler file**

```gleam
/// DPoP nonce endpoint handler
/// GET /oauth/dpop/nonce
import database/repositories/oauth_dpop_nonces
import database/types.{OAuthDpopNonce}
import gleam/bit_array
import gleam/crypto
import gleam/json
import sqlight
import wisp

/// Nonce response
pub type NonceResponse {
  NonceResponse(nonce: String, expires_in: Int)
}

/// Handle GET /oauth/dpop/nonce
/// Returns a fresh DPoP nonce for use in subsequent requests
pub fn handle(conn: sqlight.Connection) -> wisp.Response {
  // Generate a fresh nonce
  let nonce = generate_nonce()

  // Calculate expiration (5 minutes from now)
  let expires_in = 300
  let now = current_timestamp()
  let expires_at = now + expires_in

  // Store the nonce
  let nonce_record = OAuthDpopNonce(nonce: nonce, expires_at: expires_at)

  case oauth_dpop_nonces.insert(conn, nonce_record) {
    Ok(_) -> {
      let json_response =
        json.object([
          #("nonce", json.string(nonce)),
          #("expires_in", json.int(expires_in)),
        ])

      wisp.response(200)
      |> wisp.set_header("content-type", "application/json")
      |> wisp.set_header("cache-control", "no-store")
      |> wisp.set_body(wisp.Text(json.to_string(json_response)))
    }
    Error(_) -> {
      let error_response =
        json.object([
          #("error", json.string("server_error")),
          #("error_description", json.string("Failed to generate nonce")),
        ])

      wisp.response(500)
      |> wisp.set_header("content-type", "application/json")
      |> wisp.set_body(wisp.Text(json.to_string(error_response)))
    }
  }
}

/// Generate a cryptographically secure random nonce
fn generate_nonce() -> String {
  crypto.strong_random_bytes(32)
  |> bit_array.base64_url_encode(False)
}

/// Get current timestamp in seconds
fn current_timestamp() -> Int {
  get_system_time_native() / 1_000_000_000
}

@external(erlang, "os", "system_time")
fn get_system_time_native() -> Int
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/handlers/oauth/dpop_nonce.gleam
git commit -m "feat: add dpop_nonce handler"
```

---

## Task 16: Create dpop_nonce handler test

**Files:**
- Create: `server/test/oauth/dpop_nonce_test.gleam`

**Step 1: Create the test file**

```gleam
import gleeunit/should
import gleam/string
import handlers/oauth/dpop_nonce
import sqlight

pub fn handle_generates_nonce_test() {
  // Create in-memory database with schema
  let assert Ok(conn) = sqlight.open(":memory:")

  // Create the oauth_dpop_nonce table
  let assert Ok(_) =
    sqlight.exec(
      "CREATE TABLE oauth_dpop_nonce (
        nonce TEXT PRIMARY KEY,
        expires_at INTEGER NOT NULL
      )",
      conn,
    )

  let response = dpop_nonce.handle(conn)

  // Should return 200
  response.status |> should.equal(200)

  // Body should contain nonce
  case response.body {
    wisp.Text(body) -> {
      body |> string.contains("nonce") |> should.be_true
      body |> string.contains("expires_in") |> should.be_true
    }
    _ -> should.fail()
  }
}
```

**Step 2: Run the tests**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: Tests pass

**Step 3: Commit**

```bash
git add server/test/oauth/dpop_nonce_test.gleam
git commit -m "test: add dpop_nonce handler test"
```

---

## Task 17: Create client_metadata handler

**Files:**
- Create: `server/src/handlers/oauth/client_metadata.gleam`

**Step 1: Create the handler file**

```gleam
/// Client metadata endpoint for ATProtocol OAuth
/// GET /oauth-client-metadata.json
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import wisp

/// Client metadata response
pub type ClientMetadataResponse {
  ClientMetadataResponse(
    client_id: String,
    client_name: String,
    redirect_uris: List(String),
    grant_types: List(String),
    response_types: List(String),
    scope: String,
    token_endpoint_auth_method: String,
    token_endpoint_auth_signing_alg: String,
    subject_type: String,
    application_type: String,
    dpop_bound_access_tokens: Bool,
    jwks: Option(json.Json),
    jwks_uri: Option(String),
  )
}

/// Filter scopes to only ATProto-compatible scopes
fn filter_atproto_scopes(scope: String) -> String {
  let scopes = string.split(scope, " ")

  let filtered =
    list.filter(scopes, fn(s) {
      case s {
        "atproto" -> True
        "transition:generic" -> True
        _ -> False
      }
    })

  string.join(filtered, " ")
}

/// Generate client metadata
pub fn generate_metadata(
  client_id: String,
  client_name: String,
  redirect_uris: List(String),
  scope: String,
  jwks: Option(json.Json),
  jwks_uri: Option(String),
) -> ClientMetadataResponse {
  let filtered_scope = filter_atproto_scopes(scope)

  ClientMetadataResponse(
    client_id: client_id,
    client_name: client_name,
    redirect_uris: redirect_uris,
    grant_types: ["authorization_code", "refresh_token"],
    response_types: ["code"],
    scope: filtered_scope,
    token_endpoint_auth_method: "private_key_jwt",
    token_endpoint_auth_signing_alg: "ES256",
    subject_type: "public",
    application_type: "web",
    dpop_bound_access_tokens: True,
    jwks: jwks,
    jwks_uri: jwks_uri,
  )
}

/// Encode client metadata as JSON
pub fn encode_metadata(metadata: ClientMetadataResponse) -> json.Json {
  let base_fields = [
    #("client_id", json.string(metadata.client_id)),
    #("client_name", json.string(metadata.client_name)),
    #("redirect_uris", json.array(metadata.redirect_uris, json.string)),
    #("grant_types", json.array(metadata.grant_types, json.string)),
    #("response_types", json.array(metadata.response_types, json.string)),
    #("scope", json.string(metadata.scope)),
    #(
      "token_endpoint_auth_method",
      json.string(metadata.token_endpoint_auth_method),
    ),
    #(
      "token_endpoint_auth_signing_alg",
      json.string(metadata.token_endpoint_auth_signing_alg),
    ),
    #("subject_type", json.string(metadata.subject_type)),
    #("application_type", json.string(metadata.application_type)),
    #("dpop_bound_access_tokens", json.bool(metadata.dpop_bound_access_tokens)),
  ]

  let with_jwks = case metadata.jwks {
    Some(jwks) -> [#("jwks", jwks), ..base_fields]
    None -> base_fields
  }

  let with_jwks_uri = case metadata.jwks_uri {
    Some(uri) -> [#("jwks_uri", json.string(uri)), ..with_jwks]
    None -> with_jwks
  }

  json.object(with_jwks_uri)
}

/// Handle GET /oauth-client-metadata.json
pub fn handle(
  base_url: String,
  client_name: String,
  redirect_uris: List(String),
  scope: String,
  jwks: Option(json.Json),
) -> wisp.Response {
  let client_id = base_url <> "/oauth-client-metadata.json"
  let metadata =
    generate_metadata(client_id, client_name, redirect_uris, scope, jwks, None)
  let json_response = encode_metadata(metadata)

  wisp.response(200)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text(json.to_string(json_response)))
}
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/handlers/oauth/client_metadata.gleam
git commit -m "feat: add client_metadata handler"
```

---

## Task 18: Create client_metadata handler test

**Files:**
- Create: `server/test/oauth/client_metadata_test.gleam`

**Step 1: Create the test file**

```gleam
import gleeunit/should
import gleam/option.{None}
import handlers/oauth/client_metadata

pub fn generate_metadata_test() {
  let metadata =
    client_metadata.generate_metadata(
      "https://example.com/oauth-client-metadata.json",
      "Test Client",
      ["https://example.com/callback"],
      "atproto openid profile",
      None,
      None,
    )

  metadata.client_id
  |> should.equal("https://example.com/oauth-client-metadata.json")
  metadata.client_name |> should.equal("Test Client")
  metadata.scope |> should.equal("atproto")
  metadata.dpop_bound_access_tokens |> should.be_true
}

pub fn filter_atproto_scopes_test() {
  let metadata =
    client_metadata.generate_metadata(
      "https://example.com",
      "Test",
      [],
      "atproto transition:generic openid profile email",
      None,
      None,
    )

  // Should only keep atproto and transition:generic
  metadata.scope |> should.equal("atproto transition:generic")
}
```

**Step 2: Run the tests**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: Tests pass

**Step 3: Commit**

```bash
git add server/test/oauth/client_metadata_test.gleam
git commit -m "test: add client_metadata handler test"
```

---

## Task 19: Create register handler

**Files:**
- Create: `server/src/handlers/oauth/register.gleam`

**Step 1: Create the handler file**

```gleam
/// Client registration endpoint
/// POST /oauth/register
import database/repositories/oauth_clients
import database/types.{
  type GrantType, type ResponseType, AuthNone, AuthorizationCode,
  ClientSecretBasic, ClientSecretPost, Code, Confidential, OAuthClient,
  PrivateKeyJwt, Public, RefreshToken,
}
import gleam/bit_array
import gleam/crypto
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import sqlight
import wisp

/// Registration request parsed from JSON
pub type RegistrationRequest {
  RegistrationRequest(
    client_name: Option(String),
    redirect_uris: List(String),
    grant_types: Option(List(String)),
    response_types: Option(List(String)),
    token_endpoint_auth_method: Option(String),
    scope: Option(String),
  )
}

/// Registration response
pub type RegistrationResponse {
  RegistrationResponse(
    client_id: String,
    client_secret: Option(String),
    client_name: String,
    redirect_uris: List(String),
    grant_types: List(String),
    response_types: List(String),
    token_endpoint_auth_method: String,
    scope: String,
    client_id_issued_at: Int,
    client_type: String,
  )
}

/// Handle POST /oauth/register
pub fn handle(req: wisp.Request, conn: sqlight.Connection) -> wisp.Response {
  use body <- wisp.require_string_body(req)

  case parse_and_register(body, conn) {
    Ok(response) -> {
      let json_response = encode_response(response)

      wisp.response(201)
      |> wisp.set_header("content-type", "application/json")
      |> wisp.set_header("cache-control", "no-store")
      |> wisp.set_body(wisp.Text(json.to_string(json_response)))
    }
    Error(#(status, error, description)) -> {
      let json_response =
        json.object([
          #("error", json.string(error)),
          #("error_description", json.string(description)),
        ])

      wisp.response(status)
      |> wisp.set_header("content-type", "application/json")
      |> wisp.set_body(wisp.Text(json.to_string(json_response)))
    }
  }
}

fn parse_and_register(
  body: String,
  conn: sqlight.Connection,
) -> Result(RegistrationResponse, #(Int, String, String)) {
  // Parse JSON request
  use req <- result.try(
    json.parse(body, decode_registration_request())
    |> result.map_error(fn(_) {
      #(400, "invalid_request", "Invalid JSON or missing required fields")
    }),
  )

  // Validate redirect_uris (required)
  use _ <- result.try(case req.redirect_uris {
    [] -> Error(#(400, "invalid_request", "redirect_uris cannot be empty"))
    uris -> validate_redirect_uris(uris)
  })

  // Parse grant_types or use defaults
  let grant_types = case req.grant_types {
    Some(types) -> parse_grant_types(types)
    None -> [AuthorizationCode, RefreshToken]
  }

  // Parse response_types or use defaults
  let response_types = case req.response_types {
    Some(types) -> parse_response_types(types)
    None -> [Code]
  }

  // Parse token_endpoint_auth_method or use default
  let auth_method = case req.token_endpoint_auth_method {
    Some(method) -> parse_auth_method(method)
    None -> ClientSecretPost
  }

  // Determine client_type based on auth method
  let client_type = case auth_method {
    AuthNone -> Public
    _ -> Confidential
  }

  // Generate client_id and client_secret
  let client_id = generate_client_id()
  let client_secret = case client_type {
    Confidential -> Some(generate_client_secret())
    Public -> None
  }

  let now = current_timestamp()

  // Create client record
  let oauth_client =
    OAuthClient(
      client_id: client_id,
      client_secret: client_secret,
      client_name: req.client_name |> option.unwrap("OAuth Client"),
      redirect_uris: req.redirect_uris,
      grant_types: grant_types,
      response_types: response_types,
      scope: req.scope,
      token_endpoint_auth_method: auth_method,
      client_type: client_type,
      created_at: now,
      updated_at: now,
      metadata: "{}",
      access_token_expiration: 3600,
      refresh_token_expiration: 2_592_000,
      require_redirect_exact: True,
      registration_access_token: None,
      jwks: None,
    )

  // Store client
  use _ <- result.try(
    oauth_clients.insert(conn, oauth_client)
    |> result.map_error(fn(_) {
      #(500, "server_error", "Failed to store client")
    }),
  )

  // Build response
  Ok(RegistrationResponse(
    client_id: client_id,
    client_secret: client_secret,
    client_name: oauth_client.client_name,
    redirect_uris: oauth_client.redirect_uris,
    grant_types: list.map(oauth_client.grant_types, types.grant_type_to_string),
    response_types: list.map(
      oauth_client.response_types,
      types.response_type_to_string,
    ),
    token_endpoint_auth_method: types.client_auth_method_to_string(
      oauth_client.token_endpoint_auth_method,
    ),
    scope: oauth_client.scope |> option.unwrap("atproto"),
    client_id_issued_at: oauth_client.created_at,
    client_type: types.client_type_to_string(oauth_client.client_type),
  ))
}

fn decode_registration_request() -> decode.Decoder(RegistrationRequest) {
  use client_name <- decode.optional_field(
    "client_name",
    None,
    decode.optional(decode.string),
  )
  use redirect_uris <- decode.field("redirect_uris", decode.list(decode.string))
  use grant_types <- decode.optional_field(
    "grant_types",
    None,
    decode.optional(decode.list(decode.string)),
  )
  use response_types <- decode.optional_field(
    "response_types",
    None,
    decode.optional(decode.list(decode.string)),
  )
  use token_endpoint_auth_method <- decode.optional_field(
    "token_endpoint_auth_method",
    None,
    decode.optional(decode.string),
  )
  use scope <- decode.optional_field(
    "scope",
    None,
    decode.optional(decode.string),
  )

  decode.success(RegistrationRequest(
    client_name: client_name,
    redirect_uris: redirect_uris,
    grant_types: grant_types,
    response_types: response_types,
    token_endpoint_auth_method: token_endpoint_auth_method,
    scope: scope,
  ))
}

fn validate_redirect_uris(
  uris: List(String),
) -> Result(Nil, #(Int, String, String)) {
  let invalid =
    list.find(uris, fn(uri) {
      !string.starts_with(uri, "http://")
      && !string.starts_with(uri, "https://")
    })

  case invalid {
    Ok(uri) ->
      Error(#(400, "invalid_request", "Invalid redirect_uri: " <> uri))
    Error(_) -> Ok(Nil)
  }
}

fn parse_grant_types(types: List(String)) -> List(GrantType) {
  list.filter_map(types, fn(t) {
    case t {
      "authorization_code" -> Ok(AuthorizationCode)
      "refresh_token" -> Ok(RefreshToken)
      _ -> Error(Nil)
    }
  })
}

fn parse_response_types(types: List(String)) -> List(ResponseType) {
  list.filter_map(types, fn(t) {
    case t {
      "code" -> Ok(Code)
      _ -> Error(Nil)
    }
  })
}

fn parse_auth_method(method: String) -> types.ClientAuthMethod {
  case method {
    "client_secret_post" -> ClientSecretPost
    "client_secret_basic" -> ClientSecretBasic
    "private_key_jwt" -> PrivateKeyJwt
    "none" -> AuthNone
    _ -> ClientSecretPost
  }
}

fn generate_client_id() -> String {
  "client_" <> random_string(16)
}

fn generate_client_secret() -> String {
  random_string(32)
}

fn random_string(bytes: Int) -> String {
  crypto.strong_random_bytes(bytes)
  |> bit_array.base64_url_encode(False)
}

fn current_timestamp() -> Int {
  get_system_time_native() / 1_000_000_000
}

@external(erlang, "os", "system_time")
fn get_system_time_native() -> Int

fn encode_response(response: RegistrationResponse) -> json.Json {
  json.object([
    #("client_id", json.string(response.client_id)),
    #(
      "client_secret",
      case response.client_secret {
        Some(secret) -> json.string(secret)
        None -> json.null()
      },
    ),
    #("client_name", json.string(response.client_name)),
    #("redirect_uris", json.array(response.redirect_uris, json.string)),
    #("grant_types", json.array(response.grant_types, json.string)),
    #("response_types", json.array(response.response_types, json.string)),
    #(
      "token_endpoint_auth_method",
      json.string(response.token_endpoint_auth_method),
    ),
    #("scope", json.string(response.scope)),
    #("client_id_issued_at", json.int(response.client_id_issued_at)),
    #("client_type", json.string(response.client_type)),
  ])
}
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/handlers/oauth/register.gleam
git commit -m "feat: add register handler"
```

---

## Task 20: Create register handler test

**Files:**
- Create: `server/test/oauth/register_test.gleam`

**Step 1: Create the test file**

```gleam
import gleeunit/should
import gleam/string
import handlers/oauth/register
import sqlight
import wisp
import wisp/testing

pub fn register_valid_client_test() {
  // Create in-memory database with schema
  let assert Ok(conn) = sqlight.open(":memory:")

  // Create the oauth_client table
  let assert Ok(_) =
    sqlight.exec(
      "CREATE TABLE oauth_client (
        client_id TEXT PRIMARY KEY,
        client_secret TEXT,
        client_name TEXT NOT NULL,
        redirect_uris TEXT NOT NULL,
        grant_types TEXT NOT NULL,
        response_types TEXT NOT NULL,
        scope TEXT,
        token_endpoint_auth_method TEXT NOT NULL,
        client_type TEXT NOT NULL,
        created_at INTEGER NOT NULL,
        updated_at INTEGER NOT NULL,
        metadata TEXT NOT NULL,
        access_token_expiration INTEGER NOT NULL,
        refresh_token_expiration INTEGER NOT NULL,
        require_redirect_exact INTEGER NOT NULL,
        registration_access_token TEXT,
        jwks TEXT
      )",
      conn,
    )

  let body =
    "{\"client_name\": \"Test Client\", \"redirect_uris\": [\"https://example.com/callback\"]}"

  let req =
    testing.post("/oauth/register", [], body)
    |> testing.set_header("content-type", "application/json")

  let response = register.handle(req, conn)

  // Should return 201 Created
  response.status |> should.equal(201)

  // Body should contain client_id
  case response.body {
    wisp.Text(body) -> {
      body |> string.contains("client_id") |> should.be_true
      body |> string.contains("Test Client") |> should.be_true
    }
    _ -> should.fail()
  }
}

pub fn register_missing_redirect_uris_test() {
  let assert Ok(conn) = sqlight.open(":memory:")

  let body = "{\"client_name\": \"Test Client\", \"redirect_uris\": []}"

  let req =
    testing.post("/oauth/register", [], body)
    |> testing.set_header("content-type", "application/json")

  let response = register.handle(req, conn)

  // Should return 400 Bad Request
  response.status |> should.equal(400)
}
```

**Step 2: Run the tests**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: Tests pass

**Step 3: Commit**

```bash
git add server/test/oauth/register_test.gleam
git commit -m "test: add register handler test"
```

---

## Task 21: Create par handler

**Files:**
- Create: `server/src/handlers/oauth/par.gleam`

**Step 1: Create the handler file**

```gleam
/// Pushed Authorization Request (PAR) endpoint
/// POST /oauth/par
import database/repositories/oauth_clients
import database/repositories/oauth_par_requests
import database/types.{OAuthParRequest}
import gleam/bit_array
import gleam/crypto
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/uri
import sqlight
import wisp

/// PAR response
pub type PARResponse {
  PARResponse(request_uri: String, expires_in: Int)
}

/// Handle POST /oauth/par
pub fn handle(req: wisp.Request, conn: sqlight.Connection) -> wisp.Response {
  use body <- wisp.require_string_body(req)

  case process_par_request(body, conn) {
    Ok(response) -> {
      let json_response =
        json.object([
          #("request_uri", json.string(response.request_uri)),
          #("expires_in", json.int(response.expires_in)),
        ])

      wisp.response(201)
      |> wisp.set_header("content-type", "application/json")
      |> wisp.set_body(wisp.Text(json.to_string(json_response)))
    }
    Error(#(status, error, description)) -> {
      let json_response =
        json.object([
          #("error", json.string(error)),
          #("error_description", json.string(description)),
        ])

      wisp.response(status)
      |> wisp.set_header("content-type", "application/json")
      |> wisp.set_body(wisp.Text(json.to_string(json_response)))
    }
  }
}

fn process_par_request(
  body: String,
  conn: sqlight.Connection,
) -> Result(PARResponse, #(Int, String, String)) {
  // Parse form data from POST body
  use params <- result.try(
    uri.parse_query(body)
    |> result.map_error(fn(_) {
      #(400, "invalid_request", "Failed to parse form data")
    }),
  )

  // Extract client_id (required)
  use client_id <- result.try(case get_param(params, "client_id") {
    Some(id) -> Ok(id)
    None -> Error(#(400, "invalid_request", "client_id is required"))
  })

  // Get client from storage
  use client_opt <- result.try(
    oauth_clients.get(conn, client_id)
    |> result.map_error(fn(_) {
      #(401, "invalid_client", "Failed to retrieve client")
    }),
  )

  use _client <- result.try(case client_opt {
    Some(c) -> Ok(c)
    None -> Error(#(401, "invalid_client", "Client not found"))
  })

  // Extract required parameters
  use response_type <- result.try(case get_param(params, "response_type") {
    Some(rt) -> Ok(rt)
    None -> Error(#(400, "invalid_request", "response_type is required"))
  })

  use redirect_uri <- result.try(case get_param(params, "redirect_uri") {
    Some(uri) -> Ok(uri)
    None -> Error(#(400, "invalid_request", "redirect_uri is required"))
  })

  // Build authorization request JSON
  let auth_request_json =
    json.to_string(
      json.object([
        #("response_type", json.string(response_type)),
        #("client_id", json.string(client_id)),
        #("redirect_uri", json.string(redirect_uri)),
        #(
          "scope",
          case get_param(params, "scope") {
            Some(s) -> json.string(s)
            None -> json.null()
          },
        ),
        #(
          "state",
          case get_param(params, "state") {
            Some(s) -> json.string(s)
            None -> json.null()
          },
        ),
        #(
          "code_challenge",
          case get_param(params, "code_challenge") {
            Some(c) -> json.string(c)
            None -> json.null()
          },
        ),
        #(
          "code_challenge_method",
          case get_param(params, "code_challenge_method") {
            Some(m) -> json.string(m)
            None -> json.null()
          },
        ),
        #(
          "login_hint",
          case get_param(params, "login_hint") {
            Some(h) -> json.string(h)
            None -> json.null()
          },
        ),
      ]),
    )

  // Generate PAR request
  let request_uri = generate_par_request_uri()
  let now = current_timestamp()
  let expires_in = 60
  let expires_at = now + expires_in

  let par =
    OAuthParRequest(
      request_uri: request_uri,
      authorization_request: auth_request_json,
      client_id: client_id,
      created_at: now,
      expires_at: expires_at,
      subject: None,
      metadata: "{}",
    )

  // Store PAR request
  use _ <- result.try(
    oauth_par_requests.insert(conn, par)
    |> result.map_error(fn(_) {
      #(500, "server_error", "Failed to store PAR request")
    }),
  )

  Ok(PARResponse(request_uri: request_uri, expires_in: expires_in))
}

fn get_param(
  params: List(#(String, String)),
  key: String,
) -> Option(String) {
  params
  |> list.find(fn(param) { param.0 == key })
  |> result.map(fn(param) { param.1 })
  |> option.from_result
}

fn generate_par_request_uri() -> String {
  "urn:ietf:params:oauth:request_uri:" <> random_string(32)
}

fn random_string(bytes: Int) -> String {
  crypto.strong_random_bytes(bytes)
  |> bit_array.base64_url_encode(False)
}

fn current_timestamp() -> Int {
  get_system_time_native() / 1_000_000_000
}

@external(erlang, "os", "system_time")
fn get_system_time_native() -> Int
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/handlers/oauth/par.gleam
git commit -m "feat: add par handler"
```

---

## Task 22: Create par handler test

**Files:**
- Create: `server/test/oauth/par_test.gleam`

**Step 1: Create the test file**

```gleam
import gleeunit/should
import gleam/string
import handlers/oauth/par
import sqlight
import wisp
import wisp/testing

fn setup_test_db() -> sqlight.Connection {
  let assert Ok(conn) = sqlight.open(":memory:")

  // Create oauth_client table
  let assert Ok(_) =
    sqlight.exec(
      "CREATE TABLE oauth_client (
        client_id TEXT PRIMARY KEY,
        client_secret TEXT,
        client_name TEXT NOT NULL,
        redirect_uris TEXT NOT NULL,
        grant_types TEXT NOT NULL,
        response_types TEXT NOT NULL,
        scope TEXT,
        token_endpoint_auth_method TEXT NOT NULL,
        client_type TEXT NOT NULL,
        created_at INTEGER NOT NULL,
        updated_at INTEGER NOT NULL,
        metadata TEXT NOT NULL,
        access_token_expiration INTEGER NOT NULL,
        refresh_token_expiration INTEGER NOT NULL,
        require_redirect_exact INTEGER NOT NULL,
        registration_access_token TEXT,
        jwks TEXT
      )",
      conn,
    )

  // Create oauth_par_request table
  let assert Ok(_) =
    sqlight.exec(
      "CREATE TABLE oauth_par_request (
        request_uri TEXT PRIMARY KEY,
        authorization_request TEXT NOT NULL,
        client_id TEXT NOT NULL,
        created_at INTEGER NOT NULL,
        expires_at INTEGER NOT NULL,
        subject TEXT,
        metadata TEXT NOT NULL
      )",
      conn,
    )

  // Insert a test client
  let assert Ok(_) =
    sqlight.exec(
      "INSERT INTO oauth_client (client_id, client_name, redirect_uris, grant_types, response_types, token_endpoint_auth_method, client_type, created_at, updated_at, metadata, access_token_expiration, refresh_token_expiration, require_redirect_exact) VALUES ('test_client', 'Test', '[\"https://example.com/callback\"]', '[\"authorization_code\"]', '[\"code\"]', 'client_secret_post', 'confidential', 0, 0, '{}', 3600, 86400, 1)",
      conn,
    )

  conn
}

pub fn par_valid_request_test() {
  let conn = setup_test_db()

  let body =
    "client_id=test_client&response_type=code&redirect_uri=https://example.com/callback"

  let req =
    testing.post("/oauth/par", [], body)
    |> testing.set_header("content-type", "application/x-www-form-urlencoded")

  let response = par.handle(req, conn)

  // Should return 201 Created
  response.status |> should.equal(201)

  // Body should contain request_uri
  case response.body {
    wisp.Text(body) -> {
      body |> string.contains("request_uri") |> should.be_true
      body |> string.contains("expires_in") |> should.be_true
    }
    _ -> should.fail()
  }
}

pub fn par_missing_client_id_test() {
  let conn = setup_test_db()

  let body = "response_type=code&redirect_uri=https://example.com/callback"

  let req =
    testing.post("/oauth/par", [], body)
    |> testing.set_header("content-type", "application/x-www-form-urlencoded")

  let response = par.handle(req, conn)

  // Should return 400 Bad Request
  response.status |> should.equal(400)
}
```

**Step 2: Run the tests**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: Tests pass

**Step 3: Commit**

```bash
git add server/test/oauth/par_test.gleam
git commit -m "test: add par handler test"
```

---

## Task 23-30: Complex Handlers (Stub Implementation)

The remaining handlers (authorize, token, atp_callback, atp_session) are complex and depend on ATP bridge functionality. For now, we'll create stub handlers that can be wired up to the router, with the actual business logic delegating to oauth_server modules.

### Task 23: Create authorize handler stub

**Files:**
- Create: `server/src/handlers/oauth/authorize.gleam`

**Step 1: Create stub handler**

```gleam
/// Authorization endpoint stub
/// GET /oauth/authorize
///
/// Note: Full implementation requires ATP bridge integration.
/// This stub returns a placeholder response.
import gleam/json
import wisp

/// Handle GET /oauth/authorize
pub fn handle(_req: wisp.Request) -> wisp.Response {
  // Stub: Return not implemented
  let json_response =
    json.object([
      #("error", json.string("not_implemented")),
      #(
        "error_description",
        json.string("Authorization endpoint requires ATP bridge integration"),
      ),
    ])

  wisp.response(501)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text(json.to_string(json_response)))
}
```

**Step 2: Verify and commit**

```bash
cd /Users/chadmiller/code/quickslice/server && gleam build
git add server/src/handlers/oauth/authorize.gleam
git commit -m "feat: add authorize handler stub"
```

---

### Task 24: Create authorize handler test

**Files:**
- Create: `server/test/oauth/authorize_test.gleam`

**Step 1: Create test**

```gleam
import gleeunit/should
import handlers/oauth/authorize
import wisp/testing

pub fn authorize_stub_returns_501_test() {
  let req = testing.get("/oauth/authorize", [])
  let response = authorize.handle(req)

  response.status |> should.equal(501)
}
```

**Step 2: Run and commit**

```bash
cd /Users/chadmiller/code/quickslice/server && gleam test
git add server/test/oauth/authorize_test.gleam
git commit -m "test: add authorize handler test"
```

---

### Task 25: Create token handler stub

**Files:**
- Create: `server/src/handlers/oauth/token.gleam`

**Step 1: Create stub handler**

```gleam
/// Token endpoint stub
/// POST /oauth/token
import gleam/json
import wisp

/// Handle POST /oauth/token
pub fn handle(_req: wisp.Request) -> wisp.Response {
  let json_response =
    json.object([
      #("error", json.string("not_implemented")),
      #(
        "error_description",
        json.string("Token endpoint requires full OAuth implementation"),
      ),
    ])

  wisp.response(501)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text(json.to_string(json_response)))
}
```

**Step 2: Verify and commit**

```bash
cd /Users/chadmiller/code/quickslice/server && gleam build
git add server/src/handlers/oauth/token.gleam
git commit -m "feat: add token handler stub"
```

---

### Task 26: Create token handler test

**Files:**
- Create: `server/test/oauth/token_test.gleam`

**Step 1: Create test**

```gleam
import gleeunit/should
import handlers/oauth/token
import wisp/testing

pub fn token_stub_returns_501_test() {
  let req = testing.post("/oauth/token", [], "")
  let response = token.handle(req)

  response.status |> should.equal(501)
}
```

**Step 2: Run and commit**

```bash
cd /Users/chadmiller/code/quickslice/server && gleam test
git add server/test/oauth/token_test.gleam
git commit -m "test: add token handler test"
```

---

### Task 27: Create atp_callback handler stub

**Files:**
- Create: `server/src/handlers/oauth/atp_callback.gleam`

**Step 1: Create stub handler**

```gleam
/// ATP OAuth callback endpoint stub
/// GET /oauth/atp/callback
import gleam/json
import wisp

/// Handle GET /oauth/atp/callback
pub fn handle(_req: wisp.Request) -> wisp.Response {
  let json_response =
    json.object([
      #("error", json.string("not_implemented")),
      #(
        "error_description",
        json.string("ATP callback requires bridge integration"),
      ),
    ])

  wisp.response(501)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text(json.to_string(json_response)))
}
```

**Step 2: Verify and commit**

```bash
cd /Users/chadmiller/code/quickslice/server && gleam build
git add server/src/handlers/oauth/atp_callback.gleam
git commit -m "feat: add atp_callback handler stub"
```

---

### Task 28: Create atp_callback handler test

**Files:**
- Create: `server/test/oauth/atp_callback_test.gleam`

**Step 1: Create test**

```gleam
import gleeunit/should
import handlers/oauth/atp_callback
import wisp/testing

pub fn atp_callback_stub_returns_501_test() {
  let req = testing.get("/oauth/atp/callback", [])
  let response = atp_callback.handle(req)

  response.status |> should.equal(501)
}
```

**Step 2: Run and commit**

```bash
cd /Users/chadmiller/code/quickslice/server && gleam test
git add server/test/oauth/atp_callback_test.gleam
git commit -m "test: add atp_callback handler test"
```

---

### Task 29: Create atp_session handler stub

**Files:**
- Create: `server/src/handlers/oauth/atp_session.gleam`

**Step 1: Create stub handler**

```gleam
/// ATP session API endpoint stub
/// GET /api/atp/sessions/:session_id
import gleam/json
import wisp

/// Handle GET /api/atp/sessions/:session_id
pub fn handle(_req: wisp.Request, _session_id: String) -> wisp.Response {
  let json_response =
    json.object([
      #("error", json.string("not_implemented")),
      #(
        "error_description",
        json.string("ATP session API requires authentication"),
      ),
    ])

  wisp.response(501)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text(json.to_string(json_response)))
}
```

**Step 2: Verify and commit**

```bash
cd /Users/chadmiller/code/quickslice/server && gleam build
git add server/src/handlers/oauth/atp_session.gleam
git commit -m "feat: add atp_session handler stub"
```

---

### Task 30: Create atp_session handler test

**Files:**
- Create: `server/test/oauth/atp_session_test.gleam`

**Step 1: Create test**

```gleam
import gleeunit/should
import handlers/oauth/atp_session
import wisp/testing

pub fn atp_session_stub_returns_501_test() {
  let req = testing.get("/api/atp/sessions/test-session", [])
  let response = atp_session.handle(req, "test-session")

  response.status |> should.equal(501)
}
```

**Step 2: Run and commit**

```bash
cd /Users/chadmiller/code/quickslice/server && gleam test
git add server/test/oauth/atp_session_test.gleam
git commit -m "test: add atp_session handler test"
```

---

## Task 31: Wire up routes in server.gleam

**Files:**
- Modify: `server/src/server.gleam`

**Step 1: Add imports at the top of server.gleam**

After line 24 (`import handlers/upload as upload_handler`), add:

```gleam
import handlers/oauth/authorize as oauth_authorize_handler
import handlers/oauth/atp_callback as oauth_atp_callback_handler
import handlers/oauth/atp_session as oauth_atp_session_handler
import handlers/oauth/client_metadata as oauth_client_metadata_handler
import handlers/oauth/dpop_nonce as oauth_dpop_nonce_handler
import handlers/oauth/jwks as oauth_jwks_handler
import handlers/oauth/metadata as oauth_metadata_handler
import handlers/oauth/par as oauth_par_handler
import handlers/oauth/register as oauth_register_handler
import handlers/oauth/token as oauth_token_handler
```

**Step 2: Add new routes in handle_request function**

In the `handle_request` function (around line 551), add these new routes before the fallback case:

```gleam
    // New OAuth 2.0 endpoints
    [".well-known", "oauth-authorization-server"] ->
      oauth_metadata_handler.handle(ctx.oauth_config.auth_url)
    [".well-known", "jwks.json"] -> oauth_jwks_handler.handle(option.None)
    ["oauth-client-metadata.json"] ->
      oauth_client_metadata_handler.handle(
        ctx.oauth_config.auth_url,
        "Quickslice Server",
        [ctx.oauth_config.redirect_uri],
        "atproto",
        option.None,
      )
    ["oauth", "dpop", "nonce"] -> oauth_dpop_nonce_handler.handle(ctx.db)
    ["oauth", "register"] -> oauth_register_handler.handle(req, ctx.db)
    ["oauth", "par"] -> oauth_par_handler.handle(req, ctx.db)
    ["oauth", "authorize-new"] -> oauth_authorize_handler.handle(req)
    ["oauth", "token-new"] -> oauth_token_handler.handle(req)
    ["oauth", "atp", "callback-new"] -> oauth_atp_callback_handler.handle(req)
    ["api", "atp", "sessions", session_id] ->
      oauth_atp_session_handler.handle(req, session_id)
```

Note: Using `-new` suffix for stub endpoints to avoid conflicts with existing OAuth handlers.

**Step 3: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles without errors

**Step 4: Run all tests**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: All tests pass

**Step 5: Commit**

```bash
git add server/src/server.gleam
git commit -m "feat: wire up OAuth handler routes in server.gleam"
```

---

Plan complete and saved to `docs/plans/2025-11-24-oauth-handlers-implementation.md`. Two execution options:

**1. Subagent-Driven (this session)** - I dispatch fresh subagent per task, review between tasks, fast iteration

**2. Parallel Session (separate)** - Open new session with executing-plans, batch execution with checkpoints

Which approach?
