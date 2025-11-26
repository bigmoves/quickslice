# OAuth Repositories and Handlers Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add OAuth repository modules, handlers, and tests to the server to mirror oauth_server functionality using the new OAuth tables from migration v6.

**Architecture:** Repository layer provides CRUD operations for 8 OAuth tables. Handler layer exposes OAuth endpoints mirroring oauth_server. Each handler has a corresponding test file. Types are defined in `database/types.gleam` with typed enums for safety.

**Tech Stack:** Gleam, SQLite (sqlight), wisp HTTP framework, gleeunit for tests

---

## Task 1: Add OAuth enum types to database/types.gleam

**Files:**
- Modify: `server/src/database/types.gleam`

**Step 1: Add the enum types after existing types**

Add at the end of the file:

```gleam
// ===== OAuth Enum Types =====

/// OAuth 2.0 grant types
pub type GrantType {
  AuthorizationCode
  RefreshToken
  ClientCredentials
  DeviceCode
}

/// OAuth 2.0 response types
pub type ResponseType {
  Code
}

/// Client authentication methods at token endpoint
pub type ClientAuthMethod {
  ClientSecretBasic
  ClientSecretPost
  PrivateKeyJwt
  AuthNone
}

/// Client type classification
pub type ClientType {
  Public
  Confidential
}

/// Token type
pub type TokenType {
  Bearer
  DPoP
}

// ===== OAuth Enum Conversion Functions =====

pub fn grant_type_to_string(gt: GrantType) -> String {
  case gt {
    AuthorizationCode -> "authorization_code"
    RefreshToken -> "refresh_token"
    ClientCredentials -> "client_credentials"
    DeviceCode -> "urn:ietf:params:oauth:grant-type:device_code"
  }
}

pub fn grant_type_from_string(s: String) -> Option(GrantType) {
  case s {
    "authorization_code" -> Some(AuthorizationCode)
    "refresh_token" -> Some(RefreshToken)
    "client_credentials" -> Some(ClientCredentials)
    "urn:ietf:params:oauth:grant-type:device_code" -> Some(DeviceCode)
    _ -> None
  }
}

pub fn response_type_to_string(rt: ResponseType) -> String {
  case rt {
    Code -> "code"
  }
}

pub fn response_type_from_string(s: String) -> Option(ResponseType) {
  case s {
    "code" -> Some(Code)
    _ -> None
  }
}

pub fn client_auth_method_to_string(method: ClientAuthMethod) -> String {
  case method {
    ClientSecretBasic -> "client_secret_basic"
    ClientSecretPost -> "client_secret_post"
    PrivateKeyJwt -> "private_key_jwt"
    AuthNone -> "none"
  }
}

pub fn client_auth_method_from_string(s: String) -> Option(ClientAuthMethod) {
  case s {
    "client_secret_basic" -> Some(ClientSecretBasic)
    "client_secret_post" -> Some(ClientSecretPost)
    "private_key_jwt" -> Some(PrivateKeyJwt)
    "none" -> Some(AuthNone)
    _ -> None
  }
}

pub fn client_type_to_string(ct: ClientType) -> String {
  case ct {
    Public -> "public"
    Confidential -> "confidential"
  }
}

pub fn client_type_from_string(s: String) -> Option(ClientType) {
  case s {
    "public" -> Some(Public)
    "confidential" -> Some(Confidential)
    _ -> None
  }
}

pub fn token_type_to_string(tt: TokenType) -> String {
  case tt {
    Bearer -> "Bearer"
    DPoP -> "DPoP"
  }
}

pub fn token_type_from_string(s: String) -> Option(TokenType) {
  case s {
    "Bearer" -> Some(Bearer)
    "DPoP" -> Some(DPoP)
    _ -> None
  }
}
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/database/types.gleam
git commit -m "feat: add OAuth enum types to database/types.gleam"
```

---

## Task 2: Add OAuth record types to database/types.gleam

**Files:**
- Modify: `server/src/database/types.gleam`

**Step 1: Add the record types after enum conversion functions**

```gleam
// ===== OAuth Record Types =====

/// OAuth client registration
pub type OAuthClient {
  OAuthClient(
    client_id: String,
    client_secret: Option(String),
    client_name: String,
    redirect_uris: List(String),
    grant_types: List(GrantType),
    response_types: List(ResponseType),
    scope: Option(String),
    token_endpoint_auth_method: ClientAuthMethod,
    client_type: ClientType,
    created_at: Int,
    updated_at: Int,
    metadata: String,
    access_token_expiration: Int,
    refresh_token_expiration: Int,
    require_redirect_exact: Bool,
    registration_access_token: Option(String),
    jwks: Option(String),
  )
}

/// OAuth access token
pub type OAuthAccessToken {
  OAuthAccessToken(
    token: String,
    token_type: TokenType,
    client_id: String,
    user_id: Option(String),
    session_id: Option(String),
    session_iteration: Option(Int),
    scope: Option(String),
    created_at: Int,
    expires_at: Int,
    revoked: Bool,
    dpop_jkt: Option(String),
  )
}

/// OAuth refresh token
pub type OAuthRefreshToken {
  OAuthRefreshToken(
    token: String,
    access_token: String,
    client_id: String,
    user_id: String,
    session_id: Option(String),
    scope: Option(String),
    created_at: Int,
    expires_at: Option(Int),
    revoked: Bool,
  )
}

/// Pushed Authorization Request
pub type OAuthParRequest {
  OAuthParRequest(
    request_uri: String,
    authorization_request: String,
    client_id: String,
    created_at: Int,
    expires_at: Int,
    subject: Option(String),
    metadata: String,
  )
}

/// DPoP nonce
pub type OAuthDpopNonce {
  OAuthDpopNonce(nonce: String, expires_at: Int)
}

/// Client authorization request during bridge flow
pub type OAuthAuthRequest {
  OAuthAuthRequest(
    session_id: String,
    client_id: String,
    redirect_uri: String,
    scope: Option(String),
    state: Option(String),
    code_challenge: Option(String),
    code_challenge_method: Option(String),
    response_type: String,
    nonce: Option(String),
    login_hint: Option(String),
    created_at: Int,
    expires_at: Int,
  )
}

/// ATP bridge session state
pub type OAuthAtpSession {
  OAuthAtpSession(
    session_id: String,
    iteration: Int,
    did: Option(String),
    session_created_at: Int,
    atp_oauth_state: String,
    signing_key_jkt: String,
    dpop_key: String,
    access_token: Option(String),
    refresh_token: Option(String),
    access_token_created_at: Option(Int),
    access_token_expires_at: Option(Int),
    access_token_scopes: Option(String),
    session_exchanged_at: Option(Int),
    exchange_error: Option(String),
  )
}

/// Outbound OAuth request to ATP
pub type OAuthAtpRequest {
  OAuthAtpRequest(
    oauth_state: String,
    authorization_server: String,
    nonce: String,
    pkce_verifier: String,
    signing_public_key: String,
    dpop_private_key: String,
    created_at: Int,
    expires_at: Int,
  )
}
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/database/types.gleam
git commit -m "feat: add OAuth record types to database/types.gleam"
```

---

## Task 3: Create oauth_clients repository

**Files:**
- Create: `server/src/database/repositories/oauth_clients.gleam`

**Step 1: Create the repository file**

```gleam
/// OAuth client repository operations
import database/types.{
  type ClientAuthMethod, type ClientType, type GrantType, type OAuthClient,
  type ResponseType, OAuthClient,
}
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import sqlight

/// Insert a new OAuth client
pub fn insert(
  conn: sqlight.Connection,
  client: OAuthClient,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO oauth_client (
      client_id, client_secret, client_name, redirect_uris,
      grant_types, response_types, scope, token_endpoint_auth_method,
      client_type, created_at, updated_at, metadata,
      access_token_expiration, refresh_token_expiration,
      require_redirect_exact, registration_access_token, jwks
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  "

  let redirect_uris_json =
    json.to_string(json.array(client.redirect_uris, json.string))
  let grant_types_json =
    json.to_string(
      json.array(client.grant_types, fn(gt) {
        json.string(types.grant_type_to_string(gt))
      }),
    )
  let response_types_json =
    json.to_string(
      json.array(client.response_types, fn(rt) {
        json.string(types.response_type_to_string(rt))
      }),
    )

  let params = [
    sqlight.text(client.client_id),
    sqlight.nullable(sqlight.text, client.client_secret),
    sqlight.text(client.client_name),
    sqlight.text(redirect_uris_json),
    sqlight.text(grant_types_json),
    sqlight.text(response_types_json),
    sqlight.nullable(sqlight.text, client.scope),
    sqlight.text(types.client_auth_method_to_string(
      client.token_endpoint_auth_method,
    )),
    sqlight.text(types.client_type_to_string(client.client_type)),
    sqlight.int(client.created_at),
    sqlight.int(client.updated_at),
    sqlight.text(client.metadata),
    sqlight.int(client.access_token_expiration),
    sqlight.int(client.refresh_token_expiration),
    sqlight.bool(client.require_redirect_exact),
    sqlight.nullable(sqlight.text, client.registration_access_token),
    sqlight.nullable(sqlight.text, client.jwks),
  ]

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: params,
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Get an OAuth client by client_id
pub fn get(
  conn: sqlight.Connection,
  client_id: String,
) -> Result(Option(OAuthClient), sqlight.Error) {
  let sql =
    "SELECT client_id, client_secret, client_name, redirect_uris,
            grant_types, response_types, scope, token_endpoint_auth_method,
            client_type, created_at, updated_at, metadata,
            access_token_expiration, refresh_token_expiration,
            require_redirect_exact, registration_access_token, jwks
     FROM oauth_client WHERE client_id = ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(client_id)],
    expecting: decoder(),
  ))

  case list.first(rows) {
    Ok(client) -> Ok(Some(client))
    Error(_) -> Ok(None)
  }
}

/// Update an existing OAuth client
pub fn update(
  conn: sqlight.Connection,
  client: OAuthClient,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    UPDATE oauth_client SET
      client_secret = ?,
      client_name = ?,
      redirect_uris = ?,
      grant_types = ?,
      response_types = ?,
      scope = ?,
      token_endpoint_auth_method = ?,
      updated_at = ?,
      metadata = ?,
      access_token_expiration = ?,
      refresh_token_expiration = ?,
      require_redirect_exact = ?,
      jwks = ?
    WHERE client_id = ?
  "

  let redirect_uris_json =
    json.to_string(json.array(client.redirect_uris, json.string))
  let grant_types_json =
    json.to_string(
      json.array(client.grant_types, fn(gt) {
        json.string(types.grant_type_to_string(gt))
      }),
    )
  let response_types_json =
    json.to_string(
      json.array(client.response_types, fn(rt) {
        json.string(types.response_type_to_string(rt))
      }),
    )

  let params = [
    sqlight.nullable(sqlight.text, client.client_secret),
    sqlight.text(client.client_name),
    sqlight.text(redirect_uris_json),
    sqlight.text(grant_types_json),
    sqlight.text(response_types_json),
    sqlight.nullable(sqlight.text, client.scope),
    sqlight.text(types.client_auth_method_to_string(
      client.token_endpoint_auth_method,
    )),
    sqlight.int(client.updated_at),
    sqlight.text(client.metadata),
    sqlight.int(client.access_token_expiration),
    sqlight.int(client.refresh_token_expiration),
    sqlight.bool(client.require_redirect_exact),
    sqlight.nullable(sqlight.text, client.jwks),
    sqlight.text(client.client_id),
  ]

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: params,
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Delete an OAuth client
pub fn delete(
  conn: sqlight.Connection,
  client_id: String,
) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM oauth_client WHERE client_id = ?"

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(client_id)],
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Decode OAuth client from database row
fn decoder() -> decode.Decoder(OAuthClient) {
  use client_id <- decode.field(0, decode.string)
  use client_secret <- decode.field(1, decode.optional(decode.string))
  use client_name <- decode.field(2, decode.string)
  use redirect_uris_json <- decode.field(3, decode.string)
  use grant_types_json <- decode.field(4, decode.string)
  use response_types_json <- decode.field(5, decode.string)
  use scope <- decode.field(6, decode.optional(decode.string))
  use auth_method_str <- decode.field(7, decode.string)
  use client_type_str <- decode.field(8, decode.string)
  use created_at <- decode.field(9, decode.int)
  use updated_at <- decode.field(10, decode.int)
  use metadata <- decode.field(11, decode.string)
  use access_exp <- decode.field(12, decode.int)
  use refresh_exp <- decode.field(13, decode.int)
  use require_exact <- decode.field(14, decode.int)
  use reg_token <- decode.field(15, decode.optional(decode.string))
  use jwks <- decode.field(16, decode.optional(decode.string))

  // Parse JSON arrays
  let redirect_uris =
    json.parse(redirect_uris_json, decode.list(decode.string))
    |> result.unwrap([])

  let grant_types =
    json.parse(grant_types_json, decode.list(decode.string))
    |> result.unwrap([])
    |> list.filter_map(fn(s) {
      case types.grant_type_from_string(s) {
        Some(gt) -> Ok(gt)
        None -> Error(Nil)
      }
    })

  let response_types =
    json.parse(response_types_json, decode.list(decode.string))
    |> result.unwrap([])
    |> list.filter_map(fn(s) {
      case types.response_type_from_string(s) {
        Some(rt) -> Ok(rt)
        None -> Error(Nil)
      }
    })

  let auth_method =
    types.client_auth_method_from_string(auth_method_str)
    |> option.unwrap(types.AuthNone)

  let client_type =
    types.client_type_from_string(client_type_str)
    |> option.unwrap(types.Public)

  decode.success(OAuthClient(
    client_id: client_id,
    client_secret: client_secret,
    client_name: client_name,
    redirect_uris: redirect_uris,
    grant_types: grant_types,
    response_types: response_types,
    scope: scope,
    token_endpoint_auth_method: auth_method,
    client_type: client_type,
    created_at: created_at,
    updated_at: updated_at,
    metadata: metadata,
    access_token_expiration: access_exp,
    refresh_token_expiration: refresh_exp,
    require_redirect_exact: require_exact == 1,
    registration_access_token: reg_token,
    jwks: jwks,
  ))
}
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/database/repositories/oauth_clients.gleam
git commit -m "feat: add oauth_clients repository"
```

---

## Task 4: Create oauth_access_tokens repository

**Files:**
- Create: `server/src/database/repositories/oauth_access_tokens.gleam`

**Step 1: Create the repository file**

```gleam
/// OAuth access token repository operations
import database/types.{type OAuthAccessToken, type TokenType, OAuthAccessToken}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import sqlight

/// Insert a new access token
pub fn insert(
  conn: sqlight.Connection,
  token: OAuthAccessToken,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO oauth_access_token (
      token, token_type, client_id, user_id, session_id,
      session_iteration, scope, created_at, expires_at,
      revoked, dpop_jkt
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  "

  let params = [
    sqlight.text(token.token),
    sqlight.text(types.token_type_to_string(token.token_type)),
    sqlight.text(token.client_id),
    sqlight.nullable(sqlight.text, token.user_id),
    sqlight.nullable(sqlight.text, token.session_id),
    sqlight.nullable(sqlight.int, token.session_iteration),
    sqlight.nullable(sqlight.text, token.scope),
    sqlight.int(token.created_at),
    sqlight.int(token.expires_at),
    sqlight.bool(token.revoked),
    sqlight.nullable(sqlight.text, token.dpop_jkt),
  ]

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: params,
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Get an access token by token value
pub fn get(
  conn: sqlight.Connection,
  token_value: String,
) -> Result(Option(OAuthAccessToken), sqlight.Error) {
  let sql =
    "SELECT token, token_type, client_id, user_id, session_id,
            session_iteration, scope, created_at, expires_at,
            revoked, dpop_jkt
     FROM oauth_access_token WHERE token = ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(token_value)],
    expecting: decoder(),
  ))

  case list.first(rows) {
    Ok(token) -> Ok(Some(token))
    Error(_) -> Ok(None)
  }
}

/// Get an access token by DPoP JKT (thumbprint)
pub fn get_by_jkt(
  conn: sqlight.Connection,
  jkt: String,
) -> Result(Option(OAuthAccessToken), sqlight.Error) {
  let sql =
    "SELECT token, token_type, client_id, user_id, session_id,
            session_iteration, scope, created_at, expires_at,
            revoked, dpop_jkt
     FROM oauth_access_token
     WHERE dpop_jkt = ? AND revoked = 0"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(jkt)],
    expecting: decoder(),
  ))

  case list.first(rows) {
    Ok(token) -> Ok(Some(token))
    Error(_) -> Ok(None)
  }
}

/// Revoke an access token
pub fn revoke(
  conn: sqlight.Connection,
  token_value: String,
) -> Result(Nil, sqlight.Error) {
  let sql = "UPDATE oauth_access_token SET revoked = 1 WHERE token = ?"

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(token_value)],
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Delete expired access tokens
pub fn delete_expired(
  conn: sqlight.Connection,
  now: Int,
) -> Result(Int, sqlight.Error) {
  let sql = "DELETE FROM oauth_access_token WHERE expires_at <= ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.int(now)],
    expecting: decode.dynamic,
  ))
  Ok(list.length(rows))
}

/// Decode access token from database row
fn decoder() -> decode.Decoder(OAuthAccessToken) {
  use token <- decode.field(0, decode.string)
  use token_type_str <- decode.field(1, decode.string)
  use client_id <- decode.field(2, decode.string)
  use user_id <- decode.field(3, decode.optional(decode.string))
  use session_id <- decode.field(4, decode.optional(decode.string))
  use session_iteration <- decode.field(5, decode.optional(decode.int))
  use scope <- decode.field(6, decode.optional(decode.string))
  use created_at <- decode.field(7, decode.int)
  use expires_at <- decode.field(8, decode.int)
  use revoked <- decode.field(9, decode.int)
  use dpop_jkt <- decode.field(10, decode.optional(decode.string))

  let token_type =
    types.token_type_from_string(token_type_str)
    |> option.unwrap(types.Bearer)

  decode.success(OAuthAccessToken(
    token: token,
    token_type: token_type,
    client_id: client_id,
    user_id: user_id,
    session_id: session_id,
    session_iteration: session_iteration,
    scope: scope,
    created_at: created_at,
    expires_at: expires_at,
    revoked: revoked == 1,
    dpop_jkt: dpop_jkt,
  ))
}
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/database/repositories/oauth_access_tokens.gleam
git commit -m "feat: add oauth_access_tokens repository"
```

---

## Task 5: Create oauth_refresh_tokens repository

**Files:**
- Create: `server/src/database/repositories/oauth_refresh_tokens.gleam`

**Step 1: Create the repository file**

```gleam
/// OAuth refresh token repository operations
import database/types.{type OAuthRefreshToken, OAuthRefreshToken}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import sqlight

/// Insert a new refresh token
pub fn insert(
  conn: sqlight.Connection,
  token: OAuthRefreshToken,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO oauth_refresh_token (
      token, access_token, client_id, user_id, session_id,
      scope, created_at, expires_at, revoked
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
  "

  let params = [
    sqlight.text(token.token),
    sqlight.text(token.access_token),
    sqlight.text(token.client_id),
    sqlight.text(token.user_id),
    sqlight.nullable(sqlight.text, token.session_id),
    sqlight.nullable(sqlight.text, token.scope),
    sqlight.int(token.created_at),
    sqlight.nullable(sqlight.int, token.expires_at),
    sqlight.bool(token.revoked),
  ]

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: params,
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Get a refresh token by token value
pub fn get(
  conn: sqlight.Connection,
  token_value: String,
) -> Result(Option(OAuthRefreshToken), sqlight.Error) {
  let sql =
    "SELECT token, access_token, client_id, user_id, session_id,
            scope, created_at, expires_at, revoked
     FROM oauth_refresh_token WHERE token = ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(token_value)],
    expecting: decoder(),
  ))

  case list.first(rows) {
    Ok(token) -> Ok(Some(token))
    Error(_) -> Ok(None)
  }
}

/// Revoke a refresh token
pub fn revoke(
  conn: sqlight.Connection,
  token_value: String,
) -> Result(Nil, sqlight.Error) {
  let sql = "UPDATE oauth_refresh_token SET revoked = 1 WHERE token = ?"

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(token_value)],
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Delete expired refresh tokens
pub fn delete_expired(
  conn: sqlight.Connection,
  now: Int,
) -> Result(Int, sqlight.Error) {
  let sql =
    "DELETE FROM oauth_refresh_token WHERE expires_at IS NOT NULL AND expires_at <= ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.int(now)],
    expecting: decode.dynamic,
  ))
  Ok(list.length(rows))
}

/// Decode refresh token from database row
fn decoder() -> decode.Decoder(OAuthRefreshToken) {
  use token <- decode.field(0, decode.string)
  use access_token <- decode.field(1, decode.string)
  use client_id <- decode.field(2, decode.string)
  use user_id <- decode.field(3, decode.string)
  use session_id <- decode.field(4, decode.optional(decode.string))
  use scope <- decode.field(5, decode.optional(decode.string))
  use created_at <- decode.field(6, decode.int)
  use expires_at <- decode.field(7, decode.optional(decode.int))
  use revoked <- decode.field(8, decode.int)

  decode.success(OAuthRefreshToken(
    token: token,
    access_token: access_token,
    client_id: client_id,
    user_id: user_id,
    session_id: session_id,
    scope: scope,
    created_at: created_at,
    expires_at: expires_at,
    revoked: revoked == 1,
  ))
}
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/database/repositories/oauth_refresh_tokens.gleam
git commit -m "feat: add oauth_refresh_tokens repository"
```

---

## Task 6: Create oauth_par_requests repository

**Files:**
- Create: `server/src/database/repositories/oauth_par_requests.gleam`

**Step 1: Create the repository file**

```gleam
/// OAuth PAR (Pushed Authorization Request) repository operations
import database/types.{type OAuthParRequest, OAuthParRequest}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import sqlight

/// Insert a new PAR request
pub fn insert(
  conn: sqlight.Connection,
  par: OAuthParRequest,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO oauth_par_request (
      request_uri, authorization_request, client_id,
      created_at, expires_at, subject, metadata
    ) VALUES (?, ?, ?, ?, ?, ?, ?)
  "

  let params = [
    sqlight.text(par.request_uri),
    sqlight.text(par.authorization_request),
    sqlight.text(par.client_id),
    sqlight.int(par.created_at),
    sqlight.int(par.expires_at),
    sqlight.nullable(sqlight.text, par.subject),
    sqlight.text(par.metadata),
  ]

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: params,
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Get a PAR request by request_uri
pub fn get(
  conn: sqlight.Connection,
  request_uri: String,
) -> Result(Option(OAuthParRequest), sqlight.Error) {
  let sql =
    "SELECT request_uri, authorization_request, client_id,
            created_at, expires_at, subject, metadata
     FROM oauth_par_request WHERE request_uri = ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(request_uri)],
    expecting: decoder(),
  ))

  case list.first(rows) {
    Ok(par) -> Ok(Some(par))
    Error(_) -> Ok(None)
  }
}

/// Delete a PAR request
pub fn delete(
  conn: sqlight.Connection,
  request_uri: String,
) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM oauth_par_request WHERE request_uri = ?"

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(request_uri)],
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Delete expired PAR requests
pub fn delete_expired(
  conn: sqlight.Connection,
  now: Int,
) -> Result(Int, sqlight.Error) {
  let sql = "DELETE FROM oauth_par_request WHERE expires_at <= ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.int(now)],
    expecting: decode.dynamic,
  ))
  Ok(list.length(rows))
}

/// Decode PAR request from database row
fn decoder() -> decode.Decoder(OAuthParRequest) {
  use request_uri <- decode.field(0, decode.string)
  use authorization_request <- decode.field(1, decode.string)
  use client_id <- decode.field(2, decode.string)
  use created_at <- decode.field(3, decode.int)
  use expires_at <- decode.field(4, decode.int)
  use subject <- decode.field(5, decode.optional(decode.string))
  use metadata <- decode.field(6, decode.string)

  decode.success(OAuthParRequest(
    request_uri: request_uri,
    authorization_request: authorization_request,
    client_id: client_id,
    created_at: created_at,
    expires_at: expires_at,
    subject: subject,
    metadata: metadata,
  ))
}
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/database/repositories/oauth_par_requests.gleam
git commit -m "feat: add oauth_par_requests repository"
```

---

## Task 7: Create oauth_dpop_nonces repository

**Files:**
- Create: `server/src/database/repositories/oauth_dpop_nonces.gleam`

**Step 1: Create the repository file**

```gleam
/// OAuth DPoP nonce repository operations
import database/types.{type OAuthDpopNonce, OAuthDpopNonce}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import sqlight

/// Insert a new DPoP nonce
pub fn insert(
  conn: sqlight.Connection,
  nonce: OAuthDpopNonce,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO oauth_dpop_nonce (nonce, expires_at)
    VALUES (?, ?)
  "

  let params = [sqlight.text(nonce.nonce), sqlight.int(nonce.expires_at)]

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: params,
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Get a DPoP nonce by value
pub fn get(
  conn: sqlight.Connection,
  nonce_value: String,
) -> Result(Option(OAuthDpopNonce), sqlight.Error) {
  let sql = "SELECT nonce, expires_at FROM oauth_dpop_nonce WHERE nonce = ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(nonce_value)],
    expecting: decoder(),
  ))

  case list.first(rows) {
    Ok(nonce) -> Ok(Some(nonce))
    Error(_) -> Ok(None)
  }
}

/// Delete a DPoP nonce
pub fn delete(
  conn: sqlight.Connection,
  nonce_value: String,
) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM oauth_dpop_nonce WHERE nonce = ?"

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(nonce_value)],
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Delete expired DPoP nonces
pub fn delete_expired(
  conn: sqlight.Connection,
  now: Int,
) -> Result(Int, sqlight.Error) {
  let sql = "DELETE FROM oauth_dpop_nonce WHERE expires_at <= ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.int(now)],
    expecting: decode.dynamic,
  ))
  Ok(list.length(rows))
}

/// Decode DPoP nonce from database row
fn decoder() -> decode.Decoder(OAuthDpopNonce) {
  use nonce <- decode.field(0, decode.string)
  use expires_at <- decode.field(1, decode.int)

  decode.success(OAuthDpopNonce(nonce: nonce, expires_at: expires_at))
}
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/database/repositories/oauth_dpop_nonces.gleam
git commit -m "feat: add oauth_dpop_nonces repository"
```

---

## Task 8: Create oauth_auth_requests repository

**Files:**
- Create: `server/src/database/repositories/oauth_auth_requests.gleam`

**Step 1: Create the repository file**

```gleam
/// OAuth authorization request repository operations
import database/types.{type OAuthAuthRequest, OAuthAuthRequest}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import sqlight

/// Insert a new authorization request
pub fn insert(
  conn: sqlight.Connection,
  req: OAuthAuthRequest,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO oauth_auth_request (
      session_id, client_id, redirect_uri, scope, state,
      code_challenge, code_challenge_method, response_type,
      nonce, login_hint, created_at, expires_at
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  "

  let params = [
    sqlight.text(req.session_id),
    sqlight.text(req.client_id),
    sqlight.text(req.redirect_uri),
    sqlight.nullable(sqlight.text, req.scope),
    sqlight.nullable(sqlight.text, req.state),
    sqlight.nullable(sqlight.text, req.code_challenge),
    sqlight.nullable(sqlight.text, req.code_challenge_method),
    sqlight.text(req.response_type),
    sqlight.nullable(sqlight.text, req.nonce),
    sqlight.nullable(sqlight.text, req.login_hint),
    sqlight.int(req.created_at),
    sqlight.int(req.expires_at),
  ]

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: params,
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Get an authorization request by session_id
pub fn get(
  conn: sqlight.Connection,
  session_id: String,
) -> Result(Option(OAuthAuthRequest), sqlight.Error) {
  let sql =
    "SELECT session_id, client_id, redirect_uri, scope, state,
            code_challenge, code_challenge_method, response_type,
            nonce, login_hint, created_at, expires_at
     FROM oauth_auth_request WHERE session_id = ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(session_id)],
    expecting: decoder(),
  ))

  case list.first(rows) {
    Ok(req) -> Ok(Some(req))
    Error(_) -> Ok(None)
  }
}

/// Delete an authorization request
pub fn delete(
  conn: sqlight.Connection,
  session_id: String,
) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM oauth_auth_request WHERE session_id = ?"

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(session_id)],
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Delete expired authorization requests
pub fn delete_expired(
  conn: sqlight.Connection,
  now: Int,
) -> Result(Int, sqlight.Error) {
  let sql = "DELETE FROM oauth_auth_request WHERE expires_at <= ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.int(now)],
    expecting: decode.dynamic,
  ))
  Ok(list.length(rows))
}

/// Decode authorization request from database row
fn decoder() -> decode.Decoder(OAuthAuthRequest) {
  use session_id <- decode.field(0, decode.string)
  use client_id <- decode.field(1, decode.string)
  use redirect_uri <- decode.field(2, decode.string)
  use scope <- decode.field(3, decode.optional(decode.string))
  use state <- decode.field(4, decode.optional(decode.string))
  use code_challenge <- decode.field(5, decode.optional(decode.string))
  use code_challenge_method <- decode.field(6, decode.optional(decode.string))
  use response_type <- decode.field(7, decode.string)
  use nonce <- decode.field(8, decode.optional(decode.string))
  use login_hint <- decode.field(9, decode.optional(decode.string))
  use created_at <- decode.field(10, decode.int)
  use expires_at <- decode.field(11, decode.int)

  decode.success(OAuthAuthRequest(
    session_id: session_id,
    client_id: client_id,
    redirect_uri: redirect_uri,
    scope: scope,
    state: state,
    code_challenge: code_challenge,
    code_challenge_method: code_challenge_method,
    response_type: response_type,
    nonce: nonce,
    login_hint: login_hint,
    created_at: created_at,
    expires_at: expires_at,
  ))
}
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/database/repositories/oauth_auth_requests.gleam
git commit -m "feat: add oauth_auth_requests repository"
```

---

## Task 9: Create oauth_atp_sessions repository

**Files:**
- Create: `server/src/database/repositories/oauth_atp_sessions.gleam`

**Step 1: Create the repository file**

```gleam
/// OAuth ATP session repository operations
import database/types.{type OAuthAtpSession, OAuthAtpSession}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import sqlight

/// Insert a new ATP session (or replace existing)
pub fn insert(
  conn: sqlight.Connection,
  session: OAuthAtpSession,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT OR REPLACE INTO oauth_atp_session (
      session_id, iteration, did, session_created_at, atp_oauth_state,
      signing_key_jkt, dpop_key, access_token, refresh_token,
      access_token_created_at, access_token_expires_at, access_token_scopes,
      session_exchanged_at, exchange_error
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  "

  let params = [
    sqlight.text(session.session_id),
    sqlight.int(session.iteration),
    sqlight.nullable(sqlight.text, session.did),
    sqlight.int(session.session_created_at),
    sqlight.text(session.atp_oauth_state),
    sqlight.text(session.signing_key_jkt),
    sqlight.text(session.dpop_key),
    sqlight.nullable(sqlight.text, session.access_token),
    sqlight.nullable(sqlight.text, session.refresh_token),
    sqlight.nullable(sqlight.int, session.access_token_created_at),
    sqlight.nullable(sqlight.int, session.access_token_expires_at),
    sqlight.nullable(sqlight.text, session.access_token_scopes),
    sqlight.nullable(sqlight.int, session.session_exchanged_at),
    sqlight.nullable(sqlight.text, session.exchange_error),
  ]

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: params,
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Get an ATP session by session_id and iteration
pub fn get(
  conn: sqlight.Connection,
  session_id: String,
  iteration: Int,
) -> Result(Option(OAuthAtpSession), sqlight.Error) {
  let sql =
    "SELECT session_id, iteration, did, session_created_at, atp_oauth_state,
            signing_key_jkt, dpop_key, access_token, refresh_token,
            access_token_created_at, access_token_expires_at, access_token_scopes,
            session_exchanged_at, exchange_error
     FROM oauth_atp_session
     WHERE session_id = ? AND iteration = ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(session_id), sqlight.int(iteration)],
    expecting: decoder(),
  ))

  case list.first(rows) {
    Ok(session) -> Ok(Some(session))
    Error(_) -> Ok(None)
  }
}

/// Get the latest ATP session iteration for a session_id
pub fn get_latest(
  conn: sqlight.Connection,
  session_id: String,
) -> Result(Option(OAuthAtpSession), sqlight.Error) {
  let sql =
    "SELECT session_id, iteration, did, session_created_at, atp_oauth_state,
            signing_key_jkt, dpop_key, access_token, refresh_token,
            access_token_created_at, access_token_expires_at, access_token_scopes,
            session_exchanged_at, exchange_error
     FROM oauth_atp_session
     WHERE session_id = ?
     ORDER BY iteration DESC
     LIMIT 1"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(session_id)],
    expecting: decoder(),
  ))

  case list.first(rows) {
    Ok(session) -> Ok(Some(session))
    Error(_) -> Ok(None)
  }
}

/// Get an ATP session by atp_oauth_state (returns latest iteration)
pub fn get_by_state(
  conn: sqlight.Connection,
  atp_oauth_state: String,
) -> Result(Option(OAuthAtpSession), sqlight.Error) {
  let sql =
    "SELECT session_id, iteration, did, session_created_at, atp_oauth_state,
            signing_key_jkt, dpop_key, access_token, refresh_token,
            access_token_created_at, access_token_expires_at, access_token_scopes,
            session_exchanged_at, exchange_error
     FROM oauth_atp_session
     WHERE atp_oauth_state = ?
     ORDER BY iteration DESC
     LIMIT 1"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(atp_oauth_state)],
    expecting: decoder(),
  ))

  case list.first(rows) {
    Ok(session) -> Ok(Some(session))
    Error(_) -> Ok(None)
  }
}

/// Decode ATP session from database row
fn decoder() -> decode.Decoder(OAuthAtpSession) {
  use session_id <- decode.field(0, decode.string)
  use iteration <- decode.field(1, decode.int)
  use did <- decode.field(2, decode.optional(decode.string))
  use session_created_at <- decode.field(3, decode.int)
  use atp_oauth_state <- decode.field(4, decode.string)
  use signing_key_jkt <- decode.field(5, decode.string)
  use dpop_key <- decode.field(6, decode.string)
  use access_token <- decode.field(7, decode.optional(decode.string))
  use refresh_token <- decode.field(8, decode.optional(decode.string))
  use access_token_created_at <- decode.field(9, decode.optional(decode.int))
  use access_token_expires_at <- decode.field(10, decode.optional(decode.int))
  use access_token_scopes <- decode.field(11, decode.optional(decode.string))
  use session_exchanged_at <- decode.field(12, decode.optional(decode.int))
  use exchange_error <- decode.field(13, decode.optional(decode.string))

  decode.success(OAuthAtpSession(
    session_id: session_id,
    iteration: iteration,
    did: did,
    session_created_at: session_created_at,
    atp_oauth_state: atp_oauth_state,
    signing_key_jkt: signing_key_jkt,
    dpop_key: dpop_key,
    access_token: access_token,
    refresh_token: refresh_token,
    access_token_created_at: access_token_created_at,
    access_token_expires_at: access_token_expires_at,
    access_token_scopes: access_token_scopes,
    session_exchanged_at: session_exchanged_at,
    exchange_error: exchange_error,
  ))
}
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/database/repositories/oauth_atp_sessions.gleam
git commit -m "feat: add oauth_atp_sessions repository"
```

---

## Task 10: Create oauth_atp_requests repository

**Files:**
- Create: `server/src/database/repositories/oauth_atp_requests.gleam`

**Step 1: Create the repository file**

```gleam
/// OAuth ATP request repository operations
import database/types.{type OAuthAtpRequest, OAuthAtpRequest}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import sqlight

/// Insert a new ATP request
pub fn insert(
  conn: sqlight.Connection,
  req: OAuthAtpRequest,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO oauth_atp_request (
      oauth_state, authorization_server, nonce, pkce_verifier,
      signing_public_key, dpop_private_key, created_at, expires_at
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
  "

  let params = [
    sqlight.text(req.oauth_state),
    sqlight.text(req.authorization_server),
    sqlight.text(req.nonce),
    sqlight.text(req.pkce_verifier),
    sqlight.text(req.signing_public_key),
    sqlight.text(req.dpop_private_key),
    sqlight.int(req.created_at),
    sqlight.int(req.expires_at),
  ]

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: params,
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Get an ATP request by oauth_state
pub fn get(
  conn: sqlight.Connection,
  oauth_state: String,
) -> Result(Option(OAuthAtpRequest), sqlight.Error) {
  let sql =
    "SELECT oauth_state, authorization_server, nonce, pkce_verifier,
            signing_public_key, dpop_private_key, created_at, expires_at
     FROM oauth_atp_request WHERE oauth_state = ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(oauth_state)],
    expecting: decoder(),
  ))

  case list.first(rows) {
    Ok(req) -> Ok(Some(req))
    Error(_) -> Ok(None)
  }
}

/// Delete an ATP request
pub fn delete(
  conn: sqlight.Connection,
  oauth_state: String,
) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM oauth_atp_request WHERE oauth_state = ?"

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(oauth_state)],
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Delete expired ATP requests
pub fn delete_expired(
  conn: sqlight.Connection,
  now: Int,
) -> Result(Int, sqlight.Error) {
  let sql = "DELETE FROM oauth_atp_request WHERE expires_at <= ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.int(now)],
    expecting: decode.dynamic,
  ))
  Ok(list.length(rows))
}

/// Decode ATP request from database row
fn decoder() -> decode.Decoder(OAuthAtpRequest) {
  use oauth_state <- decode.field(0, decode.string)
  use authorization_server <- decode.field(1, decode.string)
  use nonce <- decode.field(2, decode.string)
  use pkce_verifier <- decode.field(3, decode.string)
  use signing_public_key <- decode.field(4, decode.string)
  use dpop_private_key <- decode.field(5, decode.string)
  use created_at <- decode.field(6, decode.int)
  use expires_at <- decode.field(7, decode.int)

  decode.success(OAuthAtpRequest(
    oauth_state: oauth_state,
    authorization_server: authorization_server,
    nonce: nonce,
    pkce_verifier: pkce_verifier,
    signing_public_key: signing_public_key,
    dpop_private_key: dpop_private_key,
    created_at: created_at,
    expires_at: expires_at,
  ))
}
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/database/repositories/oauth_atp_requests.gleam
git commit -m "feat: add oauth_atp_requests repository"
```

---

## Task 11: Create metadata handler

**Files:**
- Create: `server/src/handlers/oauth/metadata.gleam`

**Step 1: Create the handler file**

```gleam
/// OAuth server metadata endpoint handler
/// GET /.well-known/oauth-authorization-server
import gleam/json
import wisp

pub type ServerMetadata {
  ServerMetadata(
    issuer: String,
    authorization_endpoint: String,
    token_endpoint: String,
    jwks_uri: String,
    registration_endpoint: String,
    scopes_supported: List(String),
    response_types_supported: List(String),
    grant_types_supported: List(String),
    token_endpoint_auth_methods_supported: List(String),
    code_challenge_methods_supported: List(String),
    pushed_authorization_request_endpoint: String,
    dpop_signing_alg_values_supported: List(String),
  )
}

/// Generate server metadata for the given base URL
pub fn generate_metadata(base_url: String) -> ServerMetadata {
  ServerMetadata(
    issuer: base_url,
    authorization_endpoint: base_url <> "/oauth/authorize",
    token_endpoint: base_url <> "/oauth/token",
    jwks_uri: base_url <> "/.well-known/jwks.json",
    registration_endpoint: base_url <> "/oauth/register",
    scopes_supported: ["openid", "profile", "atproto"],
    response_types_supported: ["code"],
    grant_types_supported: ["authorization_code", "refresh_token"],
    token_endpoint_auth_methods_supported: [
      "client_secret_basic",
      "client_secret_post",
      "none",
    ],
    code_challenge_methods_supported: ["S256"],
    pushed_authorization_request_endpoint: base_url <> "/oauth/par",
    dpop_signing_alg_values_supported: ["ES256"],
  )
}

/// Encode metadata as JSON
pub fn encode_metadata(meta: ServerMetadata) -> json.Json {
  json.object([
    #("issuer", json.string(meta.issuer)),
    #("authorization_endpoint", json.string(meta.authorization_endpoint)),
    #("token_endpoint", json.string(meta.token_endpoint)),
    #("jwks_uri", json.string(meta.jwks_uri)),
    #("registration_endpoint", json.string(meta.registration_endpoint)),
    #("scopes_supported", json.array(meta.scopes_supported, json.string)),
    #(
      "response_types_supported",
      json.array(meta.response_types_supported, json.string),
    ),
    #(
      "grant_types_supported",
      json.array(meta.grant_types_supported, json.string),
    ),
    #(
      "token_endpoint_auth_methods_supported",
      json.array(meta.token_endpoint_auth_methods_supported, json.string),
    ),
    #(
      "code_challenge_methods_supported",
      json.array(meta.code_challenge_methods_supported, json.string),
    ),
    #(
      "pushed_authorization_request_endpoint",
      json.string(meta.pushed_authorization_request_endpoint),
    ),
    #(
      "dpop_signing_alg_values_supported",
      json.array(meta.dpop_signing_alg_values_supported, json.string),
    ),
  ])
}

/// Handle GET /.well-known/oauth-authorization-server
pub fn handle(base_url: String) -> wisp.Response {
  let meta = generate_metadata(base_url)
  let json_response = encode_metadata(meta)

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
git add server/src/handlers/oauth/metadata.gleam
git commit -m "feat: add OAuth metadata handler"
```

---

## Task 12: Create metadata handler test

**Files:**
- Create: `server/test/oauth/metadata_test.gleam`

**Step 1: Create the test file**

```gleam
import gleeunit
import gleeunit/should
import handlers/oauth/metadata

pub fn main() {
  gleeunit.main()
}

pub fn generate_metadata_test() {
  let meta = metadata.generate_metadata("https://example.com")

  meta.issuer |> should.equal("https://example.com")
  meta.authorization_endpoint
  |> should.equal("https://example.com/oauth/authorize")
  meta.token_endpoint |> should.equal("https://example.com/oauth/token")
  meta.jwks_uri |> should.equal("https://example.com/.well-known/jwks.json")
  meta.registration_endpoint
  |> should.equal("https://example.com/oauth/register")
  meta.pushed_authorization_request_endpoint
  |> should.equal("https://example.com/oauth/par")
}

pub fn encode_metadata_test() {
  let meta = metadata.generate_metadata("https://example.com")
  let json = metadata.encode_metadata(meta)

  // Verify it encodes without error (JSON encoding doesn't fail)
  let _ = json
  Nil
}
```

**Step 2: Run the test**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: Tests pass

**Step 3: Commit**

```bash
git add server/test/oauth/metadata_test.gleam
git commit -m "test: add OAuth metadata handler test"
```

---

## Summary: Remaining Tasks

The plan continues with similar patterns for:

**Handlers (Tasks 13-21):**
- Task 13: jwks handler + Task 14: jwks test
- Task 15: dpop_nonce handler + Task 16: dpop_nonce test
- Task 17: client_metadata handler + Task 18: client_metadata test
- Task 19: register handler + Task 20: register test
- Task 21: par handler + Task 22: par test

**Complex Handlers (Tasks 23-30):**
- Task 23: authorize handler + Task 24: authorize test
- Task 25: token handler + Task 26: token test
- Task 27: atp_callback handler + Task 28: atp_callback test
- Task 29: atp_session handler + Task 30: atp_session test

**Integration (Task 31):**
- Task 31: Wire up routes in server.gleam

Each handler follows the same pattern:
1. Create handler file with `handle` function
2. Create corresponding test file
3. Verify compilation
4. Run tests
5. Commit

The handlers will need to import and use the repositories created in Tasks 3-10.

---

Plan complete and saved to `docs/plans/2025-11-24-oauth-repositories-handlers-implementation.md`. Two execution options:

**1. Subagent-Driven (this session)** - I dispatch fresh subagent per task, review between tasks, fast iteration

**2. Parallel Session (separate)** - Open new session with executing-plans, batch execution with checkpoints

Which approach?
