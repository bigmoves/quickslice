# OAuth Migration Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement the remaining 3 OAuth endpoints (`/oauth/token`, `/oauth/atp/callback`, `/api/atp/sessions/:id`) by migrating functionality from oauth_server to server.

**Architecture:** Two-layer OAuth bridge - our server acts as OAuth provider to clients, and as OAuth client to ATProto PDS servers. ATP callback exchanges ATP code for tokens, generates our authorization code; token endpoint exchanges our code for tokens.

**Tech Stack:** Gleam, wisp HTTP framework, sqlight database, httpc for outbound HTTP, gleeunit for testing.

---

## Task 1: Add CodeChallengeMethod Type

**Files:**
- Modify: `server/src/database/types.gleam`

**Step 1: Add CodeChallengeMethod type after TokenType (around line 111)**

```gleam
/// PKCE code challenge method
pub type CodeChallengeMethod {
  S256
  Plain
}

pub fn code_challenge_method_to_string(method: CodeChallengeMethod) -> String {
  case method {
    S256 -> "S256"
    Plain -> "plain"
  }
}

pub fn code_challenge_method_from_string(s: String) -> Option(CodeChallengeMethod) {
  case s {
    "S256" -> option.Some(S256)
    "plain" -> option.Some(Plain)
    _ -> option.None
  }
}
```

**Step 2: Verify compilation**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
cd /Users/chadmiller/code/quickslice/server
git add src/database/types.gleam
git commit -m "feat: add CodeChallengeMethod type for PKCE"
```

---

## Task 2: Add OAuthAuthorizationCode Type

**Files:**
- Modify: `server/src/database/types.gleam`

**Step 1: Add OAuthAuthorizationCode type at end of file**

```gleam
/// OAuth authorization code
pub type OAuthAuthorizationCode {
  OAuthAuthorizationCode(
    code: String,
    client_id: String,
    user_id: String,
    session_id: Option(String),
    redirect_uri: String,
    scope: Option(String),
    code_challenge: Option(String),
    code_challenge_method: Option(CodeChallengeMethod),
    nonce: Option(String),
    created_at: Int,
    expires_at: Int,
    used: Bool,
  )
}
```

**Step 2: Verify compilation**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
cd /Users/chadmiller/code/quickslice/server
git add src/database/types.gleam
git commit -m "feat: add OAuthAuthorizationCode type"
```

---

## Task 3: Write Authorization Code Repository Tests

**Files:**
- Create: `server/test/oauth/authorization_code_test.gleam`

**Step 1: Write the failing test**

```gleam
import database/repositories/oauth_authorization_code
import database/types.{OAuthAuthorizationCode, S256}
import gleam/option.{None, Some}
import gleeunit/should
import sqlight

pub fn insert_and_get_authorization_code_test() {
  let assert Ok(conn) = sqlight.open(":memory:")

  // Create table
  let assert Ok(_) = sqlight.exec(
    "CREATE TABLE oauth_authorization_code (
      code TEXT PRIMARY KEY,
      client_id TEXT NOT NULL,
      user_id TEXT NOT NULL,
      session_id TEXT,
      redirect_uri TEXT NOT NULL,
      scope TEXT,
      code_challenge TEXT,
      code_challenge_method TEXT,
      nonce TEXT,
      created_at INTEGER NOT NULL,
      expires_at INTEGER NOT NULL,
      used INTEGER NOT NULL DEFAULT 0
    )",
    conn,
  )

  let code = OAuthAuthorizationCode(
    code: "test-code-123",
    client_id: "client-1",
    user_id: "did:plc:test",
    session_id: Some("session-1"),
    redirect_uri: "http://localhost/callback",
    scope: Some("atproto"),
    code_challenge: Some("challenge"),
    code_challenge_method: Some(S256),
    nonce: Some("nonce-1"),
    created_at: 1000,
    expires_at: 2000,
    used: False,
  )

  let assert Ok(Nil) = oauth_authorization_code.insert(conn, code)
  let assert Ok(Some(retrieved)) = oauth_authorization_code.get(conn, "test-code-123")

  retrieved.code |> should.equal("test-code-123")
  retrieved.client_id |> should.equal("client-1")
  retrieved.used |> should.equal(False)
}

pub fn mark_code_used_test() {
  let assert Ok(conn) = sqlight.open(":memory:")

  let assert Ok(_) = sqlight.exec(
    "CREATE TABLE oauth_authorization_code (
      code TEXT PRIMARY KEY,
      client_id TEXT NOT NULL,
      user_id TEXT NOT NULL,
      session_id TEXT,
      redirect_uri TEXT NOT NULL,
      scope TEXT,
      code_challenge TEXT,
      code_challenge_method TEXT,
      nonce TEXT,
      created_at INTEGER NOT NULL,
      expires_at INTEGER NOT NULL,
      used INTEGER NOT NULL DEFAULT 0
    )",
    conn,
  )

  let code = OAuthAuthorizationCode(
    code: "test-code-456",
    client_id: "client-1",
    user_id: "did:plc:test",
    session_id: None,
    redirect_uri: "http://localhost/callback",
    scope: None,
    code_challenge: None,
    code_challenge_method: None,
    nonce: None,
    created_at: 1000,
    expires_at: 2000,
    used: False,
  )

  let assert Ok(Nil) = oauth_authorization_code.insert(conn, code)
  let assert Ok(Nil) = oauth_authorization_code.mark_used(conn, "test-code-456")
  let assert Ok(Some(retrieved)) = oauth_authorization_code.get(conn, "test-code-456")

  retrieved.used |> should.equal(True)
}

pub fn get_nonexistent_code_returns_none_test() {
  let assert Ok(conn) = sqlight.open(":memory:")

  let assert Ok(_) = sqlight.exec(
    "CREATE TABLE oauth_authorization_code (
      code TEXT PRIMARY KEY,
      client_id TEXT NOT NULL,
      user_id TEXT NOT NULL,
      session_id TEXT,
      redirect_uri TEXT NOT NULL,
      scope TEXT,
      code_challenge TEXT,
      code_challenge_method TEXT,
      nonce TEXT,
      created_at INTEGER NOT NULL,
      expires_at INTEGER NOT NULL,
      used INTEGER NOT NULL DEFAULT 0
    )",
    conn,
  )

  let assert Ok(None) = oauth_authorization_code.get(conn, "nonexistent")
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: FAIL - module `oauth_authorization_code` not found

---

## Task 4: Implement Authorization Code Repository

**Files:**
- Create: `server/src/database/repositories/oauth_authorization_code.gleam`

**Step 1: Write the implementation**

```gleam
/// OAuth authorization code repository operations
import database/types.{
  type CodeChallengeMethod, type OAuthAuthorizationCode, OAuthAuthorizationCode,
  Plain, S256,
}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import sqlight

/// Insert a new authorization code
pub fn insert(
  conn: sqlight.Connection,
  code: OAuthAuthorizationCode,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO oauth_authorization_code (
      code, client_id, user_id, session_id, redirect_uri, scope,
      code_challenge, code_challenge_method, nonce, created_at, expires_at, used
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  "

  let params = [
    sqlight.text(code.code),
    sqlight.text(code.client_id),
    sqlight.text(code.user_id),
    sqlight.nullable(sqlight.text, code.session_id),
    sqlight.text(code.redirect_uri),
    sqlight.nullable(sqlight.text, code.scope),
    sqlight.nullable(sqlight.text, code.code_challenge),
    sqlight.nullable(
      sqlight.text,
      option.map(code.code_challenge_method, types.code_challenge_method_to_string),
    ),
    sqlight.nullable(sqlight.text, code.nonce),
    sqlight.int(code.created_at),
    sqlight.int(code.expires_at),
    sqlight.bool(code.used),
  ]

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: params,
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Get an authorization code by code value
pub fn get(
  conn: sqlight.Connection,
  code_value: String,
) -> Result(Option(OAuthAuthorizationCode), sqlight.Error) {
  let sql =
    "SELECT code, client_id, user_id, session_id, redirect_uri, scope,
            code_challenge, code_challenge_method, nonce, created_at, expires_at, used
     FROM oauth_authorization_code WHERE code = ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(code_value)],
    expecting: decoder(),
  ))

  case list.first(rows) {
    Ok(code) -> Ok(Some(code))
    Error(_) -> Ok(None)
  }
}

/// Mark an authorization code as used
pub fn mark_used(
  conn: sqlight.Connection,
  code_value: String,
) -> Result(Nil, sqlight.Error) {
  let sql = "UPDATE oauth_authorization_code SET used = 1 WHERE code = ?"

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(code_value)],
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Delete an authorization code
pub fn delete(
  conn: sqlight.Connection,
  code_value: String,
) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM oauth_authorization_code WHERE code = ?"

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(code_value)],
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Delete expired authorization codes
pub fn delete_expired(
  conn: sqlight.Connection,
  now: Int,
) -> Result(Int, sqlight.Error) {
  let sql = "DELETE FROM oauth_authorization_code WHERE expires_at <= ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.int(now)],
    expecting: decode.dynamic,
  ))
  Ok(list.length(rows))
}

/// Decode authorization code from database row
fn decoder() -> decode.Decoder(OAuthAuthorizationCode) {
  use code <- decode.field(0, decode.string)
  use client_id <- decode.field(1, decode.string)
  use user_id <- decode.field(2, decode.string)
  use session_id <- decode.field(3, decode.optional(decode.string))
  use redirect_uri <- decode.field(4, decode.string)
  use scope <- decode.field(5, decode.optional(decode.string))
  use code_challenge <- decode.field(6, decode.optional(decode.string))
  use code_challenge_method_str <- decode.field(7, decode.optional(decode.string))
  use nonce <- decode.field(8, decode.optional(decode.string))
  use created_at <- decode.field(9, decode.int)
  use expires_at <- decode.field(10, decode.int)
  use used <- decode.field(11, decode.int)

  let code_challenge_method = case code_challenge_method_str {
    Some("S256") -> Some(S256)
    Some("plain") -> Some(Plain)
    _ -> None
  }

  decode.success(OAuthAuthorizationCode(
    code: code,
    client_id: client_id,
    user_id: user_id,
    session_id: session_id,
    redirect_uri: redirect_uri,
    scope: scope,
    code_challenge: code_challenge,
    code_challenge_method: code_challenge_method,
    nonce: nonce,
    created_at: created_at,
    expires_at: expires_at,
    used: used == 1,
  ))
}
```

**Step 2: Run tests to verify they pass**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: All 3 authorization code tests pass

**Step 3: Commit**

```bash
cd /Users/chadmiller/code/quickslice/server
git add src/database/repositories/oauth_authorization_code.gleam test/oauth/authorization_code_test.gleam
git commit -m "feat: add oauth_authorization_code repository with tests"
```

---

## Task 5: Create ATProto Bridge Module

**Files:**
- Create: `server/src/lib/oauth/atproto/bridge.gleam`

**Step 1: Create bridge module**

Port from `oauth_server/src/oauth_server/internal/atproto/bridge.gleam`. Key functions:
- `handle_callback()` - exchange ATP code for tokens
- `fetch_protected_resource_metadata()` - get PDS metadata
- `fetch_authorization_server_metadata()` - get AS metadata
- `exchange_code_for_tokens()` - token exchange with DPoP
- `fetch_tokens_with_nonce()` - retry with DPoP nonce

**Reference:** Read `oauth_server/src/oauth_server/internal/atproto/bridge.gleam` (852 lines) and adapt:
- Replace `Storage` with `sqlight.Connection` + direct repository calls
- Import from `lib/oauth/` instead of `oauth_server/internal/`
- Keep `did_cache` Subject pattern

**Step 2: Verify compilation**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
cd /Users/chadmiller/code/quickslice/server
git add src/lib/oauth/atproto/bridge.gleam
git commit -m "feat: add ATProto bridge module"
```

---

## Task 6: Write ATP Callback Handler Tests

**Files:**
- Modify: `server/test/oauth/atp_callback_test.gleam`

**Step 1: Write the failing tests**

```gleam
import gleam/http
import gleeunit/should
import handlers/oauth/atp_callback
import lib/oauth/did_cache
import gleam/option
import sqlight
import wisp/testing as simulate

pub fn atp_callback_missing_code_returns_400_test() {
  let assert Ok(cache) = did_cache.start()
  let assert Ok(conn) = sqlight.open(":memory:")

  let req = simulate.get("/oauth/atp/callback?state=test-state", [])
  let response = atp_callback.handle(
    req,
    conn,
    cache,
    "http://localhost:8080/oauth/atp/callback",
    "test-client-id",
    option.None,
  )

  response.status |> should.equal(400)
}

pub fn atp_callback_missing_state_returns_400_test() {
  let assert Ok(cache) = did_cache.start()
  let assert Ok(conn) = sqlight.open(":memory:")

  let req = simulate.get("/oauth/atp/callback?code=test-code", [])
  let response = atp_callback.handle(
    req,
    conn,
    cache,
    "http://localhost:8080/oauth/atp/callback",
    "test-client-id",
    option.None,
  )

  response.status |> should.equal(400)
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: FAIL - handler still returns 501

---

## Task 7: Implement ATP Callback Handler

**Files:**
- Modify: `server/src/handlers/oauth/atp_callback.gleam`

**Step 1: Implement the handler**

**Reference:** Read `oauth_server/src/oauth_server/internal/endpoints/atp_callback.gleam` (207 lines).

Flow:
1. Parse query params: `code`, `state`
2. Get `OAuthAtpSession` by `atp_oauth_state` using `oauth_atp_sessions.get_by_state()`
3. Get `OAuthAtpRequest` to get `pkce_verifier` using `oauth_atp_requests.get()`
4. Get DID from session, resolve to PDS endpoint
5. Call `bridge.handle_callback()` to exchange ATP code for tokens
6. Get `OAuthAuthRequest` by `session_id` using `oauth_auth_requests.get()`
7. Generate authorization code using `token_generator.generate_authorization_code()`
8. Store `OAuthAuthorizationCode` using `oauth_authorization_code.insert()`
9. Delete one-time-use requests
10. Redirect to client with `code` and `state`

**Step 2: Run tests to verify they pass**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: ATP callback tests pass (400 for missing params)

**Step 3: Commit**

```bash
cd /Users/chadmiller/code/quickslice/server
git add src/handlers/oauth/atp_callback.gleam test/oauth/atp_callback_test.gleam
git commit -m "feat: implement ATP callback handler"
```

---

## Task 8: Write Token Endpoint Tests

**Files:**
- Modify: `server/test/oauth/token_test.gleam`

**Step 1: Write the failing tests**

```gleam
import gleam/http
import gleeunit/should
import handlers/oauth/token
import sqlight
import wisp/testing as simulate

pub fn token_missing_grant_type_returns_400_test() {
  let assert Ok(conn) = sqlight.open(":memory:")

  let req =
    simulate.post("/oauth/token", [#("content-type", "application/x-www-form-urlencoded")], "client_id=test")

  let response = token.handle(req, conn)
  response.status |> should.equal(400)
}

pub fn token_unsupported_grant_type_returns_400_test() {
  let assert Ok(conn) = sqlight.open(":memory:")

  let req =
    simulate.post("/oauth/token", [#("content-type", "application/x-www-form-urlencoded")], "grant_type=password&client_id=test")

  let response = token.handle(req, conn)
  response.status |> should.equal(400)
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: FAIL - handler returns 501

---

## Task 9: Implement Token Endpoint Handler

**Files:**
- Modify: `server/src/handlers/oauth/token.gleam`

**Step 1: Implement the handler**

**Reference:** Read `oauth_server/src/oauth_server/internal/endpoints/token.gleam` (509 lines).

Flow for `authorization_code` grant:
1. Parse form body
2. Get client using `oauth_clients.get()`
3. Get authorization code using `oauth_authorization_code.get()`
4. Validate: not used, not expired, client_id matches, redirect_uri matches
5. Verify PKCE using `pkce.verify_code_challenge()`
6. Mark code used using `oauth_authorization_code.mark_used()`
7. Generate tokens using `token_generator.generate_access_token()` / `generate_refresh_token()`
8. Store tokens using `oauth_access_tokens.insert()` / `oauth_refresh_tokens.insert()`
9. Return JSON response

Flow for `refresh_token` grant:
1. Get refresh token using `oauth_refresh_tokens.get()`
2. Validate: not revoked, not expired, client_id matches
3. Revoke old token using `oauth_refresh_tokens.revoke()`
4. Generate new tokens
5. Store and return

**Step 2: Run tests to verify they pass**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: Token endpoint tests pass

**Step 3: Commit**

```bash
cd /Users/chadmiller/code/quickslice/server
git add src/handlers/oauth/token.gleam test/oauth/token_test.gleam
git commit -m "feat: implement token endpoint handler"
```

---

## Task 10: Write ATP Session API Tests

**Files:**
- Modify: `server/test/oauth/atp_session_test.gleam`

**Step 1: Write the failing tests**

```gleam
import gleam/http
import gleeunit/should
import handlers/oauth/atp_session
import sqlight
import wisp/testing as simulate

pub fn atp_session_missing_auth_returns_401_test() {
  let assert Ok(conn) = sqlight.open(":memory:")

  let req = simulate.get("/api/atp/sessions/test-session", [])
  let response = atp_session.handle(req, conn, "test-session")

  response.status |> should.equal(401)
}

pub fn atp_session_invalid_token_returns_401_test() {
  let assert Ok(conn) = sqlight.open(":memory:")

  let req = simulate.get("/api/atp/sessions/test-session", [#("authorization", "Bearer invalid-token")])
  let response = atp_session.handle(req, conn, "test-session")

  response.status |> should.equal(401)
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: FAIL - handler returns 501

---

## Task 11: Implement ATP Session API Handler

**Files:**
- Modify: `server/src/handlers/oauth/atp_session.gleam`

**Step 1: Implement the handler**

**Reference:** Read `oauth_server/src/oauth_server/internal/endpoints/atp_session.gleam` (105 lines).

Flow:
1. Extract Bearer token from Authorization header
2. Get access token using `oauth_access_tokens.get()`
3. Validate: not revoked, not expired
4. Verify token's `session_id` matches requested session
5. Get ATP session using `oauth_atp_sessions.get_latest()`
6. Return JSON with: session_id, did, access_token, refresh_token, dpop_key, expires_at, scopes, created_at

**Step 2: Run tests to verify they pass**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: ATP session API tests pass

**Step 3: Commit**

```bash
cd /Users/chadmiller/code/quickslice/server
git add src/handlers/oauth/atp_session.gleam test/oauth/atp_session_test.gleam
git commit -m "feat: implement ATP session API handler"
```

---

## Task 12: Add Database Migration

**Files:**
- Add migration for `oauth_authorization_code` table

**Step 1: Add migration SQL**

```sql
CREATE TABLE IF NOT EXISTS oauth_authorization_code (
  code TEXT PRIMARY KEY,
  client_id TEXT NOT NULL,
  user_id TEXT NOT NULL,
  session_id TEXT,
  redirect_uri TEXT NOT NULL,
  scope TEXT,
  code_challenge TEXT,
  code_challenge_method TEXT,
  nonce TEXT,
  created_at INTEGER NOT NULL,
  expires_at INTEGER NOT NULL,
  used INTEGER NOT NULL DEFAULT 0
);

CREATE INDEX IF NOT EXISTS idx_oauth_authorization_code_expires ON oauth_authorization_code(expires_at);
```

**Step 2: Run migration**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam run -- migrate`
Expected: Migration succeeds

**Step 3: Commit**

```bash
cd /Users/chadmiller/code/quickslice/server
git add <migration-file>
git commit -m "feat: add oauth_authorization_code table migration"
```

---

## Task 13: Run Full Test Suite

**Step 1: Run all tests**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: All tests pass

**Step 2: Run build**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds with no warnings

**Step 3: Final commit if needed**

```bash
cd /Users/chadmiller/code/quickslice/server
git status
# If any uncommitted changes:
git add -A
git commit -m "chore: cleanup after OAuth migration"
```

---

## Key Reference Files

**Source (oauth_server) - read for detailed logic:**
- `oauth_server/src/oauth_server/internal/endpoints/token.gleam` (509 lines)
- `oauth_server/src/oauth_server/internal/endpoints/atp_callback.gleam` (207 lines)
- `oauth_server/src/oauth_server/internal/endpoints/atp_session.gleam` (105 lines)
- `oauth_server/src/oauth_server/internal/atproto/bridge.gleam` (852 lines)

**Target (server):**
- `server/src/handlers/oauth/token.gleam`
- `server/src/handlers/oauth/atp_callback.gleam`
- `server/src/handlers/oauth/atp_session.gleam`
- `server/src/lib/oauth/atproto/bridge.gleam`
- `server/src/database/repositories/oauth_authorization_code.gleam`

**Existing repositories to use:**
- `oauth_atp_sessions.get_by_state()` - lookup by ATP state
- `oauth_atp_sessions.get_latest()` - get latest iteration
- `oauth_atp_requests.get()` - get PKCE verifier
- `oauth_auth_requests.get()` - get client's original request
- `oauth_clients.get()` - get client config
- `oauth_access_tokens.insert()` / `get()`
- `oauth_refresh_tokens.insert()` / `get()` / `revoke()`
