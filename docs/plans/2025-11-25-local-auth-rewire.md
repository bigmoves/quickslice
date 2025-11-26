# Local Auth Rewire Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace external auth service calls with local database lookups for token validation and ATP session retrieval.

**Architecture:** Token validation looks up `oauth_access_token` table locally, then fetches linked `oauth_atp_session`. If ATP token expired, refresh synchronously via existing `bridge.refresh_tokens`. PDS endpoint resolved from DID document via existing `did_resolver`.

**Tech Stack:** Gleam, sqlight, existing OAuth repositories and bridge module

---

### Task 1: Add New Error Variants to AuthError

**Files:**
- Modify: `server/src/atproto_auth.gleam:19-26`

**Step 1: Update the AuthError type**

Replace the existing AuthError type with expanded variants:

```gleam
/// Error type for authentication operations
pub type AuthError {
  MissingAuthHeader
  InvalidAuthHeader
  UnauthorizedToken
  TokenExpired
  SessionNotFound
  SessionNotReady
  RefreshFailed(String)
  DIDResolutionFailed(String)
  NetworkError
  ParseError
}
```

**Step 2: Run build to verify no syntax errors**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds (no tests depend on AuthError variants yet)

**Step 3: Commit**

```bash
git add server/src/atproto_auth.gleam
git commit -m "feat(auth): add new AuthError variants for local auth"
```

---

### Task 2: Add verify_token Function

**Files:**
- Modify: `server/src/atproto_auth.gleam` (add imports and new function)
- Test: `server/test/atproto_auth_test.gleam` (create new)

**Step 1: Write failing test for verify_token**

Create new test file `server/test/atproto_auth_test.gleam`:

```gleam
import atproto_auth
import database/repositories/oauth_access_tokens
import database/types.{OAuthAccessToken, Bearer}
import gleam/option.{None, Some}
import gleeunit/should
import sqlight

fn setup_tables(conn: sqlight.Connection) {
  let assert Ok(_) =
    sqlight.exec(
      "CREATE TABLE oauth_access_token (
        token TEXT PRIMARY KEY,
        token_type TEXT NOT NULL,
        client_id TEXT NOT NULL,
        user_id TEXT,
        session_id TEXT,
        session_iteration INTEGER,
        scope TEXT,
        created_at INTEGER NOT NULL,
        expires_at INTEGER NOT NULL,
        revoked INTEGER NOT NULL DEFAULT 0,
        dpop_jkt TEXT
      )",
      conn,
    )
  Nil
}

pub fn verify_token_valid_returns_user_info_test() {
  let assert Ok(conn) = sqlight.open(":memory:")
  setup_tables(conn)

  // Insert a valid token
  let token = OAuthAccessToken(
    token: "valid-token",
    token_type: Bearer,
    client_id: "test-client",
    user_id: Some("did:plc:testuser123"),
    session_id: Some("session-123"),
    session_iteration: Some(1),
    scope: Some("atproto"),
    created_at: 1000,
    expires_at: 9_999_999_999,
    revoked: False,
    dpop_jkt: None,
  )
  let assert Ok(_) = oauth_access_tokens.insert(conn, token)

  let result = atproto_auth.verify_token(conn, "valid-token")

  result |> should.be_ok
  let assert Ok(user_info) = result
  user_info.did |> should.equal("did:plc:testuser123")
}

pub fn verify_token_not_found_returns_error_test() {
  let assert Ok(conn) = sqlight.open(":memory:")
  setup_tables(conn)

  let result = atproto_auth.verify_token(conn, "nonexistent-token")

  result |> should.be_error
  let assert Error(err) = result
  err |> should.equal(atproto_auth.UnauthorizedToken)
}

pub fn verify_token_revoked_returns_error_test() {
  let assert Ok(conn) = sqlight.open(":memory:")
  setup_tables(conn)

  let token = OAuthAccessToken(
    token: "revoked-token",
    token_type: Bearer,
    client_id: "test-client",
    user_id: Some("did:plc:testuser123"),
    session_id: Some("session-123"),
    session_iteration: Some(1),
    scope: Some("atproto"),
    created_at: 1000,
    expires_at: 9_999_999_999,
    revoked: True,
    dpop_jkt: None,
  )
  let assert Ok(_) = oauth_access_tokens.insert(conn, token)

  let result = atproto_auth.verify_token(conn, "revoked-token")

  result |> should.be_error
  let assert Error(err) = result
  err |> should.equal(atproto_auth.UnauthorizedToken)
}

pub fn verify_token_expired_returns_error_test() {
  let assert Ok(conn) = sqlight.open(":memory:")
  setup_tables(conn)

  let token = OAuthAccessToken(
    token: "expired-token",
    token_type: Bearer,
    client_id: "test-client",
    user_id: Some("did:plc:testuser123"),
    session_id: Some("session-123"),
    session_iteration: Some(1),
    scope: Some("atproto"),
    created_at: 1000,
    expires_at: 1,  // Already expired
    revoked: False,
    dpop_jkt: None,
  )
  let assert Ok(_) = oauth_access_tokens.insert(conn, token)

  let result = atproto_auth.verify_token(conn, "expired-token")

  result |> should.be_error
  let assert Error(err) = result
  err |> should.equal(atproto_auth.TokenExpired)
}

pub fn verify_token_no_user_id_returns_error_test() {
  let assert Ok(conn) = sqlight.open(":memory:")
  setup_tables(conn)

  let token = OAuthAccessToken(
    token: "no-user-token",
    token_type: Bearer,
    client_id: "test-client",
    user_id: None,
    session_id: Some("session-123"),
    session_iteration: Some(1),
    scope: Some("atproto"),
    created_at: 1000,
    expires_at: 9_999_999_999,
    revoked: False,
    dpop_jkt: None,
  )
  let assert Ok(_) = oauth_access_tokens.insert(conn, token)

  let result = atproto_auth.verify_token(conn, "no-user-token")

  result |> should.be_error
  let assert Error(err) = result
  err |> should.equal(atproto_auth.UnauthorizedToken)
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: FAIL - `verify_token` function not defined

**Step 3: Add imports to atproto_auth.gleam**

Add at top of `server/src/atproto_auth.gleam` after existing imports:

```gleam
import database/repositories/oauth_access_tokens
import lib/oauth/token_generator
import sqlight
```

**Step 4: Implement verify_token function**

Add to `server/src/atproto_auth.gleam` after `extract_bearer_token` function:

```gleam
/// Verify token from local database and return user info
pub fn verify_token(
  conn: sqlight.Connection,
  token: String,
) -> Result(UserInfo, AuthError) {
  // Look up token in database
  case oauth_access_tokens.get(conn, token) {
    Error(_) -> Error(UnauthorizedToken)
    Ok(None) -> Error(UnauthorizedToken)
    Ok(Some(access_token)) -> {
      // Check if revoked
      case access_token.revoked {
        True -> Error(UnauthorizedToken)
        False -> {
          // Check if expired
          let now = token_generator.current_timestamp()
          case access_token.expires_at < now {
            True -> Error(TokenExpired)
            False -> {
              // Check user_id is present
              case access_token.user_id {
                None -> Error(UnauthorizedToken)
                Some(did) -> Ok(UserInfo(sub: did, did: did))
              }
            }
          }
        }
      }
    }
  }
}
```

**Step 5: Run tests to verify they pass**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: All `verify_token` tests PASS

**Step 6: Commit**

```bash
git add server/src/atproto_auth.gleam server/test/atproto_auth_test.gleam
git commit -m "feat(auth): add verify_token for local token lookup"
```

---

### Task 3: Add get_atp_session Function

**Files:**
- Modify: `server/src/atproto_auth.gleam` (add imports and new function)
- Modify: `server/test/atproto_auth_test.gleam` (add tests)

**Step 1: Write failing test for get_atp_session (session not found case)**

Add to `server/test/atproto_auth_test.gleam`:

```gleam
import gleam/erlang/process
import lib/oauth/did_cache

fn setup_atp_session_table(conn: sqlight.Connection) {
  let assert Ok(_) =
    sqlight.exec(
      "CREATE TABLE oauth_atp_session (
        session_id TEXT NOT NULL,
        iteration INTEGER NOT NULL,
        did TEXT,
        session_created_at INTEGER NOT NULL,
        atp_oauth_state TEXT NOT NULL,
        signing_key_jkt TEXT NOT NULL,
        dpop_key TEXT NOT NULL,
        access_token TEXT,
        refresh_token TEXT,
        access_token_created_at INTEGER,
        access_token_expires_at INTEGER,
        access_token_scopes TEXT,
        session_exchanged_at INTEGER,
        exchange_error TEXT,
        PRIMARY KEY (session_id, iteration)
      )",
      conn,
    )
  Nil
}

pub fn get_atp_session_no_session_id_returns_error_test() {
  let assert Ok(conn) = sqlight.open(":memory:")
  setup_tables(conn)
  setup_atp_session_table(conn)

  // Token without session_id
  let token = OAuthAccessToken(
    token: "no-session-token",
    token_type: Bearer,
    client_id: "test-client",
    user_id: Some("did:plc:testuser123"),
    session_id: None,
    session_iteration: None,
    scope: Some("atproto"),
    created_at: 1000,
    expires_at: 9_999_999_999,
    revoked: False,
    dpop_jkt: None,
  )
  let assert Ok(_) = oauth_access_tokens.insert(conn, token)

  let assert Ok(cache) = did_cache.start()

  let result = atproto_auth.get_atp_session(
    conn,
    cache,
    "no-session-token",
    None,
  )

  result |> should.be_error
  let assert Error(err) = result
  err |> should.equal(atproto_auth.SessionNotFound)
}

pub fn get_atp_session_not_exchanged_returns_error_test() {
  let assert Ok(conn) = sqlight.open(":memory:")
  setup_tables(conn)
  setup_atp_session_table(conn)

  // Token with session_id
  let token = OAuthAccessToken(
    token: "has-session-token",
    token_type: Bearer,
    client_id: "test-client",
    user_id: Some("did:plc:testuser123"),
    session_id: Some("session-123"),
    session_iteration: Some(1),
    scope: Some("atproto"),
    created_at: 1000,
    expires_at: 9_999_999_999,
    revoked: False,
    dpop_jkt: None,
  )
  let assert Ok(_) = oauth_access_tokens.insert(conn, token)

  // ATP session that hasn't been exchanged yet
  let assert Ok(_) =
    sqlight.exec(
      "INSERT INTO oauth_atp_session (
        session_id, iteration, did, session_created_at, atp_oauth_state,
        signing_key_jkt, dpop_key, access_token, refresh_token,
        session_exchanged_at, exchange_error
      ) VALUES (
        'session-123', 1, 'did:plc:testuser123', 1000, 'state-123',
        'jkt-123', '{\"kty\":\"EC\"}', NULL, NULL, NULL, NULL
      )",
      conn,
    )

  let assert Ok(cache) = did_cache.start()

  let result = atproto_auth.get_atp_session(
    conn,
    cache,
    "has-session-token",
    None,
  )

  result |> should.be_error
  let assert Error(err) = result
  err |> should.equal(atproto_auth.SessionNotReady)
}

pub fn get_atp_session_with_exchange_error_returns_error_test() {
  let assert Ok(conn) = sqlight.open(":memory:")
  setup_tables(conn)
  setup_atp_session_table(conn)

  let token = OAuthAccessToken(
    token: "error-session-token",
    token_type: Bearer,
    client_id: "test-client",
    user_id: Some("did:plc:testuser123"),
    session_id: Some("session-456"),
    session_iteration: Some(1),
    scope: Some("atproto"),
    created_at: 1000,
    expires_at: 9_999_999_999,
    revoked: False,
    dpop_jkt: None,
  )
  let assert Ok(_) = oauth_access_tokens.insert(conn, token)

  // ATP session with exchange error
  let assert Ok(_) =
    sqlight.exec(
      "INSERT INTO oauth_atp_session (
        session_id, iteration, did, session_created_at, atp_oauth_state,
        signing_key_jkt, dpop_key, access_token, refresh_token,
        session_exchanged_at, exchange_error
      ) VALUES (
        'session-456', 1, 'did:plc:testuser123', 1000, 'state-456',
        'jkt-456', '{\"kty\":\"EC\"}', 'token', 'refresh', 1000, 'exchange failed'
      )",
      conn,
    )

  let assert Ok(cache) = did_cache.start()

  let result = atproto_auth.get_atp_session(
    conn,
    cache,
    "error-session-token",
    None,
  )

  result |> should.be_error
  let assert Error(err) = result
  err |> should.equal(atproto_auth.SessionNotReady)
}
```

**Step 2: Run tests to verify they fail**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: FAIL - `get_atp_session` function not defined

**Step 3: Add additional imports to atproto_auth.gleam**

Add to imports in `server/src/atproto_auth.gleam`:

```gleam
import database/repositories/oauth_atp_sessions
import gleam/erlang/process.{type Subject}
import gleam/option.{type Option, None, Some}
import lib/oauth/atproto/bridge
import lib/oauth/atproto/did_resolver
import lib/oauth/did_cache
```

**Step 4: Implement get_atp_session function**

Add to `server/src/atproto_auth.gleam` after `verify_token`:

```gleam
/// Get ATP session from local database, refreshing if needed
pub fn get_atp_session(
  conn: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
  token: String,
  signing_key: Option(String),
) -> Result(AtprotoSession, AuthError) {
  // Look up access token to get session_id and iteration
  use access_token <- result.try(case oauth_access_tokens.get(conn, token) {
    Error(_) -> Error(UnauthorizedToken)
    Ok(None) -> Error(UnauthorizedToken)
    Ok(Some(t)) -> Ok(t)
  })

  // Get session_id and iteration
  use #(session_id, iteration) <- result.try(case
    #(access_token.session_id, access_token.session_iteration)
  {
    #(Some(sid), Some(iter)) -> Ok(#(sid, iter))
    _ -> Error(SessionNotFound)
  })

  // Look up ATP session
  use atp_session <- result.try(
    case oauth_atp_sessions.get(conn, session_id, iteration) {
      Error(_) -> Error(SessionNotFound)
      Ok(None) -> Error(SessionNotFound)
      Ok(Some(s)) -> Ok(s)
    },
  )

  // Validate session is ready (exchanged, no error, has access token)
  use _ <- result.try(case atp_session.exchange_error {
    Some(_) -> Error(SessionNotReady)
    None -> Ok(Nil)
  })

  use _ <- result.try(case atp_session.session_exchanged_at {
    None -> Error(SessionNotReady)
    Some(_) -> Ok(Nil)
  })

  use atp_access_token <- result.try(case atp_session.access_token {
    None -> Error(SessionNotReady)
    Some(t) -> Ok(t)
  })

  // Check if ATP token is expired and refresh if needed
  let now = token_generator.current_timestamp()
  use current_session <- result.try(case atp_session.access_token_expires_at {
    Some(expires_at) if expires_at < now -> {
      // Token expired, try to refresh
      case
        bridge.refresh_tokens(
          conn,
          did_cache,
          atp_session,
          access_token.client_id,
          signing_key,
        )
      {
        Ok(refreshed) -> Ok(refreshed)
        Error(err) -> Error(RefreshFailed(string.inspect(err)))
      }
    }
    _ -> Ok(atp_session)
  })

  // Get the (possibly refreshed) access token
  use final_access_token <- result.try(case current_session.access_token {
    None -> Error(SessionNotReady)
    Some(t) -> Ok(t)
  })

  // Get DID from session
  use did <- result.try(case current_session.did {
    None -> Error(SessionNotFound)
    Some(d) -> Ok(d)
  })

  // Resolve DID to get PDS endpoint
  use did_doc <- result.try(
    did_resolver.resolve_did_with_cache(did_cache, did, False)
    |> result.map_error(fn(err) { DIDResolutionFailed(string.inspect(err)) }),
  )

  use pds_endpoint <- result.try(case did_resolver.get_pds_endpoint(did_doc) {
    None -> Error(DIDResolutionFailed("No PDS endpoint in DID document"))
    Some(endpoint) -> Ok(endpoint)
  })

  Ok(AtprotoSession(
    pds_endpoint: pds_endpoint,
    access_token: final_access_token,
    dpop_jwk: current_session.dpop_key,
  ))
}
```

**Step 5: Run tests to verify error cases pass**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: SessionNotFound and SessionNotReady tests PASS (DID resolution tests will fail without network mocking, which is expected)

**Step 6: Commit**

```bash
git add server/src/atproto_auth.gleam server/test/atproto_auth_test.gleam
git commit -m "feat(auth): add get_atp_session for local session lookup"
```

---

### Task 4: Update MutationContext Type

**Files:**
- Modify: `server/src/mutation_resolvers.gleam:26-34`

**Step 1: Update MutationContext type definition**

Replace the MutationContext type in `server/src/mutation_resolvers.gleam`:

```gleam
import gleam/erlang/process.{type Subject}
import lib/oauth/did_cache

/// Context for mutation execution
pub type MutationContext {
  MutationContext(
    db: sqlight.Connection,
    did_cache: Subject(did_cache.Message),
    signing_key: option.Option(String),
    plc_url: String,
    collection_ids: List(String),
    external_collection_ids: List(String),
  )
}
```

**Step 2: Run build to see what breaks**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: FAIL - MutationContext construction sites need updating

**Step 3: Commit (partial, will fix construction sites next)**

```bash
git add server/src/mutation_resolvers.gleam
git commit -m "refactor(auth): update MutationContext type for local auth"
```

---

### Task 5: Update MutationContext Construction Site

**Files:**
- Modify: `server/src/graphql_gleam.gleam:36-41` (function signature)
- Modify: `server/src/graphql_gleam.gleam:376-382` (construction)

**Step 1: Update build_schema_from_db signature**

In `server/src/graphql_gleam.gleam`, update the function signature (around line 36):

```gleam
import gleam/erlang/process.{type Subject}
import lib/oauth/did_cache

/// Build a GraphQL schema from database lexicons
pub fn build_schema_from_db(
  db: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
  signing_key: option.Option(String),
  plc_url: String,
  domain_authority: String,
) -> Result(schema.Schema, String) {
```

**Step 2: Update MutationContext construction**

In `server/src/graphql_gleam.gleam`, update the construction (around line 376):

```gleam
      // Step 5: Create mutation resolver factories
      let mutation_ctx =
        mutation_resolvers.MutationContext(
          db: db,
          did_cache: did_cache,
          signing_key: signing_key,
          plc_url: plc_url,
          collection_ids: collection_ids,
          external_collection_ids: external_collection_ids,
        )
```

**Step 3: Update execute_query_with_db signature and call**

In `server/src/graphql_gleam.gleam`, update (around line 465):

```gleam
pub fn execute_query_with_db(
  db: sqlight.Connection,
  query_string: String,
  variables_json_str: String,
  auth_token: Result(String, Nil),
  did_cache: Subject(did_cache.Message),
  signing_key: option.Option(String),
  plc_url: String,
) -> Result(String, String) {
```

And update the call to `build_schema_from_db` (around line 481):

```gleam
  use graphql_schema <- result.try(build_schema_from_db(
    db,
    did_cache,
    signing_key,
    plc_url,
    domain_authority,
  ))
```

**Step 4: Run build to see remaining breaks**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: FAIL - callers of execute_query_with_db need updating

**Step 5: Commit**

```bash
git add server/src/graphql_gleam.gleam
git commit -m "refactor(auth): update graphql_gleam for local auth context"
```

---

### Task 6: Update GraphQL Handler

**Files:**
- Modify: `server/src/handlers/graphql.gleam:22-27` (function signature)
- Modify: `server/src/handlers/graphql.gleam:95-101` (inner function)
- Modify: `server/src/handlers/graphql.gleam:105-112` (call site)

**Step 1: Update handle_graphql_request signature**

In `server/src/handlers/graphql.gleam`:

```gleam
import gleam/erlang/process.{type Subject}
import lib/oauth/did_cache

pub fn handle_graphql_request(
  req: wisp.Request,
  db: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
  signing_key: option.Option(String),
  plc_url: String,
) -> wisp.Response {
  case req.method {
    http.Post -> handle_graphql_post(req, db, did_cache, signing_key, plc_url)
    http.Get -> handle_graphql_get(req, db, did_cache, signing_key, plc_url)
    _ -> method_not_allowed_response()
  }
}
```

**Step 2: Update handle_graphql_post and handle_graphql_get**

Update internal functions to pass through new parameters.

**Step 3: Update execute_graphql_query**

```gleam
fn execute_graphql_query(
  db: sqlight.Connection,
  query: String,
  variables_json_str: String,
  auth_token: Result(String, Nil),
  did_cache: Subject(did_cache.Message),
  signing_key: option.Option(String),
  plc_url: String,
) -> wisp.Response {
  case
    graphql_gleam.execute_query_with_db(
      db,
      query,
      variables_json_str,
      auth_token,
      did_cache,
      signing_key,
      plc_url,
    )
  {
    Ok(result_json) -> success_response(result_json)
    Error(err) -> internal_error_response(err)
  }
}
```

**Step 4: Run build**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: FAIL - server.gleam call site needs updating

**Step 5: Commit**

```bash
git add server/src/handlers/graphql.gleam
git commit -m "refactor(auth): update graphql handler for local auth"
```

---

### Task 7: Update Server Router

**Files:**
- Modify: `server/src/server.gleam:517-522`

**Step 1: Update handle_graphql_request call**

In `server/src/server.gleam`, update the graphql route handler:

```gleam
    ["graphql"] ->
      graphql_handler.handle_graphql_request(
        req,
        ctx.db,
        ctx.did_cache,
        ctx.oauth_signing_key,
        ctx.plc_url,
      )
```

**Step 2: Run build**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build should succeed (or show remaining issues to fix)

**Step 3: Commit**

```bash
git add server/src/server.gleam
git commit -m "refactor(auth): wire did_cache and signing_key through server router"
```

---

### Task 8: Update GraphQL WebSocket Handler

**Files:**
- Modify: `server/src/handlers/graphql_ws.gleam:195-199`

**Step 1: Update build_schema_from_db call**

Find the WebSocket handler's call to `build_schema_from_db` and update it to pass `did_cache` and `signing_key`.

**Step 2: Run build**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add server/src/handlers/graphql_ws.gleam
git commit -m "refactor(auth): update websocket handler for local auth"
```

---

### Task 9: Update Mutation Resolvers to Use New Auth Functions

**Files:**
- Modify: `server/src/mutation_resolvers.gleam` (4 resolver factories)

**Step 1: Update imports in mutation_resolvers.gleam**

Ensure these imports are present:

```gleam
import atproto_auth
import gleam/erlang/process.{type Subject}
import lib/oauth/did_cache
```

**Step 2: Update create_resolver_factory auth calls**

Replace the auth pattern (appears around lines 97-134):

Old:
```gleam
      atproto_auth.verify_oauth_token(token, ctx.auth_base_url)
      ...
      atproto_auth.get_atproto_session(token, ctx.auth_base_url)
```

New:
```gleam
      atproto_auth.verify_token(ctx.db, token)
      |> result.map_error(fn(err) {
        case err {
          atproto_auth.UnauthorizedToken -> "Unauthorized"
          atproto_auth.TokenExpired -> "Token expired"
          atproto_auth.MissingAuthHeader -> "Missing authentication"
          atproto_auth.InvalidAuthHeader -> "Invalid authentication header"
          _ -> "Authentication error"
        }
      })
      ...
      atproto_auth.get_atp_session(ctx.db, ctx.did_cache, token, ctx.signing_key)
      |> result.map_error(fn(err) {
        case err {
          atproto_auth.SessionNotFound -> "Session not found"
          atproto_auth.SessionNotReady -> "Session not ready"
          atproto_auth.RefreshFailed(msg) -> "Token refresh failed: " <> msg
          atproto_auth.DIDResolutionFailed(msg) -> "DID resolution failed: " <> msg
          _ -> "Failed to get ATP session"
        }
      })
```

**Step 3: Repeat for update_resolver_factory, delete_resolver_factory, upload_blob_resolver_factory**

Apply the same pattern to all 4 resolver factories.

**Step 4: Run build**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 5: Run tests**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: Tests pass

**Step 6: Commit**

```bash
git add server/src/mutation_resolvers.gleam
git commit -m "feat(auth): use local auth functions in mutation resolvers"
```

---

### Task 10: Remove Old External Auth Functions

**Files:**
- Modify: `server/src/atproto_auth.gleam`

**Step 1: Remove verify_oauth_token and get_atproto_session**

Delete these functions and their helper functions:
- `verify_oauth_token`
- `get_atproto_session`
- `parse_userinfo`
- `parse_session`
- `extract_dpop_jwk`
- `find_json_object`
- `find_matching_brace`

Also remove unused imports:
- `gleam/http/request`
- `gleam/httpc`
- `gleam/json`

**Step 2: Run build**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds (no remaining references to old functions)

**Step 3: Run tests**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: All tests pass

**Step 4: Commit**

```bash
git add server/src/atproto_auth.gleam
git commit -m "refactor(auth): remove external auth service functions"
```

---

### Task 11: Clean Up Unused auth_base_url

**Files:**
- Modify: `server/src/server.gleam` (remove auth_base_url from Context if unused)
- Modify: Any other files still referencing auth_base_url

**Step 1: Search for remaining auth_base_url references**

Run: `grep -r "auth_base_url" server/src/`

**Step 2: Remove or update remaining references**

Keep auth_base_url only if it's used elsewhere (e.g., for admin OAuth flows). If not used, remove from server Context.

**Step 3: Run build and tests**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build && gleam test`
Expected: Build and tests pass

**Step 4: Commit**

```bash
git add -A
git commit -m "chore(auth): clean up unused auth_base_url references"
```

---

### Task 12: Manual Integration Test

**Step 1: Start the server**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam run`

**Step 2: Test a mutation with valid auth**

Use an existing OAuth flow to get a token, then test a mutation.

**Step 3: Verify token validation works**

- Valid token should succeed
- Invalid token should return 401/Unauthorized
- Expired token should trigger refresh (if refresh token available)

**Step 4: Document any issues found**

If issues are found, create follow-up tasks.

---

Plan complete and saved to `docs/plans/2025-11-25-local-auth-rewire.md`. Two execution options:

**1. Subagent-Driven (this session)** - I dispatch fresh subagent per task, review between tasks, fast iteration

**2. Parallel Session (separate)** - Open new session with executing-plans, batch execution with checkpoints

**Which approach?**
