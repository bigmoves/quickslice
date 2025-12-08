# Secure Public OAuth with DPoP Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Update the 01-statusphere example to use DPoP-bound tokens with refresh token rotation, multi-tab coordination, and persistent storage across page refreshes.

**Architecture:** Client stores DPoP private key in IndexedDB (non-extractable WebCrypto key) and tokens in localStorage. Every request includes a fresh DPoP proof signed by the private key. Server validates DPoP proofs and binds tokens to the key thumbprint. Refresh tokens rotate on each use.

**Tech Stack:** Gleam/Erlang (server), Vanilla JS with WebCrypto API (client), browser-tabs-lock pattern, IndexedDB, localStorage

---

## Part 1: Server-Side DPoP Validation

### Task 1: Add DPoP Proof Verification to jose_ffi.erl

**Files:**
- Modify: `server/src/jose_ffi.erl`

**Step 1: Add verify_dpop_proof export**

Add to the export list at line 2:

```erlang
-export([generate_dpop_proof/5, sha256_hash/1, compute_jwk_thumbprint/1, sha256_base64url/1, verify_dpop_proof/4]).
```

**Step 2: Add the verification function**

Add after `compute_jwk_thumbprint/1` function (after line 128):

```erlang
%% Verify a DPoP proof JWT
%% Args: DPoPProof (binary), ExpectedMethod (binary), ExpectedUrl (binary), MaxAgeSeconds (integer)
%% Returns: {ok, #{jkt => Thumbprint, jti => Jti, iat => Iat}} | {error, Reason}
verify_dpop_proof(DPoPProof, ExpectedMethod, ExpectedUrl, MaxAgeSeconds) ->
    try
        %% Parse the JWT without verification first to get the header
        {_JWS, JWTMap} = jose_jwt:peek(DPoPProof),

        %% Get the JWS to extract the header
        {JWSMap, _} = jose_jws:from_binary(DPoPProof),

        %% Extract the JWK from the header
        case maps:get(<<"jwk">>, JWSMap, undefined) of
            undefined ->
                {error, <<"Missing jwk in DPoP header">>};
            JWKMap ->
                %% Verify typ is dpop+jwt
                case maps:get(<<"typ">>, JWSMap, undefined) of
                    <<"dpop+jwt">> ->
                        %% Reconstruct JWK for verification
                        JWK = jose_jwk:from_map(JWKMap),

                        %% Verify the signature
                        case jose_jwt:verify(JWK, DPoPProof) of
                            {true, JWT, _JWS2} ->
                                Claims = jose_jwt:to_map(JWT),
                                validate_dpop_claims(Claims, JWK, ExpectedMethod, ExpectedUrl, MaxAgeSeconds);
                            {false, _, _} ->
                                {error, <<"Invalid DPoP signature">>}
                        end;
                    Other ->
                        {error, iolist_to_binary([<<"Invalid typ: expected dpop+jwt, got ">>,
                                                   io_lib:format("~p", [Other])])}
                end
        end
    catch
        error:Reason ->
            {error, iolist_to_binary([<<"DPoP verification failed: ">>,
                                       io_lib:format("~p", [Reason])])};
        _:Error ->
            {error, iolist_to_binary([<<"DPoP verification error: ">>,
                                       io_lib:format("~p", [Error])])}
    end.

%% Internal: Validate DPoP claims
validate_dpop_claims({_Kind, Claims}, JWK, ExpectedMethod, ExpectedUrl, MaxAgeSeconds) ->
    Now = erlang:system_time(second),

    %% Extract required claims
    Htm = maps:get(<<"htm">>, Claims, undefined),
    Htu = maps:get(<<"htu">>, Claims, undefined),
    Jti = maps:get(<<"jti">>, Claims, undefined),
    Iat = maps:get(<<"iat">>, Claims, undefined),

    %% Validate all required claims exist
    case {Htm, Htu, Jti, Iat} of
        {undefined, _, _, _} -> {error, <<"Missing htm claim">>};
        {_, undefined, _, _} -> {error, <<"Missing htu claim">>};
        {_, _, undefined, _} -> {error, <<"Missing jti claim">>};
        {_, _, _, undefined} -> {error, <<"Missing iat claim">>};
        _ ->
            %% Validate htm matches
            case Htm =:= ExpectedMethod of
                false ->
                    {error, iolist_to_binary([<<"htm mismatch: expected ">>, ExpectedMethod,
                                               <<", got ">>, Htm])};
                true ->
                    %% Validate htu matches (normalize URLs)
                    case normalize_url(Htu) =:= normalize_url(ExpectedUrl) of
                        false ->
                            {error, iolist_to_binary([<<"htu mismatch: expected ">>, ExpectedUrl,
                                                       <<", got ">>, Htu])};
                        true ->
                            %% Validate iat is within acceptable range
                            case abs(Now - Iat) =< MaxAgeSeconds of
                                false ->
                                    {error, <<"iat outside acceptable time window">>};
                                true ->
                                    %% Compute JKT
                                    {_Module, Thumbprint} = jose_jwk:thumbprint(JWK),
                                    {ok, #{
                                        jkt => Thumbprint,
                                        jti => Jti,
                                        iat => Iat
                                    }}
                            end
                    end
            end
    end.

%% Internal: Normalize URL for comparison (remove trailing slash, fragments)
normalize_url(Url) when is_binary(Url) ->
    %% Remove fragment
    case binary:split(Url, <<"#">>) of
        [Base | _] ->
            %% Remove trailing slash
            case binary:last(Base) of
                $/ -> binary:part(Base, 0, byte_size(Base) - 1);
                _ -> Base
            end;
        _ -> Url
    end.
```

**Step 3: Run tests to verify compilation**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add server/src/jose_ffi.erl
git commit -m "feat(oauth): add DPoP proof verification to jose_ffi"
```

---

### Task 2: Create DPoP Validator Gleam Module

**Files:**
- Create: `server/src/lib/oauth/dpop/validator.gleam`

**Step 1: Create the validator module**

```gleam
/// DPoP proof validation
/// Validates DPoP proofs according to RFC 9449
import gleam/option.{type Option, None, Some}
import gleam/result

/// Result of successful DPoP validation
pub type DPoPValidationResult {
  DPoPValidationResult(
    /// JWK thumbprint (SHA-256) of the public key
    jkt: String,
    /// Unique identifier for replay protection
    jti: String,
    /// Issued-at timestamp
    iat: Int,
  )
}

/// Verify a DPoP proof JWT
///
/// # Arguments
/// * `dpop_proof` - The DPoP proof JWT from the DPoP header
/// * `method` - Expected HTTP method (e.g., "POST")
/// * `url` - Expected URL being accessed
/// * `max_age_seconds` - Maximum allowed age of the proof (typically 300 = 5 minutes)
///
/// # Returns
/// * `Ok(DPoPValidationResult)` - Validation succeeded
/// * `Error(String)` - Validation failed with reason
pub fn verify_dpop_proof(
  dpop_proof: String,
  method: String,
  url: String,
  max_age_seconds: Int,
) -> Result(DPoPValidationResult, String) {
  verify_dpop_proof_internal(dpop_proof, method, url, max_age_seconds)
  |> result.map(fn(result) {
    DPoPValidationResult(
      jkt: result.jkt,
      jti: result.jti,
      iat: result.iat,
    )
  })
}

/// Internal FFI result type
type InternalResult {
  InternalResult(jkt: String, jti: String, iat: Int)
}

@external(erlang, "dpop_validator_ffi", "verify_dpop_proof")
fn verify_dpop_proof_internal(
  dpop_proof: String,
  method: String,
  url: String,
  max_age_seconds: Int,
) -> Result(InternalResult, String)

/// Extract DPoP header from request headers
pub fn get_dpop_header(
  headers: List(#(String, String)),
) -> Option(String) {
  case headers {
    [] -> None
    [#(name, value), ..rest] -> {
      case name {
        "dpop" -> Some(value)
        "DPoP" -> Some(value)
        _ -> get_dpop_header(rest)
      }
    }
  }
}
```

**Step 2: Create the FFI bridge**

Create file: `server/src/dpop_validator_ffi.erl`

```erlang
-module(dpop_validator_ffi).
-export([verify_dpop_proof/4]).

%% Bridge to jose_ffi:verify_dpop_proof with Gleam-compatible return types
verify_dpop_proof(DPoPProof, Method, Url, MaxAgeSeconds) ->
    case jose_ffi:verify_dpop_proof(DPoPProof, Method, Url, MaxAgeSeconds) of
        {ok, #{jkt := Jkt, jti := Jti, iat := Iat}} ->
            {ok, {internal_result, Jkt, Jti, Iat}};
        {error, Reason} when is_binary(Reason) ->
            {error, Reason};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.
```

**Step 3: Run build to verify**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add server/src/lib/oauth/dpop/validator.gleam server/src/dpop_validator_ffi.erl
git commit -m "feat(oauth): add DPoP validator module"
```

---

### Task 3: Add JTI Replay Protection Repository

**Files:**
- Create: `server/src/database/repositories/oauth_dpop_jti.gleam`
- Modify: `server/src/database/schema/tables.gleam`

**Step 1: Add table creation function to tables.gleam**

Add after `create_oauth_dpop_nonce_table` function:

```gleam
/// Creates the oauth_dpop_jti table for DPoP JTI replay protection
pub fn create_oauth_dpop_jti_table(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let create_table_sql =
    "
    CREATE TABLE IF NOT EXISTS oauth_dpop_jti (
      jti TEXT PRIMARY KEY,
      created_at INTEGER NOT NULL
    )
  "

  let create_created_at_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_oauth_dpop_jti_created_at
    ON oauth_dpop_jti(created_at)
  "

  use _ <- result.try(sqlight.exec(create_table_sql, conn))
  sqlight.exec(create_created_at_index_sql, conn)
}
```

**Step 2: Create the repository module**

```gleam
/// OAuth DPoP JTI replay protection repository
/// Tracks used JTI values to prevent replay attacks
import gleam/dynamic/decode
import gleam/list
import gleam/result
import sqlight

/// Check if a JTI has been used and mark it as used atomically
/// Returns Ok(True) if the JTI was successfully recorded (not previously used)
/// Returns Ok(False) if the JTI was already used (replay attack)
pub fn use_jti(
  conn: sqlight.Connection,
  jti: String,
  created_at: Int,
) -> Result(Bool, sqlight.Error) {
  // Try to insert - will fail if JTI already exists due to PRIMARY KEY constraint
  let sql =
    "INSERT OR IGNORE INTO oauth_dpop_jti (jti, created_at) VALUES (?, ?)"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(jti), sqlight.int(created_at)],
    expecting: decode.dynamic,
  ))

  // Check if insert succeeded by checking changes
  let check_sql = "SELECT changes()"
  use changes_rows <- result.try(sqlight.query(
    check_sql,
    on: conn,
    with: [],
    expecting: decode.at([0], decode.int),
  ))

  case list.first(changes_rows) {
    Ok(1) -> Ok(True)  // Insert succeeded, JTI was new
    Ok(0) -> Ok(False) // Insert ignored, JTI was duplicate
    _ -> Ok(False)
  }
}

/// Delete expired JTI entries
/// Should be called periodically to clean up old entries
pub fn delete_expired(
  conn: sqlight.Connection,
  before: Int,
) -> Result(Int, sqlight.Error) {
  let sql = "DELETE FROM oauth_dpop_jti WHERE created_at < ?"

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.int(before)],
    expecting: decode.dynamic,
  ))

  // Get count of deleted rows
  let check_sql = "SELECT changes()"
  use changes_rows <- result.try(sqlight.query(
    check_sql,
    on: conn,
    with: [],
    expecting: decode.at([0], decode.int),
  ))

  case list.first(changes_rows) {
    Ok(count) -> Ok(count)
    Error(_) -> Ok(0)
  }
}
```

**Step 3: Run build to verify**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add server/src/database/repositories/oauth_dpop_jti.gleam server/src/database/schema/tables.gleam
git commit -m "feat(oauth): add JTI replay protection repository"
```

---

### Task 4: Update Token Endpoint for DPoP Validation

**Files:**
- Modify: `server/src/handlers/oauth/token.gleam`

**Step 1: Add imports at top of file**

Add after existing imports:

```gleam
import database/repositories/oauth_dpop_jti
import lib/oauth/dpop/validator
```

**Step 2: Add DPoP extraction and validation helper**

Add after `validate_client_authentication` function (around line 109):

```gleam
/// Extract and validate DPoP proof from request
/// Returns the JKT (key thumbprint) if valid, or an error response
fn validate_dpop_for_token_endpoint(
  req: wisp.Request,
  conn: sqlight.Connection,
  client: types.OAuthClient,
) -> Result(Option(String), wisp.Response) {
  // Get DPoP header
  let dpop_header = validator.get_dpop_header(wisp.get_headers(req))

  case dpop_header, client.token_endpoint_auth_method {
    // Public clients MUST use DPoP
    None, types.AuthNone ->
      Error(error_response(
        400,
        "invalid_request",
        "DPoP proof required for public clients",
      ))

    // DPoP provided - validate it
    Some(dpop_proof), _ -> {
      // Build the token endpoint URL
      // TODO: Get from config
      let token_url = "http://localhost:8080/oauth/token"

      case validator.verify_dpop_proof(dpop_proof, "POST", token_url, 300) {
        Error(reason) ->
          Error(error_response(400, "invalid_dpop_proof", reason))
        Ok(result) -> {
          // Check JTI hasn't been used (replay protection)
          case oauth_dpop_jti.use_jti(conn, result.jti, result.iat) {
            Error(err) ->
              Error(error_response(
                500,
                "server_error",
                "Database error: " <> string.inspect(err),
              ))
            Ok(False) ->
              Error(error_response(
                400,
                "invalid_dpop_proof",
                "DPoP proof has already been used (replay detected)",
              ))
            Ok(True) ->
              Ok(Some(result.jkt))
          }
        }
      }
    }

    // Confidential client without DPoP - allowed
    None, _ -> Ok(None)
  }
}
```

**Step 3: Update handle_authorization_code to use DPoP**

In `handle_authorization_code` function, after the client authentication validation (around line 172), add DPoP validation:

Replace:
```gleam
            Ok(_) -> {
              // Get authorization code
```

With:
```gleam
            Ok(_) -> {
              // Validate DPoP if present/required
              case validate_dpop_for_token_endpoint(req, conn, client) {
                Error(err) -> err
                Ok(dpop_jkt) -> {
              // Get authorization code
```

Then update the `OAuthAccessToken` creation (around line 211) to use `dpop_jkt`:

Replace:
```gleam
                          let access_token =
                            OAuthAccessToken(
                              token: access_token_value,
                              token_type: Bearer,
                              ...
                              dpop_jkt: None,
                            )
```

With:
```gleam
                          let token_type = case dpop_jkt {
                            Some(_) -> types.DPoP
                            None -> Bearer
                          }

                          let access_token =
                            OAuthAccessToken(
                              token: access_token_value,
                              token_type: token_type,
                              ...
                              dpop_jkt: dpop_jkt,
                            )
```

And update the `token_response` call to use the correct token type string:

Replace:
```gleam
                                  token_response(
                                    access_token_value,
                                    "Bearer",
```

With:
```gleam
                                  let token_type_str = case dpop_jkt {
                                    Some(_) -> "DPoP"
                                    None -> "Bearer"
                                  }
                                  token_response(
                                    access_token_value,
                                    token_type_str,
```

Close the extra case block at the end of `handle_authorization_code`:

Add before the final closing braces:
```gleam
                }
              }
```

**Step 4: Update handle_refresh_token similarly**

Apply the same pattern to `handle_refresh_token`:
1. Add DPoP validation after client authentication
2. Verify the DPoP JKT matches the original token's JKT (for bound tokens)
3. Bind new tokens to the same JKT

**Step 5: Update handle function signature to pass request**

The `handle_authorization_code` and `handle_refresh_token` functions need access to the request for DPoP header extraction. Update their signatures and calls.

**Step 6: Run build and tests**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build && gleam test`
Expected: Build succeeds, tests pass

**Step 7: Commit**

```bash
git add server/src/handlers/oauth/token.gleam
git commit -m "feat(oauth): add DPoP validation to token endpoint"
```

---

### Task 5: Add DPoP Token Type to Database Types

**Files:**
- Modify: `server/src/database/types.gleam`

**Step 1: Add DPoP to TokenType enum**

Find the `TokenType` enum (if it exists) or add it:

```gleam
/// OAuth token types
pub type TokenType {
  Bearer
  DPoP
}
```

**Step 2: Commit**

```bash
git add server/src/database/types.gleam
git commit -m "feat(oauth): add DPoP token type"
```

---

### Task 6: Initialize JTI Table on Server Startup

**Files:**
- Modify: `server/src/server.gleam` (or wherever tables are initialized)

**Step 1: Add table creation call**

Find where other OAuth tables are created and add:

```gleam
use _ <- result.try(tables.create_oauth_dpop_jti_table(conn))
```

**Step 2: Commit**

```bash
git add server/src/server.gleam
git commit -m "feat(oauth): initialize JTI table on startup"
```

---

## Part 2: Client-Side DPoP Implementation

### Task 7: Add IndexedDB Helpers for DPoP Key Storage

**Files:**
- Modify: `examples/01-statusphere/index.html`

**Step 1: Add IndexedDB constants and helpers**

Add after the `STORAGE_KEYS` constant (around line 415):

```javascript
// =============================================================================
// INDEXEDDB FOR DPOP KEYS
// =============================================================================

const DB_NAME = "statusphere-oauth";
const DB_VERSION = 1;
const KEY_STORE = "dpop-keys";
const KEY_ID = "dpop-key";

let dbPromise = null;

function openDatabase() {
  if (dbPromise) return dbPromise;

  dbPromise = new Promise((resolve, reject) => {
    const request = indexedDB.open(DB_NAME, DB_VERSION);

    request.onerror = () => reject(request.error);
    request.onsuccess = () => resolve(request.result);

    request.onupgradeneeded = (event) => {
      const db = event.target.result;
      if (!db.objectStoreNames.contains(KEY_STORE)) {
        db.createObjectStore(KEY_STORE, { keyPath: "id" });
      }
    };
  });

  return dbPromise;
}

async function getDPoPKey() {
  const db = await openDatabase();
  return new Promise((resolve, reject) => {
    const tx = db.transaction(KEY_STORE, "readonly");
    const store = tx.objectStore(KEY_STORE);
    const request = store.get(KEY_ID);

    request.onerror = () => reject(request.error);
    request.onsuccess = () => resolve(request.result || null);
  });
}

async function storeDPoPKey(privateKey, publicJwk) {
  const db = await openDatabase();
  return new Promise((resolve, reject) => {
    const tx = db.transaction(KEY_STORE, "readwrite");
    const store = tx.objectStore(KEY_STORE);
    const request = store.put({
      id: KEY_ID,
      privateKey: privateKey,
      publicJwk: publicJwk,
      createdAt: Date.now()
    });

    request.onerror = () => reject(request.error);
    request.onsuccess = () => resolve();
  });
}

async function getOrCreateDPoPKey() {
  let keyData = await getDPoPKey();

  if (keyData) {
    return keyData;
  }

  // Generate new P-256 key pair
  const keyPair = await crypto.subtle.generateKey(
    { name: "ECDSA", namedCurve: "P-256" },
    false,  // NOT extractable - critical for security
    ["sign"]
  );

  // Export public key as JWK
  const publicJwk = await crypto.subtle.exportKey("jwk", keyPair.publicKey);

  // Store in IndexedDB
  await storeDPoPKey(keyPair.privateKey, publicJwk);

  return {
    privateKey: keyPair.privateKey,
    publicJwk: publicJwk,
    createdAt: Date.now()
  };
}
```

**Step 2: Run local test**

Open the HTML file in a browser and check console for errors.

**Step 3: Commit**

```bash
git add examples/01-statusphere/index.html
git commit -m "feat(client): add IndexedDB helpers for DPoP key storage"
```

---

### Task 8: Add DPoP Proof Generation

**Files:**
- Modify: `examples/01-statusphere/index.html`

**Step 1: Add DPoP proof generation function**

Add after the IndexedDB helpers:

```javascript
// =============================================================================
// DPOP PROOF GENERATION
// =============================================================================

async function createDPoPProof(method, url, accessToken = null) {
  const keyData = await getOrCreateDPoPKey();

  // Header
  const header = {
    alg: "ES256",
    typ: "dpop+jwt",
    jwk: keyData.publicJwk
  };

  // Payload
  const payload = {
    jti: generateRandomId(),
    htm: method,
    htu: url,
    iat: Math.floor(Date.now() / 1000)
  };

  // Add access token hash if provided (for resource requests)
  if (accessToken) {
    payload.ath = await sha256Base64Url(accessToken);
  }

  // Sign the JWT
  return await signJwt(header, payload, keyData.privateKey);
}

function generateRandomId() {
  const bytes = new Uint8Array(16);
  crypto.getRandomValues(bytes);
  return base64UrlEncode(bytes);
}

async function sha256Base64Url(data) {
  const encoder = new TextEncoder();
  const hash = await crypto.subtle.digest("SHA-256", encoder.encode(data));
  return base64UrlEncode(hash);
}

async function signJwt(header, payload, privateKey) {
  const encoder = new TextEncoder();

  // Encode header and payload
  const headerB64 = base64UrlEncode(encoder.encode(JSON.stringify(header)));
  const payloadB64 = base64UrlEncode(encoder.encode(JSON.stringify(payload)));

  // Create signing input
  const signingInput = `${headerB64}.${payloadB64}`;

  // Sign
  const signature = await crypto.subtle.sign(
    { name: "ECDSA", hash: "SHA-256" },
    privateKey,
    encoder.encode(signingInput)
  );

  // Convert signature from IEEE P1363 to DER format is NOT needed for JWS
  // JWS uses the raw R||S format which is what WebCrypto provides
  const signatureB64 = base64UrlEncode(signature);

  return `${signingInput}.${signatureB64}`;
}
```

**Step 2: Commit**

```bash
git add examples/01-statusphere/index.html
git commit -m "feat(client): add DPoP proof generation with WebCrypto"
```

---

### Task 9: Add Multi-Tab Lock Coordination

**Files:**
- Modify: `examples/01-statusphere/index.html`

**Step 1: Add browser-tabs-lock implementation**

Add after DPoP proof generation:

```javascript
// =============================================================================
// MULTI-TAB LOCK COORDINATION
// =============================================================================

// Simple lock implementation using localStorage
// Based on browser-tabs-lock pattern
const LOCK_TIMEOUT = 5000; // 5 seconds
const LOCK_PREFIX = "statusphere_lock_";

async function acquireLock(key, timeout = LOCK_TIMEOUT) {
  const lockKey = LOCK_PREFIX + key;
  const lockValue = `${Date.now()}_${Math.random()}`;
  const deadline = Date.now() + timeout;

  while (Date.now() < deadline) {
    const existing = localStorage.getItem(lockKey);

    if (existing) {
      // Check if lock is stale (older than timeout)
      const [timestamp] = existing.split("_");
      if (Date.now() - parseInt(timestamp) > LOCK_TIMEOUT) {
        // Lock is stale, remove it
        localStorage.removeItem(lockKey);
      } else {
        // Lock is held, wait and retry
        await sleep(50);
        continue;
      }
    }

    // Try to acquire
    localStorage.setItem(lockKey, lockValue);

    // Verify we got it (handle race condition)
    await sleep(10);
    if (localStorage.getItem(lockKey) === lockValue) {
      return lockValue; // Lock acquired
    }
  }

  return null; // Failed to acquire
}

function releaseLock(key, lockValue) {
  const lockKey = LOCK_PREFIX + key;
  // Only release if we still hold it
  if (localStorage.getItem(lockKey) === lockValue) {
    localStorage.removeItem(lockKey);
  }
}

function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}
```

**Step 2: Commit**

```bash
git add examples/01-statusphere/index.html
git commit -m "feat(client): add multi-tab lock coordination"
```

---

### Task 10: Update Token Storage to Use localStorage

**Files:**
- Modify: `examples/01-statusphere/index.html`

**Step 1: Update storage object**

Replace the `storage` object (around line 421):

```javascript
const storage = {
  get(key) {
    // OAuth flow state stays in sessionStorage (per-tab)
    if (key === STORAGE_KEYS.codeVerifier ||
        key === STORAGE_KEYS.oauthState) {
      return sessionStorage.getItem(key);
    }
    // Tokens go in localStorage (shared across tabs)
    return localStorage.getItem(key);
  },
  set(key, value) {
    if (key === STORAGE_KEYS.codeVerifier ||
        key === STORAGE_KEYS.oauthState) {
      sessionStorage.setItem(key, value);
    } else {
      localStorage.setItem(key, value);
    }
  },
  remove(key) {
    sessionStorage.removeItem(key);
    localStorage.removeItem(key);
  },
  clear() {
    Object.values(STORAGE_KEYS).forEach((key) => {
      sessionStorage.removeItem(key);
      localStorage.removeItem(key);
    });
  },
};
```

**Step 2: Add token expiry tracking**

Add new storage key:

```javascript
const STORAGE_KEYS = {
  accessToken: "qs_access_token",
  refreshToken: "qs_refresh_token",
  userDid: "qs_user_did",
  codeVerifier: "qs_code_verifier",
  oauthState: "qs_oauth_state",
  clientId: "qs_client_id",
  tokenExpiresAt: "qs_token_expires_at",  // NEW
  dpopJkt: "qs_dpop_jkt",                  // NEW
};
```

**Step 3: Commit**

```bash
git add examples/01-statusphere/index.html
git commit -m "feat(client): update token storage to use localStorage with expiry"
```

---

### Task 11: Update OAuth Token Exchange to Use DPoP

**Files:**
- Modify: `examples/01-statusphere/index.html`

**Step 1: Update handleOAuthCallback to send DPoP proof**

Replace the token exchange code in `handleOAuthCallback` (around line 536):

```javascript
// Exchange code for tokens with DPoP
const dpopProof = await createDPoPProof("POST", OAUTH_TOKEN_URL);

const tokenResponse = await fetch(OAUTH_TOKEN_URL, {
  method: "POST",
  headers: {
    "Content-Type": "application/x-www-form-urlencoded",
    "DPoP": dpopProof
  },
  body: new URLSearchParams({
    grant_type: "authorization_code",
    code: code,
    redirect_uri: redirectUri,
    client_id: clientId,
    code_verifier: codeVerifier,
  }),
});
```

**Step 2: Update token storage to include expiry**

After receiving tokens, store expiry:

```javascript
const tokens = await tokenResponse.json();

// Store tokens
storage.set(STORAGE_KEYS.accessToken, tokens.access_token);
if (tokens.refresh_token) {
  storage.set(STORAGE_KEYS.refreshToken, tokens.refresh_token);
}

// Store expiry time
const expiresAt = Date.now() + (tokens.expires_in * 1000);
storage.set(STORAGE_KEYS.tokenExpiresAt, expiresAt.toString());
```

**Step 3: Commit**

```bash
git add examples/01-statusphere/index.html
git commit -m "feat(client): send DPoP proof with token exchange"
```

---

### Task 12: Add Token Refresh with DPoP

**Files:**
- Modify: `examples/01-statusphere/index.html`

**Step 1: Add token refresh function**

Add after `handleOAuthCallback`:

```javascript
async function refreshTokens() {
  const refreshToken = storage.get(STORAGE_KEYS.refreshToken);
  const clientId = storage.get(STORAGE_KEYS.clientId);

  if (!refreshToken || !clientId) {
    throw new Error("No refresh token available");
  }

  const dpopProof = await createDPoPProof("POST", OAUTH_TOKEN_URL);

  const response = await fetch(OAUTH_TOKEN_URL, {
    method: "POST",
    headers: {
      "Content-Type": "application/x-www-form-urlencoded",
      "DPoP": dpopProof
    },
    body: new URLSearchParams({
      grant_type: "refresh_token",
      refresh_token: refreshToken,
      client_id: clientId,
    }),
  });

  if (!response.ok) {
    const errorData = await response.json().catch(() => ({}));
    throw new Error(
      `Token refresh failed: ${errorData.error_description || response.statusText}`
    );
  }

  const tokens = await response.json();

  // Store new tokens (rotation - new refresh token each time)
  storage.set(STORAGE_KEYS.accessToken, tokens.access_token);
  if (tokens.refresh_token) {
    storage.set(STORAGE_KEYS.refreshToken, tokens.refresh_token);
  }

  const expiresAt = Date.now() + (tokens.expires_in * 1000);
  storage.set(STORAGE_KEYS.tokenExpiresAt, expiresAt.toString());

  return tokens.access_token;
}
```

**Step 2: Add getAccessToken with auto-refresh and locking**

```javascript
async function getValidAccessToken() {
  const accessToken = storage.get(STORAGE_KEYS.accessToken);
  const expiresAt = parseInt(storage.get(STORAGE_KEYS.tokenExpiresAt) || "0");

  // Check if token is still valid (with 60 second buffer)
  if (accessToken && Date.now() < expiresAt - 60000) {
    return accessToken;
  }

  // Need to refresh - acquire lock first
  const clientId = storage.get(STORAGE_KEYS.clientId);
  const lockKey = `token_refresh_${clientId}`;
  const lockValue = await acquireLock(lockKey);

  if (!lockValue) {
    // Failed to acquire lock, another tab is refreshing
    // Wait a bit and check cache again
    await sleep(100);
    const freshToken = storage.get(STORAGE_KEYS.accessToken);
    const freshExpiry = parseInt(storage.get(STORAGE_KEYS.tokenExpiresAt) || "0");
    if (freshToken && Date.now() < freshExpiry - 60000) {
      return freshToken;
    }
    throw new Error("Failed to refresh token");
  }

  try {
    // Double-check after acquiring lock
    const freshToken = storage.get(STORAGE_KEYS.accessToken);
    const freshExpiry = parseInt(storage.get(STORAGE_KEYS.tokenExpiresAt) || "0");
    if (freshToken && Date.now() < freshExpiry - 60000) {
      return freshToken;
    }

    // Actually refresh
    return await refreshTokens();
  } finally {
    releaseLock(lockKey, lockValue);
  }
}
```

**Step 3: Commit**

```bash
git add examples/01-statusphere/index.html
git commit -m "feat(client): add token refresh with DPoP and multi-tab locking"
```

---

### Task 13: Update GraphQL Requests to Use DPoP

**Files:**
- Modify: `examples/01-statusphere/index.html`

**Step 1: Update graphqlQuery to use DPoP**

Replace the `graphqlQuery` function:

```javascript
async function graphqlQuery(query, variables = {}, requireAuth = false) {
  const headers = {
    "Content-Type": "application/json",
  };

  if (requireAuth) {
    const token = await getValidAccessToken();
    if (!token) {
      throw new Error("Not authenticated");
    }

    // Create DPoP proof bound to this request
    const dpopProof = await createDPoPProof("POST", GRAPHQL_URL, token);

    headers["Authorization"] = `DPoP ${token}`;
    headers["DPoP"] = dpopProof;
  }

  const response = await fetch(GRAPHQL_URL, {
    method: "POST",
    headers,
    body: JSON.stringify({ query, variables }),
  });

  if (!response.ok) {
    throw new Error(`GraphQL request failed: ${response.statusText}`);
  }

  const result = await response.json();

  if (result.errors && result.errors.length > 0) {
    throw new Error(`GraphQL error: ${result.errors[0].message}`);
  }

  return result.data;
}
```

**Step 2: Commit**

```bash
git add examples/01-statusphere/index.html
git commit -m "feat(client): update GraphQL requests to use DPoP authorization"
```

---

### Task 14: Update Main Initialization

**Files:**
- Modify: `examples/01-statusphere/index.html`

**Step 1: Initialize DPoP key on app start**

Update the `main` function to initialize the DPoP key early:

```javascript
async function main() {
  try {
    // Initialize DPoP key first (creates if doesn't exist)
    await getOrCreateDPoPKey();
    console.log("DPoP key initialized");
  } catch (error) {
    console.error("Failed to initialize DPoP key:", error);
    showError("Failed to initialize secure key storage. Please use a modern browser.");
    return;
  }

  try {
    // Check if this is an OAuth callback
    const isCallback = await handleOAuthCallback();
    if (isCallback) {
      console.log("OAuth callback handled successfully");
    }
  } catch (error) {
    showError(`Authentication failed: ${error.message}`);
    storage.clear();
  }

  // ... rest of existing main() code ...
}
```

**Step 2: Commit**

```bash
git add examples/01-statusphere/index.html
git commit -m "feat(client): initialize DPoP key on app start"
```

---

## Part 3: Server Resource Endpoint Protection

### Task 15: Add DPoP Validation Middleware for GraphQL

**Files:**
- Create: `server/src/middleware/dpop.gleam` (or add to existing auth middleware)

**Step 1: Create DPoP validation for resource requests**

```gleam
/// DPoP validation middleware for protected resources
import database/repositories/oauth_access_tokens
import database/repositories/oauth_dpop_jti
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import lib/oauth/dpop/validator
import sqlight
import wisp

/// Validate DPoP-bound access token
/// Returns the user_id if valid, or an error response
pub fn validate_dpop_access(
  req: wisp.Request,
  conn: sqlight.Connection,
  resource_url: String,
) -> Result(String, wisp.Response) {
  // Extract Authorization header
  let auth_header = wisp.get_header(req, "authorization")

  case auth_header {
    None -> Error(unauthorized("Missing Authorization header"))
    Some(header) -> {
      // Parse "DPoP <token>" or "Bearer <token>"
      case string.split(header, " ") {
        ["DPoP", token] -> validate_dpop_token(req, conn, token, resource_url)
        ["Bearer", token] -> validate_bearer_token(conn, token)
        _ -> Error(unauthorized("Invalid Authorization header format"))
      }
    }
  }
}

fn validate_dpop_token(
  req: wisp.Request,
  conn: sqlight.Connection,
  token: String,
  resource_url: String,
) -> Result(String, wisp.Response) {
  // Get DPoP proof from header
  case validator.get_dpop_header(wisp.get_headers(req)) {
    None -> Error(unauthorized("Missing DPoP proof for DPoP-bound token"))
    Some(dpop_proof) -> {
      // Verify the DPoP proof
      let method = wisp.method_to_string(req.method)
      case validator.verify_dpop_proof(dpop_proof, method, resource_url, 300) {
        Error(reason) -> Error(unauthorized("Invalid DPoP proof: " <> reason))
        Ok(dpop_result) -> {
          // Check JTI for replay
          case oauth_dpop_jti.use_jti(conn, dpop_result.jti, dpop_result.iat) {
            Error(_) -> Error(server_error("Database error"))
            Ok(False) -> Error(unauthorized("DPoP proof replay detected"))
            Ok(True) -> {
              // Get the access token and verify JKT matches
              case oauth_access_tokens.get(conn, token) {
                Error(_) -> Error(server_error("Database error"))
                Ok(None) -> Error(unauthorized("Invalid access token"))
                Ok(Some(access_token)) -> {
                  case access_token.dpop_jkt {
                    None -> Error(unauthorized("Token is not DPoP-bound"))
                    Some(jkt) -> {
                      case jkt == dpop_result.jkt {
                        False -> Error(unauthorized("DPoP key mismatch"))
                        True -> {
                          case access_token.user_id {
                            None -> Error(unauthorized("Token has no user"))
                            Some(user_id) -> Ok(user_id)
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

fn validate_bearer_token(
  conn: sqlight.Connection,
  token: String,
) -> Result(String, wisp.Response) {
  case oauth_access_tokens.get(conn, token) {
    Error(_) -> Error(server_error("Database error"))
    Ok(None) -> Error(unauthorized("Invalid access token"))
    Ok(Some(access_token)) -> {
      // DPoP-bound tokens MUST use DPoP authorization
      case access_token.dpop_jkt {
        Some(_) -> Error(unauthorized("DPoP-bound token requires DPoP authorization"))
        None -> {
          case access_token.user_id {
            None -> Error(unauthorized("Token has no user"))
            Some(user_id) -> Ok(user_id)
          }
        }
      }
    }
  }
}

fn unauthorized(message: String) -> wisp.Response {
  wisp.response(401)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text("{\"error\":\"" <> message <> "\"}"))
}

fn server_error(message: String) -> wisp.Response {
  wisp.response(500)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text("{\"error\":\"" <> message <> "\"}"))
}
```

**Step 2: Commit**

```bash
git add server/src/middleware/dpop.gleam
git commit -m "feat(oauth): add DPoP validation middleware for resource endpoints"
```

---

## Part 4: Testing

### Task 16: Add DPoP Validator Tests

**Files:**
- Create: `server/test/oauth/dpop_validator_test.gleam`

**Step 1: Write tests for DPoP validation**

```gleam
import gleeunit/should
import lib/oauth/dpop/validator
import lib/oauth/dpop/keygen
import lib/oauth/dpop/generator
import gleam/option

pub fn verify_valid_dpop_proof_test() {
  // Generate a key pair
  let assert Ok(jwk_json) = keygen.generate_dpop_keypair()

  // Generate a proof
  let assert Ok(proof) = generator.generate_dpop_proof_with_nonce(
    "POST",
    "https://example.com/oauth/token",
    "",  // No access token for token exchange
    jwk_json,
    option.None
  )

  // Verify the proof
  let result = validator.verify_dpop_proof(
    proof,
    "POST",
    "https://example.com/oauth/token",
    300
  )

  should.be_ok(result)
}

pub fn reject_wrong_method_test() {
  let assert Ok(jwk_json) = keygen.generate_dpop_keypair()
  let assert Ok(proof) = generator.generate_dpop_proof_with_nonce(
    "POST",
    "https://example.com/oauth/token",
    "",
    jwk_json,
    option.None
  )

  // Verify with wrong method
  let result = validator.verify_dpop_proof(
    proof,
    "GET",  // Wrong method
    "https://example.com/oauth/token",
    300
  )

  should.be_error(result)
}

pub fn reject_wrong_url_test() {
  let assert Ok(jwk_json) = keygen.generate_dpop_keypair()
  let assert Ok(proof) = generator.generate_dpop_proof_with_nonce(
    "POST",
    "https://example.com/oauth/token",
    "",
    jwk_json,
    option.None
  )

  // Verify with wrong URL
  let result = validator.verify_dpop_proof(
    proof,
    "POST",
    "https://different.com/oauth/token",  // Wrong URL
    300
  )

  should.be_error(result)
}
```

**Step 2: Run tests**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: All tests pass

**Step 3: Commit**

```bash
git add server/test/oauth/dpop_validator_test.gleam
git commit -m "test(oauth): add DPoP validator tests"
```

---

### Task 17: Manual End-to-End Test

**Step 1: Start the server**

Run: `cd /Users/chadmiller/code/quickslice && make run`

**Step 2: Register a public OAuth client**

Navigate to `http://localhost:8080/admin/settings` and create:
- Name: "Statusphere DPoP Test"
- Token Endpoint Auth Method: Public (none)
- Redirect URIs: `http://127.0.0.1:3000/`

**Step 3: Serve the example**

Run: `cd /Users/chadmiller/code/quickslice/examples/01-statusphere && npx http-server . -p 3000`

**Step 4: Test the flow**

1. Open `http://127.0.0.1:3000`
2. Open browser DevTools Network tab
3. Enter client ID and Bluesky handle
4. Complete OAuth flow
5. Verify:
   - Token request includes `DPoP` header
   - Token response has `token_type: "DPoP"`
   - GraphQL requests include `Authorization: DPoP` and `DPoP` headers
6. Open same URL in new tab - verify tokens are shared
7. Refresh page - verify session persists

**Step 5: Commit final changes**

```bash
git add -A
git commit -m "feat: complete secure public OAuth with DPoP implementation"
```

---

## Summary

This plan implements:

1. **Server-side DPoP validation** (Tasks 1-6)
   - DPoP proof verification in `jose_ffi.erl`
   - Gleam validator module
   - JTI replay protection
   - Token endpoint integration
   - DPoP token type support

2. **Client-side DPoP implementation** (Tasks 7-14)
   - IndexedDB for non-extractable DPoP key storage
   - WebCrypto-based DPoP proof generation
   - Multi-tab lock coordination
   - localStorage for token persistence
   - Automatic token refresh with DPoP

3. **Resource endpoint protection** (Task 15)
   - DPoP validation middleware for GraphQL

4. **Testing** (Tasks 16-17)
   - Unit tests for DPoP validation
   - End-to-end manual testing
