# Session Iteration Bugfix Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix the OAuth token endpoint to properly propagate `session_iteration` from ATP sessions to access tokens, enabling GraphQL mutations to work with OAuth tokens.

**Architecture:** Add `session_iteration` field to `OAuthAuthorizationCode` type, propagate it through atp_callback when creating auth codes, and use it in the token endpoint when creating access tokens. Also add `session_iteration` to `OAuthRefreshToken` to support token refresh.

**Tech Stack:** Gleam, SQLite, wisp HTTP framework

---

## Task 1: Add session_iteration to OAuthAuthorizationCode Type

**Files:**
- Modify: `src/database/types.gleam:347-362`

**Step 1: Add the field to the type definition**

In `src/database/types.gleam`, add `session_iteration: Option(Int)` to `OAuthAuthorizationCode`:

```gleam
/// OAuth authorization code
pub type OAuthAuthorizationCode {
  OAuthAuthorizationCode(
    code: String,
    client_id: String,
    user_id: String,
    session_id: Option(String),
    session_iteration: Option(Int),
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

**Step 2: Run build to verify syntax**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compile errors showing all places that construct OAuthAuthorizationCode

**Step 3: Commit**

```bash
git add src/database/types.gleam
git commit -m "feat(oauth): add session_iteration field to OAuthAuthorizationCode type"
```

---

## Task 2: Update Database Schema for Authorization Code

**Files:**
- Modify: `src/database/schema/tables.gleam` (find oauth_authorization_code table)
- Modify: `src/database/repositories/oauth_authorization_code.gleam`

**Step 1: Find and update the schema creation SQL**

Search for `oauth_authorization_code` in `src/database/schema/tables.gleam` and add `session_iteration INTEGER` column after `session_id TEXT`.

**Step 2: Update the insert function in repository**

In `src/database/repositories/oauth_authorization_code.gleam`, update the `insert` function:

```gleam
pub fn insert(
  conn: sqlight.Connection,
  code: OAuthAuthorizationCode,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO oauth_authorization_code (
      code, client_id, user_id, session_id, session_iteration, redirect_uri, scope,
      code_challenge, code_challenge_method, nonce, created_at, expires_at, used
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  "

  let params = [
    sqlight.text(code.code),
    sqlight.text(code.client_id),
    sqlight.text(code.user_id),
    sqlight.nullable(sqlight.text, code.session_id),
    sqlight.nullable(sqlight.int, code.session_iteration),
    sqlight.text(code.redirect_uri),
    sqlight.nullable(sqlight.text, code.scope),
    sqlight.nullable(sqlight.text, code.code_challenge),
    sqlight.nullable(
      sqlight.text,
      option.map(
        code.code_challenge_method,
        types.code_challenge_method_to_string,
      ),
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
```

**Step 3: Update the get function SELECT query**

Update the SQL in `get` function:

```gleam
let sql =
  "SELECT code, client_id, user_id, session_id, session_iteration, redirect_uri, scope,
          code_challenge, code_challenge_method, nonce, created_at, expires_at, used
   FROM oauth_authorization_code WHERE code = ?"
```

**Step 4: Update the decoder function**

Update the `decoder` function to read `session_iteration` at field index 4, and shift all subsequent field indices by 1:

```gleam
fn decoder() -> decode.Decoder(OAuthAuthorizationCode) {
  use code <- decode.field(0, decode.string)
  use client_id <- decode.field(1, decode.string)
  use user_id <- decode.field(2, decode.string)
  use session_id <- decode.field(3, decode.optional(decode.string))
  use session_iteration <- decode.field(4, decode.optional(decode.int))
  use redirect_uri <- decode.field(5, decode.string)
  use scope <- decode.field(6, decode.optional(decode.string))
  use code_challenge <- decode.field(7, decode.optional(decode.string))
  use code_challenge_method_str <- decode.field(
    8,
    decode.optional(decode.string),
  )
  use nonce <- decode.field(9, decode.optional(decode.string))
  use created_at <- decode.field(10, decode.int)
  use expires_at <- decode.field(11, decode.int)
  use used <- decode.field(12, decode.int)

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
    session_iteration: session_iteration,
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

**Step 5: Run build to verify**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compile errors for places constructing OAuthAuthorizationCode without session_iteration

**Step 6: Commit**

```bash
git add src/database/schema/tables.gleam src/database/repositories/oauth_authorization_code.gleam
git commit -m "feat(oauth): add session_iteration to authorization code schema and repository"
```

---

## Task 3: Update atp_callback to Set session_iteration

**Files:**
- Modify: `src/handlers/oauth/atp_callback.gleam:157-176`

**Step 1: Add session_iteration when creating auth code**

Update the `OAuthAuthorizationCode` construction at line 157 to include `session_iteration: Some(updated_session.iteration)`:

```gleam
let auth_code =
  OAuthAuthorizationCode(
    code: authorization_code_value,
    client_id: client_req.client_id,
    user_id: user_id,
    session_id: Some(updated_session.session_id),
    session_iteration: Some(updated_session.iteration),
    redirect_uri: client_req.redirect_uri,
    scope: client_req.scope,
    code_challenge: client_req.code_challenge,
    code_challenge_method: case
      client_req.code_challenge_method
    {
      Some("S256") -> Some(S256)
      Some("plain") -> Some(Plain)
      _ -> None
    },
    nonce: client_req.nonce,
    created_at: now,
    expires_at: code_expires_at,
    used: False,
  )
```

**Step 2: Run build to verify**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: SUCCESS or errors in token.gleam (next task)

**Step 3: Commit**

```bash
git add src/handlers/oauth/atp_callback.gleam
git commit -m "feat(oauth): propagate session_iteration when creating authorization code"
```

---

## Task 4: Update Token Endpoint to Use session_iteration

**Files:**
- Modify: `src/handlers/oauth/token.gleam:112-127` (authorization_code grant)
- Modify: `src/handlers/oauth/token.gleam:253-268` (refresh_token grant)

**Step 1: Update authorization_code grant handler**

At line 119, change `session_iteration: None` to `session_iteration: code.session_iteration`:

```gleam
let access_token =
  OAuthAccessToken(
    token: access_token_value,
    token_type: Bearer,
    client_id: cid,
    user_id: Some(code.user_id),
    session_id: code.session_id,
    session_iteration: code.session_iteration,
    scope: code.scope,
    created_at: now,
    expires_at: token_generator.expiration_timestamp(
      client.access_token_expiration,
    ),
    revoked: False,
    dpop_jkt: None,
  )
```

**Step 2: Run build to verify**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: SUCCESS or error about refresh_token grant

**Step 3: Commit**

```bash
git add src/handlers/oauth/token.gleam
git commit -m "fix(oauth): use session_iteration from auth code in authorization_code grant"
```

---

## Task 5: Add session_iteration to OAuthRefreshToken Type

**Files:**
- Modify: `src/database/types.gleam:262-274`

**Step 1: Add session_iteration field**

```gleam
/// OAuth refresh token
pub type OAuthRefreshToken {
  OAuthRefreshToken(
    token: String,
    access_token: String,
    client_id: String,
    user_id: String,
    session_id: Option(String),
    session_iteration: Option(Int),
    scope: Option(String),
    created_at: Int,
    expires_at: Option(Int),
    revoked: Bool,
  )
}
```

**Step 2: Run build to see what breaks**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compile errors for refresh token construction

**Step 3: Commit**

```bash
git add src/database/types.gleam
git commit -m "feat(oauth): add session_iteration field to OAuthRefreshToken type"
```

---

## Task 6: Update Refresh Token Repository

**Files:**
- Modify: `src/database/repositories/oauth_refresh_tokens.gleam`
- Modify: `src/database/schema/tables.gleam` (find oauth_refresh_token table)

**Step 1: Update schema SQL**

Find the `oauth_refresh_token` table creation and add `session_iteration INTEGER` after `session_id TEXT`.

**Step 2: Update insert function**

Add `session_iteration` to the INSERT statement and params.

**Step 3: Update get function and decoder**

Update SELECT query and decoder to include `session_iteration`.

**Step 4: Run build**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Errors in token.gleam for refresh token construction

**Step 5: Commit**

```bash
git add src/database/repositories/oauth_refresh_tokens.gleam src/database/schema/tables.gleam
git commit -m "feat(oauth): add session_iteration to refresh token schema and repository"
```

---

## Task 7: Update Token Endpoint Refresh Token Construction

**Files:**
- Modify: `src/handlers/oauth/token.gleam:129-144` (auth_code grant - refresh token)
- Modify: `src/handlers/oauth/token.gleam:260` (refresh_token grant - access token)
- Modify: `src/handlers/oauth/token.gleam:270-285` (refresh_token grant - new refresh token)

**Step 1: Update refresh token creation in authorization_code grant**

Add `session_iteration: code.session_iteration` to the OAuthRefreshToken at ~line 129:

```gleam
let refresh_token =
  OAuthRefreshToken(
    token: refresh_token_value,
    access_token: access_token_value,
    client_id: cid,
    user_id: code.user_id,
    session_id: code.session_id,
    session_iteration: code.session_iteration,
    scope: code.scope,
    created_at: now,
    expires_at: case client.refresh_token_expiration {
      0 -> None
      exp ->
        Some(token_generator.expiration_timestamp(exp))
    },
    revoked: False,
  )
```

**Step 2: Update access token creation in refresh_token grant**

At ~line 260, change to use `old_refresh_token.session_iteration`:

```gleam
session_iteration: old_refresh_token.session_iteration,
```

**Step 3: Update new refresh token creation in refresh_token grant**

At ~line 270-285, add `session_iteration: old_refresh_token.session_iteration`:

```gleam
let refresh_token =
  OAuthRefreshToken(
    token: new_refresh_token_value,
    access_token: new_access_token_value,
    client_id: cid,
    user_id: old_refresh_token.user_id,
    session_id: old_refresh_token.session_id,
    session_iteration: old_refresh_token.session_iteration,
    scope: scope,
    created_at: now,
    expires_at: case client.refresh_token_expiration {
      0 -> None
      exp ->
        Some(token_generator.expiration_timestamp(exp))
    },
    revoked: False,
  )
```

**Step 4: Run build to verify**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: SUCCESS

**Step 5: Commit**

```bash
git add src/handlers/oauth/token.gleam
git commit -m "fix(oauth): propagate session_iteration through token refresh flow"
```

---

## Task 8: Run Database Migration

**Files:**
- None (manual SQL)

**Step 1: Add columns to existing database**

Run these ALTER TABLE statements to add the new columns:

```bash
sqlite3 /Users/chadmiller/code/quickslice/server/quickslice.db "ALTER TABLE oauth_authorization_code ADD COLUMN session_iteration INTEGER;"
sqlite3 /Users/chadmiller/code/quickslice/server/quickslice.db "ALTER TABLE oauth_refresh_token ADD COLUMN session_iteration INTEGER;"
```

**Step 2: Verify columns added**

```bash
sqlite3 /Users/chadmiller/code/quickslice/server/quickslice.db ".schema oauth_authorization_code"
sqlite3 /Users/chadmiller/code/quickslice/server/quickslice.db ".schema oauth_refresh_token"
```

Expected: Both tables show `session_iteration INTEGER` column

---

## Task 9: Run Tests and Verify

**Step 1: Run all tests**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: All tests pass

**Step 2: Manual integration test**

1. Start the server: `gleam run`
2. Register a new OAuth client
3. Complete the PKCE flow
4. Verify the access token has `session_iteration` set in the database:
   ```bash
   sqlite3 quickslice.db "SELECT session_iteration FROM oauth_access_token ORDER BY created_at DESC LIMIT 1;"
   ```
5. Run a GraphQL mutation with the new token to verify it works

**Step 3: Final commit**

```bash
git add -A
git commit -m "test: verify session_iteration fix works end-to-end"
```

---

## Summary of Changes

| File | Change |
|------|--------|
| `src/database/types.gleam` | Add `session_iteration` to `OAuthAuthorizationCode` and `OAuthRefreshToken` |
| `src/database/schema/tables.gleam` | Add `session_iteration` column to auth code and refresh token tables |
| `src/database/repositories/oauth_authorization_code.gleam` | Update insert, get, and decoder for session_iteration |
| `src/database/repositories/oauth_refresh_tokens.gleam` | Update insert, get, and decoder for session_iteration |
| `src/handlers/oauth/atp_callback.gleam` | Set session_iteration when creating auth code |
| `src/handlers/oauth/token.gleam` | Use session_iteration from auth code/refresh token |
