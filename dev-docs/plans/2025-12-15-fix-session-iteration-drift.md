# Fix Session Iteration Drift

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix "Invalid refresh token" error that occurs after several hours when the client's session_iteration drifts from the current ATP session iteration.

**Tech Stack:** Gleam, SQLite/PostgreSQL

---

## Problem

When ATP tokens refresh, a new session row is created with `iteration + 1`. The current client access token gets updated to point to the new iteration, but the client refresh token retains the old iteration. When the client later refreshes its tokens, the new tokens inherit the stale iteration and look up an old ATP session with an invalidated refresh token.

**Failure sequence:**
1. User logs in → ATP session `iteration=1` → client tokens with `session_iteration=1`
2. ATP access token expires → server refreshes → creates `iteration=2` → updates client access token
3. Client access token expires → client refreshes → new tokens created from refresh token with `session_iteration=1`
4. New client access token looks up ATP session `iteration=1` → finds old, invalidated refresh token
5. Tries to refresh with stale token → "Invalid refresh token"

---

## Solution

Update ATP sessions in place instead of creating new iterations. The iteration mechanism adds complexity without benefit.

**Architecture change:**
- Current: `session_id + iteration` = primary key, each refresh creates new row
- New: `session_id` = lookup key, refresh updates existing row

---

## Tasks

### Task 1: Add update_tokens function to oauth_atp_sessions repository

**File:** `server/src/database/repositories/oauth_atp_sessions.gleam`

Add new function:

```gleam
pub fn update_tokens(
  exec: Executor,
  session_id: String,
  access_token: String,
  refresh_token: String,
  created_at: Int,
  expires_at: Int,
) -> Result(Nil, DbError) {
  let sql = "UPDATE oauth_atp_session SET
    access_token = " <> placeholder(exec, 1) <> ",
    refresh_token = " <> placeholder(exec, 2) <> ",
    access_token_created_at = " <> placeholder(exec, 3) <> ",
    access_token_expires_at = " <> placeholder(exec, 4) <> "
    WHERE session_id = " <> placeholder(exec, 5)

  executor.exec(exec, sql, [
    Text(access_token),
    Text(refresh_token),
    DbInt(created_at),
    DbInt(expires_at),
    Text(session_id),
  ])
}
```

**Verify:** `gleam build`

---

### Task 2: Change bridge.gleam to update in place

**File:** `server/src/lib/oauth/atproto/bridge.gleam`

Replace `increment_iteration` function (lines 589-614) with `update_session_tokens`:

```gleam
fn update_session_tokens(
  conn: Executor,
  session: OAuthAtpSession,
  access_token: String,
  refresh_token: String,
  expires_in: Int,
) -> Result(OAuthAtpSession, BridgeError) {
  let now = token_generator.current_timestamp()
  let expires_at = now + expires_in

  case oauth_atp_sessions.update_tokens(
    conn,
    session.session_id,
    access_token,
    refresh_token,
    now,
    expires_at,
  ) {
    Ok(_) -> Ok(OAuthAtpSession(
      ..session,
      access_token: Some(access_token),
      refresh_token: Some(refresh_token),
      access_token_created_at: Some(now),
      access_token_expires_at: Some(expires_at),
    ))
    Error(err) -> Error(StorageError("Failed to update session: " <> string.inspect(err)))
  }
}
```

Update the call site in `refresh_tokens` (around line 222) to call `update_session_tokens` instead of `increment_iteration`.

**Verify:** `gleam build`

---

### Task 3: Remove session_iteration update from atproto_auth.gleam

**File:** `server/src/atproto_auth.gleam`

Remove lines 175-181 (the `update_session_iteration` call). The session is now updated in place, so there's no new iteration to track.

```gleam
// DELETE THIS BLOCK:
// Update the access token's session_iteration to point to the new ATP session
let _ =
  oauth_access_tokens.update_session_iteration(
    conn,
    token,
    refreshed.iteration,
  )
```

**Verify:** `gleam build`

---

### Task 4: Modify oauth_atp_sessions.get to ignore iteration

**File:** `server/src/database/repositories/oauth_atp_sessions.gleam`

Change `get` function to look up by `session_id` only. Either:
- Remove iteration parameter, or
- Keep parameter but ignore it in the query (for backwards compatibility during rollout)

Recommended: Keep parameter, ignore in query:

```gleam
pub fn get(
  exec: Executor,
  session_id: String,
  _iteration: Int,  // Deprecated, ignored
) -> Result(Option(OAuthAtpSession), DbError) {
  // Query by session_id only, ORDER BY iteration DESC LIMIT 1
  // to get the latest if multiple exist during migration
}
```

**Verify:** `gleam build && gleam test`

---

### Task 5: Set session_iteration to constant in token.gleam

**File:** `server/src/handlers/oauth/token.gleam`

When creating new access/refresh tokens, set `session_iteration` to `Some(0)` instead of copying from old tokens. Search for `session_iteration:` and update to:

```gleam
session_iteration: Some(0),  // Deprecated field
```

**Verify:** `gleam build && gleam test`

---

### Task 6: Database cleanup migration (optional, can defer)

Clean up old iteration rows:

```sql
-- Keep only the latest iteration per session_id
DELETE FROM oauth_atp_session
WHERE (session_id, iteration) NOT IN (
  SELECT session_id, MAX(iteration)
  FROM oauth_atp_session
  GROUP BY session_id
);
```

---

## Testing

1. Build: `gleam build`
2. Run tests: `gleam test`
3. Manual test: Login, wait for token expirations, verify no "Invalid refresh token" error
