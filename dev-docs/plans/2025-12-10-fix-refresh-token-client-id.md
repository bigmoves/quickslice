# Fix Refresh Token Missing client_id Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix token refresh failure by adding missing `client_id` to AT Protocol PDS refresh requests.

**Architecture:** The `refresh_tokens` function in `bridge.gleam` builds a request body to send to the upstream AT Protocol PDS, but omits the required `client_id` parameter. The fix adds `client_id` to match how `exchange_code_for_tokens` builds its request body.

**Tech Stack:** Gleam, AT Protocol OAuth

---

## Root Cause

In `server/src/lib/oauth/atproto/bridge.gleam:204`, the refresh token request body is:

```gleam
let body = "grant_type=refresh_token" <> "&refresh_token=" <> refresh_token
```

The `client_id` parameter is passed to the function but never included in the body. The AT Protocol PDS requires `client_id` and returns:

```
Client credentials missing: Required at body.client_id
```

---

### Task 1: Add client_id to refresh token request body

**Files:**
- Modify: `server/src/lib/oauth/atproto/bridge.gleam:204`

**Step 1: Edit the body construction**

Change line 204 from:

```gleam
let body = "grant_type=refresh_token" <> "&refresh_token=" <> refresh_token
```

To:

```gleam
let body =
  "grant_type=refresh_token"
  <> "&refresh_token="
  <> uri.percent_encode(refresh_token)
  <> "&client_id="
  <> uri.percent_encode(client_id)
```

Note: Also adding `uri.percent_encode` for consistency with `exchange_code_for_tokens` (line 351-360).

**Step 2: Build to verify syntax**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds with no errors

**Step 3: Run existing tests**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: All tests pass (this is an internal function, existing tests should still pass)

**Step 4: Commit**

```bash
git add server/src/lib/oauth/atproto/bridge.gleam
git commit -m "fix: add client_id to AT Protocol refresh token requests

The refresh_tokens function was missing client_id in the request body
sent to the upstream AT Protocol PDS. This caused token refresh to fail
with 'Client credentials missing: Required at body.client_id'.

Also added uri.percent_encode for consistency with exchange_code_for_tokens."
```

---

### Task 2: Manual verification (optional)

**Step 1: Test in browser**

1. Start the quickslice server
2. Log in via the JS client
3. Wait for token to expire (or manually clear `quickslice_access_token` from localStorage)
4. Perform a mutation that triggers token refresh
5. Verify mutation succeeds without "Token refresh failed" error

---

## Summary

This is a one-line fix (plus formatting). The `client_id` parameter was already being passed to `refresh_tokens` but wasn't being included in the HTTP request body sent to the AT Protocol PDS.
