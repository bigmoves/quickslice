# OAuth Actor Sync Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Sync an actor's ATProto records on first OAuth login so users see their existing data immediately.

**Architecture:** Add a centralized `get_collection_ids()` helper to the backfill module, then call `actor_validator.ensure_actor_exists()` followed by `backfill.backfill_collections_for_actor()` in the OAuth callback after token exchange. Sync is blocking; errors are logged but don't fail login.

**Tech Stack:** Gleam, SQLite, ATProto

---

## Task 1: Add `get_collection_ids` Helper to Backfill Module

**Files:**
- Modify: `server/src/backfill.gleam`
- Test: Manual verification (existing tests cover partitioning logic)

**Step 1: Add imports to backfill.gleam**

Open `server/src/backfill.gleam` and ensure these imports exist at the top:

```gleam
import database/repositories/config as config_repo
import database/repositories/lexicons as lexicons_repo
```

**Step 2: Add `get_collection_ids` function**

Add this function after the existing `nsid_matches_domain_authority` function (around line 430):

```gleam
/// Get local and external collection IDs from configured lexicons
/// Returns #(local_collection_ids, external_collection_ids)
pub fn get_collection_ids(
  conn: sqlight.Connection,
) -> #(List(String), List(String)) {
  let domain_authority = case config_repo.get(conn, "domain_authority") {
    Ok(authority) -> authority
    Error(_) -> ""
  }

  case lexicons_repo.get_record_types(conn) {
    Ok(lexicons) -> {
      let #(local, external) =
        list.partition(lexicons, fn(lex) {
          nsid_matches_domain_authority(lex.id, domain_authority)
        })
      #(
        list.map(local, fn(lex) { lex.id }),
        list.map(external, fn(lex) { lex.id }),
      )
    }
    Error(_) -> #([], [])
  }
}
```

**Step 3: Verify build succeeds**

Run: `cd server && gleam build`
Expected: Build succeeds with no errors

**Step 4: Commit**

```bash
git add server/src/backfill.gleam
git commit -m "feat(backfill): add get_collection_ids helper for centralized collection retrieval"
```

---

## Task 2: Add Actor Sync to OAuth Callback

**Files:**
- Modify: `server/src/handlers/oauth/atp_callback.gleam`

**Step 1: Add imports to atp_callback.gleam**

Open `server/src/handlers/oauth/atp_callback.gleam` and add these imports after the existing imports (around line 18):

```gleam
import actor_validator
import backfill
import database/repositories/config as config_repo
import logging
```

**Step 2: Add sync logic after token exchange**

In the `handle_callback` function, find line 167 where `Ok(updated_session) -> {` begins. Add the sync logic immediately after this line, before the existing `// Clean up one-time-use oauth request` comment:

```gleam
            Ok(updated_session) -> {
              // Sync actor on first login (blocking)
              case updated_session.did {
                Some(did) -> {
                  let plc_url = config_repo.get_plc_directory_url(conn)
                  let #(collection_ids, external_collection_ids) =
                    backfill.get_collection_ids(conn)

                  case actor_validator.ensure_actor_exists(conn, did, plc_url) {
                    Ok(True) -> {
                      // New actor - backfill collections synchronously
                      logging.log(
                        logging.Info,
                        "[oauth] Syncing new actor: " <> did,
                      )
                      let _ =
                        backfill.backfill_collections_for_actor(
                          conn,
                          did,
                          collection_ids,
                          external_collection_ids,
                          plc_url,
                        )
                      Nil
                    }
                    Ok(False) -> Nil
                    // Existing actor, already synced
                    Error(e) -> {
                      logging.log(
                        logging.Warning,
                        "[oauth] Actor sync failed for "
                          <> did
                          <> ": "
                          <> string.inspect(e),
                      )
                      Nil
                    }
                  }
                }
                None -> Nil
              }

              // Clean up one-time-use oauth request (existing code continues here)
```

**Step 3: Verify build succeeds**

Run: `cd server && gleam build`
Expected: Build succeeds with no errors

**Step 4: Run existing tests**

Run: `cd server && gleam test`
Expected: All tests pass

**Step 5: Commit**

```bash
git add server/src/handlers/oauth/atp_callback.gleam
git commit -m "feat(oauth): sync actor records on first login

Ensures new users see their existing ATProto data immediately after
OAuth login. Sync is blocking but errors are logged and don't fail
the login flow."
```

---

## Task 3: Manual Integration Test

**Step 1: Start the server**

Run: `cd server && gleam run`

**Step 2: Test OAuth flow with a new account**

1. Clear any existing session/actor data for your test DID
2. Go through OAuth login flow
3. Verify in logs: `[oauth] Syncing new actor: did:plc:...`
4. Verify records appear in database immediately after redirect

**Step 3: Test OAuth flow with existing account**

1. Log in again with same account
2. Verify NO sync log appears (actor already exists)
3. Login should be fast (no blocking sync)

---

## Summary

| Task | Description | Files |
|------|-------------|-------|
| 1 | Add `get_collection_ids` helper | `backfill.gleam` |
| 2 | Add actor sync to OAuth callback | `atp_callback.gleam` |
| 3 | Manual integration test | - |

**Total changes:** ~50 lines across 2 files
