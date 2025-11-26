# OAuth Tables Migration Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add OAuth table definitions to `server/src/database/schema/tables.gleam` with inline indexes, then wire up migration v6.

**Architecture:** Each OAuth table gets a `create_oauth_*_table()` function following the existing pattern in tables.gleam. Tables use integer timestamps for expiration logic. Foreign keys reference `oauth_client` where applicable. Migration v6 creates all OAuth tables.

**Tech Stack:** Gleam, SQLite, sqlight library

---

## Task 1: Add oauth_client table

**Files:**
- Modify: `server/src/database/schema/tables.gleam:177` (append after last function)

**Step 1: Add the create_oauth_client_table function**

Add this function at the end of `tables.gleam`:

```gleam
/// Creates the oauth_client table for registered OAuth clients
pub fn create_oauth_client_table(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let create_table_sql =
    "
    CREATE TABLE IF NOT EXISTS oauth_client (
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
      metadata TEXT NOT NULL DEFAULT '{}',
      access_token_expiration INTEGER NOT NULL DEFAULT 86400,
      refresh_token_expiration INTEGER NOT NULL DEFAULT 1209600,
      require_redirect_exact INTEGER NOT NULL DEFAULT 1,
      registration_access_token TEXT,
      jwks TEXT
    )
  "

  sqlight.exec(create_table_sql, conn)
}
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/database/schema/tables.gleam
git commit -m "feat: add oauth_client table definition"
```

---

## Task 2: Add oauth_access_token table

**Files:**
- Modify: `server/src/database/schema/tables.gleam` (append after oauth_client function)

**Step 1: Add the create_oauth_access_token_table function**

```gleam
/// Creates the oauth_access_token table for issued access tokens
pub fn create_oauth_access_token_table(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let create_table_sql =
    "
    CREATE TABLE IF NOT EXISTS oauth_access_token (
      token TEXT PRIMARY KEY,
      token_type TEXT NOT NULL DEFAULT 'Bearer',
      client_id TEXT NOT NULL,
      user_id TEXT,
      session_id TEXT,
      session_iteration INTEGER,
      scope TEXT,
      created_at INTEGER NOT NULL,
      expires_at INTEGER NOT NULL,
      revoked INTEGER NOT NULL DEFAULT 0,
      dpop_jkt TEXT,
      FOREIGN KEY (client_id) REFERENCES oauth_client(client_id) ON DELETE CASCADE
    )
  "

  let create_expires_at_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_oauth_access_token_expires_at
    ON oauth_access_token(expires_at)
  "

  let create_client_id_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_oauth_access_token_client_id
    ON oauth_access_token(client_id)
  "

  let create_user_id_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_oauth_access_token_user_id
    ON oauth_access_token(user_id)
  "

  let create_dpop_jkt_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_oauth_access_token_dpop_jkt
    ON oauth_access_token(dpop_jkt)
  "

  use _ <- result.try(sqlight.exec(create_table_sql, conn))
  use _ <- result.try(sqlight.exec(create_expires_at_index_sql, conn))
  use _ <- result.try(sqlight.exec(create_client_id_index_sql, conn))
  use _ <- result.try(sqlight.exec(create_user_id_index_sql, conn))
  sqlight.exec(create_dpop_jkt_index_sql, conn)
}
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/database/schema/tables.gleam
git commit -m "feat: add oauth_access_token table definition"
```

---

## Task 3: Add oauth_refresh_token table

**Files:**
- Modify: `server/src/database/schema/tables.gleam` (append after oauth_access_token function)

**Step 1: Add the create_oauth_refresh_token_table function**

```gleam
/// Creates the oauth_refresh_token table for issued refresh tokens
pub fn create_oauth_refresh_token_table(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let create_table_sql =
    "
    CREATE TABLE IF NOT EXISTS oauth_refresh_token (
      token TEXT PRIMARY KEY,
      access_token TEXT NOT NULL,
      client_id TEXT NOT NULL,
      user_id TEXT NOT NULL,
      session_id TEXT,
      scope TEXT,
      created_at INTEGER NOT NULL,
      expires_at INTEGER,
      revoked INTEGER NOT NULL DEFAULT 0,
      FOREIGN KEY (client_id) REFERENCES oauth_client(client_id) ON DELETE CASCADE
    )
  "

  let create_expires_at_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_oauth_refresh_token_expires_at
    ON oauth_refresh_token(expires_at)
  "

  let create_client_id_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_oauth_refresh_token_client_id
    ON oauth_refresh_token(client_id)
  "

  use _ <- result.try(sqlight.exec(create_table_sql, conn))
  use _ <- result.try(sqlight.exec(create_expires_at_index_sql, conn))
  sqlight.exec(create_client_id_index_sql, conn)
}
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/database/schema/tables.gleam
git commit -m "feat: add oauth_refresh_token table definition"
```

---

## Task 4: Add oauth_par_request table

**Files:**
- Modify: `server/src/database/schema/tables.gleam` (append after oauth_refresh_token function)

**Step 1: Add the create_oauth_par_request_table function**

```gleam
/// Creates the oauth_par_request table for Pushed Authorization Requests
pub fn create_oauth_par_request_table(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let create_table_sql =
    "
    CREATE TABLE IF NOT EXISTS oauth_par_request (
      request_uri TEXT PRIMARY KEY,
      authorization_request TEXT NOT NULL,
      client_id TEXT NOT NULL,
      created_at INTEGER NOT NULL,
      expires_at INTEGER NOT NULL,
      subject TEXT,
      metadata TEXT NOT NULL DEFAULT '{}',
      FOREIGN KEY (client_id) REFERENCES oauth_client(client_id) ON DELETE CASCADE
    )
  "

  let create_expires_at_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_oauth_par_request_expires_at
    ON oauth_par_request(expires_at)
  "

  use _ <- result.try(sqlight.exec(create_table_sql, conn))
  sqlight.exec(create_expires_at_index_sql, conn)
}
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/database/schema/tables.gleam
git commit -m "feat: add oauth_par_request table definition"
```

---

## Task 5: Add oauth_dpop_nonce table

**Files:**
- Modify: `server/src/database/schema/tables.gleam` (append after oauth_par_request function)

**Step 1: Add the create_oauth_dpop_nonce_table function**

```gleam
/// Creates the oauth_dpop_nonce table for DPoP nonce tracking
pub fn create_oauth_dpop_nonce_table(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let create_table_sql =
    "
    CREATE TABLE IF NOT EXISTS oauth_dpop_nonce (
      nonce TEXT PRIMARY KEY,
      expires_at INTEGER NOT NULL
    )
  "

  let create_expires_at_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_oauth_dpop_nonce_expires_at
    ON oauth_dpop_nonce(expires_at)
  "

  use _ <- result.try(sqlight.exec(create_table_sql, conn))
  sqlight.exec(create_expires_at_index_sql, conn)
}
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/database/schema/tables.gleam
git commit -m "feat: add oauth_dpop_nonce table definition"
```

---

## Task 6: Add oauth_auth_request table

**Files:**
- Modify: `server/src/database/schema/tables.gleam` (append after oauth_dpop_nonce function)

**Step 1: Add the create_oauth_auth_request_table function**

```gleam
/// Creates the oauth_auth_request table for client authorization requests during bridge flow
pub fn create_oauth_auth_request_table(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let create_table_sql =
    "
    CREATE TABLE IF NOT EXISTS oauth_auth_request (
      session_id TEXT PRIMARY KEY,
      client_id TEXT NOT NULL,
      redirect_uri TEXT NOT NULL,
      scope TEXT,
      state TEXT,
      code_challenge TEXT,
      code_challenge_method TEXT,
      response_type TEXT NOT NULL,
      nonce TEXT,
      login_hint TEXT,
      created_at INTEGER NOT NULL,
      expires_at INTEGER NOT NULL,
      FOREIGN KEY (client_id) REFERENCES oauth_client(client_id) ON DELETE CASCADE
    )
  "

  let create_expires_at_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_oauth_auth_request_expires_at
    ON oauth_auth_request(expires_at)
  "

  let create_client_id_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_oauth_auth_request_client_id
    ON oauth_auth_request(client_id)
  "

  use _ <- result.try(sqlight.exec(create_table_sql, conn))
  use _ <- result.try(sqlight.exec(create_expires_at_index_sql, conn))
  sqlight.exec(create_client_id_index_sql, conn)
}
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/database/schema/tables.gleam
git commit -m "feat: add oauth_auth_request table definition"
```

---

## Task 7: Add oauth_atp_session table

**Files:**
- Modify: `server/src/database/schema/tables.gleam` (append after oauth_auth_request function)

**Step 1: Add the create_oauth_atp_session_table function**

```gleam
/// Creates the oauth_atp_session table for ATP bridge session state
pub fn create_oauth_atp_session_table(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let create_table_sql =
    "
    CREATE TABLE IF NOT EXISTS oauth_atp_session (
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
    )
  "

  let create_did_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_oauth_atp_session_did
    ON oauth_atp_session(did)
  "

  let create_access_token_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_oauth_atp_session_access_token
    ON oauth_atp_session(access_token)
  "

  use _ <- result.try(sqlight.exec(create_table_sql, conn))
  use _ <- result.try(sqlight.exec(create_did_index_sql, conn))
  sqlight.exec(create_access_token_index_sql, conn)
}
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/database/schema/tables.gleam
git commit -m "feat: add oauth_atp_session table definition"
```

---

## Task 8: Add oauth_atp_request table

**Files:**
- Modify: `server/src/database/schema/tables.gleam` (append after oauth_atp_session function)

**Step 1: Add the create_oauth_atp_request_table function**

```gleam
/// Creates the oauth_atp_request table for outbound OAuth requests to ATP
pub fn create_oauth_atp_request_table(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let create_table_sql =
    "
    CREATE TABLE IF NOT EXISTS oauth_atp_request (
      oauth_state TEXT PRIMARY KEY,
      authorization_server TEXT NOT NULL,
      nonce TEXT NOT NULL,
      pkce_verifier TEXT NOT NULL,
      signing_public_key TEXT NOT NULL,
      dpop_private_key TEXT NOT NULL,
      created_at INTEGER NOT NULL,
      expires_at INTEGER NOT NULL
    )
  "

  let create_expires_at_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_oauth_atp_request_expires_at
    ON oauth_atp_request(expires_at)
  "

  use _ <- result.try(sqlight.exec(create_table_sql, conn))
  sqlight.exec(create_expires_at_index_sql, conn)
}
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/database/schema/tables.gleam
git commit -m "feat: add oauth_atp_request table definition"
```

---

## Task 9: Add migration v6 for OAuth tables

**Files:**
- Modify: `server/src/database/schema/migrations.gleam`

**Step 1: Add migration_v6 function**

Add after the `migration_v5` function (around line 124):

```gleam
/// Migration v6: Add OAuth tables
fn migration_v6(conn: sqlight.Connection) -> Result(Nil, sqlight.Error) {
  logging.log(logging.Info, "Running migration v6 (OAuth tables)...")

  // Create OAuth tables in dependency order (oauth_client first due to foreign keys)
  use _ <- result.try(tables.create_oauth_client_table(conn))
  use _ <- result.try(tables.create_oauth_access_token_table(conn))
  use _ <- result.try(tables.create_oauth_refresh_token_table(conn))
  use _ <- result.try(tables.create_oauth_par_request_table(conn))
  use _ <- result.try(tables.create_oauth_dpop_nonce_table(conn))
  use _ <- result.try(tables.create_oauth_auth_request_table(conn))
  use _ <- result.try(tables.create_oauth_atp_session_table(conn))
  tables.create_oauth_atp_request_table(conn)
}
```

**Step 2: Update run_migrations to include v6**

Replace the entire `run_migrations` function with:

```gleam
/// Runs all pending migrations based on current schema version
pub fn run_migrations(conn: sqlight.Connection) -> Result(Nil, sqlight.Error) {
  use _ <- result.try(create_schema_version_table(conn))
  use current_version <- result.try(get_current_version(conn))

  logging.log(
    logging.Info,
    "Current schema version: " <> int.to_string(current_version),
  )

  // Apply migrations sequentially based on current version
  case current_version {
    // Fresh database or pre-migration database - run v1
    0 -> {
      use _ <- result.try(apply_migration(conn, 1, migration_v1))
      use _ <- result.try(apply_migration(conn, 2, migration_v2))
      use _ <- result.try(apply_migration(conn, 3, migration_v3))
      use _ <- result.try(apply_migration(conn, 4, migration_v4))
      use _ <- result.try(apply_migration(conn, 5, migration_v5))
      apply_migration(conn, 6, migration_v6)
    }

    // Run v2, v3, v4, v5, and v6 migrations
    1 -> {
      use _ <- result.try(apply_migration(conn, 2, migration_v2))
      use _ <- result.try(apply_migration(conn, 3, migration_v3))
      use _ <- result.try(apply_migration(conn, 4, migration_v4))
      use _ <- result.try(apply_migration(conn, 5, migration_v5))
      apply_migration(conn, 6, migration_v6)
    }

    // Run v3, v4, v5, and v6 migrations
    2 -> {
      use _ <- result.try(apply_migration(conn, 3, migration_v3))
      use _ <- result.try(apply_migration(conn, 4, migration_v4))
      use _ <- result.try(apply_migration(conn, 5, migration_v5))
      apply_migration(conn, 6, migration_v6)
    }

    // Run v4, v5, and v6 migrations
    3 -> {
      use _ <- result.try(apply_migration(conn, 4, migration_v4))
      use _ <- result.try(apply_migration(conn, 5, migration_v5))
      apply_migration(conn, 6, migration_v6)
    }

    // Run v5 and v6 migrations
    4 -> {
      use _ <- result.try(apply_migration(conn, 5, migration_v5))
      apply_migration(conn, 6, migration_v6)
    }

    // Run v6 migration
    5 -> apply_migration(conn, 6, migration_v6)

    // Already at latest version
    6 -> {
      logging.log(logging.Info, "Schema is up to date (v6)")
      Ok(Nil)
    }

    // Future versions would be handled here
    _ -> {
      logging.log(
        logging.Error,
        "Unknown schema version: " <> int.to_string(current_version),
      )
      Ok(Nil)
    }
  }
}
```

**Step 3: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add server/src/database/schema/migrations.gleam
git commit -m "feat: add migration v6 for OAuth tables"
```

---

## Task 10: Test migration with fresh database

**Step 1: Back up existing database (if any)**

```bash
cd /Users/chadmiller/code/quickslice/server
if [ -f quickslice.db ]; then cp quickslice.db quickslice.db.backup; fi
```

**Step 2: Remove existing database to test fresh migration**

```bash
rm -f quickslice.db quickslice.db-wal quickslice.db-shm
```

**Step 3: Run the server to trigger migrations**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam run`
Expected: Server starts, logs show migrations v1-v6 applied

**Step 4: Verify tables exist**

```bash
sqlite3 quickslice.db ".tables"
```
Expected output should include: `oauth_client`, `oauth_access_token`, `oauth_refresh_token`, `oauth_par_request`, `oauth_dpop_nonce`, `oauth_auth_request`, `oauth_atp_session`, `oauth_atp_request`

**Step 5: Verify indexes exist**

```bash
sqlite3 quickslice.db ".indexes"
```
Expected: Should show all the `idx_oauth_*` indexes

**Step 6: Verify foreign keys work**

```bash
sqlite3 quickslice.db "PRAGMA foreign_key_list(oauth_access_token);"
```
Expected: Shows foreign key to `oauth_client`

**Step 7: Restore backup if needed**

```bash
cd /Users/chadmiller/code/quickslice/server
if [ -f quickslice.db.backup ]; then mv quickslice.db.backup quickslice.db; fi
```

**Step 8: Commit final verification**

```bash
git add -A
git commit -m "test: verify OAuth tables migration works"
```

---

Plan complete and saved to `docs/plans/2025-11-24-oauth-tables-migration.md`. Two execution options:

**1. Subagent-Driven (this session)** - I dispatch fresh subagent per task, review between tasks, fast iteration

**2. Parallel Session (separate)** - Open new session with executing-plans, batch execution with checkpoints

Which approach?
