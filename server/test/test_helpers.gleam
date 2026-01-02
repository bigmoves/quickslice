/// Test helper utilities for database setup
///
/// Provides common database setup functions for tests
import database/executor.{type DbError, type Executor}
import database/sqlite/connection as db_connection
import gleam/result

/// Create an in-memory SQLite database for testing
pub fn create_test_db() -> Result(Executor, DbError) {
  db_connection.connect("sqlite::memory:")
}

/// Create the record table for tests
pub fn create_record_table(exec: Executor) -> Result(Nil, DbError) {
  executor.exec(
    exec,
    "CREATE TABLE IF NOT EXISTS record (
      uri TEXT PRIMARY KEY NOT NULL,
      cid TEXT NOT NULL,
      did TEXT NOT NULL,
      collection TEXT NOT NULL,
      json TEXT NOT NULL,
      indexed_at TEXT NOT NULL DEFAULT (datetime('now')),
      rkey TEXT GENERATED ALWAYS AS (
        substr(uri, instr(substr(uri, instr(substr(uri, 6), '/') + 6), '/') + instr(substr(uri, 6), '/') + 6)
      ) STORED
    )",
    [],
  )
}

/// Create the actor table for tests
pub fn create_actor_table(exec: Executor) -> Result(Nil, DbError) {
  executor.exec(
    exec,
    "CREATE TABLE IF NOT EXISTS actor (
      did TEXT PRIMARY KEY NOT NULL,
      handle TEXT,
      indexed_at TEXT NOT NULL
    )",
    [],
  )
}

/// Create the lexicon table for tests
pub fn create_lexicon_table(exec: Executor) -> Result(Nil, DbError) {
  executor.exec(
    exec,
    "CREATE TABLE IF NOT EXISTS lexicon (
      id TEXT PRIMARY KEY NOT NULL,
      json TEXT NOT NULL,
      created_at TEXT NOT NULL DEFAULT (datetime('now'))
    )",
    [],
  )
}

/// Create the config table for tests
pub fn create_config_table(exec: Executor) -> Result(Nil, DbError) {
  executor.exec(
    exec,
    "CREATE TABLE IF NOT EXISTS config (
      key TEXT PRIMARY KEY NOT NULL,
      value TEXT NOT NULL,
      updated_at TEXT NOT NULL DEFAULT (datetime('now'))
    )",
    [],
  )
}

/// Create jetstream activity table for tests
pub fn create_jetstream_activity_table(exec: Executor) -> Result(Nil, DbError) {
  executor.exec(
    exec,
    "CREATE TABLE IF NOT EXISTS jetstream_activity (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      timestamp TEXT NOT NULL,
      operation TEXT NOT NULL,
      collection TEXT NOT NULL,
      did TEXT NOT NULL,
      status TEXT NOT NULL,
      error_message TEXT,
      event_json TEXT NOT NULL
    )",
    [],
  )
}

/// Create jetstream cursor table for tests
pub fn create_jetstream_cursor_table(exec: Executor) -> Result(Nil, DbError) {
  executor.exec(
    exec,
    "CREATE TABLE IF NOT EXISTS jetstream_cursor (
      id INTEGER PRIMARY KEY CHECK (id = 1),
      cursor INTEGER NOT NULL,
      updated_at TEXT NOT NULL DEFAULT (datetime('now'))
    )",
    [],
  )
}

/// Create all OAuth tables for tests
pub fn create_oauth_tables(exec: Executor) -> Result(Nil, DbError) {
  use _ <- result.try(
    executor.exec(
      exec,
      "CREATE TABLE IF NOT EXISTS oauth_client (
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
    )",
      [],
    ),
  )

  use _ <- result.try(
    executor.exec(
      exec,
      "CREATE TABLE IF NOT EXISTS oauth_access_token (
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
    )",
      [],
    ),
  )

  use _ <- result.try(
    executor.exec(
      exec,
      "CREATE TABLE IF NOT EXISTS oauth_refresh_token (
      token TEXT PRIMARY KEY,
      access_token TEXT NOT NULL,
      client_id TEXT NOT NULL,
      user_id TEXT NOT NULL,
      session_id TEXT,
      session_iteration INTEGER,
      scope TEXT,
      created_at INTEGER NOT NULL,
      expires_at INTEGER,
      revoked INTEGER NOT NULL DEFAULT 0,
      FOREIGN KEY (client_id) REFERENCES oauth_client(client_id) ON DELETE CASCADE
    )",
      [],
    ),
  )

  use _ <- result.try(
    executor.exec(
      exec,
      "CREATE TABLE IF NOT EXISTS oauth_par_request (
      request_uri TEXT PRIMARY KEY,
      authorization_request TEXT NOT NULL,
      client_id TEXT NOT NULL,
      created_at INTEGER NOT NULL,
      expires_at INTEGER NOT NULL,
      subject TEXT,
      metadata TEXT NOT NULL DEFAULT '{}',
      FOREIGN KEY (client_id) REFERENCES oauth_client(client_id) ON DELETE CASCADE
    )",
      [],
    ),
  )

  use _ <- result.try(
    executor.exec(
      exec,
      "CREATE TABLE IF NOT EXISTS oauth_dpop_nonce (
      nonce TEXT PRIMARY KEY,
      expires_at INTEGER NOT NULL
    )",
      [],
    ),
  )

  use _ <- result.try(
    executor.exec(
      exec,
      "CREATE TABLE IF NOT EXISTS oauth_dpop_jti (
      jti TEXT PRIMARY KEY,
      created_at INTEGER NOT NULL
    )",
      [],
    ),
  )

  use _ <- result.try(
    executor.exec(
      exec,
      "CREATE TABLE IF NOT EXISTS oauth_auth_request (
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
    )",
      [],
    ),
  )

  use _ <- result.try(
    executor.exec(
      exec,
      "CREATE TABLE IF NOT EXISTS oauth_atp_session (
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
      [],
    ),
  )

  use _ <- result.try(
    executor.exec(
      exec,
      "CREATE TABLE IF NOT EXISTS oauth_atp_request (
      oauth_state TEXT PRIMARY KEY,
      authorization_server TEXT NOT NULL,
      nonce TEXT NOT NULL,
      pkce_verifier TEXT NOT NULL,
      signing_public_key TEXT NOT NULL,
      dpop_private_key TEXT NOT NULL,
      created_at INTEGER NOT NULL,
      expires_at INTEGER NOT NULL
    )",
      [],
    ),
  )

  executor.exec(
    exec,
    "CREATE TABLE IF NOT EXISTS oauth_authorization_code (
      code TEXT PRIMARY KEY,
      client_id TEXT NOT NULL,
      user_id TEXT NOT NULL,
      session_id TEXT,
      session_iteration INTEGER,
      redirect_uri TEXT NOT NULL,
      scope TEXT,
      code_challenge TEXT,
      code_challenge_method TEXT,
      nonce TEXT,
      created_at INTEGER NOT NULL,
      expires_at INTEGER NOT NULL,
      used INTEGER NOT NULL DEFAULT 0
    )",
    [],
  )
}

/// Create admin session table for tests
pub fn create_admin_session_table(exec: Executor) -> Result(Nil, DbError) {
  executor.exec(
    exec,
    "CREATE TABLE IF NOT EXISTS admin_session (
      session_id TEXT PRIMARY KEY,
      atp_session_id TEXT NOT NULL,
      created_at INTEGER NOT NULL DEFAULT (unixepoch())
    )",
    [],
  )
}

/// Create all core tables (record, actor, lexicon, config) for tests
pub fn create_core_tables(exec: Executor) -> Result(Nil, DbError) {
  use _ <- result.try(create_record_table(exec))
  use _ <- result.try(create_actor_table(exec))
  use _ <- result.try(create_lexicon_table(exec))
  create_config_table(exec)
}

/// Create all tables needed for a full test environment
pub fn create_all_tables(exec: Executor) -> Result(Nil, DbError) {
  use _ <- result.try(create_core_tables(exec))
  use _ <- result.try(create_jetstream_activity_table(exec))
  use _ <- result.try(create_jetstream_cursor_table(exec))
  use _ <- result.try(create_oauth_tables(exec))
  create_admin_session_table(exec)
}

/// Create label_definition table for tests
pub fn create_label_definition_table(exec: Executor) -> Result(Nil, DbError) {
  use _ <- result.try(
    executor.exec(
      exec,
      "CREATE TABLE IF NOT EXISTS label_definition (
      val TEXT PRIMARY KEY NOT NULL,
      description TEXT NOT NULL,
      severity TEXT NOT NULL CHECK (severity IN ('inform', 'alert', 'takedown')),
      default_visibility TEXT NOT NULL DEFAULT 'warn',
      created_at TEXT NOT NULL DEFAULT (datetime('now'))
    )",
      [],
    ),
  )

  // Seed default label definitions
  executor.exec(
    exec,
    "INSERT INTO label_definition (val, description, severity, default_visibility) VALUES
      ('!takedown', 'Content removed by moderators', 'takedown', 'hide'),
      ('!suspend', 'Account suspended', 'takedown', 'hide'),
      ('!warn', 'Show warning before displaying', 'alert', 'warn'),
      ('!hide', 'Hide from feeds', 'alert', 'hide'),
      ('porn', 'Pornographic content', 'alert', 'hide'),
      ('spam', 'Spam or unwanted content', 'inform', 'warn'),
      ('sexual', 'Sexually suggestive content', 'alert', 'warn'),
      ('nudity', 'Non-sexual nudity', 'alert', 'warn')",
    [],
  )
}

/// Create actor_label_preference table for tests
pub fn create_label_preference_table(exec: Executor) -> Result(Nil, DbError) {
  executor.exec(
    exec,
    "CREATE TABLE IF NOT EXISTS actor_label_preference (
      did TEXT NOT NULL,
      label_val TEXT NOT NULL,
      visibility TEXT NOT NULL,
      created_at TEXT NOT NULL DEFAULT (datetime('now')),
      PRIMARY KEY (did, label_val)
    )",
    [],
  )
}

/// Create label table for tests
pub fn create_label_table(exec: Executor) -> Result(Nil, DbError) {
  executor.exec(
    exec,
    "CREATE TABLE IF NOT EXISTS label (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      src TEXT NOT NULL,
      uri TEXT NOT NULL,
      cid TEXT,
      val TEXT NOT NULL,
      neg INTEGER NOT NULL DEFAULT 0,
      cts TEXT NOT NULL DEFAULT (datetime('now')),
      exp TEXT,
      FOREIGN KEY (val) REFERENCES label_definition(val)
    )",
    [],
  )
}

/// Create report table for tests
pub fn create_report_table(exec: Executor) -> Result(Nil, DbError) {
  executor.exec(
    exec,
    "CREATE TABLE IF NOT EXISTS report (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      reporter_did TEXT NOT NULL,
      subject_uri TEXT NOT NULL,
      reason_type TEXT NOT NULL CHECK (reason_type IN ('spam', 'violation', 'misleading', 'sexual', 'rude', 'other')),
      reason TEXT,
      status TEXT NOT NULL DEFAULT 'pending' CHECK (status IN ('pending', 'resolved', 'dismissed')),
      resolved_by TEXT,
      resolved_at TEXT,
      created_at TEXT NOT NULL DEFAULT (datetime('now'))
    )",
    [],
  )
}

/// Create labels and reports tables for tests
pub fn create_moderation_tables(exec: Executor) -> Result(Nil, DbError) {
  use _ <- result.try(create_label_definition_table(exec))
  use _ <- result.try(create_label_table(exec))
  use _ <- result.try(create_report_table(exec))
  create_label_preference_table(exec)
}

/// Insert a test token that maps to a DID for testing viewer authentication
pub fn insert_test_token(
  exec: Executor,
  token: String,
  did: String,
) -> Result(Nil, DbError) {
  // First, insert a test client if it doesn't exist (required for foreign key)
  use _ <- result.try(
    executor.exec(
      exec,
      "INSERT OR IGNORE INTO oauth_client (client_id, client_name, redirect_uris, grant_types, response_types, token_endpoint_auth_method, client_type, created_at, updated_at) VALUES ('test-client', 'Test Client', '[]', '[]', '[]', 'none', 'public', 0, 0)",
      [],
    ),
  )

  let far_future = 9_999_999_999
  // Won't expire
  executor.exec(
    exec,
    "INSERT INTO oauth_access_token (token, token_type, client_id, user_id, scope, created_at, expires_at, revoked) VALUES (?, 'Bearer', 'test-client', ?, 'atproto', 0, ?, 0)",
    [executor.Text(token), executor.Text(did), executor.Int(far_future)],
  )
}
