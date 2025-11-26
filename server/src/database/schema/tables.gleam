import gleam/result
import sqlight

/// Creates the record table if it doesn't exist
pub fn create_record_table(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let create_table_sql =
    "
    CREATE TABLE IF NOT EXISTS record (
      uri TEXT PRIMARY KEY NOT NULL,
      cid TEXT NOT NULL,
      did TEXT NOT NULL,
      collection TEXT NOT NULL,
      json TEXT NOT NULL,
      indexed_at TEXT NOT NULL DEFAULT (datetime('now'))
    )
  "

  let create_did_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_record_did
    ON record(did)
  "

  let create_collection_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_record_collection
    ON record(collection)
  "

  let create_did_collection_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_record_did_collection
    ON record(did, collection)
  "

  let create_indexed_at_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_record_indexed_at
    ON record(indexed_at DESC)
  "

  use _ <- result.try(sqlight.exec(create_table_sql, conn))
  use _ <- result.try(sqlight.exec(create_did_index_sql, conn))
  use _ <- result.try(sqlight.exec(create_collection_index_sql, conn))
  use _ <- result.try(sqlight.exec(create_did_collection_index_sql, conn))
  use _ <- result.try(sqlight.exec(create_indexed_at_index_sql, conn))
  Ok(Nil)
}

/// Creates the actor table if it doesn't exist
pub fn create_actor_table(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let create_table_sql =
    "
    CREATE TABLE IF NOT EXISTS actor (
      did TEXT PRIMARY KEY NOT NULL,
      handle TEXT,
      indexed_at TEXT NOT NULL
    )
  "

  let create_handle_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_actor_handle
    ON actor(handle)
  "

  let create_indexed_at_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_actor_indexed_at
    ON actor(indexed_at DESC)
  "

  use _ <- result.try(sqlight.exec(create_table_sql, conn))
  use _ <- result.try(sqlight.exec(create_handle_index_sql, conn))
  use _ <- result.try(sqlight.exec(create_indexed_at_index_sql, conn))
  Ok(Nil)
}

/// Creates the lexicon table if it doesn't exist
pub fn create_lexicon_table(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let create_table_sql =
    "
    CREATE TABLE IF NOT EXISTS lexicon (
      id TEXT PRIMARY KEY NOT NULL,
      json TEXT NOT NULL,
      created_at TEXT NOT NULL DEFAULT (datetime('now'))
    )
  "

  let create_created_at_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_lexicon_created_at
    ON lexicon(created_at DESC)
  "

  use _ <- result.try(sqlight.exec(create_table_sql, conn))
  use _ <- result.try(sqlight.exec(create_created_at_index_sql, conn))
  Ok(Nil)
}

/// Creates the config table if it doesn't exist
pub fn create_config_table(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let create_table_sql =
    "
    CREATE TABLE IF NOT EXISTS config (
      key TEXT PRIMARY KEY NOT NULL,
      value TEXT NOT NULL,
      updated_at TEXT NOT NULL DEFAULT (datetime('now'))
    )
  "

  sqlight.exec(create_table_sql, conn)
}

/// Creates the CID index for record deduplication
pub fn create_cid_index(conn: sqlight.Connection) -> Result(Nil, sqlight.Error) {
  let create_cid_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_record_cid
    ON record(cid)
  "

  sqlight.exec(create_cid_index_sql, conn)
}

/// Creates the jetstream_activity table for 24h activity log
pub fn create_jetstream_activity_table(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let create_table_sql =
    "
    CREATE TABLE IF NOT EXISTS jetstream_activity (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      timestamp TEXT NOT NULL,
      operation TEXT NOT NULL,
      collection TEXT NOT NULL,
      did TEXT NOT NULL,
      status TEXT NOT NULL,
      error_message TEXT,
      event_json TEXT NOT NULL
    )
  "

  let create_timestamp_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_jetstream_activity_timestamp
    ON jetstream_activity(timestamp DESC)
  "

  use _ <- result.try(sqlight.exec(create_table_sql, conn))
  sqlight.exec(create_timestamp_index_sql, conn)
}

/// Creates the jetstream_cursor table for cursor tracking
pub fn create_jetstream_cursor_table(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let create_table_sql =
    "
    CREATE TABLE IF NOT EXISTS jetstream_cursor (
      id INTEGER PRIMARY KEY CHECK (id = 1),
      cursor INTEGER NOT NULL,
      updated_at TEXT NOT NULL DEFAULT (datetime('now'))
    )
  "

  sqlight.exec(create_table_sql, conn)
}

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
      session_iteration INTEGER,
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

/// Creates the oauth_authorization_code table for OAuth authorization codes
pub fn create_oauth_authorization_code_table(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let create_table_sql =
    "
    CREATE TABLE IF NOT EXISTS oauth_authorization_code (
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
    )
  "

  let create_expires_at_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_oauth_authorization_code_expires_at
    ON oauth_authorization_code(expires_at)
  "

  use _ <- result.try(sqlight.exec(create_table_sql, conn))
  sqlight.exec(create_expires_at_index_sql, conn)
}

/// Creates the admin_session table for admin browser sessions
pub fn create_admin_session_table(
  conn: sqlight.Connection,
) -> Result(Nil, sqlight.Error) {
  let create_table_sql =
    "
    CREATE TABLE IF NOT EXISTS admin_session (
      session_id TEXT PRIMARY KEY,
      atp_session_id TEXT NOT NULL,
      created_at INTEGER NOT NULL DEFAULT (unixepoch())
    )
  "

  let create_atp_session_index_sql =
    "
    CREATE INDEX IF NOT EXISTS idx_admin_session_atp_session_id
    ON admin_session(atp_session_id)
  "

  use _ <- result.try(sqlight.exec(create_table_sql, conn))
  sqlight.exec(create_atp_session_index_sql, conn)
}
