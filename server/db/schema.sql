CREATE TABLE IF NOT EXISTS "schema_migrations" (version varchar(128) primary key);
CREATE TABLE record (
  uri TEXT PRIMARY KEY NOT NULL,
  cid TEXT NOT NULL,
  did TEXT NOT NULL,
  collection TEXT NOT NULL,
  json TEXT NOT NULL,
  indexed_at TEXT NOT NULL DEFAULT (datetime('now'))
, rkey TEXT
  GENERATED ALWAYS AS (
    substr(uri, instr(substr(uri, instr(substr(uri, 6), '/') + 6), '/') + instr(substr(uri, 6), '/') + 6)
  ) VIRTUAL);
CREATE INDEX idx_record_did ON record(did);
CREATE INDEX idx_record_collection ON record(collection);
CREATE INDEX idx_record_did_collection ON record(did, collection);
CREATE INDEX idx_record_indexed_at ON record(indexed_at DESC);
CREATE INDEX idx_record_cid ON record(cid);
CREATE TABLE actor (
  did TEXT PRIMARY KEY NOT NULL,
  handle TEXT,
  indexed_at TEXT NOT NULL
);
CREATE INDEX idx_actor_handle ON actor(handle);
CREATE INDEX idx_actor_indexed_at ON actor(indexed_at DESC);
CREATE TABLE lexicon (
  id TEXT PRIMARY KEY NOT NULL,
  json TEXT NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);
CREATE INDEX idx_lexicon_created_at ON lexicon(created_at DESC);
CREATE TABLE config (
  key TEXT PRIMARY KEY NOT NULL,
  value TEXT NOT NULL,
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);
CREATE TABLE jetstream_activity (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  timestamp TEXT NOT NULL,
  operation TEXT NOT NULL,
  collection TEXT NOT NULL,
  did TEXT NOT NULL,
  status TEXT NOT NULL,
  error_message TEXT,
  event_json TEXT NOT NULL
);
CREATE INDEX idx_jetstream_activity_timestamp ON jetstream_activity(timestamp DESC);
CREATE TABLE jetstream_cursor (
  id INTEGER PRIMARY KEY CHECK (id = 1),
  cursor INTEGER NOT NULL,
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);
CREATE TABLE oauth_client (
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
);
CREATE TABLE oauth_access_token (
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
);
CREATE INDEX idx_oauth_access_token_expires_at ON oauth_access_token(expires_at);
CREATE INDEX idx_oauth_access_token_client_id ON oauth_access_token(client_id);
CREATE INDEX idx_oauth_access_token_user_id ON oauth_access_token(user_id);
CREATE INDEX idx_oauth_access_token_dpop_jkt ON oauth_access_token(dpop_jkt);
CREATE TABLE oauth_refresh_token (
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
);
CREATE INDEX idx_oauth_refresh_token_expires_at ON oauth_refresh_token(expires_at);
CREATE INDEX idx_oauth_refresh_token_client_id ON oauth_refresh_token(client_id);
CREATE TABLE oauth_par_request (
  request_uri TEXT PRIMARY KEY,
  authorization_request TEXT NOT NULL,
  client_id TEXT NOT NULL,
  created_at INTEGER NOT NULL,
  expires_at INTEGER NOT NULL,
  subject TEXT,
  metadata TEXT NOT NULL DEFAULT '{}',
  FOREIGN KEY (client_id) REFERENCES oauth_client(client_id) ON DELETE CASCADE
);
CREATE INDEX idx_oauth_par_request_expires_at ON oauth_par_request(expires_at);
CREATE TABLE oauth_dpop_nonce (
  nonce TEXT PRIMARY KEY,
  expires_at INTEGER NOT NULL
);
CREATE INDEX idx_oauth_dpop_nonce_expires_at ON oauth_dpop_nonce(expires_at);
CREATE TABLE oauth_dpop_jti (
  jti TEXT PRIMARY KEY,
  created_at INTEGER NOT NULL
);
CREATE INDEX idx_oauth_dpop_jti_created_at ON oauth_dpop_jti(created_at);
CREATE TABLE oauth_auth_request (
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
);
CREATE INDEX idx_oauth_auth_request_expires_at ON oauth_auth_request(expires_at);
CREATE INDEX idx_oauth_auth_request_client_id ON oauth_auth_request(client_id);
CREATE TABLE oauth_atp_session (
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
);
CREATE INDEX idx_oauth_atp_session_did ON oauth_atp_session(did);
CREATE INDEX idx_oauth_atp_session_access_token ON oauth_atp_session(access_token);
CREATE TABLE oauth_atp_request (
  oauth_state TEXT PRIMARY KEY,
  authorization_server TEXT NOT NULL,
  nonce TEXT NOT NULL,
  pkce_verifier TEXT NOT NULL,
  signing_public_key TEXT NOT NULL,
  dpop_private_key TEXT NOT NULL,
  created_at INTEGER NOT NULL,
  expires_at INTEGER NOT NULL
);
CREATE INDEX idx_oauth_atp_request_expires_at ON oauth_atp_request(expires_at);
CREATE TABLE oauth_authorization_code (
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
);
CREATE INDEX idx_oauth_authorization_code_expires_at ON oauth_authorization_code(expires_at);
CREATE TABLE admin_session (
  session_id TEXT PRIMARY KEY,
  atp_session_id TEXT NOT NULL,
  created_at INTEGER NOT NULL DEFAULT (unixepoch())
);
CREATE INDEX idx_admin_session_atp_session_id ON admin_session(atp_session_id);
-- Dbmate schema migrations
INSERT INTO "schema_migrations" (version) VALUES
  ('20241210000001'),
  ('20241227000001');
