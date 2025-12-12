-- migrate:up

-- =============================================================================
-- Core Tables
-- =============================================================================

-- Record table for AT Protocol records
CREATE TABLE IF NOT EXISTS record (
  uri TEXT PRIMARY KEY NOT NULL,
  cid TEXT NOT NULL,
  did TEXT NOT NULL,
  collection TEXT NOT NULL,
  json JSONB NOT NULL,
  indexed_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_record_did ON record(did);
CREATE INDEX IF NOT EXISTS idx_record_collection ON record(collection);
CREATE INDEX IF NOT EXISTS idx_record_did_collection ON record(did, collection);
CREATE INDEX IF NOT EXISTS idx_record_indexed_at ON record(indexed_at DESC);
CREATE INDEX IF NOT EXISTS idx_record_cid ON record(cid);
CREATE INDEX IF NOT EXISTS idx_record_json_gin ON record USING GIN(json);

-- Actor table for AT Protocol actors (users)
CREATE TABLE IF NOT EXISTS actor (
  did TEXT PRIMARY KEY NOT NULL,
  handle TEXT,
  indexed_at TIMESTAMP WITH TIME ZONE NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_actor_handle ON actor(handle);
CREATE INDEX IF NOT EXISTS idx_actor_indexed_at ON actor(indexed_at DESC);

-- Lexicon table for schema definitions
CREATE TABLE IF NOT EXISTS lexicon (
  id TEXT PRIMARY KEY NOT NULL,
  json JSONB NOT NULL,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_lexicon_created_at ON lexicon(created_at DESC);

-- Config table for application settings
CREATE TABLE IF NOT EXISTS config (
  key TEXT PRIMARY KEY NOT NULL,
  value TEXT NOT NULL,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

-- =============================================================================
-- Jetstream Tables
-- =============================================================================

-- Jetstream activity log for 24h activity tracking
CREATE TABLE IF NOT EXISTS jetstream_activity (
  id SERIAL PRIMARY KEY,
  timestamp TIMESTAMP WITH TIME ZONE NOT NULL,
  operation TEXT NOT NULL,
  collection TEXT NOT NULL,
  did TEXT NOT NULL,
  status TEXT NOT NULL,
  error_message TEXT,
  event_json JSONB NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_jetstream_activity_timestamp ON jetstream_activity(timestamp DESC);

-- Jetstream cursor for tracking stream position
CREATE TABLE IF NOT EXISTS jetstream_cursor (
  id INTEGER PRIMARY KEY CHECK (id = 1),
  cursor BIGINT NOT NULL,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

-- =============================================================================
-- OAuth Tables
-- =============================================================================

-- OAuth clients (registered applications)
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
  created_at BIGINT NOT NULL,
  updated_at BIGINT NOT NULL,
  metadata JSONB NOT NULL DEFAULT '{}',
  access_token_expiration INTEGER NOT NULL DEFAULT 86400,
  refresh_token_expiration INTEGER NOT NULL DEFAULT 1209600,
  require_redirect_exact BOOLEAN NOT NULL DEFAULT TRUE,
  registration_access_token TEXT,
  jwks JSONB
);

-- OAuth access tokens
CREATE TABLE IF NOT EXISTS oauth_access_token (
  token TEXT PRIMARY KEY,
  token_type TEXT NOT NULL DEFAULT 'Bearer',
  client_id TEXT NOT NULL,
  user_id TEXT,
  session_id TEXT,
  session_iteration INTEGER,
  scope TEXT,
  created_at BIGINT NOT NULL,
  expires_at BIGINT NOT NULL,
  revoked BOOLEAN NOT NULL DEFAULT FALSE,
  dpop_jkt TEXT,
  FOREIGN KEY (client_id) REFERENCES oauth_client(client_id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_oauth_access_token_expires_at ON oauth_access_token(expires_at);
CREATE INDEX IF NOT EXISTS idx_oauth_access_token_client_id ON oauth_access_token(client_id);
CREATE INDEX IF NOT EXISTS idx_oauth_access_token_user_id ON oauth_access_token(user_id);
CREATE INDEX IF NOT EXISTS idx_oauth_access_token_dpop_jkt ON oauth_access_token(dpop_jkt);

-- OAuth refresh tokens
CREATE TABLE IF NOT EXISTS oauth_refresh_token (
  token TEXT PRIMARY KEY,
  access_token TEXT NOT NULL,
  client_id TEXT NOT NULL,
  user_id TEXT NOT NULL,
  session_id TEXT,
  session_iteration INTEGER,
  scope TEXT,
  created_at BIGINT NOT NULL,
  expires_at BIGINT,
  revoked BOOLEAN NOT NULL DEFAULT FALSE,
  FOREIGN KEY (client_id) REFERENCES oauth_client(client_id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_oauth_refresh_token_expires_at ON oauth_refresh_token(expires_at);
CREATE INDEX IF NOT EXISTS idx_oauth_refresh_token_client_id ON oauth_refresh_token(client_id);

-- OAuth Pushed Authorization Requests (PAR)
CREATE TABLE IF NOT EXISTS oauth_par_request (
  request_uri TEXT PRIMARY KEY,
  authorization_request TEXT NOT NULL,
  client_id TEXT NOT NULL,
  created_at BIGINT NOT NULL,
  expires_at BIGINT NOT NULL,
  subject TEXT,
  metadata JSONB NOT NULL DEFAULT '{}',
  FOREIGN KEY (client_id) REFERENCES oauth_client(client_id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_oauth_par_request_expires_at ON oauth_par_request(expires_at);

-- OAuth DPoP nonces
CREATE TABLE IF NOT EXISTS oauth_dpop_nonce (
  nonce TEXT PRIMARY KEY,
  expires_at BIGINT NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_oauth_dpop_nonce_expires_at ON oauth_dpop_nonce(expires_at);

-- OAuth DPoP JTI replay protection
CREATE TABLE IF NOT EXISTS oauth_dpop_jti (
  jti TEXT PRIMARY KEY,
  created_at BIGINT NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_oauth_dpop_jti_created_at ON oauth_dpop_jti(created_at);

-- OAuth authorization requests (client flow state)
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
  created_at BIGINT NOT NULL,
  expires_at BIGINT NOT NULL,
  FOREIGN KEY (client_id) REFERENCES oauth_client(client_id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_oauth_auth_request_expires_at ON oauth_auth_request(expires_at);
CREATE INDEX IF NOT EXISTS idx_oauth_auth_request_client_id ON oauth_auth_request(client_id);

-- OAuth ATP session (bridge sessions to AT Protocol)
CREATE TABLE IF NOT EXISTS oauth_atp_session (
  session_id TEXT NOT NULL,
  iteration INTEGER NOT NULL,
  did TEXT,
  session_created_at BIGINT NOT NULL,
  atp_oauth_state TEXT NOT NULL,
  signing_key_jkt TEXT NOT NULL,
  dpop_key TEXT NOT NULL,
  access_token TEXT,
  refresh_token TEXT,
  access_token_created_at BIGINT,
  access_token_expires_at BIGINT,
  access_token_scopes TEXT,
  session_exchanged_at BIGINT,
  exchange_error TEXT,
  PRIMARY KEY (session_id, iteration)
);

CREATE INDEX IF NOT EXISTS idx_oauth_atp_session_did ON oauth_atp_session(did);
CREATE INDEX IF NOT EXISTS idx_oauth_atp_session_access_token ON oauth_atp_session(access_token);

-- OAuth ATP requests (outbound OAuth to AT Protocol)
CREATE TABLE IF NOT EXISTS oauth_atp_request (
  oauth_state TEXT PRIMARY KEY,
  authorization_server TEXT NOT NULL,
  nonce TEXT NOT NULL,
  pkce_verifier TEXT NOT NULL,
  signing_public_key TEXT NOT NULL,
  dpop_private_key TEXT NOT NULL,
  created_at BIGINT NOT NULL,
  expires_at BIGINT NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_oauth_atp_request_expires_at ON oauth_atp_request(expires_at);

-- OAuth authorization codes
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
  created_at BIGINT NOT NULL,
  expires_at BIGINT NOT NULL,
  used BOOLEAN NOT NULL DEFAULT FALSE
);

CREATE INDEX IF NOT EXISTS idx_oauth_authorization_code_expires_at ON oauth_authorization_code(expires_at);

-- =============================================================================
-- Admin Tables
-- =============================================================================

-- Admin browser sessions
CREATE TABLE IF NOT EXISTS admin_session (
  session_id TEXT PRIMARY KEY,
  atp_session_id TEXT NOT NULL,
  created_at BIGINT NOT NULL DEFAULT EXTRACT(EPOCH FROM NOW())::BIGINT
);

CREATE INDEX IF NOT EXISTS idx_admin_session_atp_session_id ON admin_session(atp_session_id);

-- migrate:down

DROP TABLE IF EXISTS admin_session;
DROP TABLE IF EXISTS oauth_authorization_code;
DROP TABLE IF EXISTS oauth_atp_request;
DROP TABLE IF EXISTS oauth_atp_session;
DROP TABLE IF EXISTS oauth_auth_request;
DROP TABLE IF EXISTS oauth_dpop_jti;
DROP TABLE IF EXISTS oauth_dpop_nonce;
DROP TABLE IF EXISTS oauth_par_request;
DROP TABLE IF EXISTS oauth_refresh_token;
DROP TABLE IF EXISTS oauth_access_token;
DROP TABLE IF EXISTS oauth_client;
DROP TABLE IF EXISTS jetstream_cursor;
DROP TABLE IF EXISTS jetstream_activity;
DROP TABLE IF EXISTS config;
DROP TABLE IF EXISTS lexicon;
DROP TABLE IF EXISTS actor;
DROP TABLE IF EXISTS record;
