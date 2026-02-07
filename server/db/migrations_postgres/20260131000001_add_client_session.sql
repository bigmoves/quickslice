-- migrate:up

-- Create client_session table for cookie-based authentication
-- This stores session data for JS SDK clients using HTTP-only cookies
CREATE TABLE IF NOT EXISTS client_session (
  session_id TEXT PRIMARY KEY,
  client_id TEXT NOT NULL,
  user_did TEXT,
  atp_session_id TEXT,
  dpop_jkt TEXT NOT NULL,
  created_at BIGINT NOT NULL,
  last_activity_at BIGINT NOT NULL,
  FOREIGN KEY (client_id) REFERENCES oauth_client(client_id) ON DELETE CASCADE
);

-- Index for looking up sessions by client
CREATE INDEX IF NOT EXISTS idx_client_session_client_id ON client_session(client_id);

-- Index for looking up sessions by user
CREATE INDEX IF NOT EXISTS idx_client_session_user_did ON client_session(user_did);

-- Index for cleanup of inactive sessions
CREATE INDEX IF NOT EXISTS idx_client_session_last_activity ON client_session(last_activity_at);

-- migrate:down

DROP INDEX IF EXISTS idx_client_session_last_activity;
DROP INDEX IF EXISTS idx_client_session_user_did;
DROP INDEX IF EXISTS idx_client_session_client_id;
DROP TABLE IF EXISTS client_session;
