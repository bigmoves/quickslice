-- migrate:up

-- =============================================================================
-- Label Definition Table
-- =============================================================================

-- Defines available label values for this instance
CREATE TABLE IF NOT EXISTS label_definition (
  val TEXT PRIMARY KEY NOT NULL,
  description TEXT NOT NULL,
  severity TEXT NOT NULL CHECK (severity IN ('inform', 'alert', 'takedown')),
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

-- Seed default label definitions (Bluesky-compatible)
INSERT INTO label_definition (val, description, severity) VALUES
  ('!takedown', 'Content removed by moderators', 'takedown'),
  ('!suspend', 'Account suspended', 'takedown'),
  ('!warn', 'Show warning before displaying', 'alert'),
  ('!hide', 'Hide from feeds (still accessible via direct link)', 'alert'),
  ('porn', 'Pornographic content', 'alert'),
  ('sexual', 'Sexually suggestive content', 'alert'),
  ('nudity', 'Non-sexual nudity', 'alert'),
  ('gore', 'Graphic violence or gore', 'alert'),
  ('graphic-media', 'Disturbing or graphic media', 'alert'),
  ('impersonation', 'Account impersonating someone', 'inform'),
  ('spam', 'Spam or unwanted content', 'inform');

-- =============================================================================
-- Label Table
-- =============================================================================

-- Applied labels on records/accounts
CREATE TABLE IF NOT EXISTS label (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  src TEXT NOT NULL,
  uri TEXT NOT NULL,
  cid TEXT,
  val TEXT NOT NULL,
  neg INTEGER NOT NULL DEFAULT 0,
  cts TEXT NOT NULL DEFAULT (datetime('now')),
  exp TEXT,
  FOREIGN KEY (val) REFERENCES label_definition(val)
);

CREATE INDEX IF NOT EXISTS idx_label_uri ON label(uri);
CREATE INDEX IF NOT EXISTS idx_label_val ON label(val);
CREATE INDEX IF NOT EXISTS idx_label_src ON label(src);
CREATE INDEX IF NOT EXISTS idx_label_cts ON label(cts DESC);
-- Composite index for takedown queries (uri + val + neg)
CREATE INDEX IF NOT EXISTS idx_label_takedown ON label(uri, val, neg);

-- =============================================================================
-- Report Table
-- =============================================================================

-- User-submitted reports awaiting review
CREATE TABLE IF NOT EXISTS report (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  reporter_did TEXT NOT NULL,
  subject_uri TEXT NOT NULL,
  reason_type TEXT NOT NULL CHECK (reason_type IN ('spam', 'violation', 'misleading', 'sexual', 'rude', 'other')),
  reason TEXT,
  status TEXT NOT NULL DEFAULT 'pending' CHECK (status IN ('pending', 'resolved', 'dismissed')),
  resolved_by TEXT,
  resolved_at TEXT,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  -- Prevent duplicate reports from same user for same content
  UNIQUE(reporter_did, subject_uri)
);

CREATE INDEX IF NOT EXISTS idx_report_status ON report(status);
CREATE INDEX IF NOT EXISTS idx_report_subject_uri ON report(subject_uri);
CREATE INDEX IF NOT EXISTS idx_report_reporter_did ON report(reporter_did);
CREATE INDEX IF NOT EXISTS idx_report_created_at ON report(created_at DESC);

-- migrate:down

DROP TABLE IF EXISTS report;
DROP TABLE IF EXISTS label;
DROP TABLE IF EXISTS label_definition;
