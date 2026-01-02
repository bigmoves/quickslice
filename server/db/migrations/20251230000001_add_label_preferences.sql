-- migrate:up

-- Add default_visibility to label_definition
ALTER TABLE label_definition ADD COLUMN default_visibility TEXT NOT NULL DEFAULT 'warn';

-- Set appropriate defaults for specific labels
-- porn should hide by default, while other content warnings should show a warning
UPDATE label_definition SET default_visibility = 'hide' WHERE val = 'porn';

-- Create actor_label_preferences table
CREATE TABLE IF NOT EXISTS actor_label_preference (
  did TEXT NOT NULL,
  label_val TEXT NOT NULL,
  visibility TEXT NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  PRIMARY KEY (did, label_val)
);

-- Index for fast lookups by user
CREATE INDEX IF NOT EXISTS idx_actor_label_preference_did ON actor_label_preference(did);

-- migrate:down

DROP INDEX IF EXISTS idx_actor_label_preference_did;
DROP TABLE IF EXISTS actor_label_preference;
-- Note: SQLite doesn't support DROP COLUMN, would need table recreation
