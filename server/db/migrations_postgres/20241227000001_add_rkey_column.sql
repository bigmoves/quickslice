-- migrate:up

-- Add rkey as a generated column extracted from uri
-- URI format: at://did/collection/rkey
-- We extract everything after the last '/'
ALTER TABLE record ADD COLUMN rkey TEXT
  GENERATED ALWAYS AS (
    substring(uri from '[^/]+$')
  ) STORED;

-- Index for efficient sorting by rkey (TID-based chronological order)
CREATE INDEX IF NOT EXISTS idx_record_rkey ON record(rkey DESC);

-- migrate:down

DROP INDEX IF EXISTS idx_record_rkey;
ALTER TABLE record DROP COLUMN rkey;
