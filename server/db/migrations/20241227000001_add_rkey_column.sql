-- migrate:up

-- Add rkey as a generated column extracted from uri
-- URI format: at://did/collection/rkey
-- We extract everything after the last '/'
-- Note: Using VIRTUAL instead of STORED because SQLite doesn't allow
-- adding STORED columns via ALTER TABLE
ALTER TABLE record ADD COLUMN rkey TEXT
  GENERATED ALWAYS AS (
    substr(uri, instr(substr(uri, instr(substr(uri, 6), '/') + 6), '/') + instr(substr(uri, 6), '/') + 6)
  ) VIRTUAL;

-- Note: Cannot create index on VIRTUAL column in SQLite
-- The column will be computed on-the-fly during queries

-- migrate:down

-- Note: SQLite doesn't support DROP COLUMN, would need table rebuild
