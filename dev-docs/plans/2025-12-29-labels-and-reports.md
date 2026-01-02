# Labels and Reports Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add AT Protocol-compatible labels and moderation reports to quickslice for self-moderation.

**Architecture:** Three new tables (label_definition, label, report) with admin API for management and lexicon API for user reports. Labels attach to records/accounts via URI, with server-side filtering for `!takedown`/`!suspend` labels. Labels field added to all record types for client-side display decisions.

**Tech Stack:** Gleam, SQLite/PostgreSQL, GraphQL (swell library), existing repository patterns.

---

## Task 1: Database Migration

**Files:**
- Create: `server/db/migrations/20251229000001_add_labels_and_reports.sql`

**Step 1: Write the migration SQL**

```sql
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
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX IF NOT EXISTS idx_report_status ON report(status);
CREATE INDEX IF NOT EXISTS idx_report_subject_uri ON report(subject_uri);
CREATE INDEX IF NOT EXISTS idx_report_reporter_did ON report(reporter_did);
CREATE INDEX IF NOT EXISTS idx_report_created_at ON report(created_at DESC);

-- migrate:down

DROP TABLE IF EXISTS report;
DROP TABLE IF EXISTS label;
DROP TABLE IF EXISTS label_definition;
```

**Step 2: Verify migration file exists**

Run: `ls -la server/db/migrations/20251229000001_add_labels_and_reports.sql`
Expected: File exists with correct permissions

**Step 3: Commit**

```bash
git add server/db/migrations/20251229000001_add_labels_and_reports.sql
git commit -m "feat(db): add labels and reports tables migration"
```

---

## Task 2: Label Definition Repository

**Files:**
- Create: `server/src/database/repositories/label_definitions.gleam`

**Step 1: Write the repository module**

```gleam
/// Repository for label definitions
import database/executor.{type DbError, type Executor, Text}
import gleam/dynamic/decode
import gleam/option.{type Option, None, Some}
import gleam/result

/// Label definition domain type
pub type LabelDefinition {
  LabelDefinition(
    val: String,
    description: String,
    severity: String,
    created_at: String,
  )
}

/// Get all label definitions
pub fn get_all(exec: Executor) -> Result(List(LabelDefinition), DbError) {
  let sql = "SELECT val, description, severity, created_at FROM label_definition ORDER BY val"
  executor.query(exec, sql, [], label_definition_decoder())
}

/// Get a label definition by value
pub fn get(exec: Executor, val: String) -> Result(Option(LabelDefinition), DbError) {
  let sql = "SELECT val, description, severity, created_at FROM label_definition WHERE val = " <> executor.placeholder(exec, 1)
  case executor.query(exec, sql, [Text(val)], label_definition_decoder()) {
    Ok([def]) -> Ok(Some(def))
    Ok(_) -> Ok(None)
    Error(e) -> Error(e)
  }
}

/// Insert a new label definition
pub fn insert(
  exec: Executor,
  val: String,
  description: String,
  severity: String,
) -> Result(Nil, DbError) {
  let p1 = executor.placeholder(exec, 1)
  let p2 = executor.placeholder(exec, 2)
  let p3 = executor.placeholder(exec, 3)
  let sql = "INSERT INTO label_definition (val, description, severity) VALUES (" <> p1 <> ", " <> p2 <> ", " <> p3 <> ")"
  executor.exec(exec, sql, [Text(val), Text(description), Text(severity)])
}

/// Check if a label value exists
pub fn exists(exec: Executor, val: String) -> Result(Bool, DbError) {
  case get(exec, val) {
    Ok(Some(_)) -> Ok(True)
    Ok(None) -> Ok(False)
    Error(e) -> Error(e)
  }
}

/// Decoder for LabelDefinition
fn label_definition_decoder() -> decode.Decoder(LabelDefinition) {
  use val <- decode.field(0, decode.string)
  use description <- decode.field(1, decode.string)
  use severity <- decode.field(2, decode.string)
  use created_at <- decode.field(3, decode.string)
  decode.success(LabelDefinition(val:, description:, severity:, created_at:))
}
```

**Step 2: Verify module compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add server/src/database/repositories/label_definitions.gleam
git commit -m "feat(repo): add label_definitions repository"
```

---

## Task 3: Label Repository

**Files:**
- Create: `server/src/database/repositories/labels.gleam`

**Step 1: Write the repository module**

```gleam
/// Repository for labels
import database/executor.{type DbError, type Executor, type Value, Int, Text}
import gleam/dynamic/decode
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// Label domain type
pub type Label {
  Label(
    id: Int,
    src: String,
    uri: String,
    cid: Option(String),
    val: String,
    neg: Bool,
    cts: String,
    exp: Option(String),
  )
}

/// Insert a new label
pub fn insert(
  exec: Executor,
  src: String,
  uri: String,
  cid: Option(String),
  val: String,
  exp: Option(String),
) -> Result(Label, DbError) {
  let p1 = executor.placeholder(exec, 1)
  let p2 = executor.placeholder(exec, 2)
  let p3 = executor.placeholder(exec, 3)
  let p4 = executor.placeholder(exec, 4)
  let p5 = executor.placeholder(exec, 5)

  let cid_value = case cid {
    Some(c) -> Text(c)
    None -> executor.null_value()
  }
  let exp_value = case exp {
    Some(e) -> Text(e)
    None -> executor.null_value()
  }

  let sql = "INSERT INTO label (src, uri, cid, val, exp) VALUES (" <> p1 <> ", " <> p2 <> ", " <> p3 <> ", " <> p4 <> ", " <> p5 <> ") RETURNING id, src, uri, cid, val, neg, cts, exp"

  case executor.query(exec, sql, [Text(src), Text(uri), cid_value, Text(val), exp_value], label_decoder()) {
    Ok([label]) -> Ok(label)
    Ok(_) -> Error(executor.QueryError("Insert did not return label"))
    Error(e) -> Error(e)
  }
}

/// Insert a negation label (retraction)
pub fn insert_negation(
  exec: Executor,
  src: String,
  uri: String,
  val: String,
) -> Result(Label, DbError) {
  let p1 = executor.placeholder(exec, 1)
  let p2 = executor.placeholder(exec, 2)
  let p3 = executor.placeholder(exec, 3)

  let sql = "INSERT INTO label (src, uri, val, neg) VALUES (" <> p1 <> ", " <> p2 <> ", " <> p3 <> ", 1) RETURNING id, src, uri, cid, val, neg, cts, exp"

  case executor.query(exec, sql, [Text(src), Text(uri), Text(val)], label_decoder()) {
    Ok([label]) -> Ok(label)
    Ok(_) -> Error(executor.QueryError("Insert did not return label"))
    Error(e) -> Error(e)
  }
}

/// Get labels for a list of URIs (batch fetch for GraphQL)
/// Returns only active labels (non-negated, non-expired)
pub fn get_by_uris(exec: Executor, uris: List(String)) -> Result(List(Label), DbError) {
  case uris {
    [] -> Ok([])
    _ -> {
      let placeholders = executor.placeholders(exec, list.length(uris), 1)
      let exp_check = case executor.dialect(exec) {
        executor.SQLite -> "(exp IS NULL OR exp > datetime('now'))"
        executor.PostgreSQL -> "(exp IS NULL OR exp::timestamp > NOW())"
      }
      let sql = "SELECT id, src, uri, cid, val, neg, cts, exp FROM label WHERE uri IN (" <> placeholders <> ") AND neg = 0 AND " <> exp_check <> " ORDER BY cts DESC"
      executor.query(exec, sql, list.map(uris, Text), label_decoder())
    }
  }
}

/// Get all labels (admin query with optional filters)
pub fn get_all(
  exec: Executor,
  uri_filter: Option(String),
  val_filter: Option(String),
  limit: Int,
  cursor: Option(Int),
) -> Result(List(Label), DbError) {
  let mut_where_parts: List(String) = []
  let mut_params: List(Value) = []
  let mut_param_count = 0

  // Add URI filter if provided
  let #(where_parts, params, param_count) = case uri_filter {
    Some(uri) -> {
      let p = executor.placeholder(exec, mut_param_count + 1)
      #([p <> " = uri", ..mut_where_parts], [Text(uri), ..mut_params], mut_param_count + 1)
    }
    None -> #(mut_where_parts, mut_params, mut_param_count)
  }

  // Add val filter if provided
  let #(where_parts2, params2, param_count2) = case val_filter {
    Some(v) -> {
      let p = executor.placeholder(exec, param_count + 1)
      #(["val = " <> p, ..where_parts], [Text(v), ..params], param_count + 1)
    }
    None -> #(where_parts, params, param_count)
  }

  // Add cursor filter if provided
  let #(where_parts3, params3, param_count3) = case cursor {
    Some(c) -> {
      let p = executor.placeholder(exec, param_count2 + 1)
      #(["id < " <> p, ..where_parts2], [Int(c), ..params2], param_count2 + 1)
    }
    None -> #(where_parts2, params2, param_count2)
  }

  let where_clause = case where_parts3 {
    [] -> ""
    parts -> " WHERE " <> string.join(list.reverse(parts), " AND ")
  }

  let limit_p = executor.placeholder(exec, param_count3 + 1)
  let sql = "SELECT id, src, uri, cid, val, neg, cts, exp FROM label" <> where_clause <> " ORDER BY id DESC LIMIT " <> limit_p

  executor.query(exec, sql, list.append(list.reverse(params3), [Int(limit)]), label_decoder())
}

/// Check if a URI has an active takedown label
pub fn has_takedown(exec: Executor, uri: String) -> Result(Bool, DbError) {
  let exp_check = case executor.dialect(exec) {
    executor.SQLite -> "(exp IS NULL OR exp > datetime('now'))"
    executor.PostgreSQL -> "(exp IS NULL OR exp::timestamp > NOW())"
  }
  let p1 = executor.placeholder(exec, 1)
  let sql = "SELECT 1 FROM label WHERE uri = " <> p1 <> " AND val IN ('!takedown', '!suspend') AND neg = 0 AND " <> exp_check <> " LIMIT 1"

  case executor.query(exec, sql, [Text(uri)], decode.dynamic) {
    Ok([_]) -> Ok(True)
    Ok([]) -> Ok(False)
    Error(e) -> Error(e)
  }
}

/// Batch check for takedown labels on multiple URIs
/// Returns list of URIs that have active takedown labels
pub fn get_takedown_uris(exec: Executor, uris: List(String)) -> Result(List(String), DbError) {
  case uris {
    [] -> Ok([])
    _ -> {
      let placeholders = executor.placeholders(exec, list.length(uris), 1)
      let exp_check = case executor.dialect(exec) {
        executor.SQLite -> "(exp IS NULL OR exp > datetime('now'))"
        executor.PostgreSQL -> "(exp IS NULL OR exp::timestamp > NOW())"
      }
      let sql = "SELECT DISTINCT uri FROM label WHERE uri IN (" <> placeholders <> ") AND val IN ('!takedown', '!suspend') AND neg = 0 AND " <> exp_check

      let uri_decoder = {
        use uri <- decode.field(0, decode.string)
        decode.success(uri)
      }

      executor.query(exec, sql, list.map(uris, Text), uri_decoder)
    }
  }
}

/// Decoder for Label
fn label_decoder() -> decode.Decoder(Label) {
  use id <- decode.field(0, decode.int)
  use src <- decode.field(1, decode.string)
  use uri <- decode.field(2, decode.string)
  use cid <- decode.field(3, decode.optional(decode.string))
  use val <- decode.field(4, decode.string)
  use neg_int <- decode.field(5, decode.int)
  use cts <- decode.field(6, decode.string)
  use exp <- decode.field(7, decode.optional(decode.string))
  let neg = neg_int == 1
  decode.success(Label(id:, src:, uri:, cid:, val:, neg:, cts:, exp:))
}
```

**Step 2: Verify module compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add server/src/database/repositories/labels.gleam
git commit -m "feat(repo): add labels repository with takedown filtering"
```

---

## Task 4: Report Repository

**Files:**
- Create: `server/src/database/repositories/reports.gleam`

**Step 1: Write the repository module**

```gleam
/// Repository for moderation reports
import database/executor.{type DbError, type Executor, type Value, Int, Text}
import gleam/dynamic/decode
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// Report domain type
pub type Report {
  Report(
    id: Int,
    reporter_did: String,
    subject_uri: String,
    reason_type: String,
    reason: Option(String),
    status: String,
    resolved_by: Option(String),
    resolved_at: Option(String),
    created_at: String,
  )
}

/// Insert a new report
pub fn insert(
  exec: Executor,
  reporter_did: String,
  subject_uri: String,
  reason_type: String,
  reason: Option(String),
) -> Result(Report, DbError) {
  let p1 = executor.placeholder(exec, 1)
  let p2 = executor.placeholder(exec, 2)
  let p3 = executor.placeholder(exec, 3)
  let p4 = executor.placeholder(exec, 4)

  let reason_value = case reason {
    Some(r) -> Text(r)
    None -> executor.null_value()
  }

  let sql = "INSERT INTO report (reporter_did, subject_uri, reason_type, reason) VALUES (" <> p1 <> ", " <> p2 <> ", " <> p3 <> ", " <> p4 <> ") RETURNING id, reporter_did, subject_uri, reason_type, reason, status, resolved_by, resolved_at, created_at"

  case executor.query(exec, sql, [Text(reporter_did), Text(subject_uri), Text(reason_type), reason_value], report_decoder()) {
    Ok([report]) -> Ok(report)
    Ok(_) -> Error(executor.QueryError("Insert did not return report"))
    Error(e) -> Error(e)
  }
}

/// Get a report by ID
pub fn get(exec: Executor, id: Int) -> Result(Option(Report), DbError) {
  let sql = "SELECT id, reporter_did, subject_uri, reason_type, reason, status, resolved_by, resolved_at, created_at FROM report WHERE id = " <> executor.placeholder(exec, 1)
  case executor.query(exec, sql, [Int(id)], report_decoder()) {
    Ok([report]) -> Ok(Some(report))
    Ok(_) -> Ok(None)
    Error(e) -> Error(e)
  }
}

/// Get reports with optional status filter and pagination
pub fn get_all(
  exec: Executor,
  status_filter: Option(String),
  limit: Int,
  cursor: Option(Int),
) -> Result(List(Report), DbError) {
  let mut_where_parts: List(String) = []
  let mut_params: List(Value) = []
  let mut_param_count = 0

  // Add status filter if provided
  let #(where_parts, params, param_count) = case status_filter {
    Some(s) -> {
      let p = executor.placeholder(exec, mut_param_count + 1)
      #(["status = " <> p], [Text(s)], mut_param_count + 1)
    }
    None -> #(mut_where_parts, mut_params, mut_param_count)
  }

  // Add cursor filter if provided
  let #(where_parts2, params2, param_count2) = case cursor {
    Some(c) -> {
      let p = executor.placeholder(exec, param_count + 1)
      #(["id < " <> p, ..where_parts], [Int(c), ..params], param_count + 1)
    }
    None -> #(where_parts, params, param_count)
  }

  let where_clause = case where_parts2 {
    [] -> ""
    parts -> " WHERE " <> string.join(list.reverse(parts), " AND ")
  }

  let limit_p = executor.placeholder(exec, param_count2 + 1)
  let sql = "SELECT id, reporter_did, subject_uri, reason_type, reason, status, resolved_by, resolved_at, created_at FROM report" <> where_clause <> " ORDER BY id DESC LIMIT " <> limit_p

  executor.query(exec, sql, list.append(list.reverse(params2), [Int(limit)]), report_decoder())
}

/// Resolve a report (apply label or dismiss)
pub fn resolve(
  exec: Executor,
  id: Int,
  status: String,
  resolved_by: String,
) -> Result(Report, DbError) {
  let p1 = executor.placeholder(exec, 1)
  let p2 = executor.placeholder(exec, 2)
  let p3 = executor.placeholder(exec, 3)

  let resolved_at_expr = case executor.dialect(exec) {
    executor.SQLite -> "datetime('now')"
    executor.PostgreSQL -> "NOW()::text"
  }

  let sql = "UPDATE report SET status = " <> p1 <> ", resolved_by = " <> p2 <> ", resolved_at = " <> resolved_at_expr <> " WHERE id = " <> p3 <> " RETURNING id, reporter_did, subject_uri, reason_type, reason, status, resolved_by, resolved_at, created_at"

  case executor.query(exec, sql, [Text(status), Text(resolved_by), Int(id)], report_decoder()) {
    Ok([report]) -> Ok(report)
    Ok(_) -> Error(executor.QueryError("Update did not return report"))
    Error(e) -> Error(e)
  }
}

/// Decoder for Report
fn report_decoder() -> decode.Decoder(Report) {
  use id <- decode.field(0, decode.int)
  use reporter_did <- decode.field(1, decode.string)
  use subject_uri <- decode.field(2, decode.string)
  use reason_type <- decode.field(3, decode.string)
  use reason <- decode.field(4, decode.optional(decode.string))
  use status <- decode.field(5, decode.string)
  use resolved_by <- decode.field(6, decode.optional(decode.string))
  use resolved_at <- decode.field(7, decode.optional(decode.string))
  use created_at <- decode.field(8, decode.string)
  decode.success(Report(id:, reporter_did:, subject_uri:, reason_type:, reason:, status:, resolved_by:, resolved_at:, created_at:))
}
```

**Step 2: Verify module compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add server/src/database/repositories/reports.gleam
git commit -m "feat(repo): add reports repository"
```

---

## Task 5: Admin GraphQL Types

**Files:**
- Modify: `server/src/graphql/admin/types.gleam`

**Step 1: Add label and report types to types.gleam**

Add these types after the existing types:

```gleam
/// LabelSeverity enum for label definitions
pub fn label_severity_enum() -> schema.Type {
  schema.enum_type("LabelSeverity", "Severity level of a label", [
    schema.enum_value("INFORM", "Informational, client can show indicator"),
    schema.enum_value("ALERT", "Client should warn/blur"),
    schema.enum_value("TAKEDOWN", "Server filters, content not returned"),
  ])
}

/// LabelDefinition type
pub fn label_definition_type() -> schema.Type {
  schema.object_type("LabelDefinition", "Label value definition", [
    schema.field(
      "val",
      schema.non_null(schema.string_type()),
      "Label value (e.g., 'porn', '!takedown')",
      fn(ctx) { Ok(get_field(ctx, "val")) },
    ),
    schema.field(
      "description",
      schema.non_null(schema.string_type()),
      "Human-readable description",
      fn(ctx) { Ok(get_field(ctx, "description")) },
    ),
    schema.field(
      "severity",
      schema.non_null(label_severity_enum()),
      "Severity level",
      fn(ctx) { Ok(get_field(ctx, "severity")) },
    ),
    schema.field(
      "createdAt",
      schema.non_null(schema.string_type()),
      "Creation timestamp",
      fn(ctx) { Ok(get_field(ctx, "createdAt")) },
    ),
  ])
}

/// Label type
pub fn label_type() -> schema.Type {
  schema.object_type("Label", "Applied label on a record or account", [
    schema.field(
      "id",
      schema.non_null(schema.int_type()),
      "Label ID",
      fn(ctx) { Ok(get_field(ctx, "id")) },
    ),
    schema.field(
      "src",
      schema.non_null(schema.string_type()),
      "DID of admin who applied the label",
      fn(ctx) { Ok(get_field(ctx, "src")) },
    ),
    schema.field(
      "uri",
      schema.non_null(schema.string_type()),
      "Subject URI (at:// or did:)",
      fn(ctx) { Ok(get_field(ctx, "uri")) },
    ),
    schema.field(
      "cid",
      schema.string_type(),
      "Optional CID for version-specific label",
      fn(ctx) { Ok(get_field(ctx, "cid")) },
    ),
    schema.field(
      "val",
      schema.non_null(schema.string_type()),
      "Label value",
      fn(ctx) { Ok(get_field(ctx, "val")) },
    ),
    schema.field(
      "neg",
      schema.non_null(schema.boolean_type()),
      "True if this is a negation (retraction)",
      fn(ctx) { Ok(get_field(ctx, "neg")) },
    ),
    schema.field(
      "cts",
      schema.non_null(schema.string_type()),
      "Creation timestamp",
      fn(ctx) { Ok(get_field(ctx, "cts")) },
    ),
    schema.field(
      "exp",
      schema.string_type(),
      "Optional expiration timestamp",
      fn(ctx) { Ok(get_field(ctx, "exp")) },
    ),
  ])
}

/// ReportReasonType enum
pub fn report_reason_type_enum() -> schema.Type {
  schema.enum_type("ReportReasonType", "Reason for submitting a report", [
    schema.enum_value("SPAM", "Spam or unwanted content"),
    schema.enum_value("VIOLATION", "Violates terms of service"),
    schema.enum_value("MISLEADING", "Misleading or false information"),
    schema.enum_value("SEXUAL", "Inappropriate sexual content"),
    schema.enum_value("RUDE", "Rude or abusive behavior"),
    schema.enum_value("OTHER", "Other reason"),
  ])
}

/// ReportStatus enum
pub fn report_status_enum() -> schema.Type {
  schema.enum_type("ReportStatus", "Status of a moderation report", [
    schema.enum_value("PENDING", "Awaiting review"),
    schema.enum_value("RESOLVED", "Resolved with action"),
    schema.enum_value("DISMISSED", "Dismissed without action"),
  ])
}

/// ReportAction enum for resolving reports
pub fn report_action_enum() -> schema.Type {
  schema.enum_type("ReportAction", "Action to take when resolving a report", [
    schema.enum_value("APPLY_LABEL", "Apply a label to the subject"),
    schema.enum_value("DISMISS", "Dismiss the report without action"),
  ])
}

/// Report type
pub fn report_type() -> schema.Type {
  schema.object_type("Report", "User-submitted moderation report", [
    schema.field(
      "id",
      schema.non_null(schema.int_type()),
      "Report ID",
      fn(ctx) { Ok(get_field(ctx, "id")) },
    ),
    schema.field(
      "reporterDid",
      schema.non_null(schema.string_type()),
      "DID of reporter",
      fn(ctx) { Ok(get_field(ctx, "reporterDid")) },
    ),
    schema.field(
      "subjectUri",
      schema.non_null(schema.string_type()),
      "Subject URI (at:// or did:)",
      fn(ctx) { Ok(get_field(ctx, "subjectUri")) },
    ),
    schema.field(
      "reasonType",
      schema.non_null(report_reason_type_enum()),
      "Reason type",
      fn(ctx) { Ok(get_field(ctx, "reasonType")) },
    ),
    schema.field(
      "reason",
      schema.string_type(),
      "Optional free-text explanation",
      fn(ctx) { Ok(get_field(ctx, "reason")) },
    ),
    schema.field(
      "status",
      schema.non_null(report_status_enum()),
      "Report status",
      fn(ctx) { Ok(get_field(ctx, "status")) },
    ),
    schema.field(
      "resolvedBy",
      schema.string_type(),
      "DID of admin who resolved",
      fn(ctx) { Ok(get_field(ctx, "resolvedBy")) },
    ),
    schema.field(
      "resolvedAt",
      schema.string_type(),
      "Resolution timestamp",
      fn(ctx) { Ok(get_field(ctx, "resolvedAt")) },
    ),
    schema.field(
      "createdAt",
      schema.non_null(schema.string_type()),
      "Creation timestamp",
      fn(ctx) { Ok(get_field(ctx, "createdAt")) },
    ),
  ])
}
```

**Step 2: Verify module compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add server/src/graphql/admin/types.gleam
git commit -m "feat(graphql): add label and report types to admin API"
```

---

## Task 6: Admin GraphQL Converters

**Files:**
- Modify: `server/src/graphql/admin/converters.gleam`

**Step 1: Add converters for label and report types**

Add these functions to converters.gleam:

```gleam
import database/repositories/label_definitions
import database/repositories/labels
import database/repositories/reports

/// Convert LabelDefinition to GraphQL value
pub fn label_definition_to_value(def: label_definitions.LabelDefinition) -> value.Value {
  value.Object([
    #("val", value.String(def.val)),
    #("description", value.String(def.description)),
    #("severity", value.Enum(string.uppercase(def.severity))),
    #("createdAt", value.String(def.created_at)),
  ])
}

/// Convert Label to GraphQL value
pub fn label_to_value(label: labels.Label) -> value.Value {
  let cid_value = case label.cid {
    Some(c) -> value.String(c)
    None -> value.Null
  }
  let exp_value = case label.exp {
    Some(e) -> value.String(e)
    None -> value.Null
  }
  value.Object([
    #("id", value.Int(label.id)),
    #("src", value.String(label.src)),
    #("uri", value.String(label.uri)),
    #("cid", cid_value),
    #("val", value.String(label.val)),
    #("neg", value.Boolean(label.neg)),
    #("cts", value.String(label.cts)),
    #("exp", exp_value),
  ])
}

/// Convert Report to GraphQL value
pub fn report_to_value(report: reports.Report) -> value.Value {
  let reason_value = case report.reason {
    Some(r) -> value.String(r)
    None -> value.Null
  }
  let resolved_by_value = case report.resolved_by {
    Some(r) -> value.String(r)
    None -> value.Null
  }
  let resolved_at_value = case report.resolved_at {
    Some(r) -> value.String(r)
    None -> value.Null
  }
  value.Object([
    #("id", value.Int(report.id)),
    #("reporterDid", value.String(report.reporter_did)),
    #("subjectUri", value.String(report.subject_uri)),
    #("reasonType", value.Enum(string.uppercase(report.reason_type))),
    #("reason", reason_value),
    #("status", value.Enum(string.uppercase(report.status))),
    #("resolvedBy", resolved_by_value),
    #("resolvedAt", resolved_at_value),
    #("createdAt", value.String(report.created_at)),
  ])
}
```

**Step 2: Add necessary imports at the top**

```gleam
import gleam/string
```

**Step 3: Verify module compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add server/src/graphql/admin/converters.gleam
git commit -m "feat(graphql): add label and report converters"
```

---

## Task 7: Admin GraphQL Queries

**Files:**
- Modify: `server/src/graphql/admin/queries.gleam`

**Step 1: Add imports for new repositories**

Add to imports:

```gleam
import database/repositories/label_definitions
import database/repositories/labels
import database/repositories/reports
```

**Step 2: Add label and report queries to query_type function**

Add these fields to the query_type object_type list:

```gleam
    // labelDefinitions query
    schema.field(
      "labelDefinitions",
      schema.non_null(
        schema.list_type(schema.non_null(admin_types.label_definition_type())),
      ),
      "Get all label definitions",
      fn(_ctx) {
        case label_definitions.get_all(conn) {
          Ok(defs) ->
            Ok(value.List(list.map(defs, converters.label_definition_to_value)))
          Error(_) -> Error("Failed to fetch label definitions")
        }
      },
    ),
    // labels query (admin only)
    schema.field_with_args(
      "labels",
      schema.non_null(
        schema.list_type(schema.non_null(admin_types.label_type())),
      ),
      "Get labels with optional filters (admin only)",
      [
        schema.argument("uri", schema.string_type(), "Filter by subject URI", None),
        schema.argument("val", schema.string_type(), "Filter by label value", None),
        schema.argument("limit", schema.int_type(), "Max results (default 50)", None),
        schema.argument("cursor", schema.int_type(), "Cursor for pagination (label ID)", None),
      ],
      fn(ctx) {
        case session.get_current_session(req, conn, did_cache) {
          Ok(sess) -> {
            case config_repo.is_admin(conn, sess.did) {
              True -> {
                let uri_filter = case schema.get_argument(ctx, "uri") {
                  Some(value.String(u)) -> Some(u)
                  _ -> None
                }
                let val_filter = case schema.get_argument(ctx, "val") {
                  Some(value.String(v)) -> Some(v)
                  _ -> None
                }
                let limit = case schema.get_argument(ctx, "limit") {
                  Some(value.Int(l)) -> l
                  _ -> 50
                }
                let cursor = case schema.get_argument(ctx, "cursor") {
                  Some(value.Int(c)) -> Some(c)
                  _ -> None
                }
                case labels.get_all(conn, uri_filter, val_filter, limit, cursor) {
                  Ok(label_list) ->
                    Ok(value.List(list.map(label_list, converters.label_to_value)))
                  Error(_) -> Error("Failed to fetch labels")
                }
              }
              False -> Error("Admin privileges required")
            }
          }
          Error(_) -> Error("Authentication required")
        }
      },
    ),
    // reports query (admin only)
    schema.field_with_args(
      "reports",
      schema.non_null(
        schema.list_type(schema.non_null(admin_types.report_type())),
      ),
      "Get moderation reports with optional status filter (admin only)",
      [
        schema.argument("status", admin_types.report_status_enum(), "Filter by status", None),
        schema.argument("limit", schema.int_type(), "Max results (default 50)", None),
        schema.argument("cursor", schema.int_type(), "Cursor for pagination (report ID)", None),
      ],
      fn(ctx) {
        case session.get_current_session(req, conn, did_cache) {
          Ok(sess) -> {
            case config_repo.is_admin(conn, sess.did) {
              True -> {
                let status_filter = case schema.get_argument(ctx, "status") {
                  Some(value.Enum(s)) -> Some(string.lowercase(s))
                  _ -> None
                }
                let limit = case schema.get_argument(ctx, "limit") {
                  Some(value.Int(l)) -> l
                  _ -> 50
                }
                let cursor = case schema.get_argument(ctx, "cursor") {
                  Some(value.Int(c)) -> Some(c)
                  _ -> None
                }
                case reports.get_all(conn, status_filter, limit, cursor) {
                  Ok(report_list) ->
                    Ok(value.List(list.map(report_list, converters.report_to_value)))
                  Error(_) -> Error("Failed to fetch reports")
                }
              }
              False -> Error("Admin privileges required")
            }
          }
          Error(_) -> Error("Authentication required")
        }
      },
    ),
```

**Step 3: Add string import if not present**

```gleam
import gleam/string
```

**Step 4: Verify module compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 5: Commit**

```bash
git add server/src/graphql/admin/queries.gleam
git commit -m "feat(graphql): add label and report queries to admin API"
```

---

## Task 8: Admin GraphQL Mutations

**Files:**
- Modify: `server/src/graphql/admin/mutations.gleam`

**Step 1: Add imports for new repositories**

Add to imports:

```gleam
import database/repositories/label_definitions
import database/repositories/labels
import database/repositories/reports
```

**Step 2: Add label and report mutations to mutation_type function**

Add these fields to the mutation_type object_type list:

```gleam
    // createLabel mutation (admin only)
    schema.field_with_args(
      "createLabel",
      schema.non_null(admin_types.label_type()),
      "Create a label on a record or account (admin only)",
      [
        schema.argument("uri", schema.non_null(schema.string_type()), "Subject URI (at:// or did:)", None),
        schema.argument("val", schema.non_null(schema.string_type()), "Label value", None),
        schema.argument("cid", schema.string_type(), "Optional CID for version-specific label", None),
        schema.argument("exp", schema.string_type(), "Optional expiration datetime", None),
      ],
      fn(ctx) {
        case session.get_current_session(req, conn, did_cache) {
          Ok(sess) -> {
            case config_repo.is_admin(conn, sess.did) {
              True -> {
                case schema.get_argument(ctx, "uri"), schema.get_argument(ctx, "val") {
                  Some(value.String(uri)), Some(value.String(val)) -> {
                    // Validate label value exists
                    case label_definitions.exists(conn, val) {
                      Ok(True) -> {
                        let cid = case schema.get_argument(ctx, "cid") {
                          Some(value.String(c)) -> Some(c)
                          _ -> None
                        }
                        let exp = case schema.get_argument(ctx, "exp") {
                          Some(value.String(e)) -> Some(e)
                          _ -> None
                        }
                        case labels.insert(conn, sess.did, uri, cid, val, exp) {
                          Ok(label) -> Ok(converters.label_to_value(label))
                          Error(_) -> Error("Failed to create label")
                        }
                      }
                      Ok(False) -> Error("Unknown label value: " <> val)
                      Error(_) -> Error("Failed to validate label value")
                    }
                  }
                  _, _ -> Error("uri and val are required")
                }
              }
              False -> Error("Admin privileges required")
            }
          }
          Error(_) -> Error("Authentication required")
        }
      },
    ),
    // negateLabel mutation (admin only)
    schema.field_with_args(
      "negateLabel",
      schema.non_null(admin_types.label_type()),
      "Negate (retract) a label on a record or account (admin only)",
      [
        schema.argument("uri", schema.non_null(schema.string_type()), "Subject URI", None),
        schema.argument("val", schema.non_null(schema.string_type()), "Label value to negate", None),
      ],
      fn(ctx) {
        case session.get_current_session(req, conn, did_cache) {
          Ok(sess) -> {
            case config_repo.is_admin(conn, sess.did) {
              True -> {
                case schema.get_argument(ctx, "uri"), schema.get_argument(ctx, "val") {
                  Some(value.String(uri)), Some(value.String(val)) -> {
                    case labels.insert_negation(conn, sess.did, uri, val) {
                      Ok(label) -> Ok(converters.label_to_value(label))
                      Error(_) -> Error("Failed to negate label")
                    }
                  }
                  _, _ -> Error("uri and val are required")
                }
              }
              False -> Error("Admin privileges required")
            }
          }
          Error(_) -> Error("Authentication required")
        }
      },
    ),
    // createLabelDefinition mutation (admin only)
    schema.field_with_args(
      "createLabelDefinition",
      schema.non_null(admin_types.label_definition_type()),
      "Create a custom label definition (admin only)",
      [
        schema.argument("val", schema.non_null(schema.string_type()), "Label value", None),
        schema.argument("description", schema.non_null(schema.string_type()), "Description", None),
        schema.argument("severity", schema.non_null(admin_types.label_severity_enum()), "Severity level", None),
      ],
      fn(ctx) {
        case session.get_current_session(req, conn, did_cache) {
          Ok(sess) -> {
            case config_repo.is_admin(conn, sess.did) {
              True -> {
                case
                  schema.get_argument(ctx, "val"),
                  schema.get_argument(ctx, "description"),
                  schema.get_argument(ctx, "severity")
                {
                  Some(value.String(val)), Some(value.String(desc)), Some(value.Enum(sev)) -> {
                    let severity = string.lowercase(sev)
                    case label_definitions.insert(conn, val, desc, severity) {
                      Ok(_) -> {
                        case label_definitions.get(conn, val) {
                          Ok(Some(def)) -> Ok(converters.label_definition_to_value(def))
                          _ -> Error("Failed to fetch created definition")
                        }
                      }
                      Error(_) -> Error("Failed to create label definition")
                    }
                  }
                  _, _, _ -> Error("val, description, and severity are required")
                }
              }
              False -> Error("Admin privileges required")
            }
          }
          Error(_) -> Error("Authentication required")
        }
      },
    ),
    // resolveReport mutation (admin only)
    schema.field_with_args(
      "resolveReport",
      schema.non_null(admin_types.report_type()),
      "Resolve a moderation report (admin only)",
      [
        schema.argument("id", schema.non_null(schema.int_type()), "Report ID", None),
        schema.argument("action", schema.non_null(admin_types.report_action_enum()), "Action to take", None),
        schema.argument("labelVal", schema.string_type(), "Label value to apply (required if action is APPLY_LABEL)", None),
      ],
      fn(ctx) {
        case session.get_current_session(req, conn, did_cache) {
          Ok(sess) -> {
            case config_repo.is_admin(conn, sess.did) {
              True -> {
                case schema.get_argument(ctx, "id"), schema.get_argument(ctx, "action") {
                  Some(value.Int(id)), Some(value.Enum(action)) -> {
                    // Get the report first
                    case reports.get(conn, id) {
                      Ok(Some(report)) -> {
                        case action {
                          "APPLY_LABEL" -> {
                            case schema.get_argument(ctx, "labelVal") {
                              Some(value.String(label_val)) -> {
                                // Create the label
                                case labels.insert(conn, sess.did, report.subject_uri, None, label_val, None) {
                                  Ok(_) -> {
                                    // Mark report as resolved
                                    case reports.resolve(conn, id, "resolved", sess.did) {
                                      Ok(resolved) -> Ok(converters.report_to_value(resolved))
                                      Error(_) -> Error("Failed to resolve report")
                                    }
                                  }
                                  Error(_) -> Error("Failed to apply label")
                                }
                              }
                              _ -> Error("labelVal is required when action is APPLY_LABEL")
                            }
                          }
                          "DISMISS" -> {
                            case reports.resolve(conn, id, "dismissed", sess.did) {
                              Ok(resolved) -> Ok(converters.report_to_value(resolved))
                              Error(_) -> Error("Failed to dismiss report")
                            }
                          }
                          _ -> Error("Invalid action")
                        }
                      }
                      Ok(None) -> Error("Report not found")
                      Error(_) -> Error("Failed to fetch report")
                    }
                  }
                  _, _ -> Error("id and action are required")
                }
              }
              False -> Error("Admin privileges required")
            }
          }
          Error(_) -> Error("Authentication required")
        }
      },
    ),
```

**Step 3: Verify module compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add server/src/graphql/admin/mutations.gleam
git commit -m "feat(graphql): add label and report mutations to admin API"
```

---

## Task 9: Lexicon API - createReport Mutation

**Files:**
- Modify: `server/src/graphql/lexicon/mutations.gleam`

**Step 1: Add report repository import**

```gleam
import database/repositories/reports
```

**Step 2: Add createReport mutation**

This needs to be added to the lexicon schema. Look at how mutations are structured in `mutations.gleam` and add a public function that can be called from the schema builder to add the createReport mutation.

The mutation should:
- Require authentication (get user DID from auth token)
- Accept subjectUri, reasonType, and optional reason
- Insert into reports table
- Return the created report

**Step 3: Verify module compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add server/src/graphql/lexicon/mutations.gleam
git commit -m "feat(graphql): add createReport mutation to lexicon API"
```

---

## Task 10: Labels Field on Record Types

**Files:**
- Modify: `server/src/graphql/lexicon/fetchers.gleam`
- Modify: `lexicon_graphql/src/lexicon_graphql/schema/database.gleam`

**Step 1: Create label fetcher in fetchers.gleam**

Add a function to create a labels batch fetcher:

```gleam
import database/repositories/labels

/// Create a labels fetcher for batch loading labels by URI
pub fn labels_fetcher(db: Executor) {
  fn(uris: List(String)) -> Result(dict.Dict(String, List(value.Value)), String) {
    case labels.get_by_uris(db, uris) {
      Ok(label_list) -> {
        // Group labels by URI
        let grouped = list.fold(label_list, dict.new(), fn(acc, label) {
          let label_value = value.Object([
            #("id", value.Int(label.id)),
            #("src", value.String(label.src)),
            #("uri", value.String(label.uri)),
            #("cid", case label.cid {
              Some(c) -> value.String(c)
              None -> value.Null
            }),
            #("val", value.String(label.val)),
            #("neg", value.Boolean(label.neg)),
            #("cts", value.String(label.cts)),
            #("exp", case label.exp {
              Some(e) -> value.String(e)
              None -> value.Null
            }),
          ])
          let existing = dict.get(acc, label.uri) |> result.unwrap([])
          dict.insert(acc, label.uri, [label_value, ..existing])
        })
        Ok(grouped)
      }
      Error(_) -> Error("Failed to fetch labels")
    }
  }
}
```

**Step 2: Add labels field to record types in database.gleam**

This requires modifying the schema builder to add a `labels` field to each record type. The field should:
- Return `[Label!]!` (non-null list of non-null labels)
- Use the labels fetcher to batch load labels for the record's URI
- Be added to every record object type

**Step 3: Verify module compiles**

Run: `cd /Users/chadmiller/code/quickslice && gleam build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add server/src/graphql/lexicon/fetchers.gleam lexicon_graphql/src/lexicon_graphql/schema/database.gleam
git commit -m "feat(graphql): add labels field to all record types"
```

---

## Task 11: Takedown Filtering in Record Fetchers

**Files:**
- Modify: `server/src/graphql/lexicon/fetchers.gleam`

**Step 1: Add takedown filtering to record_fetcher**

Modify the record_fetcher to:
1. After fetching records, get the list of URIs
2. Call `labels.get_takedown_uris(db, uris)` to find which have takedown labels
3. Filter out records with takedown URIs from the result

**Step 2: Add takedown filtering to batch_fetcher**

Similar modification for batch fetches.

**Step 3: Verify module compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add server/src/graphql/lexicon/fetchers.gleam
git commit -m "feat(moderation): filter takedown-labeled records from queries"
```

---

## Task 12: Integration Testing

**Files:**
- Create: `server/test/labels_test.gleam` (if test framework exists)

**Step 1: Test label creation**

Test that:
- Admin can create a label
- Non-admin cannot create a label
- Label appears on record query

**Step 2: Test report workflow**

Test that:
- Authenticated user can create a report
- Admin can view reports
- Admin can resolve report with label
- Admin can dismiss report

**Step 3: Test takedown filtering**

Test that:
- Record with !takedown label is not returned in queries
- Record is still accessible with admin includeRemoved flag (if implemented)

**Step 4: Run full test suite**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: All tests pass

**Step 5: Commit**

```bash
git add server/test/
git commit -m "test: add labels and reports integration tests"
```

---

## Summary

This plan implements:

1. **Database Layer** (Tasks 1-4): Migration and repositories for label_definition, label, and report tables
2. **Admin API** (Tasks 5-8): GraphQL types, queries, and mutations for admin management
3. **Lexicon API** (Task 9): createReport mutation for user submissions
4. **Labels Field** (Task 10): Add labels to all record types for client display
5. **Takedown Filtering** (Task 11): Server-side filtering of !takedown/!suspend content
6. **Testing** (Task 12): Integration tests for the full workflow

Total: 12 tasks with ~40 individual steps.
