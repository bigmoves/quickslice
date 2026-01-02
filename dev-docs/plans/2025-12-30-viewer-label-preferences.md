# Viewer Label Preferences Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Allow users to configure how they want each label type displayed (ignore, show, warn, hide).

**Architecture:** Add `actor_label_preferences` table for per-user settings, extend `label_definitions` with `default_visibility`, expose via `viewerLabelPreferences` query and `setLabelPreference` mutation on the lexicon GraphQL API. System labels (starting with `!`) are excluded since the server enforces them.

**Tech Stack:** Gleam, SQLite/Postgres, swell GraphQL library

---

### Task 1: Add Migration for Label Preferences

**Files:**
- Create: `server/priv/migrations/XXXXXX_add_label_preferences.sql`

**Step 1: Create migration file**

```sql
-- Add default_visibility to label_definitions
ALTER TABLE label_definitions ADD COLUMN default_visibility TEXT NOT NULL DEFAULT 'warn';

-- Create actor_label_preferences table
CREATE TABLE IF NOT EXISTS actor_label_preferences (
  did TEXT NOT NULL,
  label_val TEXT NOT NULL,
  visibility TEXT NOT NULL,
  created_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%d %H:%M:%S', 'now')),
  PRIMARY KEY (did, label_val)
);

-- Index for fast lookups by user
CREATE INDEX IF NOT EXISTS idx_actor_label_preferences_did ON actor_label_preferences(did);
```

**Step 2: Run migration**

Run: `cd server && gleam run -m migrate`
Expected: Migration applies successfully

**Step 3: Commit**

```bash
git add server/priv/migrations/
git commit -m "feat(db): add label preferences migration"
```

---

### Task 2: Create Label Preferences Repository

**Files:**
- Create: `server/src/database/repositories/label_preferences.gleam`

**Step 1: Create repository module**

```gleam
/// Repository for actor label preferences
import database/executor.{type Executor}
import gleam/dynamic
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

/// A label preference record
pub type LabelPreference {
  LabelPreference(
    did: String,
    label_val: String,
    visibility: String,
    created_at: String,
  )
}

/// Get all preferences for a user
pub fn get_by_did(
  conn: Executor,
  did: String,
) -> Result(List(LabelPreference), String) {
  let sql =
    "SELECT did, label_val, visibility, created_at
     FROM actor_label_preferences
     WHERE did = ?"

  case executor.query(conn, sql, [executor.string(did)], preference_decoder()) {
    Ok(rows) -> Ok(rows)
    Error(_) -> Error("Failed to fetch label preferences")
  }
}

/// Get a specific preference
pub fn get(
  conn: Executor,
  did: String,
  label_val: String,
) -> Result(Option(LabelPreference), String) {
  let sql =
    "SELECT did, label_val, visibility, created_at
     FROM actor_label_preferences
     WHERE did = ? AND label_val = ?"

  case
    executor.query(conn, sql, [executor.string(did), executor.string(label_val)], preference_decoder())
  {
    Ok([pref, ..]) -> Ok(Some(pref))
    Ok([]) -> Ok(None)
    Error(_) -> Error("Failed to fetch label preference")
  }
}

/// Set a preference (upsert)
pub fn set(
  conn: Executor,
  did: String,
  label_val: String,
  visibility: String,
) -> Result(LabelPreference, String) {
  let sql =
    "INSERT INTO actor_label_preferences (did, label_val, visibility)
     VALUES (?, ?, ?)
     ON CONFLICT(did, label_val) DO UPDATE SET visibility = excluded.visibility
     RETURNING did, label_val, visibility, created_at"

  case
    executor.query(
      conn,
      sql,
      [executor.string(did), executor.string(label_val), executor.string(visibility)],
      preference_decoder(),
    )
  {
    Ok([pref, ..]) -> Ok(pref)
    Ok([]) -> Error("Failed to set preference")
    Error(_) -> Error("Failed to set label preference")
  }
}

/// Delete a preference (reset to default)
pub fn delete(
  conn: Executor,
  did: String,
  label_val: String,
) -> Result(Nil, String) {
  let sql =
    "DELETE FROM actor_label_preferences WHERE did = ? AND label_val = ?"

  case executor.execute(conn, sql, [executor.string(did), executor.string(label_val)]) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("Failed to delete label preference")
  }
}

fn preference_decoder() -> dynamic.Decoder(LabelPreference) {
  dynamic.decode4(
    LabelPreference,
    dynamic.element(0, dynamic.string),
    dynamic.element(1, dynamic.string),
    dynamic.element(2, dynamic.string),
    dynamic.element(3, dynamic.string),
  )
}
```

**Step 2: Build to verify**

Run: `cd server && gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add server/src/database/repositories/label_preferences.gleam
git commit -m "feat(db): add label preferences repository"
```

---

### Task 3: Update Label Definitions Repository

**Files:**
- Modify: `server/src/database/repositories/label_definitions.gleam`

**Step 1: Update LabelDefinition type to include default_visibility**

Add `default_visibility: String` field to the `LabelDefinition` type and update all decoders and queries to include it.

**Step 2: Update insert function signature**

Add `default_visibility` parameter to the `insert` function:

```gleam
pub fn insert(
  conn: Executor,
  val: String,
  description: String,
  severity: String,
  default_visibility: String,
) -> Result(Nil, String)
```

**Step 3: Add get_non_system function**

```gleam
/// Get all non-system label definitions (excludes labels starting with !)
pub fn get_non_system(conn: Executor) -> Result(List(LabelDefinition), String) {
  let sql =
    "SELECT val, description, severity, default_visibility, created_at
     FROM label_definitions
     WHERE val NOT LIKE '!%'
     ORDER BY val"

  case executor.query(conn, sql, [], definition_decoder()) {
    Ok(rows) -> Ok(rows)
    Error(_) -> Error("Failed to fetch label definitions")
  }
}
```

**Step 4: Build to verify**

Run: `cd server && gleam build`
Expected: Build succeeds (may have errors in callers that need updating)

**Step 5: Commit**

```bash
git add server/src/database/repositories/label_definitions.gleam
git commit -m "feat(db): add default_visibility to label definitions"
```

---

### Task 4: Update createLabelDefinition Mutation

**Files:**
- Modify: `server/src/graphql/admin/mutations.gleam`

**Step 1: Add defaultVisibility argument**

Add new argument to `createLabelDefinition` mutation:

```gleam
schema.argument(
  "defaultVisibility",
  schema.string_type(),
  "Default visibility setting (ignore, show, warn, hide). Defaults to warn.",
  None,
),
```

**Step 2: Extract and validate defaultVisibility**

```gleam
let default_visibility = case
  schema.get_argument(ctx, "defaultVisibility")
{
  Some(value.Enum(v)) -> string.lowercase(v)
  Some(value.String(v)) -> string.lowercase(v)
  _ -> "warn"
}

// Validate
let valid_visibilities = ["ignore", "show", "warn", "hide"]
use _ <- result.try(case list.contains(valid_visibilities, default_visibility) {
  True -> Ok(Nil)
  False -> Error("defaultVisibility must be one of: ignore, show, warn, hide")
})
```

**Step 3: Pass to insert**

Update the `label_definitions.insert` call to include `default_visibility`.

**Step 4: Build to verify**

Run: `cd server && gleam build`
Expected: Build succeeds

**Step 5: Commit**

```bash
git add server/src/graphql/admin/mutations.gleam
git commit -m "feat(admin): add defaultVisibility to createLabelDefinition"
```

---

### Task 5: Add Visibility Enum to Lexicon Types

**Files:**
- Modify: `server/src/graphql/lexicon/types.gleam`

**Step 1: Add LabelVisibility enum**

```gleam
/// LabelVisibility enum for user preferences
pub fn label_visibility_enum() -> schema.Type {
  schema.enum_type("LabelVisibility", "How to display labeled content", [
    schema.enum_value("IGNORE", "Show content normally, no indicator"),
    schema.enum_value("SHOW", "Explicitly show (for adult content)"),
    schema.enum_value("WARN", "Blur with click-through warning"),
    schema.enum_value("HIDE", "Do not show content"),
  ])
}
```

**Step 2: Add LabelPreference type**

```gleam
/// LabelPreference type for viewerLabelPreferences query
pub fn label_preference_type() -> schema.Type {
  schema.object_type("LabelPreference", "User preference for a label type", [
    schema.field(
      "val",
      schema.non_null(schema.string_type()),
      "Label value",
      fn(ctx) { Ok(get_field(ctx, "val")) },
    ),
    schema.field(
      "description",
      schema.non_null(schema.string_type()),
      "Label description",
      fn(ctx) { Ok(get_field(ctx, "description")) },
    ),
    schema.field(
      "severity",
      schema.non_null(schema.string_type()),
      "Label severity (inform, alert, none)",
      fn(ctx) { Ok(get_field(ctx, "severity")) },
    ),
    schema.field(
      "defaultVisibility",
      schema.non_null(label_visibility_enum()),
      "Default visibility setting",
      fn(ctx) { Ok(get_field(ctx, "defaultVisibility")) },
    ),
    schema.field(
      "visibility",
      schema.non_null(label_visibility_enum()),
      "User's effective visibility setting",
      fn(ctx) { Ok(get_field(ctx, "visibility")) },
    ),
  ])
}
```

**Step 3: Build to verify**

Run: `cd server && gleam build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add server/src/graphql/lexicon/types.gleam
git commit -m "feat(lexicon): add LabelVisibility enum and LabelPreference type"
```

---

### Task 6: Add viewerLabelPreferences Query

**Files:**
- Modify: `server/src/graphql/lexicon/queries.gleam`

**Step 1: Add viewerLabelPreferences field**

```gleam
schema.field(
  "viewerLabelPreferences",
  schema.non_null(schema.list_type(schema.non_null(types.label_preference_type()))),
  "Get label visibility preferences for the authenticated user",
  fn(_ctx) {
    // Check authentication
    case get_authenticated_session(ctx) {
      Error(e) -> Error(e)
      Ok(auth) -> {
        // Get all non-system label definitions
        case label_definitions.get_non_system(conn) {
          Error(e) -> Error(e)
          Ok(definitions) -> {
            // Get user's preferences
            case label_preferences.get_by_did(conn, auth.user_info.did) {
              Error(e) -> Error(e)
              Ok(prefs) -> {
                // Build preference map
                let pref_map =
                  list.fold(prefs, dict.new(), fn(acc, p) {
                    dict.insert(acc, p.label_val, p.visibility)
                  })

                // Merge definitions with user prefs
                let results =
                  list.map(definitions, fn(def) {
                    let visibility = case dict.get(pref_map, def.val) {
                      Ok(v) -> v
                      Error(_) -> def.default_visibility
                    }
                    value.Object([
                      #("val", value.String(def.val)),
                      #("description", value.String(def.description)),
                      #("severity", value.String(def.severity)),
                      #("defaultVisibility", value.Enum(string.uppercase(def.default_visibility))),
                      #("visibility", value.Enum(string.uppercase(visibility))),
                    ])
                  })

                Ok(value.List(results))
              }
            }
          }
        }
      }
    }
  },
),
```

**Step 2: Add required imports**

Add imports for `label_definitions`, `label_preferences`, `dict`.

**Step 3: Build to verify**

Run: `cd server && gleam build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add server/src/graphql/lexicon/queries.gleam
git commit -m "feat(lexicon): add viewerLabelPreferences query"
```

---

### Task 7: Add setLabelPreference Mutation

**Files:**
- Modify: `server/src/graphql/lexicon/mutations.gleam`

**Step 1: Add setLabelPreference field**

```gleam
schema.field_with_args(
  "setLabelPreference",
  schema.non_null(types.label_preference_type()),
  "Set visibility preference for a label type",
  [
    schema.argument(
      "val",
      schema.non_null(schema.string_type()),
      "Label value",
      None,
    ),
    schema.argument(
      "visibility",
      schema.non_null(types.label_visibility_enum()),
      "Visibility setting",
      None,
    ),
  ],
  fn(ctx) {
    // Check authentication
    case get_authenticated_session(resolver_ctx, ctx) {
      Error(e) -> Error(e)
      Ok(auth) -> {
        // Get arguments
        case
          schema.get_argument(resolver_ctx, "val"),
          schema.get_argument(resolver_ctx, "visibility")
        {
          Some(value.String(val)), Some(value.Enum(visibility)) -> {
            // Validate not a system label
            case string.starts_with(val, "!") {
              True -> Error("Cannot set preference for system labels")
              False -> {
                // Validate label exists
                case label_definitions.get(ctx.db, val) {
                  Ok(None) -> Error("Unknown label: " <> val)
                  Error(_) -> Error("Failed to validate label")
                  Ok(Some(def)) -> {
                    let visibility_lower = string.lowercase(visibility)
                    // Set the preference
                    case
                      label_preferences.set(
                        ctx.db,
                        auth.user_info.did,
                        val,
                        visibility_lower,
                      )
                    {
                      Error(e) -> Error(e)
                      Ok(_) -> {
                        Ok(value.Object([
                          #("val", value.String(def.val)),
                          #("description", value.String(def.description)),
                          #("severity", value.String(def.severity)),
                          #("defaultVisibility", value.Enum(string.uppercase(def.default_visibility))),
                          #("visibility", value.Enum(visibility)),
                        ]))
                      }
                    }
                  }
                }
              }
            }
          }
          _, _ -> Error("val and visibility are required")
        }
      }
    }
  },
),
```

**Step 2: Add required imports**

Add imports for `label_definitions`, `label_preferences`.

**Step 3: Build to verify**

Run: `cd server && gleam build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add server/src/graphql/lexicon/mutations.gleam
git commit -m "feat(lexicon): add setLabelPreference mutation"
```

---

### Task 8: Update Seed Data

**Files:**
- Modify: `server/src/database/seed.gleam` (or wherever labels are seeded)

**Step 1: Update seed to include default_visibility**

Update each seeded label definition to include appropriate default visibility:

| Label | Default Visibility |
|-------|-------------------|
| `porn` | `hide` |
| `sexual` | `warn` |
| `nudity` | `warn` |
| `gore` | `warn` |
| `nsfl` | `warn` |
| `graphic-media` | `warn` |
| `spam` | `warn` |
| `impersonation` | `warn` |

**Step 2: Build and test seed**

Run: `cd server && gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add server/src/database/seed.gleam
git commit -m "feat(db): add default_visibility to seeded labels"
```

---

### Task 9: Add Integration Tests

**Files:**
- Create: `server/test/label_preferences_test.gleam`

**Step 1: Write test for viewerLabelPreferences query**

```gleam
pub fn viewer_label_preferences_returns_all_non_system_labels_test() {
  use conn <- test_helpers.with_test_db()
  use auth <- test_helpers.with_authenticated_user(conn)

  let query = "
    query {
      viewerLabelPreferences {
        val
        visibility
        defaultVisibility
      }
    }
  "

  let result = execute_query(conn, query, auth)

  // Should return labels, none starting with !
  let prefs = get_field(result, "viewerLabelPreferences")
  assert list.length(prefs) > 0
  assert list.all(prefs, fn(p) { !string.starts_with(p.val, "!") })
}
```

**Step 2: Write test for setLabelPreference mutation**

```gleam
pub fn set_label_preference_updates_visibility_test() {
  use conn <- test_helpers.with_test_db()
  use auth <- test_helpers.with_authenticated_user(conn)

  let mutation = "
    mutation {
      setLabelPreference(val: \"spam\", visibility: HIDE) {
        val
        visibility
      }
    }
  "

  let result = execute_mutation(conn, mutation, auth)

  assert result.val == "spam"
  assert result.visibility == "HIDE"
}
```

**Step 3: Write test for system label rejection**

```gleam
pub fn set_label_preference_rejects_system_labels_test() {
  use conn <- test_helpers.with_test_db()
  use auth <- test_helpers.with_authenticated_user(conn)

  let mutation = "
    mutation {
      setLabelPreference(val: \"!takedown\", visibility: IGNORE) {
        val
      }
    }
  "

  let result = execute_mutation(conn, mutation, auth)

  assert result.errors != []
  assert string.contains(result.errors[0], "system labels")
}
```

**Step 4: Run tests**

Run: `cd server && gleam test`
Expected: All tests pass

**Step 5: Commit**

```bash
git add server/test/label_preferences_test.gleam
git commit -m "test(lexicon): add label preferences integration tests"
```

---

### Task 10: Update Documentation

**Files:**
- Modify: `docs/guides/moderation.md`

**Step 1: Add Label Preferences section**

Add after "Admin Access" section:

```markdown
## Label Preferences

Users can configure how labeled content appears to them.

### Visibility Settings

| Setting | Behavior |
|---------|----------|
| `IGNORE` | Show content normally, no indicator |
| `SHOW` | Explicitly show (for adult content) |
| `WARN` | Blur with "Show anyway" option |
| `HIDE` | Do not display content |

### Querying Preferences

Authenticated users fetch their preferences:

```graphql
query {
  viewerLabelPreferences {
    val
    description
    visibility
    defaultVisibility
  }
}
```

### Setting Preferences

Update a preference:

```graphql
mutation {
  setLabelPreference(val: "spam", visibility: HIDE) {
    val
    visibility
  }
}
```

System labels (starting with `!`) cannot be configured—the server enforces them.
```

**Step 2: Commit**

```bash
git add docs/guides/moderation.md
git commit -m "docs(moderation): add label preferences documentation"
```
