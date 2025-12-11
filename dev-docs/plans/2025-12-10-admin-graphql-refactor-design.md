# Admin GraphQL Schema Refactor Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Break up `server/src/client_schema.gleam` (2060 lines) into 5 logical modules for improved readability and maintainability.

**Architecture:** Split by concern - types, converters, queries, mutations, and schema entry point. Add a `get_field()` helper to eliminate ~270 lines of boilerplate in type resolvers.

**Tech Stack:** Gleam, swell (GraphQL library)

---

### Task 1: Create types.gleam with get_field helper

**Files:**
- Create: `server/src/graphql/admin/types.gleam`

**Step 1: Create the directory and types.gleam file**

```gleam
/// GraphQL type definitions for admin API
///
/// Contains all object types, enum types, and the get_field helper
import gleam/list
import gleam/option.{type Option, Some}
import swell/schema
import swell/value

/// Extract a field from GraphQL context data, returning Null if not found
pub fn get_field(ctx: schema.Context, field_name: String) -> value.Value {
  case ctx.data {
    Some(value.Object(fields)) -> {
      case list.key_find(fields, field_name) {
        Ok(v) -> v
        Error(_) -> value.Null
      }
    }
    _ -> value.Null
  }
}

/// TimeRange enum for activity queries
pub fn time_range_enum() -> schema.Type {
  schema.enum_type("TimeRange", "Time range for activity data", [
    schema.enum_value("ONE_HOUR", "Last 1 hour (5-min buckets)"),
    schema.enum_value("THREE_HOURS", "Last 3 hours (15-min buckets)"),
    schema.enum_value("SIX_HOURS", "Last 6 hours (30-min buckets)"),
    schema.enum_value("ONE_DAY", "Last 24 hours (1-hour buckets)"),
    schema.enum_value("SEVEN_DAYS", "Last 7 days (daily buckets)"),
  ])
}

/// Statistics type showing record, actor, and lexicon counts
pub fn statistics_type() -> schema.Type {
  schema.object_type("Statistics", "System statistics", [
    schema.field(
      "recordCount",
      schema.non_null(schema.int_type()),
      "Total number of records",
      fn(ctx) { Ok(get_field(ctx, "recordCount")) },
    ),
    schema.field(
      "actorCount",
      schema.non_null(schema.int_type()),
      "Total number of actors",
      fn(ctx) { Ok(get_field(ctx, "actorCount")) },
    ),
    schema.field(
      "lexiconCount",
      schema.non_null(schema.int_type()),
      "Total number of lexicons",
      fn(ctx) { Ok(get_field(ctx, "lexiconCount")) },
    ),
  ])
}

/// CurrentSession type for authenticated user information
pub fn current_session_type() -> schema.Type {
  schema.object_type("CurrentSession", "Current authenticated user session", [
    schema.field(
      "did",
      schema.non_null(schema.string_type()),
      "User's DID",
      fn(ctx) { Ok(get_field(ctx, "did")) },
    ),
    schema.field(
      "handle",
      schema.non_null(schema.string_type()),
      "User's handle",
      fn(ctx) { Ok(get_field(ctx, "handle")) },
    ),
    schema.field(
      "isAdmin",
      schema.non_null(schema.boolean_type()),
      "Whether the user is an admin",
      fn(ctx) { Ok(get_field(ctx, "isAdmin")) },
    ),
  ])
}

/// ActivityBucket type for aggregated activity data
pub fn activity_bucket_type() -> schema.Type {
  schema.object_type("ActivityBucket", "Time-bucketed activity counts", [
    schema.field(
      "timestamp",
      schema.non_null(schema.string_type()),
      "Bucket timestamp",
      fn(ctx) { Ok(get_field(ctx, "timestamp")) },
    ),
    schema.field(
      "total",
      schema.non_null(schema.int_type()),
      "Total operations in bucket",
      fn(ctx) { Ok(get_field(ctx, "total")) },
    ),
    schema.field(
      "creates",
      schema.non_null(schema.int_type()),
      "Create operations",
      fn(ctx) { Ok(get_field(ctx, "creates")) },
    ),
    schema.field(
      "updates",
      schema.non_null(schema.int_type()),
      "Update operations",
      fn(ctx) { Ok(get_field(ctx, "updates")) },
    ),
    schema.field(
      "deletes",
      schema.non_null(schema.int_type()),
      "Delete operations",
      fn(ctx) { Ok(get_field(ctx, "deletes")) },
    ),
  ])
}

/// Lexicon type for AT Protocol lexicon schemas
pub fn lexicon_type() -> schema.Type {
  schema.object_type("Lexicon", "AT Protocol lexicon schema definition", [
    schema.field(
      "id",
      schema.non_null(schema.string_type()),
      "Lexicon NSID (e.g., app.bsky.feed.post)",
      fn(ctx) { Ok(get_field(ctx, "id")) },
    ),
    schema.field(
      "json",
      schema.non_null(schema.string_type()),
      "Full lexicon JSON content",
      fn(ctx) { Ok(get_field(ctx, "json")) },
    ),
    schema.field(
      "createdAt",
      schema.non_null(schema.string_type()),
      "Timestamp when lexicon was created",
      fn(ctx) { Ok(get_field(ctx, "createdAt")) },
    ),
  ])
}

/// Settings type for configuration
pub fn settings_type() -> schema.Type {
  schema.object_type("Settings", "System settings and configuration", [
    schema.field(
      "id",
      schema.non_null(schema.string_type()),
      "Global ID for normalization",
      fn(_ctx) { Ok(value.String("Settings:singleton")) },
    ),
    schema.field(
      "domainAuthority",
      schema.non_null(schema.string_type()),
      "Domain authority configuration",
      fn(ctx) { Ok(get_field(ctx, "domainAuthority")) },
    ),
    schema.field(
      "adminDids",
      schema.non_null(schema.list_type(schema.non_null(schema.string_type()))),
      "List of admin DIDs",
      fn(ctx) {
        case get_field(ctx, "adminDids") {
          value.Null -> Ok(value.List([]))
          other -> Ok(other)
        }
      },
    ),
    schema.field(
      "relayUrl",
      schema.non_null(schema.string_type()),
      "AT Protocol relay URL for backfill operations",
      fn(ctx) { Ok(get_field(ctx, "relayUrl")) },
    ),
    schema.field(
      "plcDirectoryUrl",
      schema.non_null(schema.string_type()),
      "PLC directory URL for DID resolution",
      fn(ctx) { Ok(get_field(ctx, "plcDirectoryUrl")) },
    ),
    schema.field(
      "jetstreamUrl",
      schema.non_null(schema.string_type()),
      "Jetstream WebSocket endpoint for real-time indexing",
      fn(ctx) { Ok(get_field(ctx, "jetstreamUrl")) },
    ),
    schema.field(
      "oauthSupportedScopes",
      schema.non_null(schema.string_type()),
      "Space-separated OAuth scopes supported by this server",
      fn(ctx) { Ok(get_field(ctx, "oauthSupportedScopes")) },
    ),
  ])
}

/// OAuthClient type for client registration management
pub fn oauth_client_type() -> schema.Type {
  schema.object_type("OAuthClient", "OAuth client registration", [
    schema.field(
      "clientId",
      schema.non_null(schema.string_type()),
      "Client ID",
      fn(ctx) { Ok(get_field(ctx, "clientId")) },
    ),
    schema.field(
      "clientSecret",
      schema.string_type(),
      "Client secret (confidential clients only)",
      fn(ctx) { Ok(get_field(ctx, "clientSecret")) },
    ),
    schema.field(
      "clientName",
      schema.non_null(schema.string_type()),
      "Client display name",
      fn(ctx) { Ok(get_field(ctx, "clientName")) },
    ),
    schema.field(
      "clientType",
      schema.non_null(schema.string_type()),
      "PUBLIC or CONFIDENTIAL",
      fn(ctx) { Ok(get_field(ctx, "clientType")) },
    ),
    schema.field(
      "redirectUris",
      schema.non_null(schema.list_type(schema.non_null(schema.string_type()))),
      "Allowed redirect URIs",
      fn(ctx) { Ok(get_field(ctx, "redirectUris")) },
    ),
    schema.field(
      "scope",
      schema.string_type(),
      "OAuth scopes for this client (space-separated)",
      fn(ctx) { Ok(get_field(ctx, "scope")) },
    ),
    schema.field(
      "createdAt",
      schema.non_null(schema.int_type()),
      "Creation timestamp",
      fn(ctx) { Ok(get_field(ctx, "createdAt")) },
    ),
  ])
}

/// ActivityEntry type for individual activity records
pub fn activity_entry_type() -> schema.Type {
  schema.object_type("ActivityEntry", "Individual activity log entry", [
    schema.field(
      "id",
      schema.non_null(schema.int_type()),
      "Entry ID",
      fn(ctx) { Ok(get_field(ctx, "id")) },
    ),
    schema.field(
      "timestamp",
      schema.non_null(schema.string_type()),
      "Timestamp",
      fn(ctx) { Ok(get_field(ctx, "timestamp")) },
    ),
    schema.field(
      "operation",
      schema.non_null(schema.string_type()),
      "Operation type",
      fn(ctx) { Ok(get_field(ctx, "operation")) },
    ),
    schema.field(
      "collection",
      schema.non_null(schema.string_type()),
      "Collection name",
      fn(ctx) { Ok(get_field(ctx, "collection")) },
    ),
    schema.field(
      "did",
      schema.non_null(schema.string_type()),
      "DID",
      fn(ctx) { Ok(get_field(ctx, "did")) },
    ),
    schema.field(
      "status",
      schema.non_null(schema.string_type()),
      "Processing status",
      fn(ctx) { Ok(get_field(ctx, "status")) },
    ),
    schema.field(
      "errorMessage",
      schema.string_type(),
      "Error message if failed",
      fn(ctx) { Ok(get_field(ctx, "errorMessage")) },
    ),
    schema.field(
      "eventJson",
      schema.string_type(),
      "Raw event JSON",
      fn(ctx) { Ok(get_field(ctx, "eventJson")) },
    ),
  ])
}
```

**Step 2: Run gleam check to verify syntax**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam check`
Expected: No errors for types.gleam

**Step 3: Commit**

```bash
git add src/graphql/admin/types.gleam
git commit -m "refactor: extract admin GraphQL types to separate module"
```

---

### Task 2: Create converters.gleam

**Files:**
- Create: `server/src/graphql/admin/converters.gleam`

**Step 1: Create converters.gleam with all *_to_value functions**

```gleam
/// Value converters for admin GraphQL API
///
/// Transform domain types to GraphQL value.Value objects
import database/types.{type ActivityBucket, type ActivityEntry, type Lexicon}
import gleam/list
import gleam/option.{None, Some}
import swell/value

/// Convert CurrentSession data to GraphQL value
pub fn current_session_to_value(
  did: String,
  handle: String,
  is_admin: Bool,
) -> value.Value {
  value.Object([
    #("did", value.String(did)),
    #("handle", value.String(handle)),
    #("isAdmin", value.Boolean(is_admin)),
  ])
}

/// Convert statistics counts to GraphQL value
pub fn statistics_to_value(
  record_count: Int,
  actor_count: Int,
  lexicon_count: Int,
) -> value.Value {
  value.Object([
    #("recordCount", value.Int(record_count)),
    #("actorCount", value.Int(actor_count)),
    #("lexiconCount", value.Int(lexicon_count)),
  ])
}

/// Convert ActivityBucket domain type to GraphQL value
pub fn activity_bucket_to_value(bucket: ActivityBucket) -> value.Value {
  let total = bucket.create_count + bucket.update_count + bucket.delete_count
  value.Object([
    #("timestamp", value.String(bucket.timestamp)),
    #("total", value.Int(total)),
    #("creates", value.Int(bucket.create_count)),
    #("updates", value.Int(bucket.update_count)),
    #("deletes", value.Int(bucket.delete_count)),
  ])
}

/// Convert ActivityEntry domain type to GraphQL value
pub fn activity_entry_to_value(entry: ActivityEntry) -> value.Value {
  let error_msg_value = case entry.error_message {
    Some(msg) -> value.String(msg)
    None -> value.Null
  }

  value.Object([
    #("id", value.Int(entry.id)),
    #("timestamp", value.String(entry.timestamp)),
    #("operation", value.String(entry.operation)),
    #("collection", value.String(entry.collection)),
    #("did", value.String(entry.did)),
    #("status", value.String(entry.status)),
    #("errorMessage", error_msg_value),
    #("eventJson", value.String(entry.event_json)),
  ])
}

/// Convert Settings data to GraphQL value
pub fn settings_to_value(
  domain_authority: String,
  admin_dids: List(String),
  relay_url: String,
  plc_directory_url: String,
  jetstream_url: String,
  oauth_supported_scopes: String,
) -> value.Value {
  value.Object([
    #("id", value.String("Settings:singleton")),
    #("domainAuthority", value.String(domain_authority)),
    #("adminDids", value.List(list.map(admin_dids, value.String))),
    #("relayUrl", value.String(relay_url)),
    #("plcDirectoryUrl", value.String(plc_directory_url)),
    #("jetstreamUrl", value.String(jetstream_url)),
    #("oauthSupportedScopes", value.String(oauth_supported_scopes)),
  ])
}

/// Convert OAuthClient domain type to GraphQL value
pub fn oauth_client_to_value(client: types.OAuthClient) -> value.Value {
  let secret_value = case client.client_secret {
    Some(s) -> value.String(s)
    None -> value.Null
  }
  let scope_value = case client.scope {
    Some(s) -> value.String(s)
    None -> value.Null
  }
  value.Object([
    #("clientId", value.String(client.client_id)),
    #("clientSecret", secret_value),
    #("clientName", value.String(client.client_name)),
    #(
      "clientType",
      value.String(types.client_type_to_string(client.client_type)),
    ),
    #("redirectUris", value.List(list.map(client.redirect_uris, value.String))),
    #("scope", scope_value),
    #("createdAt", value.Int(client.created_at)),
  ])
}

/// Convert Lexicon domain type to GraphQL value
pub fn lexicon_to_value(lexicon: Lexicon) -> value.Value {
  value.Object([
    #("id", value.String(lexicon.id)),
    #("json", value.String(lexicon.json)),
    #("createdAt", value.String(lexicon.created_at)),
  ])
}
```

**Step 2: Run gleam check**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam check`
Expected: No errors

**Step 3: Commit**

```bash
git add src/graphql/admin/converters.gleam
git commit -m "refactor: extract admin GraphQL converters to separate module"
```

---

### Task 3: Create queries.gleam

**Files:**
- Create: `server/src/graphql/admin/queries.gleam`

**Step 1: Create queries.gleam with query_type function**

```gleam
/// Query resolvers for admin GraphQL API
import admin_session as session
import backfill_state
import database/repositories/actors
import database/repositories/config as config_repo
import database/repositories/jetstream_activity
import database/repositories/lexicons
import database/repositories/oauth_clients
import database/repositories/records
import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/option.{None, Some}
import gleam/otp/actor
import graphql/admin/converters
import graphql/admin/types as admin_types
import lib/oauth/did_cache
import sqlight
import swell/schema
import swell/value
import wisp

/// Check if a DID is in the admin list
fn is_admin(conn: sqlight.Connection, did: String) -> Bool {
  config_repo.is_admin(conn, did)
}

/// Build the Query root type with all query resolvers
pub fn query_type(
  conn: sqlight.Connection,
  req: wisp.Request,
  did_cache: Subject(did_cache.Message),
  backfill_state_subject: Subject(backfill_state.Message),
) -> schema.Type {
  schema.object_type("Query", "Root query type", [
    // currentSession query
    schema.field(
      "currentSession",
      admin_types.current_session_type(),
      "Get current authenticated user session (null if not authenticated)",
      fn(_ctx) {
        case session.get_current_session(req, conn, did_cache) {
          Ok(sess) -> {
            let user_is_admin = is_admin(conn, sess.did)
            Ok(converters.current_session_to_value(
              sess.did,
              sess.handle,
              user_is_admin,
            ))
          }
          Error(_) -> Ok(value.Null)
        }
      },
    ),
    // statistics query
    schema.field(
      "statistics",
      schema.non_null(admin_types.statistics_type()),
      "Get system statistics",
      fn(_ctx) {
        case
          records.get_count(conn),
          actors.get_count(conn),
          lexicons.get_count(conn)
        {
          Ok(record_count), Ok(actor_count), Ok(lexicon_count) -> {
            Ok(converters.statistics_to_value(
              record_count,
              actor_count,
              lexicon_count,
            ))
          }
          _, _, _ -> Error("Failed to fetch statistics")
        }
      },
    ),
    // settings query
    schema.field(
      "settings",
      schema.non_null(admin_types.settings_type()),
      "Get system settings",
      fn(_ctx) {
        let domain_authority = case config_repo.get(conn, "domain_authority") {
          Ok(authority) -> authority
          Error(_) -> ""
        }
        let admin_dids = config_repo.get_admin_dids(conn)
        let relay_url = config_repo.get_relay_url(conn)
        let plc_directory_url = config_repo.get_plc_directory_url(conn)
        let jetstream_url = config_repo.get_jetstream_url(conn)
        let oauth_supported_scopes = config_repo.get_oauth_supported_scopes(conn)

        Ok(converters.settings_to_value(
          domain_authority,
          admin_dids,
          relay_url,
          plc_directory_url,
          jetstream_url,
          oauth_supported_scopes,
        ))
      },
    ),
    // isBackfilling query
    schema.field(
      "isBackfilling",
      schema.non_null(schema.boolean_type()),
      "Check if a backfill operation is currently running",
      fn(_ctx) {
        let is_backfilling =
          actor.call(
            backfill_state_subject,
            waiting: 100,
            sending: backfill_state.IsBackfilling,
          )
        Ok(value.Boolean(is_backfilling))
      },
    ),
    // lexicons query
    schema.field(
      "lexicons",
      schema.non_null(schema.list_type(schema.non_null(admin_types.lexicon_type()))),
      "Get all lexicons",
      fn(_ctx) {
        case lexicons.get_all(conn) {
          Ok(lexicon_list) ->
            Ok(value.List(list.map(lexicon_list, converters.lexicon_to_value)))
          Error(_) -> Error("Failed to fetch lexicons")
        }
      },
    ),
    // oauthClients query (admin only)
    schema.field(
      "oauthClients",
      schema.non_null(schema.list_type(schema.non_null(admin_types.oauth_client_type()))),
      "Get all OAuth client registrations (admin only)",
      fn(_ctx) {
        case session.get_current_session(req, conn, did_cache) {
          Ok(sess) -> {
            case is_admin(conn, sess.did) {
              True -> {
                case oauth_clients.get_all(conn) {
                  Ok(clients) ->
                    Ok(value.List(list.map(clients, converters.oauth_client_to_value)))
                  Error(_) -> Error("Failed to fetch OAuth clients")
                }
              }
              False -> Error("Admin privileges required")
            }
          }
          Error(_) -> Error("Authentication required")
        }
      },
    ),
    // activityBuckets query with TimeRange argument
    schema.field_with_args(
      "activityBuckets",
      schema.non_null(schema.list_type(schema.non_null(admin_types.activity_bucket_type()))),
      "Get activity data bucketed by time range",
      [
        schema.argument(
          "range",
          schema.non_null(admin_types.time_range_enum()),
          "Time range for bucketing",
          None,
        ),
      ],
      fn(ctx) {
        case schema.get_argument(ctx, "range") {
          Some(value.String("ONE_HOUR")) -> {
            case jetstream_activity.get_activity_1hr(conn) {
              Ok(buckets) ->
                Ok(value.List(list.map(buckets, converters.activity_bucket_to_value)))
              Error(_) -> Error("Failed to fetch 1-hour activity data")
            }
          }
          Some(value.String("THREE_HOURS")) -> {
            case jetstream_activity.get_activity_3hr(conn) {
              Ok(buckets) ->
                Ok(value.List(list.map(buckets, converters.activity_bucket_to_value)))
              Error(_) -> Error("Failed to fetch 3-hour activity data")
            }
          }
          Some(value.String("SIX_HOURS")) -> {
            case jetstream_activity.get_activity_6hr(conn) {
              Ok(buckets) ->
                Ok(value.List(list.map(buckets, converters.activity_bucket_to_value)))
              Error(_) -> Error("Failed to fetch 6-hour activity data")
            }
          }
          Some(value.String("ONE_DAY")) -> {
            case jetstream_activity.get_activity_1day(conn) {
              Ok(buckets) ->
                Ok(value.List(list.map(buckets, converters.activity_bucket_to_value)))
              Error(_) -> Error("Failed to fetch 1-day activity data")
            }
          }
          Some(value.String("SEVEN_DAYS")) -> {
            case jetstream_activity.get_activity_7day(conn) {
              Ok(buckets) ->
                Ok(value.List(list.map(buckets, converters.activity_bucket_to_value)))
              Error(_) -> Error("Failed to fetch 7-day activity data")
            }
          }
          _ -> Error("Invalid or missing time range argument")
        }
      },
    ),
    // recentActivity query with hours argument
    schema.field_with_args(
      "recentActivity",
      schema.non_null(schema.list_type(schema.non_null(admin_types.activity_entry_type()))),
      "Get recent activity entries",
      [
        schema.argument(
          "hours",
          schema.non_null(schema.int_type()),
          "Number of hours to look back",
          None,
        ),
      ],
      fn(ctx) {
        case schema.get_argument(ctx, "hours") {
          Some(value.Int(hours)) -> {
            case jetstream_activity.get_recent_activity(conn, hours) {
              Ok(entries) ->
                Ok(value.List(list.map(entries, converters.activity_entry_to_value)))
              Error(_) -> Error("Failed to fetch recent activity")
            }
          }
          _ -> Error("Invalid or missing hours argument")
        }
      },
    ),
  ])
}
```

**Step 2: Run gleam check**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam check`
Expected: No errors

**Step 3: Commit**

```bash
git add src/graphql/admin/queries.gleam
git commit -m "refactor: extract admin GraphQL queries to separate module"
```

---

### Task 4: Create mutations.gleam

**Files:**
- Create: `server/src/graphql/admin/mutations.gleam`

**Step 1: Create mutations.gleam with mutation_type function**

This is a large file (~750 lines). Create the file with the full mutation_type function containing all mutation resolvers:
- `updateSettings`
- `uploadLexicons`
- `resetAll`
- `triggerBackfill`
- `backfillActor`
- `createOAuthClient`
- `updateOAuthClient`
- `deleteOAuthClient`

Copy the mutation_type function from `client_schema.gleam` lines 1003-2037, updating imports to use:
- `import graphql/admin/converters`
- `import graphql/admin/types as admin_types`

Replace direct type references with `admin_types.*` and converter calls with `converters.*`.

Add these helper functions at the top of the file (copy from client_schema.gleam):
- `is_valid_did()` (lines 32-48)
- `validate_scope_against_supported()` (lines 58-81)
- `is_admin()` (lines 53-55)

**Step 2: Run gleam check**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam check`
Expected: No errors

**Step 3: Commit**

```bash
git add src/graphql/admin/mutations.gleam
git commit -m "refactor: extract admin GraphQL mutations to separate module"
```

---

### Task 5: Create schema.gleam entry point

**Files:**
- Create: `server/src/graphql/admin/schema.gleam`

**Step 1: Create schema.gleam with build_schema function**

```gleam
/// Admin GraphQL schema entry point
///
/// This is the public API for the admin GraphQL schema.
/// External code should import this module to build the schema.
import backfill_state
import gleam/erlang/process.{type Subject}
import gleam/option.{type Option, Some}
import graphql/admin/mutations
import graphql/admin/queries
import jetstream_consumer
import lib/oauth/did_cache
import sqlight
import swell/schema
import wisp

/// Build the complete GraphQL schema for admin queries
pub fn build_schema(
  conn: sqlight.Connection,
  req: wisp.Request,
  jetstream_subject: Option(Subject(jetstream_consumer.ManagerMessage)),
  did_cache: Subject(did_cache.Message),
  oauth_supported_scopes: List(String),
  backfill_state_subject: Subject(backfill_state.Message),
) -> schema.Schema {
  schema.schema(
    queries.query_type(conn, req, did_cache, backfill_state_subject),
    Some(mutations.mutation_type(
      conn,
      req,
      jetstream_subject,
      did_cache,
      oauth_supported_scopes,
      backfill_state_subject,
    )),
  )
}
```

**Step 2: Run gleam check**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam check`
Expected: No errors

**Step 3: Commit**

```bash
git add src/graphql/admin/schema.gleam
git commit -m "refactor: add admin GraphQL schema entry point"
```

---

### Task 6: Update client_graphql handler to use new module

**Files:**
- Modify: `server/src/handlers/client_graphql.gleam:6`

**Step 1: Update import**

Change line 6 from:
```gleam
import client_schema
```

To:
```gleam
import graphql/admin/schema as admin_schema
```

**Step 2: Update build_schema call**

Change line 126 from:
```gleam
    client_schema.build_schema(
```

To:
```gleam
    admin_schema.build_schema(
```

**Step 3: Run gleam check**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam check`
Expected: No errors

**Step 4: Commit**

```bash
git add src/handlers/client_graphql.gleam
git commit -m "refactor: update client_graphql handler to use new admin schema module"
```

---

### Task 7: Delete old client_schema.gleam

**Files:**
- Delete: `server/src/client_schema.gleam`

**Step 1: Delete the file**

```bash
rm src/client_schema.gleam
```

**Step 2: Run gleam check to verify no broken imports**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam check`
Expected: No errors

**Step 3: Run gleam build to verify full compilation**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles successfully

**Step 4: Commit**

```bash
git add -A
git commit -m "refactor: remove old client_schema.gleam (replaced by graphql/admin/)"
```

---

### Task 8: Final verification

**Step 1: Run full test suite**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: All tests pass

**Step 2: Start server and verify admin GraphQL works**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam run`

Test with curl:
```bash
curl -X POST http://localhost:8080/admin/graphql \
  -H "Content-Type: application/json" \
  -d '{"query": "{ statistics { recordCount actorCount lexiconCount } }"}'
```

Expected: JSON response with statistics data

**Step 3: Final commit (if any fixes needed)**

If all tests pass and server works, the refactor is complete.

---

## Summary

| Module | Lines | Responsibility |
|--------|-------|----------------|
| types.gleam | ~150 | Object types + get_field() helper |
| converters.gleam | ~100 | *_to_value transformation functions |
| queries.gleam | ~200 | Query root type + resolvers |
| mutations.gleam | ~750 | Mutation root type + resolvers |
| schema.gleam | ~30 | Public build_schema entry point |

**Total: ~1230 lines** (down from 2060, thanks to get_field() helper eliminating ~270 lines of boilerplate)
