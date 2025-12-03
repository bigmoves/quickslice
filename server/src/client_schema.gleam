/// GraphQL schema for client statistics and activity queries
///
/// This schema is separate from the main /graphql endpoint and serves
/// the client SPA with stats and activity data via /admin/graphql
import admin_session as session
import backfill
import backfill_state
import database/repositories/actors
import database/repositories/config as config_repo
import database/repositories/jetstream_activity
import database/repositories/lexicons
import database/repositories/oauth_clients
import database/repositories/records
import database/types.{type ActivityBucket, type ActivityEntry, type Lexicon}
import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/string
import importer
import jetstream_consumer
import lib/oauth/did_cache
import lib/oauth/scopes/validator as scope_validator
import lib/oauth/token_generator
import lib/oauth/validator
import logging
import sqlight
import swell/schema
import swell/value
import wisp

/// Validate that a string is a valid DID format
/// Valid format: did:<method>:<identifier>
fn is_valid_did(s: String) -> Bool {
  case string.starts_with(s, "did:") {
    False -> False
    True -> {
      // Split into parts: should be at least 3 parts (did, method, identifier)
      let parts = string.split(s, ":")
      case parts {
        ["did", method, identifier, ..] ->
          // Method must be non-empty and identifier must be non-empty
          method != "" && identifier != ""
        _ -> False
      }
    }
  }
}

// ===== Helper Functions =====

/// Check if a DID is in the admin list
fn is_admin(conn: sqlight.Connection, did: String) -> Bool {
  config_repo.is_admin(conn, did)
}

/// Validate that all requested scopes are in the supported list
fn validate_scope_against_supported(
  requested_scope: String,
  supported_scopes: List(String),
) -> Result(Nil, String) {
  let requested =
    requested_scope
    |> string.split(" ")
    |> list.map(string.trim)
    |> list.filter(fn(s) { !string.is_empty(s) })

  let invalid =
    list.filter(requested, fn(s) { !list.contains(supported_scopes, s) })

  case invalid {
    [] -> Ok(Nil)
    _ ->
      Error(
        "Unsupported scope(s): "
        <> string.join(invalid, ", ")
        <> ". Supported: "
        <> string.join(supported_scopes, ", "),
      )
  }
}

/// Convert CurrentSession data to GraphQL value
fn current_session_to_value(
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

// ===== Enum Types =====

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

// ===== Object Types =====

/// Statistics type showing record, actor, and lexicon counts
pub fn statistics_type() -> schema.Type {
  schema.object_type("Statistics", "System statistics", [
    schema.field(
      "recordCount",
      schema.non_null(schema.int_type()),
      "Total number of records",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "recordCount") {
              Ok(count) -> Ok(count)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    ),
    schema.field(
      "actorCount",
      schema.non_null(schema.int_type()),
      "Total number of actors",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "actorCount") {
              Ok(count) -> Ok(count)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    ),
    schema.field(
      "lexiconCount",
      schema.non_null(schema.int_type()),
      "Total number of lexicons",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "lexiconCount") {
              Ok(count) -> Ok(count)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
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
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "did") {
              Ok(did) -> Ok(did)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    ),
    schema.field(
      "handle",
      schema.non_null(schema.string_type()),
      "User's handle",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "handle") {
              Ok(handle) -> Ok(handle)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    ),
    schema.field(
      "isAdmin",
      schema.non_null(schema.boolean_type()),
      "Whether the user is an admin",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "isAdmin") {
              Ok(is_admin) -> Ok(is_admin)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
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
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "timestamp") {
              Ok(ts) -> Ok(ts)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    ),
    schema.field(
      "total",
      schema.non_null(schema.int_type()),
      "Total operations in bucket",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "total") {
              Ok(total) -> Ok(total)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    ),
    schema.field(
      "creates",
      schema.non_null(schema.int_type()),
      "Create operations",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "creates") {
              Ok(creates) -> Ok(creates)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    ),
    schema.field(
      "updates",
      schema.non_null(schema.int_type()),
      "Update operations",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "updates") {
              Ok(updates) -> Ok(updates)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    ),
    schema.field(
      "deletes",
      schema.non_null(schema.int_type()),
      "Delete operations",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "deletes") {
              Ok(deletes) -> Ok(deletes)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
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
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "id") {
              Ok(id) -> Ok(id)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    ),
    schema.field(
      "json",
      schema.non_null(schema.string_type()),
      "Full lexicon JSON content",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "json") {
              Ok(json) -> Ok(json)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    ),
    schema.field(
      "createdAt",
      schema.non_null(schema.string_type()),
      "Timestamp when lexicon was created",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "createdAt") {
              Ok(created_at) -> Ok(created_at)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
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
      fn(_ctx) {
        // Settings is a singleton, so we use a constant ID
        Ok(value.String("Settings:singleton"))
      },
    ),
    schema.field(
      "domainAuthority",
      schema.non_null(schema.string_type()),
      "Domain authority configuration",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "domainAuthority") {
              Ok(authority) -> Ok(authority)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    ),
    schema.field(
      "adminDids",
      schema.non_null(schema.list_type(schema.non_null(schema.string_type()))),
      "List of admin DIDs",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "adminDids") {
              Ok(dids) -> Ok(dids)
              Error(_) -> Ok(value.List([]))
            }
          }
          _ -> Ok(value.List([]))
        }
      },
    ),
    schema.field(
      "relayUrl",
      schema.non_null(schema.string_type()),
      "AT Protocol relay URL for backfill operations",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "relayUrl") {
              Ok(url) -> Ok(url)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    ),
    schema.field(
      "plcDirectoryUrl",
      schema.non_null(schema.string_type()),
      "PLC directory URL for DID resolution",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "plcDirectoryUrl") {
              Ok(url) -> Ok(url)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    ),
    schema.field(
      "jetstreamUrl",
      schema.non_null(schema.string_type()),
      "Jetstream WebSocket endpoint for real-time indexing",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "jetstreamUrl") {
              Ok(url) -> Ok(url)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    ),
    schema.field(
      "oauthSupportedScopes",
      schema.non_null(schema.string_type()),
      "Space-separated OAuth scopes supported by this server",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "oauthSupportedScopes") {
              Ok(scopes) -> Ok(scopes)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
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
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "clientId") {
              Ok(v) -> Ok(v)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    ),
    schema.field(
      "clientSecret",
      schema.string_type(),
      "Client secret (confidential clients only)",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "clientSecret") {
              Ok(v) -> Ok(v)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    ),
    schema.field(
      "clientName",
      schema.non_null(schema.string_type()),
      "Client display name",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "clientName") {
              Ok(v) -> Ok(v)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    ),
    schema.field(
      "clientType",
      schema.non_null(schema.string_type()),
      "PUBLIC or CONFIDENTIAL",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "clientType") {
              Ok(v) -> Ok(v)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    ),
    schema.field(
      "redirectUris",
      schema.non_null(schema.list_type(schema.non_null(schema.string_type()))),
      "Allowed redirect URIs",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "redirectUris") {
              Ok(v) -> Ok(v)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    ),
    schema.field(
      "scope",
      schema.string_type(),
      "OAuth scopes for this client (space-separated)",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "scope") {
              Ok(v) -> Ok(v)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    ),
    schema.field(
      "createdAt",
      schema.non_null(schema.int_type()),
      "Creation timestamp",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "createdAt") {
              Ok(v) -> Ok(v)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    ),
  ])
}

/// ActivityEntry type for individual activity records
pub fn activity_entry_type() -> schema.Type {
  schema.object_type("ActivityEntry", "Individual activity log entry", [
    schema.field("id", schema.non_null(schema.int_type()), "Entry ID", fn(ctx) {
      case ctx.data {
        Some(value.Object(fields)) -> {
          case list.key_find(fields, "id") {
            Ok(id) -> Ok(id)
            Error(_) -> Ok(value.Null)
          }
        }
        _ -> Ok(value.Null)
      }
    }),
    schema.field(
      "timestamp",
      schema.non_null(schema.string_type()),
      "Timestamp",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "timestamp") {
              Ok(ts) -> Ok(ts)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    ),
    schema.field(
      "operation",
      schema.non_null(schema.string_type()),
      "Operation type",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "operation") {
              Ok(op) -> Ok(op)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    ),
    schema.field(
      "collection",
      schema.non_null(schema.string_type()),
      "Collection name",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "collection") {
              Ok(coll) -> Ok(coll)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    ),
    schema.field("did", schema.non_null(schema.string_type()), "DID", fn(ctx) {
      case ctx.data {
        Some(value.Object(fields)) -> {
          case list.key_find(fields, "did") {
            Ok(did) -> Ok(did)
            Error(_) -> Ok(value.Null)
          }
        }
        _ -> Ok(value.Null)
      }
    }),
    schema.field(
      "status",
      schema.non_null(schema.string_type()),
      "Processing status",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "status") {
              Ok(status) -> Ok(status)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    ),
    schema.field(
      "errorMessage",
      schema.string_type(),
      "Error message if failed",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "errorMessage") {
              Ok(err_msg) -> Ok(err_msg)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    ),
    schema.field("eventJson", schema.string_type(), "Raw event JSON", fn(ctx) {
      case ctx.data {
        Some(value.Object(fields)) -> {
          case list.key_find(fields, "eventJson") {
            Ok(json) -> Ok(json)
            Error(_) -> Ok(value.Null)
          }
        }
        _ -> Ok(value.Null)
      }
    }),
  ])
}

// ===== Conversion Helpers =====

fn statistics_to_value(
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

fn activity_bucket_to_value(bucket: ActivityBucket) -> value.Value {
  let total = bucket.create_count + bucket.update_count + bucket.delete_count
  value.Object([
    #("timestamp", value.String(bucket.timestamp)),
    #("total", value.Int(total)),
    #("creates", value.Int(bucket.create_count)),
    #("updates", value.Int(bucket.update_count)),
    #("deletes", value.Int(bucket.delete_count)),
  ])
}

fn activity_entry_to_value(entry: ActivityEntry) -> value.Value {
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

fn settings_to_value(
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

fn oauth_client_to_value(client: types.OAuthClient) -> value.Value {
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

fn lexicon_to_value(lexicon: Lexicon) -> value.Value {
  value.Object([
    #("id", value.String(lexicon.id)),
    #("json", value.String(lexicon.json)),
    #("createdAt", value.String(lexicon.created_at)),
  ])
}

// ===== Query Type =====

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
      current_session_type(),
      "Get current authenticated user session (null if not authenticated)",
      fn(_ctx) {
        case session.get_current_session(req, conn, did_cache) {
          Ok(sess) -> {
            let user_is_admin = is_admin(conn, sess.did)
            Ok(current_session_to_value(sess.did, sess.handle, user_is_admin))
          }
          Error(_) -> Ok(value.Null)
        }
      },
    ),
    // statistics query
    schema.field(
      "statistics",
      schema.non_null(statistics_type()),
      "Get system statistics",
      fn(_ctx) {
        case
          records.get_count(conn),
          actors.get_count(conn),
          lexicons.get_count(conn)
        {
          Ok(record_count), Ok(actor_count), Ok(lexicon_count) -> {
            Ok(statistics_to_value(record_count, actor_count, lexicon_count))
          }
          _, _, _ -> Error("Failed to fetch statistics")
        }
      },
    ),
    // settings query
    schema.field(
      "settings",
      schema.non_null(settings_type()),
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
        let oauth_supported_scopes =
          config_repo.get_oauth_supported_scopes(conn)

        Ok(settings_to_value(
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
      schema.non_null(schema.list_type(schema.non_null(lexicon_type()))),
      "Get all lexicons",
      fn(_ctx) {
        case lexicons.get_all(conn) {
          Ok(lexicon_list) ->
            Ok(value.List(list.map(lexicon_list, lexicon_to_value)))
          Error(_) -> Error("Failed to fetch lexicons")
        }
      },
    ),
    // oauthClients query (admin only)
    schema.field(
      "oauthClients",
      schema.non_null(schema.list_type(schema.non_null(oauth_client_type()))),
      "Get all OAuth client registrations (admin only)",
      fn(_ctx) {
        case session.get_current_session(req, conn, did_cache) {
          Ok(sess) -> {
            case is_admin(conn, sess.did) {
              True -> {
                case oauth_clients.get_all(conn) {
                  Ok(clients) ->
                    Ok(value.List(list.map(clients, oauth_client_to_value)))
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
      schema.non_null(schema.list_type(schema.non_null(activity_bucket_type()))),
      "Get activity data bucketed by time range",
      [
        schema.argument(
          "range",
          schema.non_null(time_range_enum()),
          "Time range for bucketing",
          None,
        ),
      ],
      fn(ctx) {
        case schema.get_argument(ctx, "range") {
          Some(value.String("ONE_HOUR")) -> {
            case jetstream_activity.get_activity_1hr(conn) {
              Ok(buckets) ->
                Ok(value.List(list.map(buckets, activity_bucket_to_value)))
              Error(_) -> Error("Failed to fetch 1-hour activity data")
            }
          }
          Some(value.String("THREE_HOURS")) -> {
            case jetstream_activity.get_activity_3hr(conn) {
              Ok(buckets) ->
                Ok(value.List(list.map(buckets, activity_bucket_to_value)))
              Error(_) -> Error("Failed to fetch 3-hour activity data")
            }
          }
          Some(value.String("SIX_HOURS")) -> {
            case jetstream_activity.get_activity_6hr(conn) {
              Ok(buckets) ->
                Ok(value.List(list.map(buckets, activity_bucket_to_value)))
              Error(_) -> Error("Failed to fetch 6-hour activity data")
            }
          }
          Some(value.String("ONE_DAY")) -> {
            case jetstream_activity.get_activity_1day(conn) {
              Ok(buckets) ->
                Ok(value.List(list.map(buckets, activity_bucket_to_value)))
              Error(_) -> Error("Failed to fetch 1-day activity data")
            }
          }
          Some(value.String("SEVEN_DAYS")) -> {
            case jetstream_activity.get_activity_7day(conn) {
              Ok(buckets) ->
                Ok(value.List(list.map(buckets, activity_bucket_to_value)))
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
      schema.non_null(schema.list_type(schema.non_null(activity_entry_type()))),
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
                Ok(value.List(list.map(entries, activity_entry_to_value)))
              Error(_) -> Error("Failed to fetch recent activity")
            }
          }
          _ -> Error("Invalid or missing hours argument")
        }
      },
    ),
  ])
}

/// Mutation type for settings updates
pub fn mutation_type(
  conn: sqlight.Connection,
  req: wisp.Request,
  jetstream_subject: Option(Subject(jetstream_consumer.ManagerMessage)),
  did_cache: Subject(did_cache.Message),
  oauth_supported_scopes: List(String),
  backfill_state_subject: Subject(backfill_state.Message),
) -> schema.Type {
  schema.object_type("Mutation", "Root mutation type", [
    // updateSettings mutation - consolidated settings update
    schema.field_with_args(
      "updateSettings",
      schema.non_null(settings_type()),
      "Update system settings (domain authority and/or admin DIDs)",
      [
        schema.argument(
          "domainAuthority",
          schema.string_type(),
          "New domain authority value (optional)",
          None,
        ),
        schema.argument(
          "adminDids",
          schema.list_type(schema.non_null(schema.string_type())),
          "New admin DIDs list (optional)",
          None,
        ),
        schema.argument(
          "relayUrl",
          schema.string_type(),
          "New relay URL (optional)",
          None,
        ),
        schema.argument(
          "plcDirectoryUrl",
          schema.string_type(),
          "New PLC directory URL (optional)",
          None,
        ),
        schema.argument(
          "jetstreamUrl",
          schema.string_type(),
          "New Jetstream URL (optional)",
          None,
        ),
        schema.argument(
          "oauthSupportedScopes",
          schema.string_type(),
          "New OAuth supported scopes space-separated (optional)",
          None,
        ),
      ],
      fn(ctx) {
        // Check admin privileges
        case session.get_current_session(req, conn, did_cache) {
          Error(_) -> Error("Authentication required")
          Ok(sess) -> {
            case is_admin(conn, sess.did) {
              False -> Error("Admin privileges required")
              True -> {
                // Update domain authority if provided
                let domain_authority_result = case
                  schema.get_argument(ctx, "domainAuthority")
                {
                  Some(value.String(authority)) -> {
                    // Validate not empty
                    case string.trim(authority) {
                      "" -> Error("Domain authority cannot be empty")
                      trimmed_authority -> {
                        case
                          config_repo.set(
                            conn,
                            "domain_authority",
                            trimmed_authority,
                          )
                        {
                          Ok(_) -> {
                            // Restart Jetstream consumer
                            case jetstream_subject {
                              Some(consumer) -> {
                                logging.log(
                                  logging.Info,
                                  "[updateSettings] Restarting Jetstream consumer...",
                                )
                                let _ = jetstream_consumer.restart(consumer)
                                Nil
                              }
                              None -> Nil
                            }
                            Ok(Nil)
                          }
                          Error(_) -> Error("Failed to update domain authority")
                        }
                      }
                    }
                  }
                  _ -> Ok(Nil)
                }

                case domain_authority_result {
                  Error(err) -> Error(err)
                  Ok(_) -> {
                    // Update admin DIDs if provided
                    let admin_dids_result = case
                      schema.get_argument(ctx, "adminDids")
                    {
                      Some(value.List(dids)) -> {
                        let did_strings =
                          list.filter_map(dids, fn(d) {
                            case d {
                              value.String(s) -> Ok(s)
                              _ -> Error(Nil)
                            }
                          })
                        // Validate at least one admin
                        case did_strings {
                          [] -> Error("Cannot have zero admins")
                          _ -> {
                            // Validate all DIDs have correct format
                            let invalid_dids =
                              list.filter(did_strings, fn(d) {
                                !is_valid_did(d)
                              })
                            case invalid_dids {
                              [first_invalid, ..] ->
                                Error(
                                  "Invalid DID format: "
                                  <> first_invalid
                                  <> ". DIDs must start with 'did:' followed by method and identifier (e.g., did:plc:abc123)",
                                )
                              [] -> {
                                case
                                  config_repo.set_admin_dids(conn, did_strings)
                                {
                                  Ok(_) -> Ok(Nil)
                                  Error(_) ->
                                    Error("Failed to update admin DIDs")
                                }
                              }
                            }
                          }
                        }
                      }
                      _ -> Ok(Nil)
                    }

                    case admin_dids_result {
                      Error(err) -> Error(err)
                      Ok(_) -> {
                        // Update relay URL if provided
                        let relay_url_result = case
                          schema.get_argument(ctx, "relayUrl")
                        {
                          Some(value.String(url)) -> {
                            case string.trim(url) {
                              "" -> Error("Relay URL cannot be empty")
                              trimmed_url -> {
                                case
                                  config_repo.set_relay_url(conn, trimmed_url)
                                {
                                  Ok(_) -> Ok(Nil)
                                  Error(_) ->
                                    Error("Failed to update relay URL")
                                }
                              }
                            }
                          }
                          _ -> Ok(Nil)
                        }

                        case relay_url_result {
                          Error(err) -> Error(err)
                          Ok(_) -> {
                            // Update PLC directory URL if provided
                            let plc_url_result = case
                              schema.get_argument(ctx, "plcDirectoryUrl")
                            {
                              Some(value.String(url)) -> {
                                case string.trim(url) {
                                  "" ->
                                    Error("PLC directory URL cannot be empty")
                                  trimmed_url -> {
                                    case
                                      config_repo.set_plc_directory_url(
                                        conn,
                                        trimmed_url,
                                      )
                                    {
                                      Ok(_) -> Ok(True)
                                      Error(_) ->
                                        Error(
                                          "Failed to update PLC directory URL",
                                        )
                                    }
                                  }
                                }
                              }
                              _ -> Ok(False)
                            }

                            case plc_url_result {
                              Error(err) -> Error(err)
                              Ok(plc_changed) -> {
                                // Restart Jetstream if PLC URL changed
                                case plc_changed {
                                  True -> {
                                    case jetstream_subject {
                                      Some(consumer) -> {
                                        logging.log(
                                          logging.Info,
                                          "[updateSettings] Restarting Jetstream consumer due to PLC URL change",
                                        )
                                        case
                                          jetstream_consumer.restart(consumer)
                                        {
                                          Ok(_) ->
                                            logging.log(
                                              logging.Info,
                                              "[updateSettings] Jetstream consumer restarted",
                                            )
                                          Error(err) ->
                                            logging.log(
                                              logging.Error,
                                              "[updateSettings] Failed to restart Jetstream: "
                                                <> err,
                                            )
                                        }
                                      }
                                      None -> Nil
                                    }
                                  }
                                  False -> Nil
                                }
                                // Update Jetstream URL if provided and restart consumer
                                let jetstream_url_result = case
                                  schema.get_argument(ctx, "jetstreamUrl")
                                {
                                  Some(value.String(url)) -> {
                                    case string.trim(url) {
                                      "" ->
                                        Error("Jetstream URL cannot be empty")
                                      trimmed_url -> {
                                        case
                                          config_repo.set_jetstream_url(
                                            conn,
                                            trimmed_url,
                                          )
                                        {
                                          Ok(_) -> Ok(True)
                                          Error(_) ->
                                            Error(
                                              "Failed to update Jetstream URL",
                                            )
                                        }
                                      }
                                    }
                                  }
                                  _ -> Ok(False)
                                }

                                case jetstream_url_result {
                                  Error(err) -> Error(err)
                                  Ok(jetstream_url_changed) -> {
                                    // If Jetstream URL changed, restart consumer
                                    case jetstream_url_changed {
                                      True -> {
                                        case jetstream_subject {
                                          Some(consumer) -> {
                                            logging.log(
                                              logging.Info,
                                              "[updateSettings] Restarting Jetstream consumer due to URL change",
                                            )
                                            case
                                              jetstream_consumer.restart(
                                                consumer,
                                              )
                                            {
                                              Ok(_) ->
                                                logging.log(
                                                  logging.Info,
                                                  "[updateSettings] Jetstream consumer restarted",
                                                )
                                              Error(err) ->
                                                logging.log(
                                                  logging.Error,
                                                  "[updateSettings] Failed to restart Jetstream: "
                                                    <> err,
                                                )
                                            }
                                          }
                                          None -> Nil
                                        }
                                      }
                                      False -> Nil
                                    }

                                    // Update OAuth supported scopes if provided (with validation)
                                    let oauth_scopes_result = case
                                      schema.get_argument(
                                        ctx,
                                        "oauthSupportedScopes",
                                      )
                                    {
                                      Some(value.String(scopes)) -> {
                                        case string.trim(scopes) {
                                          "" ->
                                            Error(
                                              "OAuth supported scopes cannot be empty",
                                            )
                                          trimmed_scopes -> {
                                            // Validate scope format (accepts any valid ATProto scope)
                                            case
                                              scope_validator.validate_scope_format(
                                                trimmed_scopes,
                                              )
                                            {
                                              Ok(_) -> {
                                                // Validation passed, save to database
                                                case
                                                  config_repo.set_oauth_supported_scopes(
                                                    conn,
                                                    trimmed_scopes,
                                                  )
                                                {
                                                  Ok(_) -> Ok(Nil)
                                                  Error(_) ->
                                                    Error(
                                                      "Failed to save OAuth scopes",
                                                    )
                                                }
                                              }
                                              Error(err) -> {
                                                logging.log(
                                                  logging.Error,
                                                  "[updateSettings] Invalid OAuth scope: "
                                                    <> string.inspect(err),
                                                )
                                                Error("Invalid OAuth scope")
                                              }
                                            }
                                          }
                                        }
                                      }
                                      _ -> Ok(Nil)
                                    }

                                    case oauth_scopes_result {
                                      Error(err) -> Error(err)
                                      Ok(_) -> {
                                        // Return updated settings
                                        let final_authority = case
                                          config_repo.get(
                                            conn,
                                            "domain_authority",
                                          )
                                        {
                                          Ok(a) -> a
                                          Error(_) -> ""
                                        }
                                        let final_admin_dids =
                                          config_repo.get_admin_dids(conn)
                                        let final_relay_url =
                                          config_repo.get_relay_url(conn)
                                        let final_plc_directory_url =
                                          config_repo.get_plc_directory_url(
                                            conn,
                                          )
                                        let final_jetstream_url =
                                          config_repo.get_jetstream_url(conn)
                                        let final_oauth_scopes =
                                          config_repo.get_oauth_supported_scopes(
                                            conn,
                                          )

                                        Ok(settings_to_value(
                                          final_authority,
                                          final_admin_dids,
                                          final_relay_url,
                                          final_plc_directory_url,
                                          final_jetstream_url,
                                          final_oauth_scopes,
                                        ))
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      },
    ),
    // uploadLexicons mutation
    schema.field_with_args(
      "uploadLexicons",
      schema.non_null(schema.boolean_type()),
      "Upload and import lexicons from base64-encoded ZIP",
      [
        schema.argument(
          "zipBase64",
          schema.non_null(schema.string_type()),
          "Base64-encoded ZIP file containing lexicon JSON files",
          None,
        ),
      ],
      fn(ctx) {
        case schema.get_argument(ctx, "zipBase64") {
          Some(value.String(zip_base64)) -> {
            // Import lexicons from base64-encoded ZIP
            case importer.import_lexicons_from_base64_zip(zip_base64, conn) {
              Ok(_stats) -> {
                // Restart Jetstream consumer to pick up newly imported collections
                case jetstream_subject {
                  Some(consumer) -> {
                    logging.log(
                      logging.Info,
                      "[uploadLexicons] Restarting Jetstream consumer with new lexicons...",
                    )
                    case jetstream_consumer.restart(consumer) {
                      Ok(_) -> {
                        logging.log(
                          logging.Info,
                          "[uploadLexicons] Jetstream consumer restarted successfully",
                        )
                        Ok(value.Boolean(True))
                      }
                      Error(err) -> {
                        logging.log(
                          logging.Error,
                          "[uploadLexicons] Failed to restart Jetstream consumer: "
                            <> err,
                        )
                        Error(
                          "Lexicons imported but failed to restart Jetstream consumer: "
                          <> err,
                        )
                      }
                    }
                  }
                  None -> {
                    logging.log(
                      logging.Info,
                      "[uploadLexicons] Jetstream consumer not running, skipping restart",
                    )
                    Ok(value.Boolean(True))
                  }
                }
              }
              Error(err) -> Error("Failed to import lexicons: " <> err)
            }
          }
          _ -> Error("Invalid zipBase64 argument")
        }
      },
    ),
    // resetAll mutation
    schema.field_with_args(
      "resetAll",
      schema.non_null(schema.boolean_type()),
      "Reset all data (requires RESET confirmation and admin privileges)",
      [
        schema.argument(
          "confirm",
          schema.non_null(schema.string_type()),
          "Must be the string 'RESET' to confirm",
          None,
        ),
      ],
      fn(ctx) {
        // Check if user is authenticated and admin
        case session.get_current_session(req, conn, did_cache) {
          Ok(sess) -> {
            case is_admin(conn, sess.did) {
              True -> {
                case schema.get_argument(ctx, "confirm") {
                  Some(value.String("RESET")) -> {
                    // Call multiple database functions to reset all data
                    let _ = records.delete_all(conn)
                    let _ = actors.delete_all(conn)
                    let _ = lexicons.delete_all(conn)
                    let _ = config_repo.delete_domain_authority(conn)
                    let _ = jetstream_activity.delete_all(conn)

                    // Restart Jetstream consumer after reset
                    case jetstream_subject {
                      Some(consumer) -> {
                        logging.log(
                          logging.Info,
                          "[resetAll] Restarting Jetstream consumer after reset...",
                        )
                        let _ = jetstream_consumer.restart(consumer)
                        Nil
                      }
                      None -> Nil
                    }

                    Ok(value.Boolean(True))
                  }
                  Some(value.String(_)) -> Error("Confirmation must be 'RESET'")
                  _ -> Error("Invalid confirm argument")
                }
              }
              False -> Error("Admin privileges required to reset all data")
            }
          }
          Error(_) -> Error("Authentication required to reset all data")
        }
      },
    ),
    // triggerBackfill mutation
    schema.field(
      "triggerBackfill",
      schema.non_null(schema.boolean_type()),
      "Trigger a background backfill operation for all collections (admin only)",
      fn(_ctx) {
        // Check if user is authenticated and admin
        case session.get_current_session(req, conn, did_cache) {
          Ok(sess) -> {
            case is_admin(conn, sess.did) {
              True -> {
                // Mark backfill as started
                process.send(
                  backfill_state_subject,
                  backfill_state.StartBackfill,
                )

                // Spawn background process to run backfill
                process.spawn_unlinked(fn() {
                  logging.log(
                    logging.Info,
                    "[triggerBackfill] Starting background backfill...",
                  )

                  // Get all record-type collections from database (only backfill records, not queries/procedures)
                  let collections = case lexicons.get_record_types(conn) {
                    Ok(lexicon_list) ->
                      list.map(lexicon_list, fn(lex) { lex.id })
                    Error(_) -> []
                  }

                  // Get domain authority to determine external collections
                  let domain_authority = case
                    config_repo.get(conn, "domain_authority")
                  {
                    Ok(authority) -> authority
                    Error(_) -> ""
                  }

                  // Split collections into primary and external
                  let #(primary_collections, external_collections) =
                    list.partition(collections, fn(collection) {
                      backfill.nsid_matches_domain_authority(
                        collection,
                        domain_authority,
                      )
                    })

                  // Run backfill with default config and empty repo list (fetches from relay)
                  let config = backfill.default_config(conn)
                  backfill.backfill_collections(
                    [],
                    primary_collections,
                    external_collections,
                    config,
                    conn,
                  )

                  logging.log(
                    logging.Info,
                    "[triggerBackfill] Background backfill completed",
                  )

                  // Mark backfill as stopped
                  process.send(
                    backfill_state_subject,
                    backfill_state.StopBackfill,
                  )
                })

                // Return immediately
                Ok(value.Boolean(True))
              }
              False -> Error("Admin privileges required to trigger backfill")
            }
          }
          Error(_) -> Error("Authentication required to trigger backfill")
        }
      },
    ),
    // backfillActor mutation - sync a specific actor's collections
    schema.field_with_args(
      "backfillActor",
      schema.non_null(schema.boolean_type()),
      "Trigger a background backfill for a specific actor's collections",
      [
        schema.argument(
          "did",
          schema.non_null(schema.string_type()),
          "The DID of the actor to backfill",
          None,
        ),
      ],
      fn(ctx) {
        // Check if user is authenticated (any logged-in user can trigger)
        case session.get_current_session(req, conn, did_cache) {
          Ok(_sess) -> {
            case schema.get_argument(ctx, "did") {
              Some(value.String(did)) -> {
                // Get all record-type collections from database
                let collections = case lexicons.get_record_types(conn) {
                  Ok(lexicon_list) -> list.map(lexicon_list, fn(lex) { lex.id })
                  Error(_) -> []
                }

                // Get domain authority to determine external collections
                let domain_authority = case
                  config_repo.get(conn, "domain_authority")
                {
                  Ok(authority) -> authority
                  Error(_) -> ""
                }

                // Split collections into primary and external
                let #(primary_collections, external_collections) =
                  list.partition(collections, fn(collection) {
                    backfill.nsid_matches_domain_authority(
                      collection,
                      domain_authority,
                    )
                  })

                // Get PLC URL from database config
                let plc_url = config_repo.get_plc_directory_url(conn)

                // Spawn background process to run backfill for this actor
                process.spawn_unlinked(fn() {
                  logging.log(
                    logging.Info,
                    "[backfillActor] Starting background backfill for " <> did,
                  )

                  case
                    backfill.rescue(fn() {
                      backfill.backfill_collections_for_actor(
                        conn,
                        did,
                        primary_collections,
                        external_collections,
                        plc_url,
                      )
                    })
                  {
                    Ok(_) ->
                      logging.log(
                        logging.Info,
                        "[backfillActor] Background backfill completed for "
                          <> did,
                      )
                    Error(err) ->
                      logging.log(
                        logging.Error,
                        "[backfillActor] Background backfill FAILED for "
                          <> did
                          <> ": "
                          <> string.inspect(err),
                      )
                  }
                })

                // Return immediately
                Ok(value.Boolean(True))
              }
              _ -> Error("DID argument is required")
            }
          }
          Error(_) -> Error("Authentication required to trigger backfill")
        }
      },
    ),
    // createOAuthClient mutation
    schema.field_with_args(
      "createOAuthClient",
      schema.non_null(oauth_client_type()),
      "Create a new OAuth client (admin only)",
      [
        schema.argument(
          "clientName",
          schema.non_null(schema.string_type()),
          "Client display name",
          None,
        ),
        schema.argument(
          "clientType",
          schema.non_null(schema.string_type()),
          "PUBLIC or CONFIDENTIAL",
          None,
        ),
        schema.argument(
          "redirectUris",
          schema.non_null(
            schema.list_type(schema.non_null(schema.string_type())),
          ),
          "Allowed redirect URIs",
          None,
        ),
        schema.argument(
          "scope",
          schema.non_null(schema.string_type()),
          "OAuth scopes (space-separated)",
          None,
        ),
      ],
      fn(ctx) {
        case session.get_current_session(req, conn, did_cache) {
          Ok(sess) -> {
            case is_admin(conn, sess.did) {
              True -> {
                case
                  schema.get_argument(ctx, "clientName"),
                  schema.get_argument(ctx, "clientType"),
                  schema.get_argument(ctx, "redirectUris"),
                  schema.get_argument(ctx, "scope")
                {
                  Some(value.String(name)),
                    Some(value.String(type_str)),
                    Some(value.List(uris)),
                    Some(value.String(scope))
                  -> {
                    // Validate client name
                    let trimmed_name = string.trim(name)
                    case trimmed_name {
                      "" -> Error("Client name cannot be empty")
                      _ -> {
                        let client_type = case string.uppercase(type_str) {
                          "CONFIDENTIAL" -> types.Confidential
                          _ -> types.Public
                        }
                        let redirect_uris =
                          list.filter_map(uris, fn(u) {
                            case u {
                              value.String(s) ->
                                case string.trim(s) {
                                  "" -> Error(Nil)
                                  trimmed -> Ok(trimmed)
                                }
                              _ -> Error(Nil)
                            }
                          })
                        // Validate at least one redirect URI
                        case redirect_uris {
                          [] -> Error("At least one redirect URI is required")
                          _ -> {
                            // Validate each redirect URI format
                            let invalid_uri =
                              list.find(redirect_uris, fn(uri) {
                                case validator.validate_redirect_uri(uri) {
                                  Ok(_) -> False
                                  Error(_) -> True
                                }
                              })
                            case invalid_uri {
                              Ok(uri) ->
                                Error(
                                  "Invalid redirect URI: "
                                  <> uri
                                  <> ". URIs must use https://, or http:// only for localhost.",
                                )
                              Error(_) -> {
                                // Validate scope against supported scopes
                                case
                                  validate_scope_against_supported(
                                    scope,
                                    oauth_supported_scopes,
                                  )
                                {
                                  Error(err) -> Error(err)
                                  Ok(_) -> {
                                    let now =
                                      token_generator.current_timestamp()
                                    let client_id =
                                      token_generator.generate_client_id()
                                    let client_secret = case client_type {
                                      types.Confidential ->
                                        Some(
                                          token_generator.generate_client_secret(),
                                        )
                                      types.Public -> None
                                    }
                                    let client =
                                      types.OAuthClient(
                                        client_id: client_id,
                                        client_secret: client_secret,
                                        client_name: trimmed_name,
                                        redirect_uris: redirect_uris,
                                        grant_types: [
                                          types.AuthorizationCode,
                                          types.RefreshToken,
                                        ],
                                        response_types: [types.Code],
                                        scope: case string.trim(scope) {
                                          "" -> None
                                          s -> Some(s)
                                        },
                                        token_endpoint_auth_method: case
                                          client_type
                                        {
                                          types.Confidential ->
                                            types.ClientSecretPost
                                          types.Public -> types.AuthNone
                                        },
                                        client_type: client_type,
                                        created_at: now,
                                        updated_at: now,
                                        metadata: "{}",
                                        access_token_expiration: 3600,
                                        refresh_token_expiration: 86_400 * 30,
                                        require_redirect_exact: True,
                                        registration_access_token: None,
                                        jwks: None,
                                      )
                                    case oauth_clients.insert(conn, client) {
                                      Ok(_) -> Ok(oauth_client_to_value(client))
                                      Error(_) ->
                                        Error("Failed to create OAuth client")
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                  _, _, _, _ -> Error("Invalid arguments")
                }
              }
              False -> Error("Admin privileges required")
            }
          }
          Error(_) -> Error("Authentication required")
        }
      },
    ),
    // updateOAuthClient mutation
    schema.field_with_args(
      "updateOAuthClient",
      schema.non_null(oauth_client_type()),
      "Update an existing OAuth client (admin only)",
      [
        schema.argument(
          "clientId",
          schema.non_null(schema.string_type()),
          "Client ID to update",
          None,
        ),
        schema.argument(
          "clientName",
          schema.non_null(schema.string_type()),
          "New client display name",
          None,
        ),
        schema.argument(
          "redirectUris",
          schema.non_null(
            schema.list_type(schema.non_null(schema.string_type())),
          ),
          "New redirect URIs",
          None,
        ),
        schema.argument(
          "scope",
          schema.non_null(schema.string_type()),
          "OAuth scopes (space-separated)",
          None,
        ),
      ],
      fn(ctx) {
        case session.get_current_session(req, conn, did_cache) {
          Ok(sess) -> {
            case is_admin(conn, sess.did) {
              True -> {
                case
                  schema.get_argument(ctx, "clientId"),
                  schema.get_argument(ctx, "clientName"),
                  schema.get_argument(ctx, "redirectUris"),
                  schema.get_argument(ctx, "scope")
                {
                  Some(value.String(client_id)),
                    Some(value.String(name)),
                    Some(value.List(uris)),
                    Some(value.String(scope))
                  -> {
                    // Validate client name
                    let trimmed_name = string.trim(name)
                    case trimmed_name {
                      "" -> Error("Client name cannot be empty")
                      _ -> {
                        case oauth_clients.get(conn, client_id) {
                          Ok(Some(existing)) -> {
                            let redirect_uris =
                              list.filter_map(uris, fn(u) {
                                case u {
                                  value.String(s) ->
                                    case string.trim(s) {
                                      "" -> Error(Nil)
                                      trimmed -> Ok(trimmed)
                                    }
                                  _ -> Error(Nil)
                                }
                              })
                            // Validate at least one redirect URI
                            case redirect_uris {
                              [] ->
                                Error("At least one redirect URI is required")
                              _ -> {
                                // Validate each redirect URI format
                                let invalid_uri =
                                  list.find(redirect_uris, fn(uri) {
                                    case validator.validate_redirect_uri(uri) {
                                      Ok(_) -> False
                                      Error(_) -> True
                                    }
                                  })
                                case invalid_uri {
                                  Ok(uri) ->
                                    Error(
                                      "Invalid redirect URI: "
                                      <> uri
                                      <> ". URIs must use https://, or http:// only for localhost.",
                                    )
                                  Error(_) -> {
                                    // Validate scope against supported scopes
                                    case
                                      validate_scope_against_supported(
                                        scope,
                                        oauth_supported_scopes,
                                      )
                                    {
                                      Error(err) -> Error(err)
                                      Ok(_) -> {
                                        let updated =
                                          types.OAuthClient(
                                            ..existing,
                                            client_name: trimmed_name,
                                            redirect_uris: redirect_uris,
                                            scope: case string.trim(scope) {
                                              "" -> None
                                              s -> Some(s)
                                            },
                                            updated_at: token_generator.current_timestamp(),
                                          )
                                        case
                                          oauth_clients.update(conn, updated)
                                        {
                                          Ok(_) ->
                                            Ok(oauth_client_to_value(updated))
                                          Error(_) ->
                                            Error(
                                              "Failed to update OAuth client",
                                            )
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                          Ok(None) -> Error("OAuth client not found")
                          Error(_) -> Error("Failed to fetch OAuth client")
                        }
                      }
                    }
                  }
                  _, _, _, _ -> Error("Invalid arguments")
                }
              }
              False -> Error("Admin privileges required")
            }
          }
          Error(_) -> Error("Authentication required")
        }
      },
    ),
    // deleteOAuthClient mutation
    schema.field_with_args(
      "deleteOAuthClient",
      schema.non_null(schema.boolean_type()),
      "Delete an OAuth client (admin only)",
      [
        schema.argument(
          "clientId",
          schema.non_null(schema.string_type()),
          "Client ID to delete",
          None,
        ),
      ],
      fn(ctx) {
        case session.get_current_session(req, conn, did_cache) {
          Ok(sess) -> {
            case is_admin(conn, sess.did) {
              True -> {
                case schema.get_argument(ctx, "clientId") {
                  Some(value.String(client_id)) -> {
                    case client_id {
                      "admin" -> Error("Cannot delete internal admin client")
                      _ -> {
                        case oauth_clients.delete(conn, client_id) {
                          Ok(_) -> Ok(value.Boolean(True))
                          Error(_) -> Error("Failed to delete OAuth client")
                        }
                      }
                    }
                  }
                  _ -> Error("Invalid clientId argument")
                }
              }
              False -> Error("Admin privileges required")
            }
          }
          Error(_) -> Error("Authentication required")
        }
      },
    ),
  ])
}

/// Build the complete GraphQL schema for client queries
pub fn build_schema(
  conn: sqlight.Connection,
  req: wisp.Request,
  jetstream_subject: Option(Subject(jetstream_consumer.ManagerMessage)),
  did_cache: Subject(did_cache.Message),
  oauth_supported_scopes: List(String),
  backfill_state_subject: Subject(backfill_state.Message),
) -> schema.Schema {
  schema.schema(
    query_type(conn, req, did_cache, backfill_state_subject),
    Some(mutation_type(
      conn,
      req,
      jetstream_subject,
      did_cache,
      oauth_supported_scopes,
      backfill_state_subject,
    )),
  )
}
