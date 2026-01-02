/// GraphQL type definitions for admin API
///
/// Contains all object types, enum types, and the get_field helper
import gleam/list
import gleam/option.{Some}
import swell/connection
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

/// TimeRange variants for activity queries
pub type TimeRange {
  OneHour
  ThreeHours
  SixHours
  OneDay
  SevenDays
}

/// Parse a GraphQL string value to TimeRange type
pub fn time_range_from_string(s: String) -> Result(TimeRange, Nil) {
  case s {
    "ONE_HOUR" -> Ok(OneHour)
    "THREE_HOURS" -> Ok(ThreeHours)
    "SIX_HOURS" -> Ok(SixHours)
    "ONE_DAY" -> Ok(OneDay)
    "SEVEN_DAYS" -> Ok(SevenDays)
    _ -> Error(Nil)
  }
}

/// TimeRange enum for activity queries (GraphQL schema type)
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
    schema.field("id", schema.non_null(schema.int_type()), "Entry ID", fn(ctx) {
      Ok(get_field(ctx, "id"))
    }),
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
    schema.field("did", schema.non_null(schema.string_type()), "DID", fn(ctx) {
      Ok(get_field(ctx, "did"))
    }),
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
    schema.field("eventJson", schema.string_type(), "Raw event JSON", fn(ctx) {
      Ok(get_field(ctx, "eventJson"))
    }),
  ])
}

// =============================================================================
// Label and Report Types
// =============================================================================

/// LabelSeverity enum for label definitions
pub fn label_severity_enum() -> schema.Type {
  schema.enum_type("LabelSeverity", "Severity level of a label", [
    schema.enum_value("INFORM", "Informational, client can show indicator"),
    schema.enum_value("ALERT", "Client should warn/blur"),
    schema.enum_value("TAKEDOWN", "Server filters, content not returned"),
  ])
}

/// LabelVisibility enum for user preferences
pub fn label_visibility_enum() -> schema.Type {
  schema.enum_type("LabelVisibility", "How to display labeled content", [
    schema.enum_value("IGNORE", "Show content normally, no indicator"),
    schema.enum_value("SHOW", "Explicitly show (for adult content)"),
    schema.enum_value("WARN", "Blur with click-through warning"),
    schema.enum_value("HIDE", "Do not show content"),
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
      "defaultVisibility",
      schema.non_null(label_visibility_enum()),
      "Default visibility setting for this label",
      fn(ctx) { Ok(get_field(ctx, "defaultVisibility")) },
    ),
    schema.field(
      "createdAt",
      schema.non_null(schema.string_type()),
      "Creation timestamp",
      fn(ctx) { Ok(get_field(ctx, "createdAt")) },
    ),
  ])
}

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
      schema.non_null(label_severity_enum()),
      "Label severity (inform, alert, takedown)",
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

/// Label type
pub fn label_type() -> schema.Type {
  schema.object_type("Label", "Applied label on a record or account", [
    schema.field("id", schema.non_null(schema.int_type()), "Label ID", fn(ctx) {
      Ok(get_field(ctx, "id"))
    }),
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

/// Edge type for Label connection
pub fn label_edge_type() -> schema.Type {
  connection.edge_type("Label", label_type())
}

/// Connection type for paginated Label results
pub fn label_connection_type() -> schema.Type {
  connection.connection_type("Label", label_edge_type())
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
    schema.field("id", schema.non_null(schema.int_type()), "Report ID", fn(ctx) {
      Ok(get_field(ctx, "id"))
    }),
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

/// Edge type for Report connection
pub fn report_edge_type() -> schema.Type {
  connection.edge_type("Report", report_type())
}

/// Connection type for paginated Report results
pub fn report_connection_type() -> schema.Type {
  connection.connection_type("Report", report_edge_type())
}
