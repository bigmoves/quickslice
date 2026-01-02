/// Query resolvers for admin GraphQL API
import admin_session as session
import backfill_state
import database/executor.{type Executor}
import database/repositories/actors
import database/repositories/config as config_repo
import database/repositories/jetstream_activity
import database/repositories/label_definitions
import database/repositories/label_preferences
import database/repositories/labels
import database/repositories/lexicons
import database/repositories/oauth_clients
import database/repositories/records
import database/repositories/reports
import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/option.{None, Some}
import gleam/otp/actor
import gleam/string
import graphql/admin/converters
import graphql/admin/cursor
import graphql/admin/types as admin_types
import graphql/lexicon/converters as lexicon_converters
import lib/oauth/did_cache
import swell/connection
import swell/schema
import swell/value
import wisp

/// Fetch activity buckets for a given time range
fn fetch_activity_buckets(
  conn: Executor,
  range: admin_types.TimeRange,
) -> Result(value.Value, String) {
  let fetch_result = case range {
    admin_types.OneHour -> jetstream_activity.get_activity_1hr(conn)
    admin_types.ThreeHours -> jetstream_activity.get_activity_3hr(conn)
    admin_types.SixHours -> jetstream_activity.get_activity_6hr(conn)
    admin_types.OneDay -> jetstream_activity.get_activity_1day(conn)
    admin_types.SevenDays -> jetstream_activity.get_activity_7day(conn)
  }
  case fetch_result {
    Ok(buckets) ->
      Ok(value.List(list.map(buckets, converters.activity_bucket_to_value)))
    Error(_) -> Error("Failed to fetch activity data")
  }
}

/// Build the Query root type with all query resolvers
pub fn query_type(
  conn: Executor,
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
            let user_is_admin = config_repo.is_admin(conn, sess.did)
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
        let oauth_supported_scopes =
          config_repo.get_oauth_supported_scopes(conn)

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
      schema.non_null(
        schema.list_type(schema.non_null(admin_types.lexicon_type())),
      ),
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
      schema.non_null(
        schema.list_type(schema.non_null(admin_types.oauth_client_type())),
      ),
      "Get all OAuth client registrations (admin only)",
      fn(_ctx) {
        case session.get_current_session(req, conn, did_cache) {
          Ok(sess) -> {
            case config_repo.is_admin(conn, sess.did) {
              True -> {
                case oauth_clients.get_all(conn) {
                  Ok(clients) ->
                    Ok(
                      value.List(list.map(
                        clients,
                        converters.oauth_client_to_value,
                      )),
                    )
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
      schema.non_null(
        schema.list_type(schema.non_null(admin_types.activity_bucket_type())),
      ),
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
          Some(value.String(range_str)) ->
            case admin_types.time_range_from_string(range_str) {
              Ok(range) -> fetch_activity_buckets(conn, range)
              Error(_) -> Error("Invalid time range argument")
            }
          _ -> Error("Missing time range argument")
        }
      },
    ),
    // recentActivity query with hours argument
    schema.field_with_args(
      "recentActivity",
      schema.non_null(
        schema.list_type(schema.non_null(admin_types.activity_entry_type())),
      ),
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
                Ok(
                  value.List(list.map(
                    entries,
                    converters.activity_entry_to_value,
                  )),
                )
              Error(_) -> Error("Failed to fetch recent activity")
            }
          }
          _ -> Error("Invalid or missing hours argument")
        }
      },
    ),
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
    // viewerLabelPreferences query (authenticated users)
    schema.field(
      "viewerLabelPreferences",
      schema.non_null(
        schema.list_type(schema.non_null(admin_types.label_preference_type())),
      ),
      "Get label preferences for the current user (non-system labels only)",
      fn(_ctx) {
        case session.get_current_session(req, conn, did_cache) {
          Ok(sess) -> {
            // Get non-system label definitions
            case label_definitions.get_non_system(conn) {
              Ok(defs) -> {
                // Get user's preferences
                case label_preferences.get_by_did(conn, sess.did) {
                  Ok(prefs) -> {
                    // Build a map of label_val -> visibility
                    let pref_map =
                      list.fold(prefs, [], fn(acc, pref) {
                        [#(pref.label_val, pref.visibility), ..acc]
                      })

                    // Map each definition to a preference, using user's setting or default
                    let result =
                      list.map(defs, fn(def) {
                        let visibility = case list.key_find(pref_map, def.val) {
                          Ok(v) -> v
                          Error(_) -> def.default_visibility
                        }
                        lexicon_converters.label_preference_to_value(
                          def,
                          visibility,
                        )
                      })

                    Ok(value.List(result))
                  }
                  Error(_) -> Error("Failed to fetch label preferences")
                }
              }
              Error(_) -> Error("Failed to fetch label definitions")
            }
          }
          Error(_) -> Error("Authentication required")
        }
      },
    ),
    // labels query (admin only) - Connection type
    schema.field_with_args(
      "labels",
      schema.non_null(admin_types.label_connection_type()),
      "Get labels with optional filters (admin only)",
      [
        schema.argument(
          "uri",
          schema.string_type(),
          "Filter by subject URI",
          None,
        ),
        schema.argument(
          "val",
          schema.string_type(),
          "Filter by label value",
          None,
        ),
        schema.argument(
          "first",
          schema.int_type(),
          "Number of items to fetch (default 50)",
          None,
        ),
        schema.argument(
          "after",
          schema.string_type(),
          "Cursor for pagination",
          None,
        ),
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
                let first = case schema.get_argument(ctx, "first") {
                  Some(value.Int(f)) -> f
                  _ -> 50
                }
                let after_id = case schema.get_argument(ctx, "after") {
                  Some(value.String(c)) -> {
                    case cursor.decode(c) {
                      Ok(#("Label", id)) -> Some(id)
                      _ -> None
                    }
                  }
                  _ -> None
                }

                case
                  labels.get_paginated(
                    conn,
                    uri_filter,
                    val_filter,
                    first,
                    after_id,
                  )
                {
                  Ok(paginated) -> {
                    // Build edges with cursors
                    let edges =
                      list.map(paginated.labels, fn(label) {
                        connection.Edge(
                          node: converters.label_to_value(label),
                          cursor: cursor.encode("Label", label.id),
                        )
                      })

                    // Build page info
                    let start_cursor = case list.first(paginated.labels) {
                      Ok(first_label) ->
                        Some(cursor.encode("Label", first_label.id))
                      Error(_) -> None
                    }
                    let end_cursor = case list.last(paginated.labels) {
                      Ok(last_label) ->
                        Some(cursor.encode("Label", last_label.id))
                      Error(_) -> None
                    }

                    let page_info =
                      connection.PageInfo(
                        has_next_page: paginated.has_next_page,
                        has_previous_page: option.is_some(after_id),
                        start_cursor: start_cursor,
                        end_cursor: end_cursor,
                      )

                    let conn_value =
                      connection.Connection(
                        edges: edges,
                        page_info: page_info,
                        total_count: Some(paginated.total_count),
                      )

                    Ok(connection.connection_to_value(conn_value))
                  }
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
    // reports query (admin only) - Connection type
    schema.field_with_args(
      "reports",
      schema.non_null(admin_types.report_connection_type()),
      "Get moderation reports with optional status filter (admin only)",
      [
        schema.argument(
          "status",
          admin_types.report_status_enum(),
          "Filter by status",
          None,
        ),
        schema.argument(
          "first",
          schema.int_type(),
          "Number of items to fetch (default 50)",
          None,
        ),
        schema.argument(
          "after",
          schema.string_type(),
          "Cursor for pagination",
          None,
        ),
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
                let first = case schema.get_argument(ctx, "first") {
                  Some(value.Int(f)) -> f
                  _ -> 50
                }
                let after_id = case schema.get_argument(ctx, "after") {
                  Some(value.String(c)) -> {
                    case cursor.decode(c) {
                      Ok(#("Report", id)) -> Some(id)
                      _ -> None
                    }
                  }
                  _ -> None
                }

                case
                  reports.get_paginated(conn, status_filter, first, after_id)
                {
                  Ok(paginated) -> {
                    // Build edges with cursors
                    let edges =
                      list.map(paginated.reports, fn(report) {
                        connection.Edge(
                          node: converters.report_to_value(report),
                          cursor: cursor.encode("Report", report.id),
                        )
                      })

                    // Build page info
                    let start_cursor = case list.first(paginated.reports) {
                      Ok(first_report) ->
                        Some(cursor.encode("Report", first_report.id))
                      Error(_) -> None
                    }
                    let end_cursor = case list.last(paginated.reports) {
                      Ok(last_report) ->
                        Some(cursor.encode("Report", last_report.id))
                      Error(_) -> None
                    }

                    let page_info =
                      connection.PageInfo(
                        has_next_page: paginated.has_next_page,
                        has_previous_page: option.is_some(after_id),
                        start_cursor: start_cursor,
                        end_cursor: end_cursor,
                      )

                    let conn_value =
                      connection.Connection(
                        edges: edges,
                        page_info: page_info,
                        total_count: Some(paginated.total_count),
                      )

                    Ok(connection.connection_to_value(conn_value))
                  }
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
  ])
}
