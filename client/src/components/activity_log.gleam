/// Activity Log Component
///
/// Displays recent jetstream activity entries
///
/// ```graphql
/// query GetRecentActivity($hours: Int!) {
///   recentActivity(hours: $hours) {
///     id
///     timestamp
///     operation
///     collection
///     did
///     status
///     errorMessage
///     eventJson
///   }
/// }
/// ```
import date_formatter
import generated/queries/get_recent_activity
import gleam/int
import gleam/json
import gleam/list
import gleam/option
import json_formatter
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import squall_cache.{type Cache}

pub fn view(cache: Cache, hours: Int) -> Element(msg) {
  let variables = json.object([#("hours", json.int(hours))])

  let #(_cache, result) =
    squall_cache.lookup(
      cache,
      "GetRecentActivity",
      variables,
      get_recent_activity.parse_get_recent_activity_response,
    )

  html.div([attribute.class("font-mono mb-8")], [
    // CSS for expandable details
    element.element("style", [], [
      element.text(
        "[data-entry-id].expanded [data-caret] { transform: rotate(90deg); }
          [data-entry-id].expanded [data-details] { display: block !important; }",
      ),
    ]),
    html.div([attribute.class("bg-zinc-800/50 rounded p-4")], [
      // Header
      html.div([attribute.class("flex items-center justify-between mb-3")], [
        html.div([attribute.class("text-sm text-zinc-500")], [
          element.text("JetStream Activity"),
        ]),
        case result {
          squall_cache.Data(data) ->
            html.span([attribute.class("text-xs text-zinc-600")], [
              element.text(
                int.to_string(list.length(data.recent_activity))
                <> " events ("
                <> int.to_string(hours)
                <> "h)",
              ),
            ])
          _ ->
            html.span([attribute.class("text-xs text-zinc-600")], [
              element.text("(" <> int.to_string(hours) <> "h)"),
            ])
        },
      ]),
      // Activity list - scrollable
      html.div([attribute.class("max-h-80 overflow-y-auto")], [
        case result {
          squall_cache.Loading ->
            html.div(
              [attribute.class("py-8 text-center text-zinc-600 text-xs")],
              [
                element.text("Loading activity..."),
              ],
            )

          squall_cache.Failed(msg) ->
            html.div(
              [attribute.class("py-8 text-center text-red-400 text-xs")],
              [
                element.text("Error: " <> msg),
              ],
            )

          squall_cache.Data(data) -> {
            case data.recent_activity {
              [] ->
                html.div(
                  [attribute.class("py-8 text-center text-zinc-600 text-xs")],
                  [
                    element.text(
                      "No activity in the last "
                      <> int.to_string(hours)
                      <> " hours",
                    ),
                  ],
                )
              entries -> render_activity_entries(entries)
            }
          }
        },
      ]),
    ]),
  ])
}

fn render_activity_entries(
  entries: List(get_recent_activity.ActivityEntry),
) -> Element(msg) {
  html.div(
    [],
    list.map(entries, fn(entry) {
      let status_color = case entry.status {
        "success" -> "text-green-500"
        "validation_error" -> "text-yellow-500"
        "error" -> "text-red-500"
        "processing" -> "text-blue-500"
        _ -> "text-zinc-500"
      }

      let status_icon = case entry.status {
        "success" -> "✓"
        "validation_error" -> "⚠"
        "error" -> "✗"
        "processing" -> "⋯"
        _ -> "•"
      }

      let operation_color = case entry.operation {
        "create" -> "text-green-400"
        "update" -> "text-blue-400"
        "delete" -> "text-red-400"
        _ -> "text-zinc-400"
      }

      let entry_id = "activity-" <> int.to_string(entry.id)

      html.div(
        [
          attribute.class(
            "border-l-2 border-zinc-700/50 hover:border-zinc-600 transition-colors",
          ),
          attribute.attribute("data-entry-id", entry_id),
        ],
        [
          // Main log line
          html.div(
            [
              attribute.class(
                "flex items-start gap-2 py-1 text-xs font-mono hover:bg-zinc-900/30 cursor-pointer group",
              ),
              attribute.attribute(
                "onclick",
                "this.parentElement.classList.toggle('expanded')",
              ),
            ],
            [
              // Caret for expansion (always visible)
              html.span(
                [
                  attribute.class(
                    "text-zinc-600 group-hover:text-zinc-400 shrink-0 select-none transition-transform caret",
                  ),
                  attribute.attribute("data-caret", ""),
                ],
                [element.text("›")],
              ),
              // Timestamp - show time only in local timezone
              html.span(
                [
                  attribute.class("text-zinc-600 shrink-0 w-16"),
                  attribute.attribute("data-timestamp", entry.timestamp),
                ],
                [
                  element.text(date_formatter.format_time_local(entry.timestamp)),
                ],
              ),
              // Status icon
              html.span([attribute.class(status_color <> " shrink-0 w-4")], [
                element.text(status_icon),
              ]),
              // Operation
              html.span([attribute.class(operation_color <> " shrink-0 w-12")], [
                element.text(entry.operation),
              ]),
              // Collection - full name in purple
              html.span([attribute.class("text-purple-400 shrink-0")], [
                element.text(entry.collection),
              ]),
              // DID - no truncation
              html.span([attribute.class("text-zinc-500 truncate")], [
                element.text(entry.did),
              ]),
            ],
          ),
          // Expanded details section - shows all fields and JSON snippet
          html.div(
            [
              attribute.class(
                "px-6 py-2 text-xs bg-zinc-900/50 border-t border-zinc-800 hidden space-y-1",
              ),
              attribute.attribute("data-details", ""),
            ],
            [
              // Full timestamp in local timezone
              html.div([attribute.class("flex gap-2")], [
                html.span([attribute.class("text-zinc-600 w-20")], [
                  element.text("Timestamp:"),
                ]),
                html.span([attribute.class("text-zinc-400")], [
                  element.text(date_formatter.format_datetime_local(
                    entry.timestamp,
                  )),
                ]),
              ]),
              // Full DID
              html.div([attribute.class("flex gap-2")], [
                html.span([attribute.class("text-zinc-600 w-20")], [
                  element.text("DID:"),
                ]),
                html.span(
                  [attribute.class("text-zinc-400 font-mono break-all")],
                  [element.text(entry.did)],
                ),
              ]),
              // Status
              html.div([attribute.class("flex gap-2")], [
                html.span([attribute.class("text-zinc-600 w-20")], [
                  element.text("Status:"),
                ]),
                html.span(
                  [
                    attribute.class(case entry.status {
                      "success" -> "text-green-400"
                      "validation_error" -> "text-yellow-400"
                      "error" -> "text-red-400"
                      _ -> "text-zinc-400"
                    }),
                  ],
                  [element.text(entry.status)],
                ),
              ]),
              // Error message (if present)
              case entry.error_message {
                option.Some(err_msg) ->
                  html.div([attribute.class("flex gap-2")], [
                    html.span([attribute.class("text-zinc-600 w-20")], [
                      element.text("Error:"),
                    ]),
                    html.span([attribute.class("text-red-400")], [
                      element.text(err_msg),
                    ]),
                  ])
                option.None -> element.none()
              },
              // JSON snippet - pretty printed with nested JSON expanded
              case entry.event_json {
                option.Some(json_str) -> {
                  let pretty_json = json_formatter.pretty_print_nested(json_str)
                  html.div([attribute.class("mt-2")], [
                    html.div([attribute.class("text-zinc-600 mb-1")], [
                      element.text("Event JSON:"),
                    ]),
                    html.pre(
                      [
                        attribute.class(
                          "text-zinc-400 bg-black/40 p-2 rounded text-[10px] whitespace-pre-wrap block",
                        ),
                        attribute.attribute("data-json", json_str),
                      ],
                      [element.text(pretty_json)],
                    ),
                  ])
                }
                option.None -> element.none()
              },
            ],
          ),
        ],
      )
    }),
  )
}
