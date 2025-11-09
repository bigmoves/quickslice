import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import jetstream_activity.{type ActivityEntry}
import lustre
import lustre/attribute
import lustre/effect
import lustre/element.{type Element}
import lustre/element/html
import sqlight
import stats_pubsub

// APP

pub fn component(db: sqlight.Connection) {
  lustre.application(init(db, _), update, view)
}

// MODEL

pub type Model {
  Model(
    db: sqlight.Connection,
    activities: List(ActivityEntry),
    stats_subscriber: process.Subject(stats_pubsub.StatsEvent),
  )
}

fn init(db: sqlight.Connection, _flags: Nil) -> #(Model, effect.Effect(Msg)) {
  // Get initial activity from database (last 24 hours)
  let activities =
    jetstream_activity.get_recent_activity(db, 24) |> result.unwrap([])

  // We'll subscribe in the listener process, so create a dummy subject here
  let dummy_subscriber = process.new_subject()

  #(
    Model(db: db, activities: activities, stats_subscriber: dummy_subscriber),
    start_listening_in_background(),
  )
}

// UPDATE

pub opaque type Msg {
  StatsEventReceived(stats_pubsub.StatsEvent)
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    StatsEventReceived(event) -> {
      case event {
        stats_pubsub.ActivityLogged(
          id,
          timestamp,
          operation,
          collection,
          did,
          status,
          error_message,
          event_json,
        ) -> {
          // Add new activity to the beginning of the list
          let new_activity =
            jetstream_activity.ActivityEntry(
              id: id,
              timestamp: timestamp,
              operation: operation,
              collection: collection,
              did: did,
              status: status,
              error_message: error_message,
              event_json: event_json,
            )

          // Prepend new activity and limit to 100 entries for UI performance
          let updated_activities =
            [new_activity, ..model.activities] |> list.take(100)

          #(Model(..model, activities: updated_activities), effect.none())
        }
        // Ignore other stats events
        _ -> #(model, effect.none())
      }
    }
  }
}

// EFFECTS

fn start_listening_in_background() -> effect.Effect(Msg) {
  use dispatch <- effect.from

  // Spawn a single long-running process to listen for stats events
  let _ =
    process.spawn_unlinked(fn() {
      // Subscribe in THIS process, not the component process
      let subscriber = stats_pubsub.subscribe()
      listen_loop(subscriber, dispatch)
    })

  Nil
}

fn listen_loop(
  subscriber: process.Subject(stats_pubsub.StatsEvent),
  dispatch: fn(Msg) -> Nil,
) -> Nil {
  let selector = process.new_selector() |> process.select(subscriber)

  let event = process.selector_receive_forever(selector)
  dispatch(StatsEventReceived(event))
  // Keep listening
  listen_loop(subscriber, dispatch)
}

// VIEW

/// Render static activity log for initial page load (before WebSocket connects)
pub fn render_static(activities: List(ActivityEntry)) -> Element(msg) {
  render_activity_log(activities, True)
}

/// Shared activity log renderer
fn render_activity_log(activities: List(ActivityEntry), static: Bool) -> Element(msg) {
  html.div([attribute.class("font-mono mb-8")], [
    html.div([attribute.class("bg-zinc-800/50 rounded p-4")], [
      // Header
      html.div(
        [attribute.class("flex items-center justify-between mb-3")],
        [
          html.div([attribute.class("text-sm text-zinc-500")], [
            element.text("JetStream Activity"),
          ]),
          html.span([attribute.class("text-xs text-zinc-600")], [
            element.text(int.to_string(list.length(activities)) <> " events (24h)"),
          ]),
        ],
      ),
      // Activity list - scrollable
      html.div(
        [attribute.class("max-h-80 overflow-y-auto")],
        [
          case activities {
            [] ->
              html.div([attribute.class("py-8 text-center text-zinc-600 text-xs")], [
                element.text("No activity in the last 24 hours"),
              ])
            activities -> html.div([], list.map(activities, render_activity_entry(_, static)))
          },
        ],
      ),
    ]),
  ])
}

/// Renders a single activity entry (compact Grafana-style with expandable details)
fn render_activity_entry(entry: ActivityEntry, static: Bool) -> Element(msg) {
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
      attribute.class("border-l-2 border-zinc-700/50 hover:border-zinc-600 transition-colors"),
      attribute.attribute("data-entry-id", entry_id),
    ],
    [
      // Main log line
      html.div(
        [
          attribute.class("flex items-start gap-2 py-1 text-xs font-mono hover:bg-zinc-900/30 cursor-pointer group"),
          attribute.attribute("onclick", "this.parentElement.classList.toggle('expanded')"),
        ],
        [
          // Caret for expansion (always visible)
          html.span(
            [
              attribute.class("text-zinc-600 group-hover:text-zinc-400 shrink-0 select-none transition-transform caret"),
              attribute.attribute("data-caret", ""),
            ],
            [element.text("›")],
          ),
          // Timestamp - formatted server-side for static, client-side for dynamic
          html.span(
            [
              attribute.class("text-zinc-600 shrink-0 w-16"),
              attribute.attribute("data-timestamp", entry.timestamp),
            ],
            [element.text(case static {
              True -> format_time_only(entry.timestamp)
              False -> entry.timestamp
            })],
          ),
          // Status icon
          html.span([attribute.class(status_color <> " shrink-0 w-4")], [
            element.text(status_icon),
          ]),
          // Operation
          html.span([attribute.class(operation_color <> " shrink-0 w-12")], [
            element.text(entry.operation),
          ]),
          // Collection
          html.span([attribute.class("text-purple-400 shrink-0")], [
            element.text(entry.collection),
          ]),
          // DID
          html.span([attribute.class("text-zinc-500 truncate")], [
            element.text(entry.did),
          ]),
        ],
      ),
      // Expanded details section - shows all fields and JSON snippet
      html.div(
        [
          attribute.class("px-6 py-2 text-xs bg-zinc-900/50 border-t border-zinc-800 hidden space-y-1"),
          attribute.attribute("data-details", ""),
        ],
        [
          // Full timestamp
          html.div([attribute.class("flex gap-2")], [
            html.span([attribute.class("text-zinc-600 w-20")], [element.text("Timestamp:")]),
            html.span([attribute.class("text-zinc-400")], [element.text(entry.timestamp)]),
          ]),
          // Full DID
          html.div([attribute.class("flex gap-2")], [
            html.span([attribute.class("text-zinc-600 w-20")], [element.text("DID:")]),
            html.span([attribute.class("text-zinc-400 font-mono break-all")], [element.text(entry.did)]),
          ]),
          // Status
          html.div([attribute.class("flex gap-2")], [
            html.span([attribute.class("text-zinc-600 w-20")], [element.text("Status:")]),
            html.span([attribute.class(case entry.status {
              "success" -> "text-green-400"
              "validation_error" -> "text-yellow-400"
              "error" -> "text-red-400"
              _ -> "text-zinc-400"
            })], [element.text(entry.status)]),
          ]),
          // Error message (if present)
          case entry.error_message {
            option.Some(err_msg) ->
              html.div([attribute.class("flex gap-2")], [
                html.span([attribute.class("text-zinc-600 w-20")], [element.text("Error:")]),
                html.span([attribute.class("text-red-400")], [element.text(err_msg)]),
              ])
            option.None -> element.none()
          },
          // JSON snippet - will be formatted client-side
          html.div([attribute.class("mt-2")], [
            html.div([attribute.class("text-zinc-600 mb-1")], [element.text("Event JSON:")]),
            html.pre(
              [
                attribute.class("text-zinc-400 bg-black/40 p-2 rounded text-[10px] whitespace-pre-wrap block"),
                attribute.attribute("data-json", entry.event_json),
              ],
              [element.text(entry.event_json)],
            ),
          ]),
        ],
      ),
    ],
  )
}

/// Extract time portion from ISO8601 timestamp (HH:MM:SS)
/// This matches the format used by the client-side JavaScript formatter
fn format_time_only(timestamp: String) -> String {
  // ISO8601 format: 2025-11-09T02:01:54.375Z
  // We want to extract: 02:01:54
  case string.split(timestamp, "T") {
    [_, time_part] -> {
      // time_part is like "02:01:54.375Z"
      case string.split(time_part, ".") {
        [time, _] -> time  // Returns "02:01:54"
        _ -> timestamp  // Fallback to full timestamp
      }
    }
    _ -> timestamp  // Fallback to full timestamp
  }
}

fn view(model: Model) -> Element(Msg) {
  html.div([attribute.class("font-mono")], [
    // Include Tailwind styles in the Shadow DOM
    element.element(
      "link",
      [
        attribute.attribute("rel", "stylesheet"),
        attribute.attribute("href", "/styles.css"),
      ],
      [],
    ),
    // CSS for expandable details
    element.element(
      "style",
      [],
      [
        element.text(
          "[data-entry-id].expanded [data-caret] { transform: rotate(90deg); }
          [data-entry-id].expanded [data-details] { display: block !important; }",
        ),
      ],
    ),
    render_activity_log(model.activities, False),
  ])
}
