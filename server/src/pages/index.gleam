import components/activity_chart
import components/alert
import components/backfill_button
import components/button
import components/jetstream_activity_log
import components/layout
import components/stats_cards
import database
import gleam/option.{type Option}
import gleam/result
import jetstream_activity
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/server_component
import sqlight

/// Page data aggregated from database queries
pub type IndexData {
  IndexData(
    record_count: Int,
    lexicon_count: Int,
    actor_count: Int,
    record_lexicons: List(database.Lexicon),
    activity_chart_data: List(jetstream_activity.ActivityBucket),
    jetstream_activity: List(jetstream_activity.ActivityEntry),
  )
}

/// Main view function that renders the index page
pub fn view(
  db: sqlight.Connection,
  current_user: Option(#(String, String)),
  is_admin: Bool,
  domain_authority: Option(String),
  backfilling: Bool,
) -> Element(msg) {
  let data = fetch_data(db)
  render(data, current_user, is_admin, domain_authority, backfilling)
}

/// Fetch all data needed for the index page
fn fetch_data(db: sqlight.Connection) -> IndexData {
  let record_count = case database.get_record_count(db) {
    Ok(count) -> count
    Error(_) -> 0
  }

  let lexicon_count = case database.get_lexicon_count(db) {
    Ok(count) -> count
    Error(_) -> 0
  }

  let actor_count = case database.get_actor_count(db) {
    Ok(count) -> count
    Error(_) -> 0
  }

  let record_lexicons = case database.get_record_type_lexicons(db) {
    Ok(lexicons) -> lexicons
    Error(_) -> []
  }

  // Get 1-day activity data for the chart (default view)
  let activity_chart_data = jetstream_activity.get_activity_1day(db)
    |> result.unwrap([])

  // Get last 24h of individual activity entries for the log
  let jetstream_activity_data =
    jetstream_activity.get_recent_activity(db, 168)
    |> result.unwrap([])

  IndexData(
    record_count: record_count,
    lexicon_count: lexicon_count,
    actor_count: actor_count,
    record_lexicons: record_lexicons,
    activity_chart_data: activity_chart_data,
    jetstream_activity: jetstream_activity_data,
  )
}

/// Render the complete index page
fn render(
  data: IndexData,
  current_user: Option(#(String, String)),
  is_admin: Bool,
  domain_authority: Option(String),
  backfilling: Bool,
) -> Element(msg) {
  layout.page_with_header(
    title: "ATProto Database Stats",
    content: [
      render_alerts(domain_authority, data.lexicon_count),
      render_action_buttons(current_user, is_admin, backfilling),
      // Real-time stats cards server component with initial content
      server_component.element(
        [attribute.id("stats-cards"), server_component.route("/stats-ws")],
        [
          stats_cards.render_stats_grid(
            data.record_count,
            data.actor_count,
            data.lexicon_count,
          ),
        ],
      ),
      // Activity chart with time range selector
      html.div([attribute.class("mb-8")], [
        server_component.element(
          [
            attribute.id("activity-chart"),
            server_component.route("/activity-chart-ws"),
          ],
          [activity_chart.render_static(data.activity_chart_data, activity_chart.OneDay)],
        ),
      ]),
      // JetStream activity log with pre-rendered content
      html.div([attribute.class("mb-8")], [
        server_component.element(
          [
            attribute.id("activity-log"),
            server_component.route("/activity-ws"),
          ],
          [jetstream_activity_log.render_static(data.jetstream_activity)],
        ),
      ]),
    ],
    current_user: current_user,
    domain_authority: domain_authority,
  )
}

/// Render configuration alerts if domain authority is missing or no lexicons loaded
fn render_alerts(
  domain_authority: Option(String),
  lexicon_count: Int,
) -> Element(msg) {
  let domain_alert = case domain_authority {
    option.None ->
      alert.alert_with_link(
        alert.Warning,
        "No domain authority configured.",
        "Settings",
        "/settings",
      )
    option.Some(value) ->
      case value {
        "" ->
          alert.alert_with_link(
            alert.Warning,
            "No domain authority configured.",
            "Settings",
            "/settings",
          )
        _ -> element.none()
      }
  }

  let lexicon_alert = case lexicon_count {
    0 ->
      alert.alert_with_link(
        alert.Info,
        "No lexicons loaded.",
        "Settings",
        "/settings",
      )
    _ -> element.none()
  }

  html.div([], [domain_alert, lexicon_alert])
}

/// Render action buttons for authenticated users
fn render_action_buttons(
  current_user: Option(#(String, String)),
  is_admin: Bool,
  backfilling: Bool,
) -> Element(msg) {
  case current_user {
    option.Some(_) -> {
      html.div([attribute.class("mb-8 flex gap-3")], [
        button.link(href: "/graphiql", text: "Open GraphiQL"),
        case is_admin {
          True ->
            server_component.element(
              [
                attribute.id("backfill-button"),
                server_component.route("/backfill-ws"),
              ],
              [backfill_button.render_button_static(is_admin, backfilling)],
            )
          False -> element.none()
        },
      ])
    }
    option.None -> element.none()
  }
}

