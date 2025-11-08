import components/alert
import components/backfill_button
import components/button
import components/collection_table
import components/layout
import components/sparkline
import components/stats_cards
import database
import gleam/option.{type Option}
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
    collection_stats: List(database.CollectionStat),
    record_lexicons: List(database.Lexicon),
    record_activity: List(database.ActivityPoint),
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

  let collection_stats = case database.get_collection_stats(db) {
    Ok(stats) -> stats
    Error(_) -> []
  }

  let record_lexicons = case database.get_record_type_lexicons(db) {
    Ok(lexicons) -> lexicons
    Error(_) -> []
  }

  let record_activity = case database.get_record_activity(db, 168) {
    Ok(activity) -> activity
    Error(_) -> []
  }

  IndexData(
    record_count: record_count,
    lexicon_count: lexicon_count,
    actor_count: actor_count,
    collection_stats: collection_stats,
    record_lexicons: record_lexicons,
    record_activity: record_activity,
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
      render_action_buttons(current_user),
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
      render_activity_section(data.record_activity),
      render_collections_section(
        data.collection_stats,
        data.record_lexicons,
        is_admin,
        backfilling,
      ),
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
) -> Element(msg) {
  case current_user {
    option.Some(_) -> {
      html.div([attribute.class("mb-8 flex gap-3")], [
        button.link(href: "/graphiql", text: "Open GraphiQL"),
      ])
    }
    option.None -> element.none()
  }
}

/// Render the activity chart section
fn render_activity_section(
  activity: List(database.ActivityPoint),
) -> Element(msg) {
  html.div([attribute.class("mb-8")], [
    html.div([attribute.class("bg-zinc-800/50 rounded p-4")], [
      html.div([attribute.class("text-sm text-zinc-500 mb-3")], [
        element.text("Activity (Last 7 Days)"),
      ]),
      sparkline.view(activity),
    ]),
  ])
}

/// Render the collections table section
fn render_collections_section(
  collection_stats: List(database.CollectionStat),
  record_lexicons: List(database.Lexicon),
  is_admin: Bool,
  backfilling: Bool,
) -> Element(msg) {
  let backfill_button = case is_admin {
    True ->
      server_component.element(
        [
          attribute.id("backfill-button"),
          server_component.route("/backfill-ws"),
        ],
        [backfill_button.render_button_static(is_admin, backfilling)],
      )
    False -> element.none()
  }

  html.div([], [
    html.div([attribute.class("flex justify-between items-center mb-4")], [
      html.h2([attribute.class("text-2xl font-semibold text-zinc-300")], [
        element.text("Collections"),
      ]),
      backfill_button,
    ]),
    collection_table.view(collection_stats, record_lexicons),
  ])
}
