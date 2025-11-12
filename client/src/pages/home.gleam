/// Home Page Component
///
/// Displays dashboard with stats cards, activity chart, and recent activity
import components/activity_chart
import components/activity_log
import components/alert
import components/button
import components/stats_cards
import generated/queries/get_activity_buckets
import generated/queries/get_settings
import generated/queries/get_statistics
import gleam/json
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import squall_cache.{type Cache}

pub type Msg {
  ChangeTimeRange(get_activity_buckets.TimeRange)
  TriggerBackfill
  OpenGraphiQL
}

pub fn view(
  cache: Cache,
  time_range: get_activity_buckets.TimeRange,
  is_backfilling: Bool,
  is_admin: Bool,
) -> Element(Msg) {
  // Get statistics to check lexicon count
  let #(_cache1, stats_result) =
    squall_cache.lookup(
      cache,
      "GetStatistics",
      json.object([]),
      get_statistics.parse_get_statistics_response,
    )

  // Get settings to check domain authority
  let #(_cache2, settings_result) =
    squall_cache.lookup(
      cache,
      "GetSettings",
      json.object([]),
      get_settings.parse_get_settings_response,
    )

  // Extract domain authority and lexicon count for alerts
  let alerts = case stats_result, settings_result {
    squall_cache.Data(stats), squall_cache.Data(settings) ->
      render_alerts(settings.settings.domain_authority, stats.statistics.lexicon_count)
    _, _ -> element.none()
  }

  html.div([], [
    // Configuration alerts
    alerts,
    // Action buttons
    html.div([attribute.class("mb-8 flex gap-3")], case is_admin {
      True -> [
        button.button(disabled: False, on_click: OpenGraphiQL, text: "Open GraphiQL"),
        button.button(
          disabled: is_backfilling,
          on_click: TriggerBackfill,
          text: case is_backfilling {
            True -> "Backfilling..."
            False -> "Trigger Backfill"
          },
        ),
      ]
      False -> [button.button(disabled: False, on_click: OpenGraphiQL, text: "Open GraphiQL")]
    }),
    // Stats cards component
    stats_cards.view(cache),
    // Activity chart component
    activity_chart.view(cache, time_range, ChangeTimeRange),
    // Activity log component
    activity_log.view(cache, 24),
  ])
}

/// Render configuration alerts if domain authority is missing or no lexicons loaded
fn render_alerts(
  domain_authority: String,
  lexicon_count: Int,
) -> Element(Msg) {
  let domain_alert = case domain_authority {
    "" ->
      alert.alert_with_link(
        alert.Warning,
        "No domain authority configured.",
        "Settings",
        "/settings",
      )
    _ -> element.none()
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
