/// Home Page Component
///
/// Displays dashboard with stats cards, activity chart, and recent activity
import backfill_polling
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
  backfill_status: backfill_polling.BackfillStatus,
  is_admin: Bool,
  is_authenticated: Bool,
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
      render_alerts(
        settings.settings.domain_authority,
        stats.statistics.lexicon_count,
      )
    _, _ -> element.none()
  }

  html.div([], [
    // Configuration alerts
    alerts,
    // Action buttons (only shown when authenticated)
    case is_authenticated {
      True ->
        html.div(
          [attribute.class("mb-8 flex gap-3 flex-wrap items-start")],
          case is_admin {
            True -> [
              button.button(
                disabled: False,
                on_click: OpenGraphiQL,
                text: "Open GraphiQL",
              ),
              html.div([attribute.class("flex items-center gap-3")], [
                button.button(
                  disabled: backfill_polling.should_poll(backfill_status),
                  on_click: TriggerBackfill,
                  text: case backfill_status {
                    backfill_polling.Triggered -> "Backfilling..."
                    backfill_polling.InProgress -> "Backfilling..."
                    _ -> "Trigger Backfill"
                  },
                ),
                case backfill_status {
                  backfill_polling.Completed ->
                    html.p([attribute.class("text-sm text-green-600")], [
                      html.text("Backfill complete!"),
                    ])
                  _ -> element.none()
                },
              ]),
            ]
            False -> [
              button.button(
                disabled: False,
                on_click: OpenGraphiQL,
                text: "Open GraphiQL",
              ),
            ]
          },
        )
      False -> element.none()
    },
    // Stats cards component
    stats_cards.view(cache),
    // Activity chart component
    activity_chart.view(cache, time_range, ChangeTimeRange),
    // Activity log component
    activity_log.view(cache, 24),
  ])
}

/// Render configuration alerts if domain authority is missing or no lexicons loaded
fn render_alerts(domain_authority: String, lexicon_count: Int) -> Element(Msg) {
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
