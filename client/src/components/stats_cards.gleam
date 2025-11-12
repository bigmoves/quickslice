/// Stats Cards Component
///
/// Displays system statistics (record count, actor count, lexicon count)
///
/// ```graphql
/// query GetStatistics {
///   statistics {
///     recordCount
///     actorCount
///     lexiconCount
///   }
/// }
/// ```
import generated/queries/get_statistics
import gleam/json
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import number_formatter
import squall_cache.{type Cache}

pub fn view(cache: Cache) -> Element(msg) {
  let #(_cache, result) =
    squall_cache.lookup(
      cache,
      "GetStatistics",
      json.object([]),
      get_statistics.parse_get_statistics_response,
    )

  case result {
    squall_cache.Loading ->
      html.div([attribute.class("mb-8 grid grid-cols-3 gap-4")], [
        loading_card("Total Records"),
        loading_card("Total Actors"),
        loading_card("Total Lexicons"),
      ])

    squall_cache.Failed(msg) ->
      html.div([attribute.class("mb-8")], [
        html.div([attribute.class("bg-red-800/50 rounded p-4 text-red-200")], [
          html.text("Error loading statistics: " <> msg),
        ]),
      ])

    squall_cache.Data(data) -> {
      let stats = data.statistics

      html.div([attribute.class("mb-8 grid grid-cols-3 gap-4")], [
        stat_card(
          "Total Records",
          number_formatter.format_number(stats.record_count),
        ),
        stat_card(
          "Total Actors",
          number_formatter.format_number(stats.actor_count),
        ),
        stat_card(
          "Total Lexicons",
          number_formatter.format_number(stats.lexicon_count),
        ),
      ])
    }
  }
}

fn stat_card(label: String, value: String) -> Element(msg) {
  html.div([attribute.class("bg-zinc-800/50 rounded p-4")], [
    html.div([attribute.class("text-sm text-zinc-500 mb-1")], [
      html.text(label),
    ]),
    html.div([attribute.class("text-2xl font-semibold text-zinc-200")], [
      html.text(value),
    ]),
  ])
}

fn loading_card(label: String) -> Element(msg) {
  html.div([attribute.class("bg-zinc-800/50 rounded p-4 animate-pulse")], [
    html.div([attribute.class("text-sm text-zinc-500 mb-1")], [
      html.text(label),
    ]),
    html.div([attribute.class("h-8 bg-zinc-700 rounded w-24")], []),
  ])
}
