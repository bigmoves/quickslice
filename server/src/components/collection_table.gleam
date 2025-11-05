import database
import gleam/int
import gleam/list
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

/// Renders a table of collections with their record counts
pub fn view(
  collection_stats: List(database.CollectionStat),
  record_lexicons: List(database.Lexicon),
) -> Element(msg) {
  let rows = build_rows(collection_stats, record_lexicons)

  html.div(
    [
      attribute.class(
        "bg-white rounded-lg shadow-sm border border-gray-200 overflow-hidden",
      ),
    ],
    [
      html.table([attribute.class("min-w-full divide-y divide-gray-200")], [
        render_header(),
        html.tbody([attribute.class("bg-white divide-y divide-gray-200")], rows),
      ]),
    ],
  )
}

/// Render the table header
fn render_header() -> Element(msg) {
  html.thead([attribute.class("bg-gray-50")], [
    html.tr([], [
      html.th(
        [
          attribute.class(
            "px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider",
          ),
        ],
        [element.text("Collection")],
      ),
      html.th(
        [
          attribute.class(
            "px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider",
          ),
        ],
        [element.text("Record Count")],
      ),
    ]),
  ])
}

/// Build all table rows from collection stats and lexicons
fn build_rows(
  collection_stats: List(database.CollectionStat),
  record_lexicons: List(database.Lexicon),
) -> List(Element(msg)) {
  // Build rows from actual records
  let record_rows =
    collection_stats
    |> list.map(fn(stat) { render_stat_row(stat.collection, stat.count) })

  // Build rows for lexicons without records yet
  let lexicon_rows =
    record_lexicons
    |> list.filter(fn(lexicon) {
      // Only show lexicons that don't already appear in collection_stats
      !list.any(collection_stats, fn(stat) { stat.collection == lexicon.id })
    })
    |> list.map(fn(lexicon) { render_empty_row(lexicon.id) })

  // Combine both types of rows
  list.append(record_rows, lexicon_rows)
}

/// Render a row for a collection with records
fn render_stat_row(collection: String, count: Int) -> Element(msg) {
  html.tr([attribute.class("hover:bg-gray-50 transition-colors")], [
    html.td([attribute.class("px-4 py-3 text-sm text-gray-900")], [
      element.text(collection),
    ]),
    html.td([attribute.class("px-4 py-3 text-sm text-gray-700")], [
      element.text(int.to_string(count)),
    ]),
  ])
}

/// Render a row for a lexicon without records yet
fn render_empty_row(collection: String) -> Element(msg) {
  html.tr([attribute.class("hover:bg-gray-50 transition-colors")], [
    html.td([attribute.class("px-4 py-3 text-sm text-gray-900")], [
      element.text(collection),
    ]),
    html.td([attribute.class("px-4 py-3 text-sm text-gray-500 italic")], [
      element.text("0"),
    ]),
  ])
}
