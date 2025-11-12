/// Activity Chart Component
///
/// Displays stacked bar chart of Jetstream activity with time range selection
///
/// ```graphql
/// query GetActivityBuckets($range: TimeRange!) {
///   activityBuckets(range: $range) {
///     timestamp
///     total
///     creates
///     updates
///     deletes
///   }
/// }
/// ```
import generated/queries/get_activity_buckets.{
  type TimeRange, ONEDAY, ONEHOUR, SEVENDAYS, SIXHOURS, THREEHOURS,
}
import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/result
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/svg
import lustre/event
import squall_cache.{type Cache}

pub fn view(
  cache: Cache,
  range: TimeRange,
  on_range_change: fn(TimeRange) -> msg,
) -> Element(msg) {
  html.div([attribute.class("bg-zinc-800/50 rounded p-4 font-mono mb-8")], [
    render_time_range_buttons(range, on_range_change),
    render_chart(cache, range),
  ])
}

fn render_time_range_buttons(
  current_range: TimeRange,
  on_range_change: fn(TimeRange) -> msg,
) -> Element(msg) {
  let get_button_class = fn(range: TimeRange) {
    let base = "px-3 py-1 text-xs rounded transition-colors cursor-pointer"
    case range == current_range {
      True -> base <> " bg-zinc-700 text-zinc-100"
      False ->
        base
        <> " bg-zinc-800/50 text-zinc-400 hover:bg-zinc-700/50 hover:text-zinc-300"
    }
  }

  html.div([attribute.class("flex gap-2 mb-4")], [
    html.button(
      [
        attribute.class(get_button_class(ONEHOUR)),
        event.on_click(on_range_change(ONEHOUR)),
      ],
      [element.text("1hr")],
    ),
    html.button(
      [
        attribute.class(get_button_class(THREEHOURS)),
        event.on_click(on_range_change(THREEHOURS)),
      ],
      [element.text("3hr")],
    ),
    html.button(
      [
        attribute.class(get_button_class(SIXHOURS)),
        event.on_click(on_range_change(SIXHOURS)),
      ],
      [element.text("6hr")],
    ),
    html.button(
      [
        attribute.class(get_button_class(ONEDAY)),
        event.on_click(on_range_change(ONEDAY)),
      ],
      [element.text("1 day")],
    ),
    html.button(
      [
        attribute.class(get_button_class(SEVENDAYS)),
        event.on_click(on_range_change(SEVENDAYS)),
      ],
      [element.text("7 day")],
    ),
  ])
}

fn render_chart(cache: Cache, range: TimeRange) -> Element(msg) {
  let variables =
    json.object([
      #("range", json.string(get_activity_buckets.time_range_to_string(range))),
    ])

  let #(_cache, result) =
    squall_cache.lookup(
      cache,
      "GetActivityBuckets",
      variables,
      get_activity_buckets.parse_get_activity_buckets_response,
    )

  case result {
    squall_cache.Loading ->
      html.div([attribute.class("py-8 text-center text-zinc-600 text-xs")], [
        element.text("Loading activity data..."),
      ])

    squall_cache.Failed(msg) ->
      html.div([attribute.class("py-8 text-center text-red-400 text-xs")], [
        element.text("Error: " <> msg),
      ])

    squall_cache.Data(data) -> {
      case data.activity_buckets {
        [] ->
          html.div([attribute.class("py-8 text-center text-zinc-600 text-xs")], [
            element.text("No activity data available"),
          ])
        buckets -> render_stacked_bar_chart(buckets, range)
      }
    }
  }
}

fn render_stacked_bar_chart(
  buckets: List(get_activity_buckets.ActivityBucket),
  range: TimeRange,
) -> Element(msg) {
  let max_value = calculate_max_value(buckets)
  let #(bar_width, gap) = case range {
    SEVENDAYS -> #(160.0, 12.0)
    _ -> #(30.0, 4.0)
  }
  let num_buckets = list.length(buckets)
  let chart_width =
    int.to_float(num_buckets)
    *. bar_width
    +. int.to_float(num_buckets - 1)
    *. gap
  let chart_height = 120.0

  html.div([attribute.class("w-full")], [
    svg.svg(
      [
        attribute.attribute(
          "viewBox",
          "0 0 "
            <> float.to_string(chart_width)
            <> " "
            <> float.to_string(chart_height),
        ),
        attribute.attribute("width", "100%"),
        attribute.attribute("height", float.to_string(chart_height)),
        attribute.attribute("style", "min-height: 120px"),
        attribute.attribute("preserveAspectRatio", "none"),
      ],
      list.index_map(buckets, fn(bucket, index) {
        render_stacked_bar(
          bucket,
          index,
          bar_width,
          gap,
          chart_height,
          max_value,
        )
      }),
    ),
  ])
}

fn calculate_max_value(
  buckets: List(get_activity_buckets.ActivityBucket),
) -> Int {
  buckets
  |> list.map(fn(b) { b.creates + b.updates + b.deletes })
  |> list.reduce(int.max)
  |> result.unwrap(1)
}

fn render_stacked_bar(
  bucket: get_activity_buckets.ActivityBucket,
  index: Int,
  bar_width: Float,
  gap: Float,
  chart_height: Float,
  max_value: Int,
) -> Element(msg) {
  let x = int.to_float(index) *. { bar_width +. gap }
  let total = bucket.creates + bucket.updates + bucket.deletes

  case total {
    0 -> {
      // Render placeholder bar for empty bins
      let placeholder_height = 4.0
      let placeholder_y = chart_height -. placeholder_height
      svg.g([], [
        svg.rect([
          attribute.attribute("x", float.to_string(x)),
          attribute.attribute("y", float.to_string(placeholder_y)),
          attribute.attribute("width", float.to_string(bar_width)),
          attribute.attribute("height", float.to_string(placeholder_height)),
          attribute.attribute(
            "style",
            "fill: #3f3f46 !important; stroke: none; display: inline",
          ),
        ]),
      ])
    }
    _ -> {
      let scale = chart_height /. int.to_float(max_value)

      // Calculate heights for each segment
      let delete_height = int.to_float(bucket.deletes) *. scale
      let update_height = int.to_float(bucket.updates) *. scale
      let create_height = int.to_float(bucket.creates) *. scale

      // Calculate y positions (bottom to top: delete, update, create)
      let delete_y = chart_height -. delete_height
      let update_y = delete_y -. update_height
      let create_y = update_y -. create_height

      svg.g([attribute.class("group")], [
        // Delete segment (red) - bottom
        case bucket.deletes > 0 {
          True ->
            svg.rect([
              attribute.attribute("x", float.to_string(x)),
              attribute.attribute("y", float.to_string(delete_y)),
              attribute.attribute("width", float.to_string(bar_width)),
              attribute.attribute("height", float.to_string(delete_height)),
              attribute.attribute(
                "style",
                "fill: #ef4444 !important; stroke: none; display: inline; cursor: pointer; transition: opacity 0.2s",
              ),
              attribute.class("group-hover:opacity-80"),
            ])
          False -> element.none()
        },
        // Update segment (blue) - middle
        case bucket.updates > 0 {
          True ->
            svg.rect([
              attribute.attribute("x", float.to_string(x)),
              attribute.attribute("y", float.to_string(update_y)),
              attribute.attribute("width", float.to_string(bar_width)),
              attribute.attribute("height", float.to_string(update_height)),
              attribute.attribute(
                "style",
                "fill: #60a5fa !important; stroke: none; display: inline; cursor: pointer; transition: opacity 0.2s",
              ),
              attribute.class("group-hover:opacity-80"),
            ])
          False -> element.none()
        },
        // Create segment (green) - top
        case bucket.creates > 0 {
          True ->
            svg.rect([
              attribute.attribute("x", float.to_string(x)),
              attribute.attribute("y", float.to_string(create_y)),
              attribute.attribute("width", float.to_string(bar_width)),
              attribute.attribute("height", float.to_string(create_height)),
              attribute.attribute(
                "style",
                "fill: #22c55e !important; stroke: none; display: inline; cursor: pointer; transition: opacity 0.2s",
              ),
              attribute.class("group-hover:opacity-80"),
            ])
          False -> element.none()
        },
      ])
    }
  }
}
