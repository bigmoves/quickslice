import gleam/erlang/process
import gleam/float
import gleam/int
import gleam/list
import gleam/result
import jetstream_activity.{type ActivityBucket}
import lustre
import lustre/attribute
import lustre/effect
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/svg
import lustre/event
import sqlight
import stats_pubsub

// APP

pub fn component(db: sqlight.Connection) {
  lustre.application(init(db, _), update, view)
}

// MODEL

pub type TimeRange {
  OneHour
  ThreeHour
  SixHour
  OneDay
  SevenDay
}

pub type Model {
  Model(db: sqlight.Connection, range: TimeRange, data: List(ActivityBucket))
}

fn init(db: sqlight.Connection, _flags: Nil) -> #(Model, effect.Effect(Msg)) {
  // Default to 1 day
  let data = jetstream_activity.get_activity_1day(db) |> result.unwrap([])
  #(Model(db: db, range: OneDay, data: data), start_listening_in_background())
}

// UPDATE

pub opaque type Msg {
  ChangeRange(TimeRange)
  StatsEventReceived(stats_pubsub.StatsEvent)
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    ChangeRange(range) -> {
      let data = case range {
        OneHour -> jetstream_activity.get_activity_1hr(model.db)
        ThreeHour -> jetstream_activity.get_activity_3hr(model.db)
        SixHour -> jetstream_activity.get_activity_6hr(model.db)
        OneDay -> jetstream_activity.get_activity_1day(model.db)
        SevenDay -> jetstream_activity.get_activity_7day(model.db)
      }
      |> result.unwrap([])

      #(Model(..model, range: range, data: data), effect.none())
    }

    StatsEventReceived(event) -> {
      case event {
        // When activity is logged or records are created/deleted, refresh the chart data
        stats_pubsub.ActivityLogged(_, _, _, _, _, _, _, _)
        | stats_pubsub.RecordCreated
        | stats_pubsub.RecordDeleted -> {
          // Refresh data for current time range
          let data = case model.range {
            OneHour -> jetstream_activity.get_activity_1hr(model.db)
            ThreeHour -> jetstream_activity.get_activity_3hr(model.db)
            SixHour -> jetstream_activity.get_activity_6hr(model.db)
            OneDay -> jetstream_activity.get_activity_1day(model.db)
            SevenDay -> jetstream_activity.get_activity_7day(model.db)
          }
          |> result.unwrap([])

          #(Model(..model, data: data), effect.none())
        }
        // Ignore other events
        _ -> #(model, effect.none())
      }
    }
  }
}

// VIEW

/// Render static chart for server-side pre-rendering
pub fn render_static(data: List(ActivityBucket), range: TimeRange) -> Element(msg) {
  html.div([attribute.class("bg-zinc-800/50 rounded p-4 font-mono")], [
    render_time_range_buttons_static(range),
    render_chart(data, range),
  ])
}

fn view(model: Model) -> Element(Msg) {
  html.div([], [
    // Include Tailwind styles in the Shadow DOM
    element.element(
      "link",
      [
        attribute.attribute("rel", "stylesheet"),
        attribute.attribute("href", "/styles.css"),
      ],
      [],
    ),
    html.div([attribute.class("bg-zinc-800/50 rounded p-4 font-mono")], [
      render_time_range_buttons(model.range),
      render_chart(model.data, model.range),
    ]),
  ])
}

fn get_button_class(current_range: TimeRange, range: TimeRange) -> String {
  let base = "px-3 py-1 text-xs rounded transition-colors"
  case range == current_range {
    True -> base <> " bg-zinc-700 text-zinc-100"
    False -> base <> " bg-zinc-800/50 text-zinc-400 hover:bg-zinc-700/50 hover:text-zinc-300"
  }
}

fn render_time_range_buttons(current_range: TimeRange) -> Element(Msg) {
  let button = fn(range: TimeRange, label: String) {
    html.button(
      [
        attribute.class(get_button_class(current_range, range)),
        event.on_click(ChangeRange(range)),
      ],
      [element.text(label)],
    )
  }

  html.div([attribute.class("flex gap-2 mb-4")], [
    button(OneHour, "1hr"),
    button(ThreeHour, "3hr"),
    button(SixHour, "6hr"),
    button(OneDay, "1 day"),
    button(SevenDay, "7 day"),
  ])
}

fn render_time_range_buttons_static(current_range: TimeRange) -> Element(msg) {
  let button = fn(range: TimeRange, label: String) {
    html.button(
      [attribute.class(get_button_class(current_range, range))],
      [element.text(label)],
    )
  }

  html.div([attribute.class("flex gap-2 mb-4")], [
    button(OneHour, "1hr"),
    button(ThreeHour, "3hr"),
    button(SixHour, "6hr"),
    button(OneDay, "1 day"),
    button(SevenDay, "7 day"),
  ])
}

fn render_chart(data: List(ActivityBucket), range: TimeRange) -> Element(msg) {
  case data {
    [] -> {
      html.div([attribute.class("py-8 text-center text-zinc-600 text-xs")], [
        element.text("No activity data available"),
      ])
    }
    buckets -> {
      let max_value = calculate_max_value(buckets)
      let #(bar_width, gap) = case range {
        SevenDay -> #(160.0, 12.0)
        _ -> #(30.0, 4.0)
      }
      let num_buckets = list.length(buckets)
      // Width = (num_bars * bar_width) + ((num_bars - 1) * gap)
      let chart_width = int.to_float(num_buckets) *. bar_width +. int.to_float(num_buckets - 1) *. gap
      let chart_height = 120.0

      html.div([attribute.class("w-full")], [
        svg.svg(
          [
            attribute.attribute("viewBox", "0 0 " <> float.to_string(chart_width) <> " " <> float.to_string(chart_height)),
            attribute.attribute("width", "100%"),
            attribute.attribute("height", float.to_string(chart_height)),
            attribute.attribute("style", "min-height: 120px"),
            attribute.attribute("preserveAspectRatio", "none"),
          ],
          list.index_map(buckets, fn(bucket, index) {
            render_stacked_bar(bucket, index, bar_width, gap, chart_height, max_value)
          }),
        ),
      ])
    }
  }
}

fn calculate_max_value(buckets: List(ActivityBucket)) -> Int {
  buckets
  |> list.map(fn(b) { b.create_count + b.update_count + b.delete_count })
  |> list.reduce(int.max)
  |> result.unwrap(1)
}

fn render_stacked_bar(
  bucket: ActivityBucket,
  index: Int,
  bar_width: Float,
  gap: Float,
  chart_height: Float,
  max_value: Int,
) -> Element(msg) {
  let x = int.to_float(index) *. { bar_width +. gap }
  let total = bucket.create_count + bucket.update_count + bucket.delete_count

  case total {
    0 -> {
      // Render placeholder bar for empty bins
      let placeholder_height = 4.0
      let placeholder_y = chart_height -. placeholder_height
      svg.g(
        [
          attribute.class("group"),
          attribute.attribute("data-tooltip-timestamp", bucket.timestamp),
          attribute.attribute("data-create", "0"),
          attribute.attribute("data-update", "0"),
          attribute.attribute("data-delete", "0"),
        ],
        [
          svg.rect([
            attribute.attribute("x", float.to_string(x)),
            attribute.attribute("y", float.to_string(placeholder_y)),
            attribute.attribute("width", float.to_string(bar_width)),
            attribute.attribute("height", float.to_string(placeholder_height)),
            attribute.attribute("style", "fill: #3f3f46 !important; stroke: none; display: inline; cursor: pointer"),
            attribute.class("group-hover:fill-zinc-700"),
          ])
        ],
      )
    }
    _ -> {
      let scale = chart_height /. int.to_float(max_value)

      // Calculate heights for each segment
      let delete_height = int.to_float(bucket.delete_count) *. scale
      let update_height = int.to_float(bucket.update_count) *. scale
      let create_height = int.to_float(bucket.create_count) *. scale

      // Calculate y positions (bottom to top: delete, update, create)
      let delete_y = chart_height -. delete_height
      let update_y = delete_y -. update_height
      let create_y = update_y -. create_height

      svg.g(
        [
          attribute.class("group"),
          attribute.attribute("data-tooltip-timestamp", bucket.timestamp),
          attribute.attribute("data-create", int.to_string(bucket.create_count)),
          attribute.attribute("data-update", int.to_string(bucket.update_count)),
          attribute.attribute("data-delete", int.to_string(bucket.delete_count)),
        ],
        [
          // Delete segment (red) - bottom
          case bucket.delete_count > 0 {
            True ->
              svg.rect([
                attribute.attribute("x", float.to_string(x)),
                attribute.attribute("y", float.to_string(delete_y)),
                attribute.attribute("width", float.to_string(bar_width)),
                attribute.attribute("height", float.to_string(delete_height)),
                attribute.attribute("style", "fill: #ef4444 !important; stroke: none; display: inline; cursor: pointer; transition: opacity 0.2s"),
                attribute.class("group-hover:opacity-80"),
              ])
            False -> element.none()
          },
          // Update segment (blue) - middle
          case bucket.update_count > 0 {
            True ->
              svg.rect([
                attribute.attribute("x", float.to_string(x)),
                attribute.attribute("y", float.to_string(update_y)),
                attribute.attribute("width", float.to_string(bar_width)),
                attribute.attribute("height", float.to_string(update_height)),
                attribute.attribute("style", "fill: #60a5fa !important; stroke: none; display: inline; cursor: pointer; transition: opacity 0.2s"),
                attribute.class("group-hover:opacity-80"),
              ])
            False -> element.none()
          },
          // Create segment (green) - top
          case bucket.create_count > 0 {
            True ->
              svg.rect([
                attribute.attribute("x", float.to_string(x)),
                attribute.attribute("y", float.to_string(create_y)),
                attribute.attribute("width", float.to_string(bar_width)),
                attribute.attribute("height", float.to_string(create_height)),
                attribute.attribute("style", "fill: #22c55e !important; stroke: none; display: inline; cursor: pointer; transition: opacity 0.2s"),
                attribute.class("group-hover:opacity-80"),
              ])
            False -> element.none()
          },
        ],
      )
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

