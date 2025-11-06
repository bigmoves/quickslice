import database
import gleam/float
import gleam/int
import gleam/list
import gleam/string
import lustre/attribute
import lustre/element.{type Element}

/// Renders a sparkline chart from activity data points
pub fn view(activity: List(database.ActivityPoint)) -> Element(msg) {
  case activity {
    [] -> element.none()
    _ -> render_chart(activity)
  }
}

fn render_chart(data: List(database.ActivityPoint)) -> Element(msg) {
  let width = 800
  let height = 80

  // Calculate min/max for scaling
  let counts = list.map(data, fn(point) { point.count })
  let max = case list.reduce(counts, int.max) {
    Ok(m) -> int.max(m, 1)
    Error(_) -> 1
  }
  let min = case list.reduce(counts, int.min) {
    Ok(m) -> int.min(m, 0)
    Error(_) -> 0
  }

  let range = max - min
  let range_float = case range {
    0 -> 1.0
    _ -> int.to_float(range)
  }
  let max_height = int.to_float(height) *. 0.75

  // Generate polyline points
  let data_length = list.length(data)
  let points_string =
    data
    |> list.index_map(fn(point, index) {
      let x = case data_length {
        1 -> int.to_float(width) /. 2.0
        _ -> int.to_float(index) /. int.to_float(data_length - 1) *. int.to_float(width)
      }
      let y_normalized = int.to_float(point.count - min) /. range_float
      let y = int.to_float(height) -. y_normalized *. max_height

      float.to_string(x) <> "," <> float.to_string(y)
    })
    |> string.join(" ")

  // Generate area path for gradient fill
  let area_path =
    "M 0," <> int.to_string(height)
    <> " L " <> points_string
    <> " L " <> int.to_string(width) <> "," <> int.to_string(height)
    <> " Z"

  // Create SVG element
  element.element(
    "svg",
    [
      attribute.attribute("width", int.to_string(width)),
      attribute.attribute("height", int.to_string(height)),
      attribute.class("w-full"),
      attribute.attribute("viewBox", "0 0 " <> int.to_string(width) <> " " <> int.to_string(height)),
      attribute.attribute("preserveAspectRatio", "none"),
    ],
    [
      // Define gradient
      element.element(
        "defs",
        [],
        [
          element.element(
            "linearGradient",
            [
              attribute.id("sparklineGradient"),
              attribute.attribute("x1", "0%"),
              attribute.attribute("y1", "0%"),
              attribute.attribute("x2", "0%"),
              attribute.attribute("y2", "100%"),
            ],
            [
              element.element(
                "stop",
                [
                  attribute.attribute("offset", "0%"),
                  attribute.attribute("stop-color", "#22d3ee"),
                  attribute.attribute("stop-opacity", "0.5"),
                ],
                [],
              ),
              element.element(
                "stop",
                [
                  attribute.attribute("offset", "100%"),
                  attribute.attribute("stop-color", "#22d3ee"),
                  attribute.attribute("stop-opacity", "0.1"),
                ],
                [],
              ),
            ],
          ),
        ],
      ),
      // Area fill
      element.element(
        "path",
        [
          attribute.attribute("d", area_path),
          attribute.attribute("fill", "url(#sparklineGradient)"),
          attribute.attribute("stroke-width", "0"),
        ],
        [],
      ),
      // Line
      element.element(
        "polyline",
        [
          attribute.attribute("points", points_string),
          attribute.attribute("fill", "none"),
          attribute.attribute("stroke", "#22d3ee"),
          attribute.attribute("stroke-width", "2"),
          attribute.attribute("stroke-linecap", "round"),
          attribute.attribute("stroke-linejoin", "round"),
        ],
        [],
      ),
    ],
  )
}
