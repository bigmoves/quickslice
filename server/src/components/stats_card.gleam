import gleam/int
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

/// Renders a statistics card with a count and description
pub fn card(
  count count: Int,
  description description: String,
  color color: String,
) -> Element(msg) {
  let bg_class = "bg-" <> color <> "-900/20"
  let border_class = "border-" <> color <> "-800"
  let text_class = "text-" <> color <> "-400"

  html.div(
    [
      attribute.class(
        "bg-zinc-900 "
        <> bg_class
        <> " rounded-lg p-6 border "
        <> border_class
        <> " shadow-sm",
      ),
    ],
    [
      html.div(
        [attribute.class("text-4xl font-bold " <> text_class <> " mb-2")],
        [
          element.text(int.to_string(count)),
        ],
      ),
      html.div([attribute.class("text-zinc-400")], [element.text(description)]),
    ],
  )
}
