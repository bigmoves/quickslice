import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

pub type AlertKind {
  Success
  Error
  Info
  Warning
}

/// Render an alert message with appropriate styling
pub fn alert(kind: AlertKind, message: String) -> Element(msg) {
  let #(bg_class, border_class, text_class, icon) = case kind {
    Success -> #("bg-green-900/30", "border-green-800", "text-green-300", "✓")
    Error -> #("bg-red-900/30", "border-red-800", "text-red-300", "✗")
    Info -> #("bg-blue-900/30", "border-blue-800", "text-blue-300", "ℹ")
    Warning -> #(
      "bg-yellow-900/30",
      "border-yellow-800",
      "text-yellow-300",
      "⚠",
    )
  }

  html.div(
    [
      attribute.class(
        "mb-6 p-4 rounded border " <> bg_class <> " " <> border_class,
      ),
    ],
    [
      html.div([attribute.class("flex items-center gap-3")], [
        html.span([attribute.class("text-lg " <> text_class)], [
          element.text(icon),
        ]),
        html.span([attribute.class("text-sm " <> text_class)], [
          element.text(message),
        ]),
      ]),
    ],
  )
}

/// Render an alert message with a link
pub fn alert_with_link(
  kind: AlertKind,
  message: String,
  link_text: String,
  link_url: String,
) -> Element(msg) {
  let #(bg_class, border_class, text_class, icon) = case kind {
    Success -> #("bg-green-900/30", "border-green-800", "text-green-300", "✓")
    Error -> #("bg-red-900/30", "border-red-800", "text-red-300", "✗")
    Info -> #("bg-blue-900/30", "border-blue-800", "text-blue-300", "ℹ")
    Warning -> #(
      "bg-yellow-900/30",
      "border-yellow-800",
      "text-yellow-300",
      "⚠",
    )
  }

  html.div(
    [
      attribute.class(
        "mb-6 p-4 rounded border " <> bg_class <> " " <> border_class,
      ),
    ],
    [
      html.div([attribute.class("flex items-center gap-3")], [
        html.span([attribute.class("text-lg " <> text_class)], [
          element.text(icon),
        ]),
        html.span([attribute.class("text-sm " <> text_class)], [
          element.text(message <> " "),
          html.a(
            [
              attribute.href(link_url),
              attribute.class("underline hover:no-underline"),
            ],
            [element.text(link_text)],
          ),
        ]),
      ]),
    ],
  )
}
