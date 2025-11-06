import gleam/option.{type Option}
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
  let #(bg_class, border_class, text_class) = case kind {
    Success -> #("bg-green-900/30", "border-green-800", "text-green-300")
    Error -> #("bg-red-900/30", "border-red-800", "text-red-300")
    Info -> #("bg-blue-900/30", "border-blue-800", "text-blue-300")
    Warning -> #("bg-yellow-900/30", "border-yellow-800", "text-yellow-300")
  }

  html.div(
    [
      attribute.class(
        "mb-6 p-4 rounded border " <> bg_class <> " " <> border_class,
      ),
    ],
    [
      html.span([attribute.class("text-sm " <> text_class)], [
        element.text(message),
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
  let #(bg_class, border_class, text_class) = case kind {
    Success -> #("bg-green-900/30", "border-green-800", "text-green-300")
    Error -> #("bg-red-900/30", "border-red-800", "text-red-300")
    Info -> #("bg-blue-900/30", "border-blue-800", "text-blue-300")
    Warning -> #("bg-yellow-900/30", "border-yellow-800", "text-yellow-300")
  }

  html.div(
    [
      attribute.class(
        "mb-6 p-4 rounded border " <> bg_class <> " " <> border_class,
      ),
    ],
    [
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
    ],
  )
}

/// Helper to conditionally render alert based on optional kind and message
pub fn maybe_alert(
  kind: Option(String),
  message: Option(String),
) -> Element(msg) {
  case kind, message {
    option.Some(k), option.Some(m) -> {
      let alert_kind = case k {
        "success" -> Success
        "error" -> Error
        "warning" -> Warning
        _ -> Info
      }
      alert(alert_kind, m)
    }
    _, _ -> element.none()
  }
}
