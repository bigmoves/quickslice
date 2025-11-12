import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

/// Standard button styling used throughout the app
const button_classes = "font-mono px-4 py-2 text-sm text-zinc-300 bg-zinc-800 hover:bg-zinc-700 rounded transition-colors cursor-pointer disabled:opacity-50 disabled:cursor-not-allowed disabled:hover:bg-zinc-800"

/// Render a button with standard styling
pub fn button(
  disabled disabled: Bool,
  on_click on_click: msg,
  text text: String,
) -> Element(msg) {
  html.button(
    [
      attribute.type_("button"),
      attribute.class(button_classes),
      attribute.disabled(disabled),
      event.on_click(on_click),
    ],
    [html.text(text)],
  )
}

/// Render a link styled as a button (for SPA routes)
pub fn link(href href: String, text text: String) -> Element(msg) {
  html.a([attribute.href(href), attribute.class(button_classes)], [
    html.text(text),
  ])
}

/// Render an external link styled as a button (opens in current tab, navigates away from SPA)
pub fn external_link(href href: String, text text: String) -> Element(msg) {
  html.a(
    [
      attribute.href(href),
      attribute.class(button_classes),
      attribute.attribute("rel", "noopener noreferrer"),
    ],
    [html.text(text)],
  )
}
