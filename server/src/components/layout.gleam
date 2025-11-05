import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

/// Renders a complete HTML page with the given title and content
pub fn page(title title: String, content content: List(Element(msg))) -> Element(msg) {
  html.html([attribute.class("h-full")], [
    head(title),
    body(content),
  ])
}

/// Renders the HTML head with meta tags and styles
fn head(title: String) -> Element(msg) {
  html.head([], [
    html.title([], title),
    element.element("meta", [attribute.attribute("charset", "UTF-8")], []),
    element.element(
      "meta",
      [
        attribute.attribute("name", "viewport"),
        attribute.attribute("content", "width=device-width, initial-scale=1.0"),
      ],
      [],
    ),
    element.element(
      "script",
      [attribute.attribute("src", "https://cdn.tailwindcss.com")],
      [],
    ),
  ])
}

/// Renders the HTML body with a max-width container
fn body(content: List(Element(msg))) -> Element(msg) {
  html.body([attribute.class("bg-gray-50 min-h-screen p-8")], [
    html.div([attribute.class("max-w-4xl mx-auto")], content),
  ])
}
