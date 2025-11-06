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
    // Lustre server component runtime
    html.script(
      [
        attribute.type_("module"),
        attribute.attribute("src", "/lustre/runtime.mjs"),
      ],
      "",
    ),
    // Listen for backfill-complete event and reload page
    html.script(
      [],
      "
      // Wait for DOM to be ready
      document.addEventListener('DOMContentLoaded', function() {
        const backfillButton = document.querySelector('lustre-server-component#backfill-button');
        if (backfillButton) {
          backfillButton.addEventListener('backfill-complete', function() {
            // Reload page to show updated database stats
            window.location.reload();
          });
        }
      });
      ",
    ),
  ])
}

/// Renders the HTML body with a max-width container
fn body(content: List(Element(msg))) -> Element(msg) {
  html.body([attribute.class("bg-zinc-950 text-zinc-300 font-mono min-h-screen")], [
    html.div([attribute.class("max-w-4xl mx-auto px-6 py-12")], content),
  ])
}
