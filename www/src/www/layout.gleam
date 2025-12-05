/// Docs site layout with sidebar navigation

import gleam/list
import lustre/attribute.{attribute, class, href}
import lustre/element.{type Element}
import lustre/element/html.{
  a, aside, body, div, head, html, li, main, meta, nav, style, title, ul,
}
import www/config.{type DocPage}

/// Minimal CSS for the docs layout
const css: String = "
* { box-sizing: border-box; margin: 0; padding: 0; }
body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, sans-serif;
  line-height: 1.6;
  color: #1a1a1a;
}
.container { display: flex; min-height: 100vh; }
.sidebar {
  width: 260px;
  padding: 2rem 1rem;
  background: #f5f5f5;
  border-right: 1px solid #e0e0e0;
  position: fixed;
  height: 100vh;
  overflow-y: auto;
}
.sidebar h1 { font-size: 1.25rem; margin-bottom: 1.5rem; padding: 0 0.5rem; }
.sidebar ul { list-style: none; }
.sidebar li { margin: 0.25rem 0; }
.sidebar a {
  display: block;
  padding: 0.5rem;
  color: #444;
  text-decoration: none;
  border-radius: 4px;
}
.sidebar a:hover { background: #e8e8e8; }
.sidebar a.active { background: #007acc; color: white; }
.content {
  flex: 1;
  margin-left: 260px;
  padding: 2rem 3rem;
  max-width: 900px;
}
.content h1 { font-size: 2rem; margin-bottom: 1rem; border-bottom: 1px solid #eee; padding-bottom: 0.5rem; }
.content h2 { font-size: 1.5rem; margin: 2rem 0 1rem; }
.content h3 { font-size: 1.25rem; margin: 1.5rem 0 0.75rem; }
.content p { margin: 1rem 0; }
.content pre {
  background: #f5f5f5;
  padding: 1rem;
  overflow-x: auto;
  border-radius: 4px;
  margin: 1rem 0;
}
.content code {
  background: #f0f0f0;
  padding: 0.2em 0.4em;
  border-radius: 3px;
  font-size: 0.9em;
}
.content pre code { background: none; padding: 0; }
.content ul, .content ol { margin: 1rem 0; padding-left: 2rem; }
.content li { margin: 0.5rem 0; }
.content blockquote {
  border-left: 4px solid #007acc;
  margin: 1rem 0;
  padding: 0.5rem 1rem;
  background: #f8f8f8;
}
.content a { color: #007acc; }
.content table { border-collapse: collapse; margin: 1rem 0; width: 100%; }
.content th, .content td { border: 1px solid #ddd; padding: 0.5rem; text-align: left; }
.content th { background: #f5f5f5; }
"

/// Wrap content in the docs layout
pub fn wrap(
  page: DocPage,
  all_pages: List(DocPage),
  content_html: String,
) -> Element(Nil) {
  html([], [
    head([], [
      meta([attribute("charset", "UTF-8")]),
      meta([
        attribute("name", "viewport"),
        attribute("content", "width=device-width, initial-scale=1.0"),
      ]),
      title([], "quickslice - " <> page.title),
      style([], css),
    ]),
    body([], [
      div([class("container")], [
        sidebar(page.path, all_pages),
        main([class("content")], [
          element.unsafe_raw_html("", "div", [], content_html),
        ]),
      ]),
    ]),
  ])
}

fn sidebar(current_path: String, pages: List(DocPage)) -> Element(Nil) {
  aside([class("sidebar")], [
    html.h1([], [html.text("quickslice")]),
    nav([], [
      ul(
        [],
        list.map(pages, fn(p) {
          let is_active = p.path == current_path
          let classes = case is_active {
            True -> "active"
            False -> ""
          }
          li([], [a([href(p.path), class(classes)], [html.text(p.title)])])
        }),
      ),
    ]),
  ])
}
