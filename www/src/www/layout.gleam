/// Docs site layout with sidebar navigation

import gleam/list
import lustre/attribute.{attribute, class, href, rel}
import lustre/element.{type Element}
import lustre/element/html.{
  a, aside, body, div, head, html, li, link, main, meta, nav, script, title, ul,
}
import www/config.{type DocPage}

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
      link([rel("stylesheet"), href("/styles.css")]),
    ]),
    body([], [
      div([class("container")], [
        sidebar(page.path, all_pages),
        main([class("content")], [
          element.unsafe_raw_html("", "div", [], content_html),
        ]),
      ]),
      script([attribute("src", "/highlight.js")], ""),
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
