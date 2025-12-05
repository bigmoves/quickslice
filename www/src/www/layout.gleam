/// Docs site layout with sidebar navigation
import gleam/list
import lustre/attribute.{attribute, class, href, id, rel}
import lustre/element.{type Element}
import lustre/element/html.{
  a, aside, body, button, div, head, html, li, link, main, meta, nav, script,
  span, title, ul,
}
import lustre/element/svg
import www/config.{type DocPage}

/// Heading extracted from page content
pub type Heading {
  Heading(level: Int, id: String, text: String)
}

/// Wrap content in the docs layout
pub fn wrap(
  page: DocPage,
  all_pages: List(DocPage),
  content_html: String,
  headings: List(Heading),
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
      menu_toggle(),
      div([class("sidebar-backdrop")], []),
      div([class("container")], [
        sidebar(page.path, all_pages),
        main([class("content")], [
          element.unsafe_raw_html("", "div", [], content_html),
          minimap(headings),
        ]),
      ]),
      script([attribute("src", "/highlight.js")], ""),
      script([attribute("src", "/mobile-nav.js")], ""),
      script([attribute("src", "/minimap.js")], ""),
    ]),
  ])
}

/// Render the minimap navigation
fn minimap(headings: List(Heading)) -> Element(Nil) {
  case headings {
    [] -> element.none()
    _ ->
      nav([class("minimap"), attribute("aria-label", "Page sections")], [
        div([class("minimap-header")], [html.text("On this page")]),
        ..list.map(headings, fn(h) {
          let classes = case h.level {
            3 -> "minimap-item minimap-item-sub"
            _ -> "minimap-item"
          }
          a(
            [
              href("#" <> h.id),
              class(classes),
              attribute("data-target-id", h.id),
            ],
            [html.text(h.text)],
          )
        })
      ])
  }
}

/// Hamburger menu button for mobile
fn menu_toggle() -> Element(Nil) {
  button([class("menu-toggle"), attribute("aria-label", "Toggle menu")], [
    svg.svg(
      [
        attribute("viewBox", "0 0 24 24"),
        attribute("fill", "none"),
        attribute("stroke", "currentColor"),
        attribute("stroke-width", "2"),
      ],
      [
        svg.line([
          attribute("x1", "3"),
          attribute("y1", "6"),
          attribute("x2", "21"),
          attribute("y2", "6"),
        ]),
        svg.line([
          attribute("x1", "3"),
          attribute("y1", "12"),
          attribute("x2", "21"),
          attribute("y2", "12"),
        ]),
        svg.line([
          attribute("x1", "3"),
          attribute("y1", "18"),
          attribute("x2", "21"),
          attribute("y2", "18"),
        ]),
      ],
    ),
  ])
}

fn sidebar(current_path: String, pages: List(DocPage)) -> Element(Nil) {
  aside([class("sidebar")], [
    div([class("sidebar-brand")], [
      logo(),
      span([class("sidebar-title")], [html.text("quickslice")]),
    ]),
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

/// Render the quickslice logo SVG
fn logo() -> Element(Nil) {
  svg.svg(
    [
      attribute("viewBox", "0 0 128 128"),
      attribute("xmlns", "http://www.w3.org/2000/svg"),
      class("sidebar-logo"),
    ],
    [
      // Define gradients
      svg.defs([], [
        svg.linear_gradient(
          [
            id("board1"),
            attribute("x1", "0%"),
            attribute("y1", "0%"),
            attribute("x2", "100%"),
            attribute("y2", "100%"),
          ],
          [
            svg.stop([
              attribute("offset", "0%"),
              attribute("stop-color", "#FF6347"),
              attribute("stop-opacity", "1"),
            ]),
            svg.stop([
              attribute("offset", "100%"),
              attribute("stop-color", "#FF4500"),
              attribute("stop-opacity", "1"),
            ]),
          ],
        ),
        svg.linear_gradient(
          [
            id("board2"),
            attribute("x1", "0%"),
            attribute("y1", "0%"),
            attribute("x2", "100%"),
            attribute("y2", "100%"),
          ],
          [
            svg.stop([
              attribute("offset", "0%"),
              attribute("stop-color", "#00CED1"),
              attribute("stop-opacity", "1"),
            ]),
            svg.stop([
              attribute("offset", "100%"),
              attribute("stop-color", "#4682B4"),
              attribute("stop-opacity", "1"),
            ]),
          ],
        ),
      ]),
      // Surfboard/skateboard deck shapes stacked
      svg.g([attribute("transform", "translate(64, 64)")], [
        // Top board slice
        svg.ellipse([
          attribute("cx", "0"),
          attribute("cy", "-28"),
          attribute("rx", "50"),
          attribute("ry", "20"),
          attribute("fill", "url(#board1)"),
        ]),
        // Middle board slice
        svg.ellipse([
          attribute("cx", "0"),
          attribute("cy", "0"),
          attribute("rx", "60"),
          attribute("ry", "20"),
          attribute("fill", "url(#board2)"),
        ]),
        // Bottom board slice
        svg.ellipse([
          attribute("cx", "0"),
          attribute("cy", "28"),
          attribute("rx", "40"),
          attribute("ry", "20"),
          attribute("fill", "#32CD32"),
        ]),
      ]),
    ],
  )
}
