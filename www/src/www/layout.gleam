/// Docs site layout with sidebar navigation
import gleam/list
import gleam/option.{type Option, None, Some}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/svg
import www/config.{type DocPage}
import www/logo

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
  html.html([], [
    html.head([], [
      html.meta([attribute.attribute("charset", "UTF-8")]),
      html.meta([
        attribute.attribute("name", "viewport"),
        attribute.attribute("content", "width=device-width, initial-scale=1.0"),
      ]),
      html.title([], "quickslice - " <> page.title),
      html.meta([
        attribute.attribute("property", "og:title"),
        attribute.attribute("content", "quickslice - " <> page.title),
      ]),
      html.meta([
        attribute.attribute("property", "og:image"),
        attribute.attribute("content", config.og_image_url(page)),
      ]),
      html.meta([
        attribute.attribute("property", "og:type"),
        attribute.attribute("content", "website"),
      ]),
      html.meta([
        attribute.attribute("name", "twitter:card"),
        attribute.attribute("content", "summary_large_image"),
      ]),
      html.link([attribute.rel("stylesheet"), attribute.href("/styles.css")]),
      html.script(
        [
          attribute.attribute(
            "src",
            "https://cdn.jsdelivr.net/gh/bigmoves/elements@main/dist/elements.min.js",
          ),
        ],
        "",
      ),
    ]),
    html.body([], [
      mobile_header(),
      html.div([attribute.class("sidebar-backdrop")], []),
      html.div([attribute.class("container")], [
        sidebar(page.path, all_pages),
        html.main([attribute.class("content")], [
          html.div([], [
            element.unsafe_raw_html("", "div", [], content_html),
            page_nav(page, all_pages),
          ]),
          minimap(headings),
        ]),
      ]),
      html.script([attribute.attribute("src", "/mobile-nav.js")], ""),
      html.script([attribute.attribute("src", "/minimap.js")], ""),
      html.script([attribute.attribute("src", "/fuse.min.js")], ""),
      html.script([attribute.attribute("src", "/search.js")], ""),
    ]),
  ])
}

/// Render the minimap navigation
fn minimap(headings: List(Heading)) -> Element(Nil) {
  case headings {
    [] -> element.none()
    _ ->
      html.nav(
        [
          attribute.class("minimap"),
          attribute.attribute("aria-label", "Page sections"),
        ],
        [
          html.div([attribute.class("minimap-header")], [
            html.text("On this page"),
          ]),
          ..list.map(headings, fn(h) {
            let classes = case h.level {
              3 -> "minimap-item minimap-item-sub"
              _ -> "minimap-item"
            }
            html.a(
              [
                attribute.href("#" <> h.id),
                attribute.class(classes),
                attribute.attribute("data-target-id", h.id),
              ],
              [html.text(h.text)],
            )
          })
        ],
      )
  }
}

/// Render previous/next page navigation
fn page_nav(current: DocPage, all_pages: List(DocPage)) -> Element(Nil) {
  let #(prev, next) = find_adjacent_pages(current, all_pages)

  html.nav([attribute.class("page-nav")], [
    case prev {
      Some(p) ->
        html.a(
          [
            attribute.href(p.path),
            attribute.class("page-nav-link page-nav-prev"),
          ],
          [
            html.span([attribute.class("page-nav-label")], [
              html.text("Previous"),
            ]),
            html.span([attribute.class("page-nav-title")], [html.text(p.title)]),
          ],
        )
      None -> html.div([attribute.class("page-nav-link page-nav-empty")], [])
    },
    case next {
      Some(p) ->
        html.a(
          [
            attribute.href(p.path),
            attribute.class("page-nav-link page-nav-next"),
          ],
          [
            html.span([attribute.class("page-nav-label")], [html.text("Next")]),
            html.span([attribute.class("page-nav-title")], [html.text(p.title)]),
          ],
        )
      None -> html.div([attribute.class("page-nav-link page-nav-empty")], [])
    },
  ])
}

/// Find the previous and next pages relative to current
fn find_adjacent_pages(
  current: DocPage,
  all_pages: List(DocPage),
) -> #(Option(DocPage), Option(DocPage)) {
  find_adjacent_helper(current, all_pages, None)
}

fn find_adjacent_helper(
  current: DocPage,
  remaining: List(DocPage),
  prev: Option(DocPage),
) -> #(Option(DocPage), Option(DocPage)) {
  case remaining {
    [] -> #(None, None)
    [page] ->
      case page.path == current.path {
        True -> #(prev, None)
        False -> #(None, None)
      }
    [page, next, ..rest] ->
      case page.path == current.path {
        True -> #(prev, Some(next))
        False -> find_adjacent_helper(current, [next, ..rest], Some(page))
      }
  }
}

/// Mobile header with brand and menu toggle (visible only on mobile)
fn mobile_header() -> Element(Nil) {
  html.header([attribute.class("mobile-header")], [
    html.div([attribute.class("mobile-header-brand")], [
      logo.logo(),
      html.span([attribute.class("sidebar-title")], [html.text("quickslice")]),
      html.span([attribute.class("sidebar-version")], [html.text(version)]),
    ]),
    html.button(
      [
        attribute.class("menu-toggle"),
        attribute.attribute("aria-label", "Toggle menu"),
      ],
      [
        svg.svg(
          [
            attribute.attribute("viewBox", "0 0 24 24"),
            attribute.attribute("fill", "none"),
            attribute.attribute("stroke", "currentColor"),
            attribute.attribute("stroke-width", "2"),
          ],
          [
            svg.line([
              attribute.attribute("x1", "3"),
              attribute.attribute("y1", "6"),
              attribute.attribute("x2", "21"),
              attribute.attribute("y2", "6"),
            ]),
            svg.line([
              attribute.attribute("x1", "3"),
              attribute.attribute("y1", "12"),
              attribute.attribute("x2", "21"),
              attribute.attribute("y2", "12"),
            ]),
            svg.line([
              attribute.attribute("x1", "3"),
              attribute.attribute("y1", "18"),
              attribute.attribute("x2", "21"),
              attribute.attribute("y2", "18"),
            ]),
          ],
        ),
      ],
    ),
  ])
}

const version = "v0.20.0"

fn sidebar(current_path: String, pages: List(DocPage)) -> Element(Nil) {
  html.aside([attribute.class("sidebar")], [
    html.div([attribute.class("sidebar-brand")], [
      logo.logo(),
      html.span([attribute.class("sidebar-title")], [html.text("quickslice")]),
      html.span([attribute.class("sidebar-version")], [html.text(version)]),
    ]),
    html.div([attribute.class("search-container")], [
      html.input([
        attribute.attribute("type", "text"),
        attribute.attribute("placeholder", "Search docs..."),
        attribute.attribute("id", "search-input"),
        attribute.class("search-input"),
      ]),
      html.div(
        [
          attribute.attribute("id", "search-results"),
          attribute.class("search-results"),
        ],
        [],
      ),
    ]),
    element.unsafe_raw_html(
      "",
      "qs-tangled-stars",
      [
        attribute.attribute("handle", "slices.network"),
        attribute.attribute("repo", "quickslice"),
        attribute.attribute(
          "instance",
          "https://quickslice-production-ddc3.up.railway.app",
        ),
      ],
      "",
    ),
    html.nav([], render_grouped_nav(current_path, pages)),
  ])
}

/// Group pages by their group field and render navigation
fn render_grouped_nav(
  current_path: String,
  pages: List(DocPage),
) -> List(Element(Nil)) {
  pages
  |> group_by_group
  |> list.map(fn(group) {
    let #(group_name, group_pages) = group
    case group_name {
      "" ->
        // Standalone pages - render without group header
        html.div(
          [attribute.class("sidebar-standalone")],
          list.map(group_pages, fn(p) {
            let is_active = p.path == current_path
            let classes = case is_active {
              True -> "active"
              False -> ""
            }
            html.a([attribute.href(p.path), attribute.class(classes)], [
              html.text(p.title),
            ])
          }),
        )
      _ ->
        // Grouped pages - render with header
        html.div([attribute.class("sidebar-group")], [
          html.div([attribute.class("sidebar-group-label")], [
            html.text(group_name),
          ]),
          html.ul(
            [],
            list.map(group_pages, fn(p) {
              let is_active = p.path == current_path
              let classes = case is_active {
                True -> "active"
                False -> ""
              }
              html.li([], [
                html.a([attribute.href(p.path), attribute.class(classes)], [
                  html.text(p.title),
                ]),
              ])
            }),
          ),
        ])
    }
  })
}

/// Group pages by their group field, preserving order
fn group_by_group(pages: List(DocPage)) -> List(#(String, List(DocPage))) {
  list.fold(pages, [], fn(acc, page) {
    case list.key_pop(acc, page.group) {
      Ok(#(existing, rest)) -> [
        #(page.group, list.append(existing, [page])),
        ..rest
      ]
      Error(Nil) -> [#(page.group, [page]), ..acc]
    }
  })
  |> list.reverse
}
