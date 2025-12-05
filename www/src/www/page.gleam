/// Renders a doc page by converting markdown to HTML and wrapping in layout
import gleam/option.{Some}
import gleam/regexp
import lustre/element.{type Element}
import mork
import www/config.{type DocPage}
import www/layout

/// Render a doc page to a full HTML element
pub fn render(page: DocPage, all_pages: List(DocPage)) -> Element(Nil) {
  let html_content =
    mork.configure()
    |> mork.tables(True)
    |> mork.heading_ids(True)
    |> mork.parse_with_options(page.content)
    |> mork.to_html
    |> transform_links

  layout.wrap(page, all_pages, html_content)
}

/// Transform ./filename.md links to /filename paths
fn transform_links(html: String) -> String {
  // Match href="./something.md" and replace with href="/something"
  let assert Ok(re) = regexp.from_string("href=\"\\./([^\"]+)\\.md\"")
  regexp.match_map(re, html, fn(m) {
    case m.submatches {
      [Some(filename)] ->
        case filename {
          "README" -> "href=\"/\""
          _ -> "href=\"/" <> filename <> "\""
        }
      _ -> m.content
    }
  })
}
