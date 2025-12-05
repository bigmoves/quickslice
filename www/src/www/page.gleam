/// Renders a doc page by converting markdown to HTML and wrapping in layout

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

  layout.wrap(page, all_pages, html_content)
}
