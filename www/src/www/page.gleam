/// Renders a doc page by converting markdown to HTML and wrapping in layout
import gleam/list
import gleam/option.{None, Some}
import gleam/regexp
import gleam/string
import lustre/element.{type Element}
import mork
import www/config.{type DocPage}
import www/highlighter
import www/layout.{type Heading}

/// Render a doc page to a full HTML element
pub fn render(page: DocPage, all_pages: List(DocPage)) -> Element(Nil) {
  let html_before_anchors =
    mork.configure()
    |> mork.tables(True)
    |> mork.heading_ids(True)
    |> mork.parse_with_options(page.content)
    |> mork.to_html
    |> transform_links

  // Extract headings BEFORE adding anchor links (regex expects clean headings)
  // Skip minimap for changelog (has duplicate heading IDs like "Added", "Fixed")
  let headings = case page.slug {
    "changelog" -> []
    _ -> extract_headings(html_before_anchors)
  }

  let html_content =
    html_before_anchors
    |> add_header_anchors
    |> highlighter.highlight_html

  layout.wrap(page, all_pages, html_content, headings)
}

/// Extract h2 and h3 headings with their IDs from HTML
fn extract_headings(html: String) -> List(Heading) {
  let assert Ok(re) =
    regexp.from_string("<h([23]) id=\"([^\"]+)\">([^<]+)</h[23]>")

  regexp.scan(re, html)
  |> list.map(fn(match) {
    case match.submatches {
      [Some(level), Some(id), Some(text)] ->
        Some(layout.Heading(
          level: case level {
            "2" -> 2
            _ -> 3
          },
          id: id,
          text: decode_html_entities(text),
        ))
      _ -> None
    }
  })
  |> list.filter_map(fn(x) {
    case x {
      Some(h) -> Ok(h)
      None -> Error(Nil)
    }
  })
}

/// Decode common HTML entities back to plain text
fn decode_html_entities(text: String) -> String {
  text
  |> string.replace("&quot;", "\"")
  |> string.replace("&amp;", "&")
  |> string.replace("&lt;", "<")
  |> string.replace("&gt;", ">")
  |> string.replace("&#39;", "'")
  |> string.replace("&apos;", "'")
}

/// Add anchor links to h2 and h3 headings for direct linking
fn add_header_anchors(html: String) -> String {
  let assert Ok(re) = regexp.from_string("<h([23]) id=\"([^\"]+)\">")
  regexp.match_map(re, html, fn(m) {
    case m.submatches {
      [Some(level), Some(id)] ->
        "<h"
        <> level
        <> " id=\""
        <> id
        <> "\"><a href=\"#"
        <> id
        <> "\" class=\"header-anchor\">#</a>"
      _ -> m.content
    }
  })
}

/// Transform .md links to clean paths
fn transform_links(html: String) -> String {
  // Match href="./something.md" or href="something.md" and replace with href="/something"
  let assert Ok(re) = regexp.from_string("href=\"(?:\\./)?([^\"]+)\\.md\"")
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
