/// Search index generation for docs site
import gleam/json
import gleam/list
import gleam/option.{Some}
import gleam/regexp
import gleam/string
import www/config.{type DocPage}

/// Entry in the search index
pub type SearchEntry {
  SearchEntry(
    path: String,
    title: String,
    group: String,
    content: String,
    headings: List(SearchHeading),
  )
}

pub type SearchHeading {
  SearchHeading(id: String, text: String)
}

/// Generate JSON search index from all pages
pub fn generate_index(pages: List(DocPage)) -> String {
  pages
  |> list.map(page_to_entry)
  |> entries_to_json
}

fn page_to_entry(page: DocPage) -> SearchEntry {
  SearchEntry(
    path: page.path,
    title: page.title,
    group: page.group,
    content: strip_markdown(page.content),
    headings: extract_headings(page.content),
  )
}

/// Strip markdown syntax to get plain text for searching
fn strip_markdown(markdown: String) -> String {
  markdown
  |> remove_code_blocks
  |> remove_inline_code
  |> remove_links
  |> remove_images
  |> remove_headings_syntax
  |> remove_emphasis
  |> remove_html_tags
  |> collapse_whitespace
}

fn remove_code_blocks(text: String) -> String {
  let assert Ok(re) = regexp.from_string("```[\\s\\S]*?```")
  regexp.replace(re, text, " ")
}

fn remove_inline_code(text: String) -> String {
  let assert Ok(re) = regexp.from_string("`[^`]+`")
  regexp.replace(re, text, " ")
}

fn remove_links(text: String) -> String {
  let assert Ok(re) = regexp.from_string("\\[([^\\]]+)\\]\\([^)]+\\)")
  regexp.replace(re, text, "\\1")
}

fn remove_images(text: String) -> String {
  let assert Ok(re) = regexp.from_string("!\\[([^\\]]*)\\]\\([^)]+\\)")
  regexp.replace(re, text, "")
}

fn remove_headings_syntax(text: String) -> String {
  let assert Ok(re) = regexp.from_string("^#{1,6}\\s+")
  regexp.replace(re, text, "")
}

fn remove_emphasis(text: String) -> String {
  let assert Ok(bold) = regexp.from_string("\\*\\*([^*]+)\\*\\*")
  let assert Ok(italic) = regexp.from_string("\\*([^*]+)\\*")
  let assert Ok(underscore) = regexp.from_string("_([^_]+)_")
  text
  |> regexp.replace(bold, _, "\\1")
  |> regexp.replace(italic, _, "\\1")
  |> regexp.replace(underscore, _, "\\1")
}

fn remove_html_tags(text: String) -> String {
  let assert Ok(re) = regexp.from_string("<[^>]+>")
  regexp.replace(re, text, " ")
}

fn collapse_whitespace(text: String) -> String {
  let assert Ok(re) = regexp.from_string("\\s+")
  regexp.replace(re, text, " ")
  |> string.trim
}

/// Extract h2 and h3 headings from markdown
fn extract_headings(markdown: String) -> List(SearchHeading) {
  let assert Ok(re) = regexp.from_string("^#{2,3}\\s+(.+)$")
  regexp.scan(re, markdown)
  |> list.filter_map(fn(match) {
    case match.submatches {
      [Some(text)] -> {
        let id = text_to_id(text)
        Ok(SearchHeading(id: id, text: text))
      }
      _ -> Error(Nil)
    }
  })
}

/// Convert heading text to URL-friendly ID
fn text_to_id(text: String) -> String {
  text
  |> string.replace(" ", "-")
  |> string.replace("(", "")
  |> string.replace(")", "")
  |> string.replace(":", "")
  |> string.replace(",", "")
}

/// Convert entries to JSON string
fn entries_to_json(entries: List(SearchEntry)) -> String {
  entries
  |> list.map(entry_to_json)
  |> json.array(fn(x) { x })
  |> json.to_string
}

fn entry_to_json(entry: SearchEntry) -> json.Json {
  json.object([
    #("path", json.string(entry.path)),
    #("title", json.string(entry.title)),
    #("group", json.string(entry.group)),
    #("content", json.string(entry.content)),
    #("headings", json.array(entry.headings, heading_to_json)),
  ])
}

fn heading_to_json(heading: SearchHeading) -> json.Json {
  json.object([
    #("id", json.string(heading.id)),
    #("text", json.string(heading.text)),
  ])
}
