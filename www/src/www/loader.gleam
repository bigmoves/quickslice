/// Loads documentation markdown files from disk

import www/config.{type DocPage, DocPage, docs_dir, page_order}
import gleam/list
import simplifile

/// Load all doc pages in configured order
pub fn load_all() -> Result(List(DocPage), String) {
  list.try_map(page_order, fn(entry) {
    let #(filename, path, title) = entry
    let filepath = docs_dir <> "/" <> filename

    case simplifile.read(filepath) {
      Ok(content) -> {
        let slug = case path {
          "/" -> "index"
          _ -> remove_leading_slash(path)
        }
        Ok(DocPage(slug: slug, path: path, title: title, content: content))
      }
      Error(err) -> Error("Failed to read " <> filename <> ": " <> simplifile.describe_error(err))
    }
  })
}

fn remove_leading_slash(path: String) -> String {
  case path {
    "/" <> rest -> rest
    other -> other
  }
}
