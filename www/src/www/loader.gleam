/// Loads documentation markdown files from disk
import gleam/list
import simplifile
import www/config.{
  type DocPage, type NavGroup, DocPage, NavGroup, docs_dir, navigation,
}

/// Load all doc pages in configured order
pub fn load_all() -> Result(List(DocPage), String) {
  list.try_map(navigation, load_group)
  |> result_flatten
}

/// Load all pages in a navigation group
fn load_group(group: NavGroup) -> Result(List(DocPage), String) {
  let NavGroup(group_name, pages) = group
  list.try_map(pages, fn(entry) {
    let #(filename, path, title) = entry
    let filepath = docs_dir <> "/" <> filename

    case simplifile.read(filepath) {
      Ok(content) -> {
        let slug = case path {
          "/" -> "index"
          _ -> remove_leading_slash(path)
        }
        Ok(DocPage(
          slug: slug,
          path: path,
          title: title,
          group: group_name,
          content: content,
        ))
      }
      Error(err) ->
        Error(
          "Failed to read "
          <> filename
          <> ": "
          <> simplifile.describe_error(err),
        )
    }
  })
}

/// Flatten a Result of List of Lists into Result of List
fn result_flatten(result: Result(List(List(a)), e)) -> Result(List(a), e) {
  case result {
    Ok(lists) -> Ok(list.flatten(lists))
    Error(e) -> Error(e)
  }
}

fn remove_leading_slash(path: String) -> String {
  case path {
    "/" <> rest -> rest
    other -> other
  }
}
