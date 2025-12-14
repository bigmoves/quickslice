/// Loads documentation markdown files from disk
import gleam/list
import simplifile
import www/config.{
  type DocPage, type NavGroup, type NavItem, DocPage, Group, NavGroup, Page,
  docs_dir, navigation,
}

/// Load all doc pages in configured order
pub fn load_all() -> Result(List(DocPage), String) {
  list.try_map(navigation, load_item)
  |> result_flatten
}

/// Load a navigation item (group or standalone page)
fn load_item(item: NavItem) -> Result(List(DocPage), String) {
  case item {
    Group(group) -> load_group(group)
    Page(filename, path, title) -> {
      case load_page(filename, path, title, "") {
        Ok(page) -> Ok([page])
        Error(e) -> Error(e)
      }
    }
  }
}

/// Load all pages in a navigation group
fn load_group(group: NavGroup) -> Result(List(DocPage), String) {
  let NavGroup(group_name, pages) = group
  list.try_map(pages, fn(entry) {
    let #(filename, path, title) = entry
    load_page(filename, path, title, group_name)
  })
}

/// Load a single page from disk
fn load_page(
  filename: String,
  path: String,
  title: String,
  group_name: String,
) -> Result(DocPage, String) {
  let filepath = case filename {
    "CHANGELOG.md" -> "../CHANGELOG.md"
    _ -> docs_dir <> "/" <> filename
  }

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
        "Failed to read " <> filename <> ": " <> simplifile.describe_error(err),
      )
  }
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
