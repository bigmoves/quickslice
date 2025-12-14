# Changelog in Sidebar Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add the root CHANGELOG.md to the www docs site as a standalone link at the top of the sidebar (no group header).

**Architecture:** Introduce a `NavItem` union type that supports both grouped pages (`Group(NavGroup)`) and standalone pages (`Page(...)`). Update loader to handle the new type and CHANGELOG.md special path. Update layout to render standalone items without a group header.

**Tech Stack:** Gleam, Lustre SSG

---

### Task 1: Add NavItem type to config.gleam

**Files:**
- Modify: `www/src/www/config.gleam`

**Step 1: Add NavItem union type after NavGroup**

Add after line 21:

```gleam
/// Navigation item - either a group or standalone page
pub type NavItem {
  /// A group with header and multiple pages
  Group(NavGroup)
  /// A standalone page (no group header)
  Page(filename: String, path: String, title: String)
}
```

**Step 2: Update navigation constant to use NavItem**

Replace the navigation constant (lines 25-63) with:

```gleam
/// Sidebar navigation structure
pub const navigation: List(NavItem) = [
  Page("CHANGELOG.md", "/changelog", "Changelog"),
  Group(NavGroup(
    "Getting Started",
    [
      #("README.md", "/", "Introduction"),
      #("tutorial.md", "/tutorial", "Tutorial"),
    ],
  )),
  Group(NavGroup(
    "Guides",
    [
      #("guides/queries.md", "/guides/queries", "Queries"),
      #("guides/joins.md", "/guides/joins", "Joins"),
      #("guides/mutations.md", "/guides/mutations", "Mutations"),
      #("guides/authentication.md", "/guides/authentication", "Authentication"),
      #("guides/deployment.md", "/guides/deployment", "Deployment"),
      #("guides/patterns.md", "/guides/patterns", "Patterns"),
      #(
        "guides/troubleshooting.md",
        "/guides/troubleshooting",
        "Troubleshooting",
      ),
    ],
  )),
  Group(NavGroup(
    "Reference",
    [
      #("reference/aggregations.md", "/reference/aggregations", "Aggregations"),
      #(
        "reference/subscriptions.md",
        "/reference/subscriptions",
        "Subscriptions",
      ),
      #("reference/blobs.md", "/reference/blobs", "Blobs"),
      #("reference/variables.md", "/reference/variables", "Variables"),
      #("reference/mcp.md", "/reference/mcp", "MCP"),
    ],
  )),
]
```

**Step 3: Verify syntax**

Run: `cd www && gleam build`
Expected: Compilation errors in loader.gleam (expected, will fix in Task 2)

---

### Task 2: Update loader.gleam for NavItem type

**Files:**
- Modify: `www/src/www/loader.gleam`

**Step 1: Update imports**

Change line 4-6 to import NavItem and its variants:

```gleam
import www/config.{
  type DocPage, type NavGroup, type NavItem, DocPage, Group, NavGroup, Page,
  docs_dir, navigation,
}
```

**Step 2: Replace load_all function**

Replace the `load_all` function (lines 9-12) with:

```gleam
/// Load all doc pages in configured order
pub fn load_all() -> Result(List(DocPage), String) {
  list.try_map(navigation, load_item)
  |> result_flatten
}
```

**Step 3: Add load_item function after load_all**

Add after the new `load_all`:

```gleam
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
```

**Step 4: Refactor load_group to use load_page helper**

Replace `load_group` function (lines 15-44) with:

```gleam
/// Load all pages in a navigation group
fn load_group(group: NavGroup) -> Result(List(DocPage), String) {
  let NavGroup(group_name, pages) = group
  list.try_map(pages, fn(entry) {
    let #(filename, path, title) = entry
    load_page(filename, path, title, group_name)
  })
}
```

**Step 5: Add load_page helper function**

Add after `load_group`:

```gleam
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
        "Failed to read "
        <> filename
        <> ": "
        <> simplifile.describe_error(err),
      )
  }
}
```

**Step 6: Verify build**

Run: `cd www && gleam build`
Expected: Build succeeds (layout.gleam doesn't need changes - it groups by page.group field, empty string groups will need handling)

---

### Task 3: Update layout.gleam to handle standalone pages

**Files:**
- Modify: `www/src/www/layout.gleam`

**Step 1: Update render_grouped_nav to handle empty group names**

Replace `render_grouped_nav` function (lines 255-284) with:

```gleam
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
```

**Step 2: Add CSS for standalone items**

Modify: `www/static/styles.css`

Add at end of sidebar styles section:

```css
.sidebar-standalone {
  margin-bottom: 1.5rem;
}

.sidebar-standalone a {
  display: block;
  padding: 0.375rem 0.75rem;
  color: var(--text-secondary);
  text-decoration: none;
  border-radius: 4px;
  font-size: 0.875rem;
}

.sidebar-standalone a:hover {
  color: var(--text-primary);
  background: var(--hover-bg);
}

.sidebar-standalone a.active {
  color: var(--accent);
  background: var(--accent-bg);
}
```

**Step 3: Build and run**

Run: `cd www && gleam run`
Expected: Build succeeds, generates changelog page

---

### Task 4: Verify and commit

**Step 1: Check generated output**

Run: `ls www/priv/changelog/`
Expected: `index.html` exists

**Step 2: Inspect sidebar in generated HTML**

Run: `grep -A5 "sidebar-standalone" www/priv/index.html`
Expected: Shows changelog link without group header

**Step 3: Commit changes**

```bash
git add www/src/www/config.gleam www/src/www/loader.gleam www/src/www/layout.gleam www/static/styles.css
git commit -m "feat(www): add changelog to sidebar as standalone link"
```
