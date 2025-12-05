# Docs Site with lustre_ssg Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Generate a static documentation site from markdown files using lustre_ssg with a sidebar navigation layout.

**Architecture:** Read markdown files from `/docs/`, convert to HTML with mork, wrap in a docs layout with sidebar navigation, output to `./priv/` using index routes (clean URLs).

**Tech Stack:** Gleam, lustre_ssg, mork, simplifile, Lustre HTML elements

---

## Task 1: Add mork and simplifile dependencies

**Files:**
- Modify: `www/gleam.toml:15-17`

**Step 1: Add dependencies to gleam.toml**

```toml
[dependencies]
gleam_stdlib = ">= 0.44.0 and < 2.0.0"
lustre_ssg = ">= 0.11.0 and < 1.0.0"
mork = ">= 1.8.0 and < 2.0.0"
simplifile = ">= 2.0.0 and < 3.0.0"
```

**Step 2: Download dependencies**

Run: `gleam deps download`
Expected: Dependencies downloaded successfully

**Step 3: Commit**

```bash
git add gleam.toml manifest.toml
git commit -m "chore: add mork and simplifile dependencies for docs site"
```

---

## Task 2: Create the DocPage type and page order configuration

**Files:**
- Create: `www/src/docs/config.gleam`

**Step 1: Create the config module with types and page order**

```gleam
/// Configuration for the docs site

/// Represents a documentation page
pub type DocPage {
  DocPage(
    /// Filename without extension (e.g., "queries")
    slug: String,
    /// URL path (e.g., "/queries")
    path: String,
    /// Display title for navigation
    title: String,
    /// Markdown content (loaded from file)
    content: String,
  )
}

/// Manual ordering of documentation pages
/// Format: #(filename, path, nav_title)
pub const page_order: List(#(String, String, String)) = [
  #("README.md", "/", "Getting Started"),
  #("authentication.md", "/authentication", "Authentication"),
  #("queries.md", "/queries", "Queries"),
  #("mutations.md", "/mutations", "Mutations"),
  #("joins.md", "/joins", "Joins"),
  #("aggregations.md", "/aggregations", "Aggregations"),
  #("subscriptions.md", "/subscriptions", "Subscriptions"),
  #("blobs.md", "/blobs", "Blobs"),
  #("variables.md", "/variables", "Variables"),
  #("deployment.md", "/deployment", "Deployment"),
  #("mcp.md", "/mcp", "MCP"),
]

/// Path to the docs directory (relative to project root)
pub const docs_dir: String = "../docs"

/// Output directory for generated site
pub const out_dir: String = "./priv"
```

**Step 2: Verify it compiles**

Run: `gleam build`
Expected: Build succeeds with no errors

**Step 3: Commit**

```bash
git add src/docs/config.gleam
git commit -m "feat: add docs config with page order and types"
```

---

## Task 3: Create the docs loader module

**Files:**
- Create: `www/src/docs/loader.gleam`

**Step 1: Create the loader that reads markdown files**

```gleam
/// Loads documentation markdown files from disk

import docs/config.{type DocPage, DocPage, docs_dir, page_order}
import gleam/list
import gleam/result
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
```

**Step 2: Verify it compiles**

Run: `gleam build`
Expected: Build succeeds with no errors

**Step 3: Commit**

```bash
git add src/docs/loader.gleam
git commit -m "feat: add docs loader to read markdown files"
```

---

## Task 4: Create the layout module with CSS and sidebar

**Files:**
- Create: `www/src/docs/layout.gleam`

**Step 1: Create the layout with embedded CSS and sidebar**

```gleam
/// Docs site layout with sidebar navigation

import docs/config.{type DocPage}
import gleam/list
import lustre/attribute.{attribute, class, href}
import lustre/element.{type Element}
import lustre/element/html.{
  a, aside, body, div, head, html, li, link, main, meta, nav, style, title, ul,
}

/// Minimal CSS for the docs layout
const css: String = "
* { box-sizing: border-box; margin: 0; padding: 0; }
body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, sans-serif;
  line-height: 1.6;
  color: #1a1a1a;
}
.container { display: flex; min-height: 100vh; }
.sidebar {
  width: 260px;
  padding: 2rem 1rem;
  background: #f5f5f5;
  border-right: 1px solid #e0e0e0;
  position: fixed;
  height: 100vh;
  overflow-y: auto;
}
.sidebar h1 { font-size: 1.25rem; margin-bottom: 1.5rem; padding: 0 0.5rem; }
.sidebar ul { list-style: none; }
.sidebar li { margin: 0.25rem 0; }
.sidebar a {
  display: block;
  padding: 0.5rem;
  color: #444;
  text-decoration: none;
  border-radius: 4px;
}
.sidebar a:hover { background: #e8e8e8; }
.sidebar a.active { background: #007acc; color: white; }
.content {
  flex: 1;
  margin-left: 260px;
  padding: 2rem 3rem;
  max-width: 900px;
}
.content h1 { font-size: 2rem; margin-bottom: 1rem; border-bottom: 1px solid #eee; padding-bottom: 0.5rem; }
.content h2 { font-size: 1.5rem; margin: 2rem 0 1rem; }
.content h3 { font-size: 1.25rem; margin: 1.5rem 0 0.75rem; }
.content p { margin: 1rem 0; }
.content pre {
  background: #f5f5f5;
  padding: 1rem;
  overflow-x: auto;
  border-radius: 4px;
  margin: 1rem 0;
}
.content code {
  background: #f0f0f0;
  padding: 0.2em 0.4em;
  border-radius: 3px;
  font-size: 0.9em;
}
.content pre code { background: none; padding: 0; }
.content ul, .content ol { margin: 1rem 0; padding-left: 2rem; }
.content li { margin: 0.5rem 0; }
.content blockquote {
  border-left: 4px solid #007acc;
  margin: 1rem 0;
  padding: 0.5rem 1rem;
  background: #f8f8f8;
}
.content a { color: #007acc; }
.content table { border-collapse: collapse; margin: 1rem 0; width: 100%; }
.content th, .content td { border: 1px solid #ddd; padding: 0.5rem; text-align: left; }
.content th { background: #f5f5f5; }
"

/// Wrap content in the docs layout
pub fn wrap(
  page: DocPage,
  all_pages: List(DocPage),
  content_html: String,
) -> Element(Nil) {
  html([], [
    head([], [
      meta([attribute("charset", "UTF-8")]),
      meta([
        attribute("name", "viewport"),
        attribute("content", "width=device-width, initial-scale=1.0"),
      ]),
      title([], "quickslice - " <> page.title),
      style([], css),
    ]),
    body([], [
      div([class("container")], [
        sidebar(page.path, all_pages),
        main([class("content")], [
          element.unsafe_raw_html("", "div", [], content_html),
        ]),
      ]),
    ]),
  ])
}

fn sidebar(current_path: String, pages: List(DocPage)) -> Element(Nil) {
  aside([class("sidebar")], [
    html.h1([], [html.text("quickslice")]),
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
```

**Step 2: Verify it compiles**

Run: `gleam build`
Expected: Build succeeds with no errors

**Step 3: Commit**

```bash
git add src/docs/layout.gleam
git commit -m "feat: add docs layout with sidebar and minimal CSS"
```

---

## Task 5: Create the page renderer module

**Files:**
- Create: `www/src/docs/page.gleam`

**Step 1: Create the page module that combines mork + layout**

```gleam
/// Renders a doc page by converting markdown to HTML and wrapping in layout

import docs/config.{type DocPage}
import docs/layout
import lustre/element.{type Element}
import mork

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
```

**Step 2: Verify it compiles**

Run: `gleam build`
Expected: Build succeeds with no errors

**Step 3: Commit**

```bash
git add src/docs/page.gleam
git commit -m "feat: add page renderer with mork markdown conversion"
```

---

## Task 6: Update main build script

**Files:**
- Modify: `www/src/www.gleam`

**Step 1: Replace www.gleam with the SSG build script**

```gleam
/// Docs site static site generator

import docs/config
import docs/loader
import docs/page
import gleam/io
import gleam/list
import lustre/ssg

pub fn main() -> Nil {
  case loader.load_all() {
    Error(err) -> {
      io.println("Error loading docs: " <> err)
    }
    Ok(pages) -> {
      let result =
        list.fold(pages, ssg.new(config.out_dir), fn(cfg, p) {
          ssg.add_static_route(cfg, p.path, page.render(p, pages))
        })
        |> ssg.use_index_routes
        |> ssg.build

      case result {
        Ok(_) -> io.println("Build succeeded! Output: " <> config.out_dir)
        Error(e) -> {
          io.debug(e)
          io.println("Build failed!")
        }
      }
    }
  }
}
```

**Step 2: Verify it compiles**

Run: `gleam build`
Expected: Build succeeds with no errors

**Step 3: Commit**

```bash
git add src/www.gleam
git commit -m "feat: implement SSG build script for docs site"
```

---

## Task 7: Run the build and verify output

**Step 1: Run the build**

Run: `gleam run`
Expected: "Build succeeded! Output: ./priv"

**Step 2: Verify the output structure**

Run: `ls -la priv/`
Expected:
```
index.html
authentication/
queries/
mutations/
joins/
aggregations/
subscriptions/
blobs/
variables/
deployment/
mcp/
```

**Step 3: Verify a generated page has correct structure**

Run: `head -50 priv/index.html`
Expected: HTML document with:
- DOCTYPE
- `<head>` with title and CSS
- Sidebar with navigation links
- Main content with rendered markdown

**Step 4: Commit**

```bash
git add priv/
git commit -m "feat: generate initial docs site"
```

---

## Summary

After completing all tasks, you will have:

1. **`src/docs/config.gleam`** - Page order and types
2. **`src/docs/loader.gleam`** - Reads markdown from `/docs/`
3. **`src/docs/layout.gleam`** - HTML layout with sidebar + CSS
4. **`src/docs/page.gleam`** - Combines mork + layout
5. **`src/www.gleam`** - SSG build script
6. **`priv/`** - Generated static site with clean URLs
