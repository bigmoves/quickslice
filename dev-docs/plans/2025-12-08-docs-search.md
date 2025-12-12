# Docs Search Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add full-text search to the docs site with a search input in the sidebar and dropdown results.

**Architecture:** Generate a `search-index.json` at build time containing page titles, paths, headings, and plain-text content. Load the index lazily on first search focus. Use Fuse.js for fuzzy matching. Display results in a dropdown below the search input with keyboard navigation.

**Tech Stack:** Gleam (index generation), Fuse.js (fuzzy search), vanilla JavaScript (UI)

---

### Task 1: Create Search Index Module

**Files:**
- Create: `www/src/www/search.gleam`

**Step 1: Create the search module**

Create `www/src/www/search.gleam`:

```gleam
/// Search index generation for docs site
import gleam/json
import gleam/list
import gleam/option.{None, Some}
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
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/www && gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add www/src/www/search.gleam
git commit -m "feat(www): add search index generation module"
```

---

### Task 2: Generate Search Index at Build Time

**Files:**
- Modify: `www/src/www.gleam`

**Step 1: Update www.gleam to generate search index**

Add import at top of `www/src/www.gleam`:

```gleam
import www/search
```

**Step 2: Add search index generation after OG image**

In the `Ok(_)` branch of `ssg.build`, after `generate_og_image()`, add:

```gleam
          // Generate search index
          generate_search_index(all_pages)
```

**Step 3: Add the generate_search_index function**

Add after `generate_og_image` function:

```gleam
fn generate_search_index(pages: List(DocPage)) -> Nil {
  let json = search.generate_index(pages)
  case simplifile.write("./priv/search-index.json", json) {
    Ok(_) -> io.println("Generated: priv/search-index.json")
    Error(_) -> io.println("Failed to write search index")
  }
}
```

**Step 4: Verify it compiles and generates the index**

Run: `cd /Users/chadmiller/code/quickslice/www && gleam run`
Expected: Output includes "Generated: priv/search-index.json"

**Step 5: Verify the index content**

Run: `head -c 500 /Users/chadmiller/code/quickslice/www/priv/search-index.json`
Expected: Valid JSON array with page entries

**Step 6: Commit**

```bash
git add www/src/www.gleam
git commit -m "feat(www): generate search-index.json at build time"
```

---

### Task 3: Add Search Input to Sidebar

**Files:**
- Modify: `www/src/www/layout.gleam`

**Step 1: Add input import**

In the imports at top, add `input` to the html imports:

```gleam
import lustre/element/html.{
  a, aside, body, button, div, head, html, input, li, link, main, meta, nav,
  script, span, title, ul,
}
```

**Step 2: Add search component to sidebar**

Find the `sidebar` function and add the search input between the tangled-link and nav:

```gleam
fn sidebar(current_path: String, pages: List(DocPage)) -> Element(Nil) {
  aside([class("sidebar")], [
    div([class("sidebar-brand")], [
      logo.logo(),
      span([class("sidebar-title")], [html.text("quickslice")]),
    ]),
    a([href("https://tangled.sh"), class("tangled-link")], [
      tangled_logo(),
      span([], [html.text("tangled.sh")]),
    ]),
    div([class("search-container")], [
      input([
        attribute("type", "text"),
        attribute("placeholder", "Search docs..."),
        attribute("id", "search-input"),
        class("search-input"),
      ]),
      div([attribute("id", "search-results"), class("search-results")], []),
    ]),
    nav([], render_grouped_nav(current_path, pages)),
  ])
}
```

**Step 3: Add search.js script to layout**

In the `wrap` function, add the search script after minimap.js:

```gleam
      script([attribute("src", "/search.js")], ""),
```

**Step 4: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/www && gleam build`
Expected: Build succeeds

**Step 5: Commit**

```bash
git add www/src/www/layout.gleam
git commit -m "feat(www): add search input to sidebar"
```

---

### Task 4: Add Search CSS Styles

**Files:**
- Modify: `www/static/styles.css`

**Step 1: Add search styles at end of file**

```css
/* Search */
.search-container {
  position: relative;
  padding: 0 var(--space-3);
  margin-bottom: var(--space-4);
}

.search-input {
  width: 100%;
  padding: var(--space-2) var(--space-3);
  background: var(--bg-elevated);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  color: var(--text-primary);
  font-family: var(--font-body);
  font-size: var(--text-sm);
  outline: none;
  transition: border-color 0.15s ease;
}

.search-input::placeholder {
  color: var(--text-dim);
}

.search-input:focus {
  border-color: var(--text-muted);
}

.search-results {
  position: absolute;
  top: 100%;
  left: var(--space-3);
  right: var(--space-3);
  margin-top: var(--space-1);
  background: var(--bg-elevated);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  max-height: 300px;
  overflow-y: auto;
  z-index: 50;
  display: none;
}

.search-results.open {
  display: block;
}

.search-result {
  display: block;
  padding: var(--space-2) var(--space-3);
  text-decoration: none;
  border-bottom: 1px solid var(--border);
  transition: background 0.15s ease;
}

.search-result:last-child {
  border-bottom: none;
}

.search-result:hover,
.search-result.active {
  background: oklab(0.30 0 0);
}

.search-result-title {
  color: var(--text-primary);
  font-weight: var(--font-medium);
  font-size: var(--text-sm);
}

.search-result-group {
  color: var(--text-dim);
  font-size: var(--text-xs);
}

.search-result-snippet {
  color: var(--text-muted);
  font-size: var(--text-xs);
  margin-top: var(--space-1);
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.search-result-snippet mark {
  background: var(--accent-cyan);
  color: var(--bg-base);
  padding: 0 2px;
  border-radius: 2px;
}

.search-no-results {
  padding: var(--space-3);
  color: var(--text-muted);
  font-size: var(--text-sm);
  text-align: center;
}
```

**Step 2: Commit**

```bash
git add www/static/styles.css
git commit -m "feat(www): add search CSS styles"
```

---

### Task 5: Add Fuse.js Library

**Files:**
- Create: `www/static/fuse.min.js`

**Step 1: Download Fuse.js**

Run: `curl -o /Users/chadmiller/code/quickslice/www/static/fuse.min.js https://cdn.jsdelivr.net/npm/fuse.js@7.0.0/dist/fuse.min.js`

**Step 2: Commit**

```bash
git add www/static/fuse.min.js
git commit -m "feat(www): add fuse.js for fuzzy search"
```

---

### Task 6: Add Search JavaScript

**Files:**
- Create: `www/static/search.js`

**Step 1: Create the search script**

Create `www/static/search.js`:

```javascript
// Search functionality for docs site
(function() {
  let fuse = null;
  let searchIndex = null;
  let activeIndex = -1;

  const input = document.getElementById('search-input');
  const results = document.getElementById('search-results');

  if (!input || !results) return;

  // Load search index on first focus
  input.addEventListener('focus', loadIndex);

  // Handle input
  input.addEventListener('input', debounce(handleSearch, 150));

  // Handle keyboard navigation
  input.addEventListener('keydown', handleKeydown);

  // Close results when clicking outside
  document.addEventListener('click', function(e) {
    if (!e.target.closest('.search-container')) {
      closeResults();
    }
  });

  async function loadIndex() {
    if (searchIndex) return;

    try {
      const response = await fetch('/search-index.json');
      searchIndex = await response.json();

      fuse = new Fuse(searchIndex, {
        keys: [
          { name: 'title', weight: 3 },
          { name: 'headings.text', weight: 2 },
          { name: 'content', weight: 1 }
        ],
        includeMatches: true,
        threshold: 0.4,
        ignoreLocation: true,
        minMatchCharLength: 2
      });
    } catch (err) {
      console.error('Failed to load search index:', err);
    }
  }

  function handleSearch() {
    const query = input.value.trim();

    if (!query || !fuse) {
      closeResults();
      return;
    }

    const matches = fuse.search(query, { limit: 8 });

    if (matches.length === 0) {
      results.innerHTML = '<div class="search-no-results">No results found</div>';
      results.classList.add('open');
      activeIndex = -1;
      return;
    }

    results.innerHTML = matches.map((match, i) => {
      const item = match.item;
      const snippet = getSnippet(match, query);

      return `
        <a href="${item.path}" class="search-result" data-index="${i}">
          <div class="search-result-title">${escapeHtml(item.title)}</div>
          <div class="search-result-group">${escapeHtml(item.group)}</div>
          ${snippet ? `<div class="search-result-snippet">${snippet}</div>` : ''}
        </a>
      `;
    }).join('');

    results.classList.add('open');
    activeIndex = -1;
  }

  function getSnippet(match, query) {
    // Find content match
    const contentMatch = match.matches?.find(m => m.key === 'content');
    if (!contentMatch) return null;

    const content = match.item.content;
    const indices = contentMatch.indices[0];
    if (!indices) return null;

    const start = Math.max(0, indices[0] - 30);
    const end = Math.min(content.length, indices[1] + 50);

    let snippet = content.slice(start, end);
    if (start > 0) snippet = '...' + snippet;
    if (end < content.length) snippet = snippet + '...';

    // Highlight match
    const queryLower = query.toLowerCase();
    const snippetLower = snippet.toLowerCase();
    const matchStart = snippetLower.indexOf(queryLower);

    if (matchStart >= 0) {
      const before = escapeHtml(snippet.slice(0, matchStart));
      const matched = escapeHtml(snippet.slice(matchStart, matchStart + query.length));
      const after = escapeHtml(snippet.slice(matchStart + query.length));
      return before + '<mark>' + matched + '</mark>' + after;
    }

    return escapeHtml(snippet);
  }

  function handleKeydown(e) {
    const items = results.querySelectorAll('.search-result');
    if (!items.length) return;

    if (e.key === 'ArrowDown') {
      e.preventDefault();
      activeIndex = Math.min(activeIndex + 1, items.length - 1);
      updateActive(items);
    } else if (e.key === 'ArrowUp') {
      e.preventDefault();
      activeIndex = Math.max(activeIndex - 1, 0);
      updateActive(items);
    } else if (e.key === 'Enter' && activeIndex >= 0) {
      e.preventDefault();
      items[activeIndex].click();
    } else if (e.key === 'Escape') {
      closeResults();
      input.blur();
    }
  }

  function updateActive(items) {
    items.forEach((item, i) => {
      item.classList.toggle('active', i === activeIndex);
    });

    if (activeIndex >= 0) {
      items[activeIndex].scrollIntoView({ block: 'nearest' });
    }
  }

  function closeResults() {
    results.classList.remove('open');
    results.innerHTML = '';
    activeIndex = -1;
  }

  function escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
  }

  function debounce(fn, delay) {
    let timeout;
    return function(...args) {
      clearTimeout(timeout);
      timeout = setTimeout(() => fn.apply(this, args), delay);
    };
  }
})();
```

**Step 2: Commit**

```bash
git add www/static/search.js
git commit -m "feat(www): add search JavaScript with Fuse.js"
```

---

### Task 7: Add Fuse.js Script Tag to Layout

**Files:**
- Modify: `www/src/www/layout.gleam`

**Step 1: Add fuse.js script before search.js**

In the `wrap` function, find the scripts at the end and add fuse.min.js before search.js:

```gleam
      script([attribute("src", "/fuse.min.js")], ""),
      script([attribute("src", "/search.js")], ""),
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/www && gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add www/src/www/layout.gleam
git commit -m "feat(www): add fuse.js script to layout"
```

---

### Task 8: Build and Test

**Step 1: Full rebuild**

Run: `cd /Users/chadmiller/code/quickslice/www && gleam run`
Expected: All files generated including search-index.json

**Step 2: Verify search index exists**

Run: `ls -la /Users/chadmiller/code/quickslice/www/priv/search-index.json`
Expected: File exists

**Step 3: Manual test**

Open the site in a browser, click the search input, type a query, verify:
- Results appear in dropdown
- Arrow keys navigate
- Enter navigates to result
- Escape closes dropdown
- Clicking outside closes dropdown

**Step 4: Final commit**

```bash
git add -A
git commit -m "feat(www): complete docs search implementation"
```

---

## Summary

| Task | Description |
|------|-------------|
| 1 | Create search index generation module |
| 2 | Generate search-index.json at build time |
| 3 | Add search input to sidebar |
| 4 | Add search CSS styles |
| 5 | Add Fuse.js library |
| 6 | Add search JavaScript |
| 7 | Add Fuse.js script to layout |
| 8 | Build and test |
