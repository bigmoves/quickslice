# Mobile Sticky Header Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add a sticky header on mobile that displays the brand (logo, title, version) and hamburger menu, visible while scrolling.

**Architecture:** Create a new `mobile_header()` function in layout.gleam that renders a header element containing the brand and menu toggle. Style it with CSS to be sticky on mobile only, hidden on desktop.

**Tech Stack:** Gleam, Lustre (HTML generation), CSS

---

## Task 1: Add Mobile Header Element

**Files:**
- Modify: `www/src/www/layout.gleam:150-182` (menu_toggle function)
- Modify: `www/src/www/layout.gleam:52-54` (wrap function body)

**Step 1: Create mobile_header function**

Add this function after the existing `menu_toggle()` function (around line 182):

```gleam
/// Mobile header with brand and menu toggle (visible only on mobile)
fn mobile_header() -> Element(Nil) {
  html.header([class("mobile-header")], [
    div([class("mobile-header-brand")], [
      logo.logo(),
      span([class("sidebar-title")], [html.text("quickslice")]),
      span([class("sidebar-version")], [html.text(version)]),
    ]),
    button([class("menu-toggle"), attribute("aria-label", "Toggle menu")], [
      svg.svg(
        [
          attribute("viewBox", "0 0 24 24"),
          attribute("fill", "none"),
          attribute("stroke", "currentColor"),
          attribute("stroke-width", "2"),
        ],
        [
          svg.line([
            attribute("x1", "3"),
            attribute("y1", "6"),
            attribute("x2", "21"),
            attribute("y2", "6"),
          ]),
          svg.line([
            attribute("x1", "3"),
            attribute("y1", "12"),
            attribute("x2", "21"),
            attribute("y2", "12"),
          ]),
          svg.line([
            attribute("x1", "3"),
            attribute("y1", "18"),
            attribute("x2", "21"),
            attribute("y2", "18"),
          ]),
        ],
      ),
    ]),
  ])
}
```

**Step 2: Update wrap() to use mobile_header**

In the `wrap()` function, replace `menu_toggle()` with `mobile_header()`. Change line 53 from:

```gleam
      menu_toggle(),
```

to:

```gleam
      mobile_header(),
```

**Step 3: Add html.header import**

Update the import at line 7. Change:

```gleam
  a, aside, body, button, div, head, html, input, li, link, main, meta, nav,
  script, span, title, ul,
```

to:

```gleam
  a, aside, body, button, div, head, header, html, input, li, link, main, meta,
  nav, script, span, title, ul,
```

**Step 4: Build to verify no compile errors**

Run: `cd /Users/chadmiller/code/quickslice/www && gleam build`
Expected: Compiled successfully

**Step 5: Commit**

```bash
git add www/src/www/layout.gleam
git commit -m "feat(www): add mobile header element with brand and menu toggle"
```

---

## Task 2: Style Mobile Header

**Files:**
- Modify: `www/static/styles.css` (add after line 402, before mobile styles section)

**Step 1: Add mobile header styles**

Add these styles after the `.sidebar-backdrop` rules (around line 402) and before the `@media (max-width: 767px)` block:

```css
/* Mobile header - sticky brand bar */
.mobile-header {
  display: none;
}
```

**Step 2: Update mobile media query styles**

Inside the existing `@media (max-width: 767px)` block (starts around line 404), add the mobile header styles. After the `.menu-toggle { display: block; }` rule, replace it and add the mobile header rules:

Find this section (around lines 409-411):

```css
  .menu-toggle {
    display: block;
  }
```

Replace with:

```css
  .mobile-header {
    display: flex;
    align-items: center;
    justify-content: space-between;
    position: sticky;
    top: 0;
    z-index: 50;
    padding: var(--space-3) var(--space-4);
    background: var(--bg-base);
    border-bottom: 1px solid var(--border);
  }

  .mobile-header-brand {
    display: flex;
    align-items: center;
    gap: var(--space-3);
  }

  .mobile-header .sidebar-logo {
    width: 28px;
    height: 28px;
  }

  .mobile-header .sidebar-title {
    font-size: var(--text-lg);
  }

  .mobile-header .sidebar-version {
    margin-left: var(--space-1);
  }

  .mobile-header .menu-toggle {
    display: block;
    position: static;
    background: transparent;
    border: none;
    padding: var(--space-2);
  }
```

**Step 3: Update content padding**

In the same mobile media query, find the `.content` rule (around line 440-446):

```css
  .content {
    margin: 0;
    border-radius: 0;
    border: none;
    padding: var(--space-16) var(--space-5) var(--space-8);
    min-height: 100vh;
  }
```

Change the padding to remove the extra top space (since header handles it now):

```css
  .content {
    margin: 0;
    border-radius: 0;
    border: none;
    padding: var(--space-6) var(--space-5) var(--space-8);
    min-height: 100vh;
  }
```

**Step 4: Copy styles.css to priv directory**

Run: `cp /Users/chadmiller/code/quickslice/www/static/styles.css /Users/chadmiller/code/quickslice/www/priv/styles.css`

**Step 5: Build and verify**

Run: `cd /Users/chadmiller/code/quickslice/www && gleam build`
Expected: Compiled successfully

**Step 6: Commit**

```bash
git add www/static/styles.css www/priv/styles.css
git commit -m "feat(www): style mobile sticky header with brand and menu"
```

---

## Task 3: Remove Standalone menu_toggle Function

**Files:**
- Modify: `www/src/www/layout.gleam:150-182`

**Step 1: Delete the old menu_toggle function**

Remove the entire `menu_toggle()` function (lines 150-182) since it's now inlined in `mobile_header()`:

```gleam
/// Hamburger menu button for mobile
fn menu_toggle() -> Element(Nil) {
  button([class("menu-toggle"), attribute("aria-label", "Toggle menu")], [
    svg.svg(
      [
        attribute("viewBox", "0 0 24 24"),
        attribute("fill", "none"),
        attribute("stroke", "currentColor"),
        attribute("stroke-width", "2"),
      ],
      [
        svg.line([
          attribute("x1", "3"),
          attribute("y1", "6"),
          attribute("x2", "21"),
          attribute("y2", "6"),
        ]),
        svg.line([
          attribute("x1", "3"),
          attribute("y1", "12"),
          attribute("x2", "21"),
          attribute("y2", "12"),
        ]),
        svg.line([
          attribute("x1", "3"),
          attribute("y1", "18"),
          attribute("x2", "21"),
          attribute("y2", "18"),
        ]),
      ],
    ),
  ])
}
```

**Step 2: Build to verify**

Run: `cd /Users/chadmiller/code/quickslice/www && gleam build`
Expected: Compiled successfully

**Step 3: Commit**

```bash
git add www/src/www/layout.gleam
git commit -m "refactor(www): remove unused menu_toggle function"
```

---

## Task 4: Test Mobile Header

**Step 1: Generate the site**

Run: `cd /Users/chadmiller/code/quickslice/www && gleam run`
Expected: Site generates successfully

**Step 2: Manual verification**

Open generated HTML in browser, resize to mobile width (<768px) and verify:
- Header appears at top with logo, "quickslice", version
- Hamburger menu on right side of header
- Header stays sticky when scrolling
- Clicking hamburger opens sidebar
- Header hidden on desktop width

**Step 3: Final commit (if any adjustments needed)**

```bash
git add -A
git commit -m "test(www): verify mobile sticky header works"
```
