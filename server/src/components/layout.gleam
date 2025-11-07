import components/logo
import gleam/option.{type Option}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

/// Renders a complete HTML page with unified header
pub fn page_with_header(
  title title: String,
  content content: List(Element(msg)),
  current_user current_user: Option(#(String, String)),
  domain_authority domain_authority: Option(String),
) -> Element(msg) {
  html.html([attribute.class("h-full")], [
    head(title),
    html.body(
      [attribute.class("bg-zinc-950 text-zinc-300 font-mono min-h-screen")],
      [
        html.div([attribute.class("max-w-4xl mx-auto px-6 py-12")], [
          render_header(current_user, domain_authority),
          ..content
        ]),
      ],
    ),
  ])
}

/// Renders a complete HTML page with the given title and content
pub fn page(
  title title: String,
  content content: List(Element(msg)),
) -> Element(msg) {
  html.html([attribute.class("h-full")], [
    head(title),
    body(content),
  ])
}

/// Renders the HTML head with meta tags and styles
fn head(title: String) -> Element(msg) {
  html.head([], [
    html.title([], title),
    element.element("meta", [attribute.attribute("charset", "UTF-8")], []),
    element.element(
      "meta",
      [
        attribute.attribute("name", "viewport"),
        attribute.attribute("content", "width=device-width, initial-scale=1.0"),
      ],
      [],
    ),
    element.element(
      "script",
      [attribute.attribute("src", "https://cdn.tailwindcss.com")],
      [],
    ),
    // Lustre server component runtime
    html.script(
      [
        attribute.type_("module"),
        attribute.attribute("src", "/lustre/runtime.mjs"),
      ],
      "",
    ),
    // Listen for backfill-complete event and reload page
    html.script(
      [],
      "
      // Wait for DOM to be ready
      document.addEventListener('DOMContentLoaded', function() {
        const backfillButton = document.querySelector('lustre-server-component#backfill-button');
        if (backfillButton) {
          backfillButton.addEventListener('backfill-complete', function() {
            // Reload page to show updated database stats
            window.location.reload();
          });
        }
      });
      ",
    ),
  ])
}

/// Renders the HTML body with a max-width container and navigation
fn body(content: List(Element(msg))) -> Element(msg) {
  html.body(
    [attribute.class("bg-zinc-950 text-zinc-300 font-mono min-h-screen")],
    [
      nav_header(),
      html.div([attribute.class("max-w-4xl mx-auto px-6 py-12")], content),
    ],
  )
}

/// Renders the navigation header
fn nav_header() -> Element(msg) {
  html.nav([attribute.class("bg-zinc-900 border-b border-zinc-800")], [
    html.div([attribute.class("max-w-4xl mx-auto px-6 py-4")], [
      html.div([attribute.class("flex items-center justify-between")], [
        html.a(
          [
            attribute.href("/"),
            attribute.class(
              "text-zinc-300 hover:text-zinc-100 transition-colors",
            ),
          ],
          [element.text("quickslice")],
        ),
        html.div([attribute.class("flex gap-4")], [
          html.a(
            [
              attribute.href("/"),
              attribute.class(
                "text-zinc-400 hover:text-zinc-200 transition-colors text-sm",
              ),
            ],
            [element.text("Home")],
          ),
          html.a(
            [
              attribute.href("/settings"),
              attribute.class(
                "text-zinc-400 hover:text-zinc-200 transition-colors text-sm",
              ),
            ],
            [element.text("Settings")],
          ),
        ]),
      ]),
    ]),
  ])
}

/// Renders the unified header with logo, nav links, and user section
fn render_header(
  current_user: Option(#(String, String)),
  domain_authority: Option(String),
) -> Element(msg) {
  html.div([attribute.class("border-b border-zinc-800 pb-4 mb-8")], [
    html.div([attribute.class("flex items-end justify-between")], [
      // Left: Brand with logo
      html.a(
        [
          attribute.href("/"),
          attribute.class(
            "flex items-center gap-3 hover:opacity-80 transition-opacity",
          ),
        ],
        [
          logo.view("w-10 h-10"),
          html.div([], [
            html.h1(
              [
                attribute.class(
                  "text-xs font-medium uppercase tracking-wider text-zinc-500",
                ),
              ],
              [element.text("quickslice")],
            ),
            case domain_authority {
              option.Some(value) ->
                case value {
                  "" -> element.none()
                  _ ->
                    html.p([attribute.class("text-xs text-zinc-600 mt-1")], [
                      element.text(value),
                    ])
                }
              option.None -> element.none()
            },
          ]),
        ],
      ),
      // Right: Navigation + User section
      case current_user {
        option.Some(_) -> {
          html.div([attribute.class("flex gap-4 text-xs items-center")], [
            html.a(
              [
                attribute.href("/"),
                attribute.class(
                  "px-2 py-1 text-zinc-400 hover:text-zinc-300 transition-colors",
                ),
              ],
              [element.text("Home")],
            ),
            html.a(
              [
                attribute.href("/settings"),
                attribute.class(
                  "px-2 py-1 text-zinc-400 hover:text-zinc-300 transition-colors",
                ),
              ],
              [element.text("Settings")],
            ),
            render_user_section(current_user),
          ])
        }
        option.None -> {
          html.div([attribute.class("flex items-center")], [
            render_user_section(current_user),
          ])
        }
      },
    ]),
  ])
}

/// Renders the user section showing login or user info
fn render_user_section(current_user: Option(#(String, String))) -> Element(msg) {
  case current_user {
    option.Some(#(_did, handle)) -> {
      html.span([attribute.class("px-2 py-1 text-zinc-400")], [
        element.text("@" <> handle),
      ])
    }
    option.None -> {
      // Show inline login form
      html.form(
        [
          attribute.method("post"),
          attribute.action("/oauth/authorize"),
          attribute.class("flex items-center gap-2"),
        ],
        [
          html.input([
            attribute.type_("text"),
            attribute.name("loginHint"),
            attribute.placeholder("your-handle.bsky.social"),
            attribute.class(
              "px-3 py-2 bg-zinc-900 border border-zinc-800 rounded text-xs text-zinc-300 focus:outline-none focus:border-zinc-700",
            ),
            attribute.attribute("required", ""),
          ]),
          html.button(
            [
              attribute.type_("submit"),
              attribute.class(
                "px-3 py-2 text-xs text-zinc-300 bg-zinc-800 hover:bg-zinc-700 rounded transition-colors cursor-pointer",
              ),
            ],
            [element.text("Login")],
          ),
        ],
      )
    }
  }
}
