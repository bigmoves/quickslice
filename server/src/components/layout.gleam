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
      "link",
      [
        attribute.attribute("rel", "stylesheet"),
        attribute.attribute("href", "/styles.css"),
      ],
      [],
    ),
    // Tippy.js for tooltips
    html.script(
      [attribute.attribute("src", "https://unpkg.com/@popperjs/core@2")],
      "",
    ),
    html.script(
      [attribute.attribute("src", "https://unpkg.com/tippy.js@6")],
      "",
    ),
    element.element(
      "link",
      [
        attribute.attribute("rel", "stylesheet"),
        attribute.attribute("href", "https://unpkg.com/tippy.js@6/themes/light.css"),
      ],
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
    // Define custom elements for client-side formatting
    html.script(
      [],
      "
      // Format timestamps inside Shadow DOM
      function formatTimestamps(shadowRoot) {
        if (!shadowRoot) return;

        const timeElements = shadowRoot.querySelectorAll('[data-timestamp]');
        timeElements.forEach(el => {
          const utcTime = el.getAttribute('data-timestamp');
          if (utcTime) {
            try {
              const date = new Date(utcTime);
              const formatted = date.toLocaleTimeString([], {
                hour: '2-digit',
                minute: '2-digit',
                second: '2-digit',
                hour12: false
              });
              // Always reformat - Lustre may reuse elements with different data
              if (el.textContent !== formatted) {
                el.textContent = formatted;
              }
            } catch (e) {
              console.error('[Timestamp Formatter] Error:', e);
            }
          }
        });

        // Format JSON elements
        const jsonElements = shadowRoot.querySelectorAll('[data-json]');
        jsonElements.forEach(el => {
          const jsonStr = el.getAttribute('data-json');
          if (jsonStr) {
            try {
              const parsed = JSON.parse(jsonStr);
              if (parsed.commit && typeof parsed.commit.record === 'string') {
                try {
                  parsed.commit.record = JSON.parse(parsed.commit.record);
                } catch (e) {}
              }
              const formatted = JSON.stringify(parsed, null, 2);
              if (el.textContent !== formatted) {
                el.textContent = formatted;
              }
            } catch (e) {
              console.error('[JSON Formatter] Error:', e);
            }
          }
        });
      }

      // Listen for activity log component mount and updates
      document.addEventListener('DOMContentLoaded', function() {
        const activityLog = document.querySelector('lustre-server-component#activity-log');

        if (activityLog) {
          // Format on initial mount
          activityLog.addEventListener('lustre:mount', function() {
            formatTimestamps(activityLog.shadowRoot);
          });

          // Also try formatting immediately in case already mounted
          if (activityLog.shadowRoot) {
            formatTimestamps(activityLog.shadowRoot);
          }

          // Watch for changes in Shadow DOM using MutationObserver
          const observer = new MutationObserver(() => {
            formatTimestamps(activityLog.shadowRoot);
          });

          // Wait a bit for shadow root to be available
          setTimeout(() => {
            if (activityLog.shadowRoot) {
              observer.observe(activityLog.shadowRoot, {
                childList: true,
                subtree: true
              });
              // Format once more to catch anything that loaded during timeout
              formatTimestamps(activityLog.shadowRoot);
            }
          }, 100);
        }

        // Initialize tooltips for activity chart
        function initChartTooltips(shadowRoot) {
          if (!shadowRoot || !window.tippy) return;

          const bars = shadowRoot.querySelectorAll('[data-tooltip-timestamp]');
          bars.forEach(bar => {
            // Destroy existing tippy instance if it exists
            if (bar._tippy) {
              bar._tippy.destroy();
            }

            const timestamp = bar.getAttribute('data-tooltip-timestamp');
            const create = bar.getAttribute('data-create') || '0';
            const update = bar.getAttribute('data-update') || '0';
            const del = bar.getAttribute('data-delete') || '0';
            const total = parseInt(create) + parseInt(update) + parseInt(del);

            if (!timestamp) return;

            try {
              const date = new Date(timestamp);
              const formatted = date.toLocaleString([], {
                month: 'short',
                day: 'numeric',
                hour: 'numeric',
                minute: '2-digit',
                hour12: true
              });
              const timezone = Intl.DateTimeFormat().resolvedOptions().timeZone;

              let content = formatted + ' (' + timezone + ')';
              if (total === 0) {
                content += '\\nNo activity';
              } else {
                content += '\\nCreate: ' + create;
                content += '\\nUpdate: ' + update;
                content += '\\nDelete: ' + del;
                content += '\\nTotal: ' + total;
              }

              tippy(bar, {
                content: content.replace(/\\n/g, '<br>'),
                allowHTML: true,
                theme: 'dark',
                placement: 'top'
              });
            } catch (e) {
              console.error('[Tooltip] Error:', e);
            }
          });
        }

        const activityChart = document.querySelector('lustre-server-component#activity-chart');
        if (activityChart) {
          activityChart.addEventListener('lustre:mount', function() {
            initChartTooltips(activityChart.shadowRoot);
          });

          if (activityChart.shadowRoot) {
            initChartTooltips(activityChart.shadowRoot);
          }

          const chartObserver = new MutationObserver(() => {
            initChartTooltips(activityChart.shadowRoot);
          });

          setTimeout(() => {
            if (activityChart.shadowRoot) {
              chartObserver.observe(activityChart.shadowRoot, {
                childList: true,
                subtree: true
              });
              initChartTooltips(activityChart.shadowRoot);
            }
          }, 100);
        }

        // Listen for backfill-complete event and reload page
        const backfillButton = document.querySelector('lustre-server-component#backfill-button');
        if (backfillButton) {
          backfillButton.addEventListener('backfill-complete', function() {
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
