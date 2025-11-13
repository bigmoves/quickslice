import components/logo
import gleam/list
import gleam/option.{type Option}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

/// Renders the unified header with logo and nav
/// auth_info: Option containing (handle, is_admin) if authenticated, None if not
pub fn header(auth_info: Option(#(String, Bool))) -> Element(msg) {
  html.div([attribute.class("border-b border-zinc-800 pb-3 mb-8")], [
    html.div([attribute.class("flex items-center justify-between")], [
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
          ]),
        ],
      ),
      // Right: Navigation and Auth
      html.div(
        [attribute.class("flex gap-4 text-xs items-center")],
        case auth_info {
          option.None -> [
            // Only show login form when not authenticated
            html.form(
              [
                attribute.method("POST"),
                attribute.action("/oauth/authorize"),
                attribute.class("flex gap-2 items-center"),
              ],
              [
                html.input([
                  attribute.type_("text"),
                  attribute.name("login_hint"),
                  attribute.placeholder("handle.bsky.social"),
                  attribute.class(
                    "bg-zinc-900 border border-zinc-700 rounded px-2 py-1 text-xs text-zinc-300 placeholder-zinc-600 focus:border-zinc-500 focus:outline-none w-48",
                  ),
                  attribute.required(True),
                ]),
                html.button(
                  [
                    attribute.type_("submit"),
                    attribute.class(
                      "bg-zinc-800 hover:bg-zinc-700 text-zinc-300 px-3 py-1 rounded transition-colors",
                    ),
                  ],
                  [element.text("Login")],
                ),
              ],
            ),
          ]
          option.Some(#(handle, is_admin)) -> {
            // Show navigation links when authenticated
            let nav_links = [
              html.a(
                [
                  attribute.href("/"),
                  attribute.class(
                    "px-3 py-1 text-zinc-400 hover:text-zinc-300 transition-colors",
                  ),
                ],
                [element.text("Home")],
              ),
            ]

            // Add Settings link only for admins
            let nav_links = case is_admin {
              True ->
                list.append(nav_links, [
                  html.a(
                    [
                      attribute.href("/settings"),
                      attribute.class(
                        "px-3 py-1 text-zinc-400 hover:text-zinc-300 transition-colors",
                      ),
                    ],
                    [element.text("Settings")],
                  ),
                ])
              False -> nav_links
            }

            list.append(nav_links, [
              // User handle
              html.span([attribute.class("px-3 py-1 text-zinc-400")], [
                element.text(handle),
              ]),
              // Logout button
              html.form(
                [
                  attribute.method("POST"),
                  attribute.action("/logout"),
                ],
                [
                  html.button(
                    [
                      attribute.type_("submit"),
                      attribute.class(
                        "px-3 py-1 text-zinc-400 hover:text-zinc-300 transition-colors cursor-pointer",
                      ),
                    ],
                    [element.text("Logout")],
                  ),
                ],
              ),
            ])
          }
        },
      ),
    ]),
  ])
}
