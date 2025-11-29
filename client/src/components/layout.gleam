import components/actor_autocomplete
import components/backfill_animation
import components/icon
import components/logo
import gleam/list
import gleam/option.{type Option}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

/// Renders the unified header with logo and nav
/// auth_info: Option containing (handle, is_admin) if authenticated, None if not
/// is_backfilling: True if a backfill is in progress (shows animation instead of logo)
/// mobile_menu_open: True if the mobile menu is expanded
/// on_toggle_menu: Message to send when hamburger button is clicked
pub fn header(
  auth_info: Option(#(String, Bool)),
  is_backfilling: Bool,
  mobile_menu_open: Bool,
  on_toggle_menu: msg,
  login_autocomplete: actor_autocomplete.Model,
  on_autocomplete_input: fn(String) -> msg,
  on_autocomplete_select: fn(String) -> msg,
  on_autocomplete_keydown: fn(String) -> msg,
  on_autocomplete_blur: fn() -> msg,
  on_autocomplete_focus: fn() -> msg,
) -> Element(msg) {
  html.div([attribute.class("mb-8")], [
    // Main header row
    html.div(
      [
        attribute.class(
          "flex items-center justify-between border-b border-zinc-800 pb-3",
        ),
      ],
      [
        // Left: Brand with logo (or animation if backfilling)
        html.a(
          [
            attribute.href("/"),
            attribute.class(
              "flex items-center gap-3 hover:opacity-80 transition-opacity ml-1",
            ),
          ],
          [
            html.div([attribute.class("overflow-visible")], [
              case is_backfilling {
                True -> backfill_animation.view("w-10 h-10")
                False -> logo.view("w-10 h-10")
              },
            ]),
            html.h1(
              [
                attribute.class(
                  "text-xs font-medium uppercase tracking-wider text-zinc-500",
                ),
              ],
              [element.text("quickslice")],
            ),
          ],
        ),
        // Right side
        html.div([attribute.class("flex items-center gap-2")], [
          // Handle display (mobile only, when logged in)
          case auth_info {
            option.Some(#(handle, _)) ->
              html.span(
                [
                  attribute.class(
                    "sm:hidden text-xs text-zinc-400 px-2 truncate max-w-[150px]",
                  ),
                ],
                [element.text("@" <> handle)],
              )
            option.None -> element.none()
          },
          // Desktop navigation (hidden on mobile)
          html.div(
            [attribute.class("hidden sm:flex gap-4 text-xs items-center")],
            desktop_nav_content(
              auth_info,
              login_autocomplete,
              on_autocomplete_input,
              on_autocomplete_select,
              on_autocomplete_keydown,
              on_autocomplete_blur,
              on_autocomplete_focus,
            ),
          ),
          // Hamburger button (mobile only)
          html.button(
            [
              attribute.class(
                "sm:hidden p-2 text-zinc-400 hover:text-zinc-300 transition-colors",
              ),
              event.on_click(on_toggle_menu),
            ],
            [
              case mobile_menu_open {
                True -> icon.icon("x", icon.Md, [])
                False -> icon.icon("menu", icon.Md, [])
              },
            ],
          ),
        ]),
      ],
    ),
    // Mobile dropdown menu
    case mobile_menu_open {
      True ->
        mobile_menu(
          auth_info,
          login_autocomplete,
          on_autocomplete_input,
          on_autocomplete_select,
          on_autocomplete_keydown,
          on_autocomplete_blur,
          on_autocomplete_focus,
        )
      False -> element.none()
    },
  ])
}

/// Desktop navigation content (inline in header)
fn desktop_nav_content(
  auth_info: Option(#(String, Bool)),
  login_autocomplete: actor_autocomplete.Model,
  on_autocomplete_input: fn(String) -> msg,
  on_autocomplete_select: fn(String) -> msg,
  on_autocomplete_keydown: fn(String) -> msg,
  on_autocomplete_blur: fn() -> msg,
  on_autocomplete_focus: fn() -> msg,
) -> List(Element(msg)) {
  case auth_info {
    option.None -> [
      // Login form when not authenticated
      html.form(
        [
          attribute.method("POST"),
          attribute.action("/admin/oauth/authorize"),
          attribute.class("flex gap-2 items-center"),
        ],
        [
          actor_autocomplete.view(
            login_autocomplete,
            "login-hint-desktop",
            "login_hint",
            "handle.bsky.social",
            "bg-zinc-900 border border-zinc-700 rounded px-2 py-1 text-xs text-zinc-300 placeholder-zinc-600 focus:border-zinc-500 focus:outline-none w-48",
            on_autocomplete_input,
            on_autocomplete_select,
            on_autocomplete_keydown,
            on_autocomplete_blur,
            on_autocomplete_focus,
          ),
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
      // Navigation links when authenticated
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
  }
}

/// Mobile dropdown menu content
fn mobile_menu(
  auth_info: Option(#(String, Bool)),
  login_autocomplete: actor_autocomplete.Model,
  on_autocomplete_input: fn(String) -> msg,
  on_autocomplete_select: fn(String) -> msg,
  on_autocomplete_keydown: fn(String) -> msg,
  on_autocomplete_blur: fn() -> msg,
  on_autocomplete_focus: fn() -> msg,
) -> Element(msg) {
  html.div(
    [
      attribute.class(
        "sm:hidden mt-4 pb-4 border-b border-zinc-800 flex flex-col gap-3",
      ),
    ],
    case auth_info {
      option.None -> [
        // Login form for mobile (full width)
        html.form(
          [
            attribute.method("POST"),
            attribute.action("/admin/oauth/authorize"),
            attribute.class("flex flex-col gap-3"),
          ],
          [
            actor_autocomplete.view(
              login_autocomplete,
              "login-hint-mobile",
              "login_hint",
              "handle.bsky.social",
              "bg-zinc-900 border border-zinc-700 rounded px-3 py-2 text-sm text-zinc-300 placeholder-zinc-600 focus:border-zinc-500 focus:outline-none w-full",
              on_autocomplete_input,
              on_autocomplete_select,
              on_autocomplete_keydown,
              on_autocomplete_blur,
              on_autocomplete_focus,
            ),
            html.button(
              [
                attribute.type_("submit"),
                attribute.class(
                  "bg-zinc-800 hover:bg-zinc-700 text-zinc-300 px-4 py-2 rounded transition-colors w-full text-sm",
                ),
              ],
              [element.text("Login")],
            ),
          ],
        ),
      ]
      option.Some(#(_handle, is_admin)) -> {
        // Navigation links for mobile
        let nav_items = [
          html.a(
            [
              attribute.href("/"),
              attribute.class(
                "block px-3 py-2 text-sm text-zinc-400 hover:text-zinc-300 hover:bg-zinc-800/50 rounded transition-colors",
              ),
            ],
            [element.text("Home")],
          ),
        ]

        // Add Settings link only for admins
        let nav_items = case is_admin {
          True ->
            list.append(nav_items, [
              html.a(
                [
                  attribute.href("/settings"),
                  attribute.class(
                    "block px-3 py-2 text-sm text-zinc-400 hover:text-zinc-300 hover:bg-zinc-800/50 rounded transition-colors",
                  ),
                ],
                [element.text("Settings")],
              ),
            ])
          False -> nav_items
        }

        // Add logout button
        list.append(nav_items, [
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
                    "w-full text-left px-3 py-2 text-sm text-zinc-400 hover:text-zinc-300 hover:bg-zinc-800/50 rounded transition-colors cursor-pointer",
                  ),
                ],
                [element.text("Logout")],
              ),
            ],
          ),
        ])
      }
    },
  )
}
