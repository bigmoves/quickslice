import components/collection_table
import components/layout
import components/stats_card
import database
import gleam/list
import gleam/option.{type Option}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import sqlight

/// Page data aggregated from database queries
pub type IndexData {
  IndexData(
    lexicon_count: Int,
    actor_count: Int,
    collection_stats: List(database.CollectionStat),
    record_lexicons: List(database.Lexicon),
  )
}

/// Main view function that renders the index page
pub fn view(
  db: sqlight.Connection,
  current_user: Option(#(String, String)),
  is_admin: Bool,
) -> Element(msg) {
  let data = fetch_data(db)
  render(data, current_user, is_admin)
}

/// Fetch all data needed for the index page
fn fetch_data(db: sqlight.Connection) -> IndexData {
  let lexicon_count = case database.get_lexicon_count(db) {
    Ok(count) -> count
    Error(_) -> 0
  }

  let actor_count = case database.get_actor_count(db) {
    Ok(count) -> count
    Error(_) -> 0
  }

  let collection_stats = case database.get_collection_stats(db) {
    Ok(stats) -> stats
    Error(_) -> []
  }

  let record_lexicons = case database.get_record_type_lexicons(db) {
    Ok(lexicons) -> lexicons
    Error(_) -> []
  }

  IndexData(
    lexicon_count: lexicon_count,
    actor_count: actor_count,
    collection_stats: collection_stats,
    record_lexicons: record_lexicons,
  )
}

/// Render the complete index page
fn render(
  data: IndexData,
  current_user: Option(#(String, String)),
  is_admin: Bool,
) -> Element(msg) {
  layout.page(
    title: "ATProto Database Stats",
    content: [
      render_header(current_user, is_admin),
      render_lexicons_section(data.lexicon_count),
      render_actors_section(data.actor_count),
      render_collections_section(data.collection_stats, data.record_lexicons),
    ],
  )
}

/// Render the page header with title and action buttons
fn render_header(
  current_user: Option(#(String, String)),
  is_admin: Bool,
) -> Element(msg) {
  let action_buttons = case current_user {
    option.Some(_) -> {
      // Build list of action buttons based on permissions
      let backfill_button = case is_admin {
        True -> [
          html.form(
            [
              attribute.method("post"),
              attribute.action("/backfill"),
              attribute.class("inline"),
            ],
            [
              html.button(
                [
                  attribute.type_("submit"),
                  attribute.class(
                    "bg-blue-600 hover:bg-blue-700 text-white font-semibold py-2 px-4 rounded-lg transition-colors shadow-sm",
                  ),
                ],
                [element.text("Backfill Collections")],
              ),
            ],
          ),
        ]
        False -> []
      }

      let common_buttons = [
        html.a(
          [
            attribute.href("/graphiql"),
            attribute.class(
              "bg-purple-600 hover:bg-purple-700 text-white font-semibold py-2 px-4 rounded-lg transition-colors shadow-sm",
            ),
          ],
          [element.text("Open GraphiQL")],
        ),
        html.a(
          [
            attribute.href("/upload"),
            attribute.class(
              "bg-green-600 hover:bg-green-700 text-white font-semibold py-2 px-4 rounded-lg transition-colors shadow-sm",
            ),
          ],
          [element.text("Upload Blob")],
        ),
      ]

      [
        html.div(
          [attribute.class("flex gap-3")],
          list.append(backfill_button, common_buttons),
        ),
      ]
    }
    option.None -> []
  }

  html.div([attribute.class("mb-8")], [
    // Title and user info row
    html.div([attribute.class("flex justify-between items-center mb-4")], [
      html.h1([attribute.class("text-4xl font-bold text-gray-900")], [
        element.text("quickslice"),
      ]),
      render_user_section(current_user),
    ]),
    ..action_buttons
  ])
}

/// Render the user section showing login or user info
fn render_user_section(current_user: Option(#(String, String))) -> Element(msg) {
  case current_user {
    option.Some(#(_did, handle)) -> {
      // User is logged in
      html.div([attribute.class("flex items-center gap-3")], [
        html.span([attribute.class("text-gray-700")], [
          element.text("Logged in as "),
          html.span([attribute.class("font-semibold text-gray-900")], [
            element.text("@" <> handle),
          ]),
        ]),
        html.form(
          [attribute.method("post"), attribute.action("/logout"), attribute.class("inline")],
          [
            html.button(
              [
                attribute.type_("submit"),
                attribute.class(
                  "bg-gray-600 hover:bg-gray-700 text-white font-semibold py-2 px-4 rounded-lg transition-colors shadow-sm",
                ),
              ],
              [element.text("Logout")],
            ),
          ],
        ),
      ])
    }
    option.None -> {
      // User is not logged in - show login form
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
              "px-3 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500",
            ),
            attribute.attribute("required", ""),
          ]),
          html.button(
            [
              attribute.type_("submit"),
              attribute.class(
                "bg-blue-600 hover:bg-blue-700 text-white font-semibold py-2 px-4 rounded-lg transition-colors shadow-sm",
              ),
            ],
            [element.text("Login")],
          ),
        ],
      )
    }
  }
}

/// Render the lexicons statistics section
fn render_lexicons_section(lexicon_count: Int) -> Element(msg) {
  html.div([attribute.class("mb-8")], [
    html.h2([attribute.class("text-2xl font-semibold text-gray-700 mb-4")], [
      element.text("Lexicons"),
    ]),
    stats_card.card(
      count: lexicon_count,
      description: "Lexicon schemas loaded",
      color: "purple",
    ),
  ])
}

/// Render the actors statistics section
fn render_actors_section(actor_count: Int) -> Element(msg) {
  html.div([attribute.class("mb-8")], [
    html.h2([attribute.class("text-2xl font-semibold text-gray-700 mb-4")], [
      element.text("Actors"),
    ]),
    stats_card.card(
      count: actor_count,
      description: "Total actors indexed",
      color: "blue",
    ),
  ])
}

/// Render the collections table section
fn render_collections_section(
  collection_stats: List(database.CollectionStat),
  record_lexicons: List(database.Lexicon),
) -> Element(msg) {
  html.div([], [
    html.h2([attribute.class("text-2xl font-semibold text-gray-700 mb-4")], [
      element.text("Collections"),
    ]),
    collection_table.view(collection_stats, record_lexicons),
  ])
}
