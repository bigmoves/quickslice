import components/button
import components/collection_table
import components/layout
import components/sparkline
import database
import format
import gleam/option.{type Option}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/server_component
import sqlight

/// Page data aggregated from database queries
pub type IndexData {
  IndexData(
    record_count: Int,
    lexicon_count: Int,
    actor_count: Int,
    collection_stats: List(database.CollectionStat),
    record_lexicons: List(database.Lexicon),
    record_activity: List(database.ActivityPoint),
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
  let record_count = case database.get_record_count(db) {
    Ok(count) -> count
    Error(_) -> 0
  }

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

  let record_activity = case database.get_record_activity(db, 168) {
    Ok(activity) -> activity
    Error(_) -> []
  }

  IndexData(
    record_count: record_count,
    lexicon_count: lexicon_count,
    actor_count: actor_count,
    collection_stats: collection_stats,
    record_lexicons: record_lexicons,
    record_activity: record_activity,
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
      render_stats_section(data.record_count, data.lexicon_count, data.actor_count),
      render_activity_section(data.record_activity),
      render_collections_section(
        data.collection_stats,
        data.record_lexicons,
        is_admin,
      ),
    ],
  )
}

/// Render the page header with title and action buttons
fn render_header(
  current_user: Option(#(String, String)),
  _is_admin: Bool,
) -> Element(msg) {
  let action_buttons = case current_user {
    option.Some(_) -> {
      let common_buttons = [
        button.link(href: "/graphiql", text: "Open GraphiQL"),
        button.link(href: "/upload", text: "Upload Blob"),
      ]

      [
        html.div([attribute.class("flex gap-3")], common_buttons),
      ]
    }
    option.None -> []
  }

  html.div([attribute.class("mb-8")], [
    // Title and user info row
    html.div([attribute.class("flex justify-between items-center mb-4")], [
      html.h1([attribute.class("text-4xl font-bold text-zinc-200")], [
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
        html.span([attribute.class("text-zinc-300")], [
          element.text("Logged in as "),
          html.span([attribute.class("font-semibold text-zinc-200")], [
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
                  "px-4 py-2 text-sm text-zinc-400 border border-zinc-800 hover:border-zinc-700 hover:text-zinc-300 rounded transition-colors cursor-pointer",
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
              "px-3 py-2 bg-zinc-900 border border-zinc-800 rounded text-sm text-zinc-300 focus:outline-none focus:border-zinc-700",
            ),
            attribute.attribute("required", ""),
          ]),
          html.button(
            [
              attribute.type_("submit"),
              attribute.class(
                "px-4 py-2 text-sm text-zinc-300 bg-zinc-800 hover:bg-zinc-700 rounded transition-colors cursor-pointer",
              ),
            ],
            [element.text("Login")],
          ),
        ],
      )
    }
  }
}

/// Render the combined statistics section
fn render_stats_section(record_count: Int, lexicon_count: Int, actor_count: Int) -> Element(msg) {
  html.div([attribute.class("mb-8 grid grid-cols-3 gap-4")], [
    // Total records stat card
    html.div([attribute.class("bg-zinc-800/50 rounded p-4")], [
      html.div([attribute.class("text-sm text-zinc-500 mb-1")], [
        element.text("Total Records"),
      ]),
      html.div([attribute.class("text-2xl font-semibold text-zinc-200")], [
        element.text(format.format_number(record_count)),
      ]),
    ]),
    // Actors stat card
    html.div([attribute.class("bg-zinc-800/50 rounded p-4")], [
      html.div([attribute.class("text-sm text-zinc-500 mb-1")], [
        element.text("Total Actors"),
      ]),
      html.div([attribute.class("text-2xl font-semibold text-zinc-200")], [
        element.text(format.format_number(actor_count)),
      ]),
    ]),
    // Lexicons stat card
    html.div([attribute.class("bg-zinc-800/50 rounded p-4")], [
      html.div([attribute.class("text-sm text-zinc-500 mb-1")], [
        element.text("Total Lexicons"),
      ]),
      html.div([attribute.class("text-2xl font-semibold text-zinc-200")], [
        element.text(format.format_number(lexicon_count)),
      ]),
    ]),
  ])
}

/// Render the activity chart section
fn render_activity_section(
  activity: List(database.ActivityPoint),
) -> Element(msg) {
  html.div([attribute.class("mb-8")], [
    html.div([attribute.class("bg-zinc-800/50 rounded p-4")], [
      html.div([attribute.class("text-sm text-zinc-500 mb-3")], [
        element.text("Activity (Last 7 Days)"),
      ]),
      sparkline.view(activity),
    ]),
  ])
}

/// Render the collections table section
fn render_collections_section(
  collection_stats: List(database.CollectionStat),
  record_lexicons: List(database.Lexicon),
  is_admin: Bool,
) -> Element(msg) {
  let backfill_button = case is_admin {
    True ->
      server_component.element(
        [attribute.id("backfill-button"), server_component.route("/backfill-ws")],
        [],
      )
    False -> element.none()
  }

  html.div([], [
    html.div([attribute.class("flex justify-between items-center mb-4")], [
      html.h2([attribute.class("text-2xl font-semibold text-zinc-300")], [
        element.text("Collections"),
      ]),
      backfill_button,
    ]),
    collection_table.view(collection_stats, record_lexicons),
  ])
}
