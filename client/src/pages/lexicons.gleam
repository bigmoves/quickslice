/// Lexicons Page Component
///
/// Displays all lexicons with their NSIDs and JSON content
///
/// ```graphql
/// query GetLexicons {
///   lexicons {
///     id
///     json
///     createdAt
///   }
/// }
/// ```
import components/icon
import generated/queries/get_lexicons
import gleam/json
import gleam/list
import gleam/string
import json_highlighter
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import squall_cache.{type Cache}

pub type Msg {
  CopyJson(String)
}

pub fn update(msg: Msg) -> Effect(Msg) {
  case msg {
    CopyJson(json_text) -> {
      use _dispatch <- effect.from
      do_copy_to_clipboard(json_text)
    }
  }
}

@external(javascript, "../copy_to_clipboard.ffi.mjs", "copyToClipboard")
fn do_copy_to_clipboard(text: String) -> Nil

pub fn view(cache: Cache) -> Element(Msg) {
  // Fetch lexicons from cache
  let #(_cache, lexicons_result) =
    squall_cache.lookup(
      cache,
      "GetLexicons",
      json.object([]),
      get_lexicons.parse_get_lexicons_response,
    )

  html.div([attribute.class("space-y-6")], [
    // Back button
    html.div([attribute.class("mb-2")], [
      html.a(
        [
          attribute.href("/"),
          attribute.class(
            "text-zinc-400 hover:text-zinc-300 transition-colors text-sm",
          ),
        ],
        [element.text("← Back")],
      ),
    ]),
    // Header
    html.h1([attribute.class("text-2xl font-semibold text-zinc-300 mb-6")], [
      element.text("Lexicons"),
    ]),
    // Content
    case lexicons_result {
      squall_cache.Data(data) -> render_lexicons(data.lexicons)
      squall_cache.Loading -> render_loading()
      squall_cache.Failed(err) -> render_error(err)
    },
  ])
}

fn render_loading() -> Element(Msg) {
  html.div([attribute.class("py-8 text-center text-zinc-600 text-sm")], [
    element.text("Loading lexicons..."),
  ])
}

fn render_error(error: String) -> Element(Msg) {
  html.div([attribute.class("py-8 text-center text-red-400 text-sm")], [
    element.text("Error: " <> error),
  ])
}

fn render_lexicons(lexicons: List(get_lexicons.Lexicon)) -> Element(Msg) {
  case lexicons {
    [] -> render_empty()
    _ ->
      html.div([attribute.class("space-y-4")], [
        html.p([attribute.class("text-sm text-zinc-500 mb-4")], [
          element.text(string.inspect(list.length(lexicons)) <> " lexicons"),
        ]),
        html.div(
          [attribute.class("space-y-3")],
          list.map(lexicons, render_lexicon),
        ),
      ])
  }
}

fn render_empty() -> Element(Msg) {
  html.div(
    [
      attribute.class(
        "bg-zinc-800/50 rounded p-8 text-center border border-zinc-700",
      ),
    ],
    [
      html.p([attribute.class("text-zinc-400 mb-2")], [
        element.text("No lexicons found."),
      ]),
      html.p([attribute.class("text-sm text-zinc-500")], [
        element.text("Upload lexicons from the Settings page to get started."),
      ]),
    ],
  )
}

fn render_lexicon(lexicon: get_lexicons.Lexicon) -> Element(Msg) {
  html.div([attribute.class("bg-zinc-800/50 rounded p-4 relative")], [
    html.button(
      [
        attribute.class(
          "absolute top-4 right-4 p-1.5 rounded hover:bg-zinc-700 text-zinc-400 hover:text-zinc-300 transition-colors",
        ),
        attribute.type_("button"),
        event.on_click(CopyJson(lexicon.json)),
      ],
      [icon.icon("copy", icon.Sm, [])],
    ),
    html.details([attribute.class("group")], [
      html.summary(
        [
          attribute.class(
            "cursor-pointer text-base font-mono text-zinc-300 hover:text-zinc-200 select-none list-none pr-10",
          ),
        ],
        [element.text(lexicon.id)],
      ),
      html.pre(
        [
          attribute.class(
            "mt-3 bg-zinc-900 rounded p-3 overflow-x-auto text-xs font-mono",
          ),
        ],
        [
          html.code([], [
            json_highlighter.highlight(json_highlighter.format(lexicon.json)),
          ]),
        ],
      ),
    ]),
  ])
}
