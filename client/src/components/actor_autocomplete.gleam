/// Actor autocomplete component for Bluesky handle lookup
/// Provides typeahead search against the public AT Protocol API
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

/// Actor data returned from the search API
pub type Actor {
  Actor(
    did: String,
    handle: String,
    display_name: String,
    avatar: Option(String),
  )
}

/// Internal state for the autocomplete component
pub type Model {
  Model(
    query: String,
    actors: List(Actor),
    highlighted_index: Int,
    is_open: Bool,
  )
}

/// Messages the autocomplete component can receive
pub type Msg {
  UpdateQuery(String)
  SearchResult(Result(List(Actor), String))
  SelectActor(Actor)
  HighlightNext
  HighlightPrevious
  Close
  Open
}

/// FFI binding for debounced actor search
@external(javascript, "./actor_autocomplete.ffi.mjs", "searchActors")
fn do_search_actors(
  query: String,
  debounce_ms: Int,
  dispatch: fn(Result(List(Actor), String)) -> Nil,
) -> Nil

/// FFI binding to cancel pending search
@external(javascript, "./actor_autocomplete.ffi.mjs", "cancelSearch")
pub fn cancel_search() -> Nil

/// Initialize the autocomplete model
pub fn init() -> Model {
  Model(query: "", actors: [], highlighted_index: -1, is_open: False)
}

/// Update the autocomplete state based on messages
pub fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    UpdateQuery(query) -> {
      // Trigger search effect
      let search_effect =
        effect.from(fn(dispatch) {
          do_search_actors(query, 300, fn(result) {
            dispatch(SearchResult(result))
          })
        })
      #(Model(..model, query: query, is_open: query != ""), search_effect)
    }

    SearchResult(Ok(actors)) -> {
      #(Model(..model, actors: actors, highlighted_index: -1), effect.none())
    }

    SearchResult(Error(_)) -> {
      // Silent fail - just clear results
      #(Model(..model, actors: [], highlighted_index: -1), effect.none())
    }

    SelectActor(actor) -> {
      // Set query to handle and close dropdown
      #(
        Model(
          query: actor.handle,
          actors: [],
          highlighted_index: -1,
          is_open: False,
        ),
        effect.none(),
      )
    }

    HighlightNext -> {
      let len = list.length(model.actors)
      let new_index = case model.highlighted_index < len - 1 {
        True -> model.highlighted_index + 1
        False -> 0
      }
      #(Model(..model, highlighted_index: new_index), effect.none())
    }

    HighlightPrevious -> {
      let len = list.length(model.actors)
      let new_index = case model.highlighted_index > 0 {
        True -> model.highlighted_index - 1
        False -> len - 1
      }
      #(Model(..model, highlighted_index: new_index), effect.none())
    }

    Close -> {
      cancel_search()
      #(Model(..model, is_open: False, highlighted_index: -1), effect.none())
    }

    Open -> {
      #(Model(..model, is_open: model.query != ""), effect.none())
    }
  }
}

/// Get the currently highlighted actor (if any)
pub fn get_highlighted_actor(model: Model) -> Option(Actor) {
  case model.highlighted_index >= 0 {
    True -> {
      model.actors
      |> list.drop(model.highlighted_index)
      |> list.first
      |> option.from_result
    }
    False -> None
  }
}

/// Render the autocomplete input and dropdown
/// on_select: callback when an actor is selected (receives the handle)
pub fn view(
  model: Model,
  input_id: String,
  input_name: String,
  placeholder: String,
  input_class: String,
  on_input: fn(String) -> msg,
  on_select: fn(String) -> msg,
  on_keydown: fn(String) -> msg,
  on_blur: fn() -> msg,
  on_focus: fn() -> msg,
) -> Element(msg) {
  html.div([attribute.class("relative")], [
    // Input field
    html.input([
      attribute.type_("text"),
      attribute.id(input_id),
      attribute.name(input_name),
      attribute.value(model.query),
      attribute.placeholder(placeholder),
      attribute.class(input_class),
      attribute.required(True),
      attribute.attribute("autocomplete", "off"),
      event.on_input(on_input),
      event.on("blur", decode.success(on_blur())),
      event.on("focus", decode.success(on_focus())),
      event.on("keydown", {
        use key <- decode.field("key", decode.string)
        decode.success(on_keydown(key))
      }),
    ]),
    // Dropdown (only show if open and has results)
    case model.is_open && model.actors != [] {
      True -> view_dropdown(model, on_select)
      False -> element.none()
    },
  ])
}

fn view_dropdown(model: Model, on_select: fn(String) -> msg) -> Element(msg) {
  html.div(
    [
      attribute.class(
        "absolute z-50 w-full mt-1 bg-zinc-800 border border-zinc-700 rounded shadow-lg max-h-60 overflow-y-auto",
      ),
    ],
    list.index_map(model.actors, fn(actor, index) {
      view_actor_item(actor, index == model.highlighted_index, on_select)
    }),
  )
}

fn view_actor_item(
  actor: Actor,
  is_highlighted: Bool,
  on_select: fn(String) -> msg,
) -> Element(msg) {
  let bg_class = case is_highlighted {
    True -> "bg-zinc-700"
    False -> "hover:bg-zinc-700"
  }

  html.div(
    [
      attribute.class(
        "flex items-center gap-2 px-3 py-2 cursor-pointer transition-colors "
        <> bg_class,
      ),
      event.on("mousedown", decode.success(on_select(actor.handle))),
    ],
    [
      // Avatar (if present)
      case actor.avatar {
        Some(url) ->
          html.img([
            attribute.src(url),
            attribute.class("w-8 h-8 rounded-full flex-shrink-0"),
            attribute.alt(""),
          ])
        None -> element.none()
      },
      // Text content
      html.div([attribute.class("flex-1 min-w-0")], [
        // Display name (if present)
        case actor.display_name {
          "" -> element.none()
          name ->
            html.div([attribute.class("text-sm text-zinc-200 truncate")], [
              element.text(name),
            ])
        },
        // Handle
        html.div([attribute.class("text-xs text-zinc-400 truncate")], [
          element.text("@" <> actor.handle),
        ]),
      ]),
    ],
  )
}
