import database
import format
import gleam/erlang/process
import gleam/int
import gleam/result
import lustre
import lustre/attribute
import lustre/effect
import lustre/element.{type Element}
import lustre/element/html
import sqlight
import stats_pubsub

// APP

pub fn component(db: sqlight.Connection) {
  lustre.application(init(db, _), update, view)
}

// MODEL

pub type Model {
  Model(
    db: sqlight.Connection,
    record_count: Int,
    actor_count: Int,
    lexicon_count: Int,
    stats_subscriber: process.Subject(stats_pubsub.StatsEvent),
  )
}

fn init(db: sqlight.Connection, _flags: Nil) -> #(Model, effect.Effect(Msg)) {
  // Get initial counts from database
  let record_count = database.get_record_count(db) |> result.unwrap(0)
  let actor_count = database.get_actor_count(db) |> result.unwrap(0)
  let lexicon_count = database.get_lexicon_count(db) |> result.unwrap(0)

  // We'll subscribe in the listener process, so create a dummy subject here
  let dummy_subscriber = process.new_subject()

  #(
    Model(
      db: db,
      record_count: record_count,
      actor_count: actor_count,
      lexicon_count: lexicon_count,
      stats_subscriber: dummy_subscriber,
    ),
    start_listening_in_background(),
  )
}

// UPDATE

pub opaque type Msg {
  StatsEventReceived(stats_pubsub.StatsEvent)
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    StatsEventReceived(event) -> {
      case event {
        stats_pubsub.RecordCreated -> {
          #(Model(..model, record_count: model.record_count + 1), effect.none())
        }
        stats_pubsub.RecordDeleted -> {
          #(
            Model(..model, record_count: int.max(0, model.record_count - 1)),
            effect.none(),
          )
        }
        stats_pubsub.ActorCreated -> {
          #(Model(..model, actor_count: model.actor_count + 1), effect.none())
        }
        // Ignore activity logged events - those are for the activity log component
        stats_pubsub.ActivityLogged(..) -> #(model, effect.none())
      }
    }
  }
}

// EFFECTS

fn start_listening_in_background() -> effect.Effect(Msg) {
  use dispatch <- effect.from

  // Spawn a single long-running process to listen for stats events
  let _ =
    process.spawn_unlinked(fn() {
      // Subscribe in THIS process, not the component process
      let subscriber = stats_pubsub.subscribe()
      listen_loop(subscriber, dispatch)
    })

  Nil
}

fn listen_loop(
  subscriber: process.Subject(stats_pubsub.StatsEvent),
  dispatch: fn(Msg) -> Nil,
) -> Nil {
  let selector = process.new_selector() |> process.select(subscriber)

  let event = process.selector_receive_forever(selector)
  dispatch(StatsEventReceived(event))
  // Keep listening
  listen_loop(subscriber, dispatch)
}

// VIEW

/// Renders the stats cards grid with the given counts
pub fn render_stats_grid(
  record_count: Int,
  actor_count: Int,
  lexicon_count: Int,
) -> Element(msg) {
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
    // Lexicons stat card (static for now)
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

fn view(model: Model) -> Element(Msg) {
  html.div([attribute.class("font-mono")], [
    // Include Tailwind styles in the Shadow DOM
    element.element(
      "link",
      [
        attribute.attribute("rel", "stylesheet"),
        attribute.attribute("href", "/styles.css"),
      ],
      [],
    ),
    render_stats_grid(model.record_count, model.actor_count, model.lexicon_count),
  ])
}
