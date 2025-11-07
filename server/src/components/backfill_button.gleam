import backfill
import backfill_state
import components/button
import config
import database
import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option
import gleam/otp/actor
import lustre
import lustre/attribute
import lustre/effect
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import sqlight

// APP

pub fn component(
  db: sqlight.Connection,
  backfill_state_subject: process.Subject(backfill_state.Message),
  config_subject: process.Subject(config.Message),
) {
  lustre.application(
    init(db, backfill_state_subject, config_subject, _),
    update,
    view,
  )
}

// MODEL

pub type Model {
  Model(
    backfilling: Bool,
    is_admin: Bool,
    db: sqlight.Connection,
    backfill_state: process.Subject(backfill_state.Message),
    config: process.Subject(config.Message),
  )
}

fn init(
  db: sqlight.Connection,
  backfill_state_subject: process.Subject(backfill_state.Message),
  config_subject: process.Subject(config.Message),
  flags: #(Bool, Bool),
) -> #(Model, effect.Effect(Msg)) {
  let #(is_admin, backfilling) = flags
  let initial_effect = case backfilling {
    True -> start_polling()
    False -> effect.none()
  }

  #(
    Model(
      backfilling: backfilling,
      is_admin: is_admin,
      db: db,
      backfill_state: backfill_state_subject,
      config: config_subject,
    ),
    initial_effect,
  )
}

// UPDATE

pub opaque type Msg {
  UserClickedBackfill
  CheckBackfillState
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    UserClickedBackfill -> #(
      Model(..model, backfilling: True),
      effect.batch([
        do_backfill(model.db, model.backfill_state, model.config),
        start_polling(),
      ]),
    )

    CheckBackfillState -> {
      // Query the global backfill state
      let backfilling =
        actor.call(
          model.backfill_state,
          waiting: 100,
          sending: backfill_state.IsBackfilling,
        )

      // Update model and continue polling only if still backfilling
      case backfilling, model.backfilling {
        // Still backfilling - continue polling
        True, _ -> #(Model(..model, backfilling: True), start_polling())
        // Just completed (was backfilling, now not) - emit event to reload page
        False, True -> #(
          Model(..model, backfilling: False),
          event.emit("backfill-complete", json.null()),
        )
        // Was already not backfilling - do nothing
        False, False -> #(model, effect.none())
      }
    }
  }
}

// EFFECTS

fn do_backfill(
  db: sqlight.Connection,
  backfill_state_subject: process.Subject(backfill_state.Message),
  config_subject: process.Subject(config.Message),
) -> effect.Effect(Msg) {
  effect.from(fn(_dispatch) {
    // Update global state to indicate backfill is starting
    process.send(backfill_state_subject, backfill_state.StartBackfill)

    // Get domain authority from config
    let domain_authority = case config.get_domain_authority(config_subject) {
      option.Some(authority) -> authority
      option.None -> ""
    }

    // Spawn async process to run backfill without blocking the UI
    let _ =
      process.spawn_unlinked(fn() {
        // Run the backfill
        case database.get_record_type_lexicons(db) {
          Ok(lexicons) -> {
            let #(collections, external_collections) =
              lexicons
              |> list.partition(fn(lex) {
                backfill.nsid_matches_domain_authority(lex.id, domain_authority)
              })

            let collection_ids = list.map(collections, fn(lex) { lex.id })
            let external_collection_ids =
              list.map(external_collections, fn(lex) { lex.id })

            let config = backfill.default_config()

            // Run backfill (this will take time)
            let _ =
              backfill.backfill_collections(
                [],
                collection_ids,
                external_collection_ids,
                config,
                db,
              )

            // Backfill is complete, update global state
            process.send(backfill_state_subject, backfill_state.StopBackfill)
          }
          Error(_) -> {
            // No lexicons, stop backfill immediately
            process.send(backfill_state_subject, backfill_state.StopBackfill)
          }
        }
      })

    Nil
  })
}

fn start_polling() -> effect.Effect(Msg) {
  use dispatch <- effect.from

  // Spawn a process that waits 2 seconds then dispatches CheckBackfillState
  let _ =
    process.spawn_unlinked(fn() {
      process.sleep(2000)
      dispatch(CheckBackfillState)
    })

  Nil
}

// VIEW

fn view(model: Model) -> Element(Msg) {
  case model.is_admin {
    False -> element.none()
    True -> {
      let button_text = case model.backfilling {
        True -> "Backfilling..."
        False -> "Backfill Collections"
      }

      html.div([attribute.class("inline")], [
        button.button(
          disabled: model.backfilling,
          on_click: UserClickedBackfill,
          text: button_text,
        ),
      ])
    }
  }
}
