/// Backfill Page Component
///
/// Allows authenticated users to trigger a backfill for a specific actor DID
///
/// ```graphql
/// mutation BackfillActor($did: String!) {
///   backfillActor(did: $did)
/// }
/// ```
import components/alert
import components/button
import gleam/option.{type Option, None, Some}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

pub type Msg {
  UpdateDidInput(String)
  SubmitBackfill
}

pub type Model {
  Model(
    did_input: String,
    is_submitting: Bool,
    alert: Option(#(String, String)),
  )
}

pub fn init() -> Model {
  Model(did_input: "", is_submitting: False, alert: None)
}

pub fn set_alert(model: Model, kind: String, message: String) -> Model {
  Model(..model, alert: Some(#(kind, message)))
}

pub fn clear_alert(model: Model) -> Model {
  Model(..model, alert: None)
}

pub fn set_submitting(model: Model, submitting: Bool) -> Model {
  Model(..model, is_submitting: submitting)
}

pub fn view(model: Model) -> Element(Msg) {
  let input_classes =
    "font-mono px-4 py-2 text-sm text-zinc-300 bg-zinc-900 border border-zinc-800 rounded focus:outline-none focus:border-zinc-700 w-full"
  let label_classes = "block text-sm font-medium text-zinc-400 mb-2"

  html.div([attribute.class("space-y-6")], [
    // Page header
    html.div([], [
      html.h1([attribute.class("text-xl font-bold text-zinc-100 mb-2")], [
        html.text("Backfill Actor"),
      ]),
      html.p([attribute.class("text-zinc-400 text-sm")], [
        html.text(
          "Sync all collections for a specific actor via CAR file repository sync.",
        ),
      ]),
    ]),
    // Alert
    case model.alert {
      Some(#(kind, message)) -> {
        let alert_kind = case kind {
          "success" -> alert.Success
          "error" -> alert.Error
          _ -> alert.Info
        }
        alert.alert(alert_kind, message)
      }
      None -> element.none()
    },
    // Form card
    html.div(
      [
        attribute.class(
          "bg-zinc-900/50 border border-zinc-800 rounded-lg p-6 max-w-xl",
        ),
      ],
      [
        html.div([attribute.class("mb-4")], [
          html.label([attribute.class(label_classes)], [
            html.text("Actor DID"),
          ]),
          html.input([
            attribute.type_("text"),
            attribute.class(input_classes),
            attribute.placeholder("did:plc:... or did:web:..."),
            attribute.value(model.did_input),
            event.on_input(UpdateDidInput),
          ]),
          html.p([attribute.class("text-xs text-zinc-500 mt-1")], [
            html.text(
              "Enter the DID of the actor whose records you want to sync.",
            ),
          ]),
        ]),
        html.div([attribute.class("flex items-center gap-4")], [
          button.button(
            disabled: model.is_submitting || model.did_input == "",
            on_click: SubmitBackfill,
            text: case model.is_submitting {
              True -> "Syncing..."
              False -> "Sync Actor"
            },
          ),
        ]),
      ],
    ),
  ])
}
