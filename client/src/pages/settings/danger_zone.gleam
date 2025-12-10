/// Danger Zone Section
///
/// Displays reset functionality with confirmation.
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import pages/settings/helpers.{render_alert}
import pages/settings/types.{
  type Model, type Msg, SubmitReset, UpdateResetConfirmation,
}

pub fn view(model: Model) -> Element(Msg) {
  html.div([attribute.class("bg-zinc-800/50 rounded p-6")], [
    html.h2([attribute.class("text-xl font-semibold text-zinc-300 mb-4")], [
      element.text("Danger Zone"),
    ]),
    render_alert(model.danger_zone_alert),
    html.p([attribute.class("text-sm text-zinc-400 mb-4")], [
      element.text("This will clear all indexed data:"),
    ]),
    html.ul([attribute.class("text-sm text-zinc-400 mb-4 ml-4 list-disc")], [
      html.li([], [element.text("Domain authority configuration")]),
      html.li([], [element.text("All lexicon definitions")]),
      html.li([], [element.text("All indexed records")]),
      html.li([], [element.text("All actors")]),
    ]),
    html.p([attribute.class("text-sm text-zinc-400 mb-4")], [
      element.text("Records can be re-indexed via backfill."),
    ]),
    html.div([attribute.class("space-y-4")], [
      html.div([attribute.class("mb-4")], [
        html.label([attribute.class("block text-sm text-zinc-400 mb-2")], [
          element.text("Type RESET to confirm"),
        ]),
        html.input([
          attribute.type_("text"),
          attribute.class(
            "font-mono px-4 py-2 text-sm text-zinc-300 bg-zinc-900 border border-zinc-800 rounded focus:outline-none focus:border-zinc-700 w-full",
          ),
          attribute.placeholder("RESET"),
          attribute.value(model.reset_confirmation),
          event.on_input(UpdateResetConfirmation),
        ]),
      ]),
      html.div([attribute.class("flex gap-3")], [
        html.button(
          [
            attribute.class(
              "font-mono px-4 py-2 text-sm text-red-400 border border-red-900 hover:bg-red-900/30 rounded transition-colors cursor-pointer",
            ),
            attribute.disabled(model.reset_confirmation != "RESET"),
            event.on_click(SubmitReset),
          ],
          [element.text("Reset Everything")],
        ),
      ]),
    ]),
  ])
}
