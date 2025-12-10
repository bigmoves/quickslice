/// Lexicons Section
///
/// Displays file upload for lexicon ZIP files.
import components/button
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import pages/settings/helpers.{render_alert}
import pages/settings/types.{
  type Model, type Msg, SelectLexiconFile, UploadLexicons,
}

pub fn view(model: Model) -> Element(Msg) {
  html.div([attribute.class("bg-zinc-800/50 rounded p-6")], [
    html.h2([attribute.class("text-xl font-semibold text-zinc-300 mb-4")], [
      element.text("Lexicons"),
    ]),
    render_alert(model.lexicons_alert),
    html.div([attribute.class("space-y-4")], [
      html.div([attribute.class("mb-4")], [
        html.label([attribute.class("block text-sm text-zinc-400 mb-2")], [
          element.text("Upload Lexicons (ZIP)"),
        ]),
        html.input([
          attribute.type_("file"),
          attribute.accept([".zip"]),
          attribute.class(
            "font-mono px-4 py-2 text-sm text-zinc-300 bg-zinc-900 border border-zinc-800 rounded focus:outline-none focus:border-zinc-700 w-full",
          ),
          attribute.id("lexicon-file-input"),
          event.on_input(fn(_) { SelectLexiconFile }),
        ]),
      ]),
      html.p([attribute.class("text-sm text-zinc-500 mb-2")], [
        element.text("Upload a ZIP file containing your lexicon JSON files."),
      ]),
      html.p([attribute.class("text-sm text-zinc-500 mb-4")], [
        element.text(
          "NOTE: If you're swapping out lexicons with a totally new set, consider using RESET instead to clear all records as well.",
        ),
      ]),
      html.div([attribute.class("flex gap-3")], [
        button.button(disabled: False, on_click: UploadLexicons, text: "Upload"),
      ]),
    ]),
  ])
}
