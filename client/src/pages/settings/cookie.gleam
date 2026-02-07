/// Cookie Settings Section
///
/// Displays form for SameSite and Secure cookie configuration.
import components/button
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import pages/settings/helpers.{render_alert}
import pages/settings/types.{
  type Model, type Msg, SubmitCookieSettings, UpdateCookieSameSiteInput,
  UpdateCookieSecureInput,
}

pub fn view(model: Model, is_saving: Bool) -> Element(Msg) {
  html.div([attribute.class("bg-zinc-800/50 rounded p-6")], [
    html.h2([attribute.class("text-xl font-semibold text-zinc-300 mb-4")], [
      element.text("Cookie Settings"),
    ]),
    render_alert(model.cookie_alert),
    html.form(
      [
        attribute.class("space-y-6"),
        event.on_submit(fn(_) { SubmitCookieSettings }),
      ],
      [
        // SameSite
        html.div([attribute.class("space-y-2")], [
          html.label([attribute.class("block text-sm text-zinc-400 mb-2")], [
            element.text("SameSite"),
          ]),
          html.select(
            [
              attribute.class(
                "font-mono px-4 py-2 text-sm text-zinc-300 bg-zinc-900 border border-zinc-800 rounded focus:outline-none focus:border-zinc-700 w-full",
              ),
              attribute.value(model.cookie_same_site_input),
              event.on_input(UpdateCookieSameSiteInput),
            ],
            [
              html.option([attribute.value("STRICT")], "Strict"),
              html.option([attribute.value("LAX")], "Lax"),
              html.option([attribute.value("NONE")], "None"),
            ],
          ),
          html.p([attribute.class("text-xs text-zinc-500")], [
            element.text(
              "Controls when cookies are sent with cross-site requests. Strict prevents all cross-site sending, Lax allows top-level navigations, None allows all cross-site requests (requires Secure).",
            ),
          ]),
        ]),
        // Secure
        html.div([attribute.class("space-y-2")], [
          html.label([attribute.class("block text-sm text-zinc-400 mb-2")], [
            element.text("Secure"),
          ]),
          html.select(
            [
              attribute.class(
                "font-mono px-4 py-2 text-sm text-zinc-300 bg-zinc-900 border border-zinc-800 rounded focus:outline-none focus:border-zinc-700 w-full",
              ),
              attribute.value(model.cookie_secure_input),
              event.on_input(UpdateCookieSecureInput),
            ],
            [
              html.option([attribute.value("AUTO")], "Auto"),
              html.option([attribute.value("ALWAYS")], "Always"),
              html.option([attribute.value("NEVER")], "Never"),
            ],
          ),
          html.p([attribute.class("text-xs text-zinc-500")], [
            element.text(
              "Controls the Secure flag on cookies. Auto detects based on the request protocol, Always forces HTTPS-only cookies, Never omits the Secure flag.",
            ),
          ]),
        ]),
        // Save button
        html.div([attribute.class("flex gap-3 pt-4")], [
          button.submit(disabled: is_saving, text: case is_saving {
            True -> "Saving..."
            False -> "Save"
          }),
        ]),
      ],
    ),
  ])
}
