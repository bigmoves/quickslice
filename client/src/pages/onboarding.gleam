/// Onboarding Page Component
///
/// Displays the initial admin setup page for first-time configuration.
/// This page is shown when there are no admins configured in the system.
/// The user enters their handle and is redirected to the OAuth flow.
import components/actor_autocomplete
import components/button
import components/logo
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

pub type Msg {
  AutocompleteInput(String)
  AutocompleteSelect(String)
  AutocompleteKeydown(String)
  AutocompleteBlur
  AutocompleteFocus
}

pub type Model {
  Model
}

pub fn init() -> Model {
  Model
}

pub fn view(
  autocomplete_model: actor_autocomplete.Model,
  on_input: fn(String) -> msg,
  on_select: fn(String) -> msg,
  on_keydown: fn(String) -> msg,
  on_blur: fn() -> msg,
  on_focus: fn() -> msg,
) -> Element(msg) {
  html.div([attribute.class("max-w-md mx-auto mt-12")], [
    html.div(
      [attribute.class("bg-zinc-800/50 rounded p-8 border border-zinc-700")],
      [
        // Logo centered at top
        html.div([attribute.class("flex justify-center mb-6")], [
          logo.view("w-16 h-16"),
        ]),
        html.h1(
          [
            attribute.class(
              "text-2xl font-semibold text-zinc-300 mb-2 text-center",
            ),
          ],
          [
            element.text("Welcome to Quickslice"),
          ],
        ),
        html.p([attribute.class("text-zinc-400 mb-6 text-center")], [
          element.text("Set up an administrator account to get started."),
        ]),
        html.form(
          [
            attribute.action("/admin/oauth/authorize"),
            attribute.method("POST"),
            attribute.class("space-y-4"),
          ],
          [
            html.div([], [
              html.label([attribute.class("block text-sm text-zinc-400 mb-2")], [
                element.text("AT Protocol Handle"),
              ]),
              actor_autocomplete.view(
                autocomplete_model,
                "onboarding-handle",
                "login_hint",
                "user.bsky.social",
                "font-mono px-4 py-2 text-sm text-zinc-300 bg-zinc-900 border border-zinc-800 rounded focus:outline-none focus:border-zinc-700 w-full",
                on_input,
                on_select,
                on_keydown,
                on_blur,
                on_focus,
              ),
            ]),

            html.div([attribute.class("pt-2")], [
              button.submit(
                disabled: autocomplete_model.query == "",
                text: "Continue",
              ),
            ]),
          ],
        ),
      ],
    ),
  ])
}
