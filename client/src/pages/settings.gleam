/// Settings Page Component
///
/// Displays system settings and configuration options
///
/// ```graphql
/// query GetSettings {
///   settings {
///     id
///     domainAuthority
///     oauthClientId
///   }
/// }
/// ```
///
/// ```graphql
/// mutation UpdateDomainAuthority($domainAuthority: String!) {
///   updateDomainAuthority(domainAuthority: $domainAuthority) {
///     id
///     domainAuthority
///     oauthClientId
///   }
/// }
/// ```
///
/// ```graphql
/// mutation UploadLexicons($zipBase64: String!) {
///   uploadLexicons(zipBase64: $zipBase64)
/// }
/// ```
///
/// ```graphql
/// mutation ResetAll($confirm: String!) {
///   resetAll(confirm: $confirm)
/// }
/// ```
import components/alert
import generated/queries/get_settings
import gleam/json
import gleam/option.{type Option, None, Some}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import squall_cache.{type Cache}

pub type Msg {
  UpdateDomainAuthorityInput(String)
  SubmitDomainAuthority
  SelectLexiconFile
  UploadLexicons
  UpdateResetConfirmation(String)
  SubmitReset
}

pub type Model {
  Model(
    domain_authority_input: String,
    reset_confirmation: String,
    selected_file: Option(String),
    alert: Option(#(String, String)),
  )
}

pub fn set_alert(model: Model, kind: String, message: String) -> Model {
  Model(..model, alert: Some(#(kind, message)))
}

pub fn clear_alert(model: Model) -> Model {
  Model(..model, alert: None)
}

pub fn init() -> Model {
  Model(
    domain_authority_input: "",
    reset_confirmation: "",
    selected_file: None,
    alert: None,
  )
}

pub fn view(cache: Cache, model: Model, is_admin: Bool) -> Element(Msg) {
  // If not admin, show access denied message
  case is_admin {
    False ->
      html.div([attribute.class("max-w-2xl space-y-6")], [
        html.h1([attribute.class("text-2xl font-semibold text-zinc-300 mb-8")], [
          element.text("Settings"),
        ]),
        html.div(
          [
            attribute.class(
              "bg-zinc-800/50 rounded p-8 text-center border border-zinc-700",
            ),
          ],
          [
            html.p([attribute.class("text-zinc-400 mb-4")], [
              element.text("Access Denied"),
            ]),
            html.p([attribute.class("text-sm text-zinc-500")], [
              element.text(
                "You must be an administrator to access the settings page.",
              ),
            ]),
          ],
        ),
      ])

    True -> {
      let variables = json.object([])

      let #(_cache, result) =
        squall_cache.lookup(
          cache,
          "GetSettings",
          variables,
          get_settings.parse_get_settings_response,
        )

      // Check if there's a pending optimistic mutation
      let is_saving = has_pending_mutations(cache)

      html.div([attribute.class("max-w-2xl space-y-6")], [
        html.h1([attribute.class("text-2xl font-semibold text-zinc-300 mb-8")], [
          element.text("Settings"),
        ]),
    // Alert message
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
    // Settings sections
    case result {
      squall_cache.Loading ->
        html.div([attribute.class("py-8 text-center text-zinc-600 text-sm")], [
          element.text("Loading settings..."),
        ])

      squall_cache.Failed(msg) ->
        html.div([attribute.class("py-8 text-center text-red-400 text-sm")], [
          element.text("Error: " <> msg),
        ])

      squall_cache.Data(data) ->
        html.div([attribute.class("space-y-6")], [
          domain_authority_section(data.settings, model, is_saving),
          lexicons_section(model),
          oauth_section(data.settings),
          danger_zone_section(model),
        ])
    },
      ])
    }
  }
}

/// Check if there are any pending optimistic mutations
fn has_pending_mutations(cache: Cache) -> Bool {
  squall_cache.has_pending_mutations(cache)
}

fn domain_authority_section(
  _settings: get_settings.Settings,
  model: Model,
  is_saving: Bool,
) -> Element(Msg) {
  html.div([attribute.class("bg-zinc-800/50 rounded p-6")], [
    html.h2([attribute.class("text-xl font-semibold text-zinc-300 mb-4")], [
      element.text("Domain Authority"),
    ]),
    html.div([attribute.class("space-y-4")], [
      html.div([attribute.class("mb-4")], [
        html.label(
          [attribute.class("block text-sm text-zinc-400 mb-2")],
          [element.text("Domain Authority")],
        ),
        html.input([
          attribute.type_("text"),
          attribute.class(
            "font-mono px-4 py-2 text-sm text-zinc-300 bg-zinc-900 border border-zinc-800 rounded focus:outline-none focus:border-zinc-700 w-full",
          ),
          attribute.placeholder("e.g. com.example"),
          attribute.value(model.domain_authority_input),
          event.on_input(UpdateDomainAuthorityInput),
        ]),
      ]),
      html.p([attribute.class("text-sm text-zinc-500 mb-4")], [
        element.text(
          "The domain authority is used to determine which collections are considered \"primary\" vs \"external\" when backfilling records. For example, if the authority is \"xyz.statusphere\", then \"xyz.statusphere.status\" is treated as primary and \"app.bsky.actor.profile\" is external.",
        ),
      ]),
      html.div([attribute.class("flex gap-3")], [
        html.button(
          [
            attribute.class(case is_saving {
              True ->
                "font-mono px-4 py-2 text-sm text-zinc-500 bg-zinc-800 rounded cursor-not-allowed"
              False ->
                "font-mono px-4 py-2 text-sm text-zinc-300 bg-zinc-800 hover:bg-zinc-700 rounded transition-colors cursor-pointer"
            }),
            attribute.disabled(is_saving),
            event.on_click(SubmitDomainAuthority),
          ],
          [element.text(case is_saving {
            True -> "Saving..."
            False -> "Save"
          })],
        ),
      ]),
    ]),
  ])
}

fn oauth_section(settings: get_settings.Settings) -> Element(Msg) {
  html.div([attribute.class("bg-zinc-800/50 rounded p-6")], [
    html.h2([attribute.class("text-xl font-semibold text-zinc-300 mb-4")], [
      element.text("OAuth Configuration"),
    ]),
    case settings.oauth_client_id {
      Some(client_id) ->
        html.div([attribute.class("space-y-3")], [
          html.div([attribute.class("flex items-center gap-2")], [
            html.div([attribute.class("w-2 h-2 bg-green-500 rounded-full")], []),
            html.p([attribute.class("text-sm text-zinc-300")], [
              element.text("OAuth client registered"),
            ]),
          ]),
          html.div([attribute.class("bg-zinc-900/50 rounded p-3")], [
            html.p([attribute.class("text-xs text-zinc-500 mb-1")], [
              element.text("Client ID:"),
            ]),
            html.p([attribute.class("text-sm text-zinc-300 font-mono")], [
              element.text(client_id),
            ]),
          ]),
          html.p([attribute.class("text-sm text-zinc-500")], [
            element.text(
              "OAuth client credentials are stored in the database. Use \"Reset Everything\" to clear and trigger re-registration.",
            ),
          ]),
        ])
      None ->
        html.div([attribute.class("space-y-3")], [
          html.div([attribute.class("flex items-center gap-2")], [
            html.div([attribute.class("w-2 h-2 bg-zinc-500 rounded-full")], []),
            html.p([attribute.class("text-sm text-zinc-400")], [
              element.text("OAuth client not registered"),
            ]),
          ]),
          html.p([attribute.class("text-sm text-zinc-500")], [
            element.text(
              "Set ENABLE_OAUTH_AUTO_REGISTER=true in your .env file to enable automatic OAuth client registration. The server will automatically register with your configured AIP server on startup.",
            ),
          ]),
        ])
    },
  ])
}

fn lexicons_section(_model: Model) -> Element(Msg) {
  html.div([attribute.class("bg-zinc-800/50 rounded p-6")], [
    html.h2([attribute.class("text-xl font-semibold text-zinc-300 mb-4")], [
      element.text("Lexicons"),
    ]),
    html.div([attribute.class("space-y-4")], [
      html.div([attribute.class("mb-4")], [
        html.label(
          [attribute.class("block text-sm text-zinc-400 mb-2")],
          [element.text("Upload Lexicons (ZIP)")],
        ),
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
      html.p([attribute.class("text-sm text-zinc-500 mb-4")], [
        element.text(
          "Upload a ZIP file containing lexicon JSON files. The ZIP file will be extracted and all .json files will be imported into the database. This replaces the need to manually place lexicons in the priv/lexicons directory.",
        ),
      ]),
      html.div([attribute.class("flex gap-3")], [
        html.button(
          [
            attribute.class(
              "font-mono px-4 py-2 text-sm text-zinc-300 bg-zinc-800 hover:bg-zinc-700 rounded transition-colors cursor-pointer",
            ),
            event.on_click(UploadLexicons),
          ],
          [element.text("Upload")],
        ),
      ]),
    ]),
  ])
}

fn danger_zone_section(model: Model) -> Element(Msg) {
  html.div([attribute.class("bg-zinc-800/50 rounded p-6")], [
    html.h2([attribute.class("text-xl font-semibold text-zinc-300 mb-4")], [
      element.text("Danger Zone"),
    ]),
    html.p([attribute.class("text-sm text-zinc-400 mb-4")], [
      element.text("This will clear all indexed data:"),
    ]),
    html.ul([attribute.class("text-sm text-zinc-400 mb-4 ml-4 list-disc")], [
      html.li([], [element.text("Domain authority configuration")]),
      html.li([], [element.text("OAuth client credentials")]),
      html.li([], [element.text("All lexicon definitions")]),
      html.li([], [element.text("All indexed records")]),
      html.li([], [element.text("All actors")]),
    ]),
    html.p([attribute.class("text-sm text-zinc-400 mb-4")], [
      element.text("Records can be re-indexed via backfill."),
    ]),
    html.div([attribute.class("space-y-4")], [
      html.div([attribute.class("mb-4")], [
        html.label(
          [attribute.class("block text-sm text-zinc-400 mb-2")],
          [element.text("Type RESET to confirm")],
        ),
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
