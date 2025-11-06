import components/alert
import components/input
import components/layout
import database
import gleam/option.{type Option}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import sqlight

/// Main view function that renders the settings page
pub fn view(
  db: sqlight.Connection,
  current_user: Option(#(String, String)),
  _is_admin: Bool,
  flash_kind: Option(String),
  flash_message: Option(String),
) -> Element(msg) {
  let data = fetch_settings(db)
  render(data, current_user, flash_kind, flash_message)
}

/// Settings data
pub type SettingsData {
  SettingsData(domain_authority: String)
}

/// Fetch current settings
fn fetch_settings(db: sqlight.Connection) -> SettingsData {
  let domain_authority = case database.get_config(db, "domain_authority") {
    Ok(authority) -> authority
    Error(_) -> ""
  }

  SettingsData(domain_authority: domain_authority)
}

/// Render the complete settings page
fn render(
  data: SettingsData,
  current_user: Option(#(String, String)),
  flash_kind: Option(String),
  flash_message: Option(String),
) -> Element(msg) {
  layout.page_with_header(
    title: "Settings - quickslice",
    content: [
      html.h1([attribute.class("text-2xl font-semibold text-zinc-300 mb-8")], [
        element.text("Settings"),
      ]),
      alert.maybe_alert(flash_kind, flash_message),
      render_settings_form(data),
    ],
    current_user: current_user,
    domain_authority: option.None,
  )
}

/// Render the settings form
fn render_settings_form(data: SettingsData) -> Element(msg) {
  html.div([attribute.class("max-w-2xl space-y-6")], [
    // Domain Authority Section
    html.div([attribute.class("bg-zinc-800/50 rounded p-6")], [
      html.h2([attribute.class("text-xl font-semibold text-zinc-300 mb-4")], [
        element.text("Domain Authority"),
      ]),
      html.form(
        [
          attribute.method("post"),
          attribute.action("/settings"),
        ],
        [
          input.form_text_input(
            label: "Domain Authority",
            name: "domain_authority",
            value: data.domain_authority,
            placeholder: "e.g. com.example",
            required: True,
          ),
          html.p([attribute.class("text-sm text-zinc-500 mb-4")], [
            element.text(
              "The domain authority is used to determine which collections are considered \"primary\" vs \"external\" when backfilling records. For example, if the authority is \"xyz.statusphere\", then \"xyz.statusphere.status\" is treated as primary and \"app.bsky.actor.profile\" is external.",
            ),
          ]),
          html.div([attribute.class("flex gap-3")], [
            html.button(
              [
                attribute.type_("submit"),
                attribute.class(
                  "font-mono px-4 py-2 text-sm text-zinc-300 bg-zinc-800 hover:bg-zinc-700 rounded transition-colors cursor-pointer",
                ),
              ],
              [element.text("Save")],
            ),
          ]),
        ],
      ),
    ]),
    // Lexicons Upload Section
    html.div([attribute.class("bg-zinc-800/50 rounded p-6")], [
      html.h2([attribute.class("text-xl font-semibold text-zinc-300 mb-4")], [
        element.text("Lexicons"),
      ]),
      html.form(
        [
          attribute.method("post"),
          attribute.action("/settings"),
          attribute.attribute("enctype", "multipart/form-data"),
        ],
        [
          input.form_file_input(
            label: "Upload Lexicons (ZIP)",
            name: "lexicons_zip",
            accept: ".zip",
            required: False,
          ),
          html.p([attribute.class("text-sm text-zinc-500 mb-4")], [
            element.text(
              "Upload a ZIP file containing lexicon JSON files. The ZIP file will be extracted and all .json files will be imported into the database. This replaces the need to manually place lexicons in the priv/lexicons directory.",
            ),
          ]),
          html.div([attribute.class("flex gap-3")], [
            html.button(
              [
                attribute.type_("submit"),
                attribute.class(
                  "font-mono px-4 py-2 text-sm text-zinc-300 bg-zinc-800 hover:bg-zinc-700 rounded transition-colors cursor-pointer",
                ),
              ],
              [element.text("Upload")],
            ),
          ]),
        ],
      ),
    ]),
    // Sign Out Section
    html.div([attribute.class("bg-zinc-800/50 rounded p-6")], [
      html.h2([attribute.class("text-xl font-semibold text-zinc-300 mb-4")], [
        element.text("Account"),
      ]),
      html.form(
        [attribute.method("post"), attribute.action("/logout")],
        [
          html.button(
            [
              attribute.type_("submit"),
              attribute.class(
                "font-mono px-4 py-2 text-sm text-zinc-400 border border-zinc-700 hover:border-zinc-600 hover:text-zinc-300 rounded transition-colors cursor-pointer",
              ),
            ],
            [element.text("Sign Out")],
          ),
        ],
      ),
    ]),
  ])
}
