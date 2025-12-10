/// Basic Settings Section
///
/// Displays form for domain authority, relay URL, PLC directory URL,
/// jetstream URL, and OAuth supported scopes.
import components/button
import generated/queries/get_settings
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import pages/settings/helpers.{render_alert}
import pages/settings/types.{
  type Model, type Msg, SubmitBasicSettings, UpdateDomainAuthorityInput,
  UpdateJetstreamUrlInput, UpdateOAuthSupportedScopesInput,
  UpdatePlcDirectoryUrlInput, UpdateRelayUrlInput,
}

pub fn view(
  settings: get_settings.Settings,
  model: Model,
  is_saving: Bool,
) -> Element(Msg) {
  html.div([attribute.class("bg-zinc-800/50 rounded p-6")], [
    html.h2([attribute.class("text-xl font-semibold text-zinc-300 mb-4")], [
      element.text("Basic Settings"),
    ]),
    render_alert(model.alert),
    html.form(
      [
        attribute.class("space-y-6"),
        event.on_submit(fn(_) { SubmitBasicSettings }),
      ],
      [
        // Domain Authority
        html.div([attribute.class("space-y-2")], [
          html.label([attribute.class("block text-sm text-zinc-400 mb-2")], [
            element.text("Domain Authority"),
          ]),
          html.input([
            attribute.type_("text"),
            attribute.class(
              "font-mono px-4 py-2 text-sm text-zinc-300 bg-zinc-900 border border-zinc-800 rounded focus:outline-none focus:border-zinc-700 w-full",
            ),
            attribute.placeholder("e.g. com.example"),
            attribute.value(model.domain_authority_input),
            attribute.required(True),
            event.on_input(UpdateDomainAuthorityInput),
          ]),
          html.p([attribute.class("text-xs text-zinc-500")], [
            element.text(
              "Determines which collections are considered \"primary\" vs \"external\" when backfilling records.",
            ),
          ]),
        ]),
        // Relay URL
        html.div([attribute.class("space-y-2")], [
          html.label([attribute.class("block text-sm text-zinc-400 mb-2")], [
            element.text("Relay URL"),
          ]),
          html.input([
            attribute.type_("text"),
            attribute.class(
              "font-mono px-4 py-2 text-sm text-zinc-300 bg-zinc-900 border border-zinc-800 rounded focus:outline-none focus:border-zinc-700 w-full",
            ),
            attribute.placeholder(settings.relay_url),
            attribute.value(model.relay_url_input),
            attribute.required(True),
            event.on_input(UpdateRelayUrlInput),
          ]),
          html.p([attribute.class("text-xs text-zinc-500")], [
            element.text("AT Protocol relay URL for backfill operations."),
          ]),
        ]),
        // PLC Directory URL
        html.div([attribute.class("space-y-2")], [
          html.label([attribute.class("block text-sm text-zinc-400 mb-2")], [
            element.text("PLC Directory URL"),
          ]),
          html.input([
            attribute.type_("text"),
            attribute.class(
              "font-mono px-4 py-2 text-sm text-zinc-300 bg-zinc-900 border border-zinc-800 rounded focus:outline-none focus:border-zinc-700 w-full",
            ),
            attribute.placeholder(settings.plc_directory_url),
            attribute.value(model.plc_directory_url_input),
            attribute.required(True),
            event.on_input(UpdatePlcDirectoryUrlInput),
          ]),
          html.p([attribute.class("text-xs text-zinc-500")], [
            element.text("PLC directory URL for DID resolution."),
          ]),
        ]),
        // Jetstream URL
        html.div([attribute.class("space-y-2")], [
          html.label([attribute.class("block text-sm text-zinc-400 mb-2")], [
            element.text("Jetstream URL"),
          ]),
          html.input([
            attribute.type_("text"),
            attribute.class(
              "font-mono px-4 py-2 text-sm text-zinc-300 bg-zinc-900 border border-zinc-800 rounded focus:outline-none focus:border-zinc-700 w-full",
            ),
            attribute.placeholder(settings.jetstream_url),
            attribute.value(model.jetstream_url_input),
            attribute.required(True),
            event.on_input(UpdateJetstreamUrlInput),
          ]),
          html.p([attribute.class("text-xs text-zinc-500")], [
            element.text("Jetstream WebSocket endpoint for real-time indexing."),
          ]),
        ]),
        // OAuth Supported Scopes
        html.div([attribute.class("space-y-2")], [
          html.label([attribute.class("block text-sm text-zinc-400 mb-2")], [
            element.text("OAuth Supported Scopes"),
          ]),
          html.input([
            attribute.type_("text"),
            attribute.class(
              "font-mono px-4 py-2 text-sm text-zinc-300 bg-zinc-900 border border-zinc-800 rounded focus:outline-none focus:border-zinc-700 w-full",
            ),
            attribute.placeholder(settings.oauth_supported_scopes),
            attribute.value(model.oauth_supported_scopes_input),
            attribute.required(True),
            event.on_input(UpdateOAuthSupportedScopesInput),
          ]),
          html.p([attribute.class("text-xs text-zinc-500")], [
            element.text(
              "Space-separated OAuth scopes supported by this server.",
            ),
          ]),
        ]),
        // Save button for all basic settings
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
