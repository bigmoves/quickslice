/// Settings Page Component
///
/// Displays system settings and configuration options
///
/// ```graphql
/// query GetSettings {
///   settings {
///     id
///     domainAuthority
///     adminDids
///     relayUrl
///     plcDirectoryUrl
///     jetstreamUrl
///     oauthSupportedScopes
///   }
/// }
/// ```
///
/// ```graphql
/// mutation UpdateSettings($domainAuthority: String, $adminDids: [String!], $relayUrl: String, $plcDirectoryUrl: String, $jetstreamUrl: String, $oauthSupportedScopes: String) {
///   updateSettings(domainAuthority: $domainAuthority, adminDids: $adminDids, relayUrl: $relayUrl, plcDirectoryUrl: $plcDirectoryUrl, jetstreamUrl: $jetstreamUrl, oauthSupportedScopes: $oauthSupportedScopes) {
///     id
///     domainAuthority
///     adminDids
///     relayUrl
///     plcDirectoryUrl
///     jetstreamUrl
///     oauthSupportedScopes
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
///
/// ```graphql
/// query GetOAuthClients {
///   oauthClients {
///     clientId
///     clientSecret
///     clientName
///     clientType
///     redirectUris
///     scope
///     createdAt
///   }
/// }
/// ```
///
/// ```graphql
/// mutation CreateOAuthClient($clientName: String!, $clientType: String!, $redirectUris: [String!]!, $scope: String!) {
///   createOAuthClient(clientName: $clientName, clientType: $clientType, redirectUris: $redirectUris, scope: $scope) {
///     clientId
///     clientSecret
///     clientName
///     clientType
///     redirectUris
///     scope
///     createdAt
///   }
/// }
/// ```
///
/// ```graphql
/// mutation UpdateOAuthClient($clientId: String!, $clientName: String!, $redirectUris: [String!]!, $scope: String!) {
///   updateOAuthClient(clientId: $clientId, clientName: $clientName, redirectUris: $redirectUris, scope: $scope) {
///     clientId
///     clientSecret
///     clientName
///     clientType
///     redirectUris
///     scope
///     createdAt
///   }
/// }
/// ```
///
/// ```graphql
/// mutation DeleteOAuthClient($clientId: String!) {
///   deleteOAuthClient(clientId: $clientId)
/// }
/// ```
import components/alert
import components/button
import generated/queries/get_o_auth_clients
import generated/queries/get_settings
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import squall_cache.{type Cache}

pub type Msg {
  UpdateDomainAuthorityInput(String)
  SelectLexiconFile
  UploadLexicons
  UpdateResetConfirmation(String)
  SubmitReset
  // Basic settings messages (domain authority + external services)
  UpdateRelayUrlInput(String)
  UpdatePlcDirectoryUrlInput(String)
  UpdateJetstreamUrlInput(String)
  UpdateOAuthSupportedScopesInput(String)
  SubmitBasicSettings
  // OAuth client messages
  ToggleNewClientForm
  UpdateNewClientName(String)
  UpdateNewClientType(String)
  UpdateNewClientRedirectUris(String)
  UpdateNewClientScope(String)
  SubmitNewClient
  StartEditClient(String)
  CancelEditClient
  UpdateEditClientName(String)
  UpdateEditClientRedirectUris(String)
  UpdateEditClientScope(String)
  SubmitEditClient
  ToggleSecretVisibility(String)
  ConfirmDeleteClient(String)
  CancelDeleteClient
  SubmitDeleteClient
  // Admin management messages
  UpdateNewAdminDid(String)
  SubmitAddAdmin
  ConfirmRemoveAdmin(String)
  CancelRemoveAdmin
  SubmitRemoveAdmin
}

pub type Model {
  Model(
    domain_authority_input: String,
    reset_confirmation: String,
    selected_file: Option(String),
    alert: Option(#(String, String)),
    // External services state
    relay_url_input: String,
    plc_directory_url_input: String,
    jetstream_url_input: String,
    oauth_supported_scopes_input: String,
    // Lexicon upload state
    lexicons_alert: Option(#(String, String)),
    // OAuth client state
    show_new_client_form: Bool,
    new_client_name: String,
    new_client_type: String,
    new_client_redirect_uris: String,
    new_client_scope: String,
    editing_client_id: Option(String),
    edit_client_name: String,
    edit_client_redirect_uris: String,
    edit_client_scope: String,
    visible_secrets: Set(String),
    delete_confirm_client_id: Option(String),
    oauth_alert: Option(#(String, String)),
    // Admin management state
    new_admin_did: String,
    remove_confirm_did: Option(String),
    admin_alert: Option(#(String, String)),
    danger_zone_alert: Option(#(String, String)),
  )
}

pub fn set_alert(model: Model, kind: String, message: String) -> Model {
  Model(..model, alert: Some(#(kind, message)))
}

pub fn clear_alert(model: Model) -> Model {
  Model(..model, alert: None)
}

pub fn set_oauth_alert(model: Model, kind: String, message: String) -> Model {
  Model(..model, oauth_alert: Some(#(kind, message)))
}

pub fn clear_oauth_alert(model: Model) -> Model {
  Model(..model, oauth_alert: None)
}

pub fn set_admin_alert(model: Model, kind: String, message: String) -> Model {
  Model(..model, admin_alert: Some(#(kind, message)))
}

pub fn clear_admin_alert(model: Model) -> Model {
  Model(..model, admin_alert: None)
}

pub fn set_danger_zone_alert(
  model: Model,
  kind: String,
  message: String,
) -> Model {
  Model(..model, danger_zone_alert: Some(#(kind, message)))
}

pub fn clear_danger_zone_alert(model: Model) -> Model {
  Model(..model, danger_zone_alert: None)
}

pub fn set_lexicons_alert(model: Model, kind: String, message: String) -> Model {
  Model(..model, lexicons_alert: Some(#(kind, message)))
}

pub fn clear_lexicons_alert(model: Model) -> Model {
  Model(..model, lexicons_alert: None)
}

pub fn init() -> Model {
  Model(
    domain_authority_input: "",
    reset_confirmation: "",
    selected_file: None,
    alert: None,
    relay_url_input: "",
    plc_directory_url_input: "",
    jetstream_url_input: "",
    oauth_supported_scopes_input: "",
    lexicons_alert: None,
    show_new_client_form: False,
    new_client_name: "",
    new_client_type: "PUBLIC",
    new_client_redirect_uris: "",
    new_client_scope: "atproto transition:generic",
    editing_client_id: None,
    edit_client_name: "",
    edit_client_redirect_uris: "",
    edit_client_scope: "",
    visible_secrets: set.new(),
    delete_confirm_client_id: None,
    oauth_alert: None,
    new_admin_did: "",
    remove_confirm_did: None,
    admin_alert: None,
    danger_zone_alert: None,
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
        // Settings sections
        case result {
          squall_cache.Loading ->
            html.div(
              [attribute.class("py-8 text-center text-zinc-600 text-sm")],
              [
                element.text("Loading settings..."),
              ],
            )

          squall_cache.Failed(msg) ->
            html.div(
              [attribute.class("py-8 text-center text-red-400 text-sm")],
              [
                element.text("Error: " <> msg),
              ],
            )

          squall_cache.Data(data) ->
            html.div([attribute.class("space-y-6")], [
              basic_settings_section(data.settings, model, is_saving),
              lexicons_section(model),
              oauth_clients_section(cache, model),
              admin_management_section(data.settings, model),
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

fn basic_settings_section(
  settings: get_settings.Settings,
  model: Model,
  is_saving: Bool,
) -> Element(Msg) {
  html.div([attribute.class("bg-zinc-800/50 rounded p-6")], [
    html.h2([attribute.class("text-xl font-semibold text-zinc-300 mb-4")], [
      element.text("Basic Settings"),
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

fn lexicons_section(model: Model) -> Element(Msg) {
  html.div([attribute.class("bg-zinc-800/50 rounded p-6")], [
    html.h2([attribute.class("text-xl font-semibold text-zinc-300 mb-4")], [
      element.text("Lexicons"),
    ]),
    // Alert message
    case model.lexicons_alert {
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
      html.p([attribute.class("text-sm text-zinc-500 mb-4")], [
        element.text("Upload a ZIP file containing lexicon JSON files."),
      ]),
      html.div([attribute.class("flex gap-3")], [
        button.button(disabled: False, on_click: UploadLexicons, text: "Upload"),
      ]),
    ]),
  ])
}

fn danger_zone_section(model: Model) -> Element(Msg) {
  html.div([attribute.class("bg-zinc-800/50 rounded p-6")], [
    html.h2([attribute.class("text-xl font-semibold text-zinc-300 mb-4")], [
      element.text("Danger Zone"),
    ]),
    case model.danger_zone_alert {
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

fn oauth_clients_section(cache: Cache, model: Model) -> Element(Msg) {
  let variables = json.object([])
  let #(_cache, result) =
    squall_cache.lookup(
      cache,
      "GetOAuthClients",
      variables,
      get_o_auth_clients.parse_get_o_auth_clients_response,
    )

  html.div([attribute.class("bg-zinc-800/50 rounded p-6")], [
    html.h2([attribute.class("text-xl font-semibold text-zinc-300 mb-4")], [
      element.text("OAuth Clients"),
    ]),
    // OAuth alert message
    case model.oauth_alert {
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
    // New client button / form
    case model.show_new_client_form {
      False ->
        html.div([attribute.class("mb-4")], [
          button.button(
            disabled: False,
            on_click: ToggleNewClientForm,
            text: "Register New Client",
          ),
        ])
      True -> new_client_form(model)
    },
    // Client list
    case result {
      squall_cache.Loading ->
        html.div([attribute.class("py-4 text-center text-zinc-600 text-sm")], [
          element.text("Loading clients..."),
        ])
      squall_cache.Failed(msg) ->
        html.div([attribute.class("py-4 text-center text-red-400 text-sm")], [
          element.text("Error: " <> msg),
        ])
      squall_cache.Data(data) ->
        html.div(
          [attribute.class("space-y-3")],
          list.map(data.oauth_clients, fn(client) {
            oauth_client_card(client, model)
          }),
        )
    },
    // Delete confirmation dialog
    case model.delete_confirm_client_id {
      Some(client_id) -> delete_confirmation_dialog(client_id)
      None -> element.none()
    },
  ])
}

fn new_client_form(model: Model) -> Element(Msg) {
  html.div(
    [attribute.class("bg-zinc-900 rounded p-4 mb-4 border border-zinc-700")],
    [
      html.h3([attribute.class("text-lg font-semibold text-zinc-300 mb-3")], [
        element.text("Register New Client"),
      ]),
      html.div([attribute.class("space-y-3")], [
        // Name input
        html.div([], [
          html.label([attribute.class("block text-sm text-zinc-400 mb-1")], [
            element.text("Client Name"),
          ]),
          html.input([
            attribute.type_("text"),
            attribute.class(
              "font-mono px-3 py-2 text-sm text-zinc-300 bg-zinc-800 border border-zinc-700 rounded w-full",
            ),
            attribute.placeholder("My Application"),
            attribute.value(model.new_client_name),
            event.on_input(UpdateNewClientName),
          ]),
        ]),
        // Client Type dropdown
        html.div([], [
          html.label([attribute.class("block text-sm text-zinc-400 mb-1")], [
            element.text("Client Type"),
          ]),
          html.select(
            [
              attribute.class(
                "font-mono px-3 py-2 text-sm text-zinc-300 bg-zinc-800 border border-zinc-700 rounded w-full",
              ),
              event.on_change(UpdateNewClientType),
            ],
            [
              html.option(
                [
                  attribute.value("PUBLIC"),
                  attribute.selected(model.new_client_type == "PUBLIC"),
                ],
                "Public",
              ),
              html.option(
                [
                  attribute.value("CONFIDENTIAL"),
                  attribute.selected(model.new_client_type == "CONFIDENTIAL"),
                ],
                "Confidential",
              ),
            ],
          ),
          html.p([attribute.class("text-xs text-zinc-500 mt-1")], [
            element.text(case model.new_client_type {
              "CONFIDENTIAL" ->
                "For server-side apps that can securely store a client secret"
              _ ->
                "For browser apps (SPAs) and mobile apps that cannot securely store a secret"
            }),
          ]),
        ]),
        // Redirect URIs
        html.div([], [
          html.label([attribute.class("block text-sm text-zinc-400 mb-1")], [
            element.text("Redirect URIs (one per line)"),
          ]),
          html.textarea(
            [
              attribute.class(
                "font-mono px-3 py-2 text-sm text-zinc-300 bg-zinc-800 border border-zinc-700 rounded w-full h-20",
              ),
              attribute.placeholder("http://localhost:3000/callback"),
              attribute.value(model.new_client_redirect_uris),
              event.on_input(UpdateNewClientRedirectUris),
            ],
            "",
          ),
        ]),
        // Scope input
        html.div([], [
          html.label([attribute.class("block text-sm text-zinc-400 mb-1")], [
            element.text("Scope"),
          ]),
          html.input([
            attribute.type_("text"),
            attribute.class(
              "font-mono px-3 py-2 text-sm text-zinc-300 bg-zinc-800 border border-zinc-700 rounded w-full",
            ),
            attribute.placeholder("atproto transition:generic"),
            attribute.value(model.new_client_scope),
            event.on_input(UpdateNewClientScope),
          ]),
          html.p([attribute.class("text-xs text-zinc-500 mt-1")], [
            element.text("Space-separated OAuth scopes"),
          ]),
        ]),
        // Buttons
        html.div([attribute.class("flex gap-2")], [
          button.button(
            disabled: False,
            on_click: SubmitNewClient,
            text: "Create",
          ),
          html.button(
            [
              attribute.class(
                "font-mono px-4 py-2 text-sm text-zinc-400 hover:text-zinc-300 rounded transition-colors cursor-pointer",
              ),
              event.on_click(ToggleNewClientForm),
            ],
            [element.text("Cancel")],
          ),
        ]),
      ]),
    ],
  )
}

fn oauth_client_card(
  client: get_o_auth_clients.OAuthClient,
  model: Model,
) -> Element(Msg) {
  let is_editing = model.editing_client_id == Some(client.client_id)
  let secret_visible = set.contains(model.visible_secrets, client.client_id)

  case is_editing {
    True -> edit_client_form(client, model)
    False ->
      html.div(
        [attribute.class("bg-zinc-900 rounded p-4 border border-zinc-700")],
        [
          html.div([attribute.class("flex justify-between items-start mb-2")], [
            html.div([], [
              html.span([attribute.class("text-zinc-300 font-medium")], [
                element.text(client.client_name),
              ]),
            ]),
            html.div([attribute.class("flex gap-2")], [
              html.button(
                [
                  attribute.class(
                    "text-sm text-zinc-400 hover:text-zinc-300 cursor-pointer",
                  ),
                  event.on_click(StartEditClient(client.client_id)),
                ],
                [element.text("Edit")],
              ),
              html.button(
                [
                  attribute.class(
                    "text-sm text-red-400 hover:text-red-300 cursor-pointer",
                  ),
                  event.on_click(ConfirmDeleteClient(client.client_id)),
                ],
                [element.text("Delete")],
              ),
            ]),
          ]),
          // Client ID
          html.div([attribute.class("mb-2")], [
            html.span([attribute.class("text-xs text-zinc-500")], [
              element.text("Client ID: "),
            ]),
            html.code([attribute.class("text-xs text-zinc-400 font-mono")], [
              element.text(client.client_id),
            ]),
          ]),
          // Client Type
          html.div([attribute.class("mb-2")], [
            html.span([attribute.class("text-xs text-zinc-500")], [
              element.text("Type: "),
            ]),
            html.span([attribute.class("text-xs text-zinc-400")], [
              element.text(case client.client_type {
                "PUBLIC" -> "Public"
                "CONFIDENTIAL" -> "Confidential"
                _ -> client.client_type
              }),
            ]),
          ]),
          // Client Secret (if confidential)
          case client.client_secret {
            Some(secret) ->
              html.div([attribute.class("mb-2")], [
                html.span([attribute.class("text-xs text-zinc-500")], [
                  element.text("Secret: "),
                ]),
                case secret_visible {
                  True ->
                    html.code(
                      [attribute.class("text-xs text-zinc-400 font-mono")],
                      [
                        element.text(secret),
                      ],
                    )
                  False ->
                    html.code(
                      [attribute.class("text-xs text-zinc-400 font-mono")],
                      [
                        element.text("••••••••••••••••"),
                      ],
                    )
                },
                html.button(
                  [
                    attribute.class(
                      "ml-2 text-xs text-zinc-500 hover:text-zinc-400 cursor-pointer",
                    ),
                    event.on_click(ToggleSecretVisibility(client.client_id)),
                  ],
                  [
                    element.text(case secret_visible {
                      True -> "Hide"
                      False -> "Show"
                    }),
                  ],
                ),
              ])
            None -> element.none()
          },
          // Redirect URIs
          case client.redirect_uris {
            [] -> element.none()
            uris ->
              html.div([], [
                html.span([attribute.class("text-xs text-zinc-500")], [
                  element.text("Redirect URIs:"),
                ]),
                html.ul(
                  [attribute.class("text-xs text-zinc-400 font-mono ml-4")],
                  list.map(uris, fn(uri) { html.li([], [element.text(uri)]) }),
                ),
              ])
          },
          // Scope
          case client.scope {
            Some(scope) ->
              html.div([attribute.class("mt-2")], [
                html.span([attribute.class("text-xs text-zinc-500")], [
                  element.text("Scope: "),
                ]),
                html.code([attribute.class("text-xs text-zinc-400 font-mono")], [
                  element.text(scope),
                ]),
              ])
            None -> element.none()
          },
        ],
      )
  }
}

fn edit_client_form(
  client: get_o_auth_clients.OAuthClient,
  model: Model,
) -> Element(Msg) {
  html.div(
    [attribute.class("bg-zinc-900 rounded p-4 border border-amber-700")],
    [
      html.h3([attribute.class("text-lg font-semibold text-zinc-300 mb-3")], [
        element.text("Edit Client"),
      ]),
      html.div([attribute.class("space-y-3")], [
        // Client ID (read-only)
        html.div([], [
          html.label([attribute.class("block text-sm text-zinc-400 mb-1")], [
            element.text("Client ID"),
          ]),
          html.code([attribute.class("text-sm text-zinc-500 font-mono")], [
            element.text(client.client_id),
          ]),
        ]),
        // Name input
        html.div([], [
          html.label([attribute.class("block text-sm text-zinc-400 mb-1")], [
            element.text("Client Name"),
          ]),
          html.input([
            attribute.type_("text"),
            attribute.class(
              "font-mono px-3 py-2 text-sm text-zinc-300 bg-zinc-800 border border-zinc-700 rounded w-full",
            ),
            attribute.value(model.edit_client_name),
            event.on_input(UpdateEditClientName),
          ]),
        ]),
        // Redirect URIs
        html.div([], [
          html.label([attribute.class("block text-sm text-zinc-400 mb-1")], [
            element.text("Redirect URIs (one per line)"),
          ]),
          html.textarea(
            [
              attribute.class(
                "font-mono px-3 py-2 text-sm text-zinc-300 bg-zinc-800 border border-zinc-700 rounded w-full h-20",
              ),
              event.on_input(UpdateEditClientRedirectUris),
            ],
            model.edit_client_redirect_uris,
          ),
        ]),
        // Scope input
        html.div([], [
          html.label([attribute.class("block text-sm text-zinc-400 mb-1")], [
            element.text("Scope"),
          ]),
          html.input([
            attribute.type_("text"),
            attribute.class(
              "font-mono px-3 py-2 text-sm text-zinc-300 bg-zinc-800 border border-zinc-700 rounded w-full",
            ),
            attribute.value(model.edit_client_scope),
            event.on_input(UpdateEditClientScope),
          ]),
        ]),
        // Buttons
        html.div([attribute.class("flex gap-2")], [
          button.button(
            disabled: False,
            on_click: SubmitEditClient,
            text: "Save",
          ),
          html.button(
            [
              attribute.class(
                "font-mono px-4 py-2 text-sm text-zinc-400 hover:text-zinc-300 rounded transition-colors cursor-pointer",
              ),
              event.on_click(CancelEditClient),
            ],
            [element.text("Cancel")],
          ),
        ]),
      ]),
    ],
  )
}

fn delete_confirmation_dialog(client_id: String) -> Element(Msg) {
  html.div(
    [
      attribute.class(
        "fixed inset-0 bg-black/50 flex items-center justify-center z-50",
      ),
    ],
    [
      html.div([attribute.class("bg-zinc-800 rounded p-6 max-w-md")], [
        html.h3([attribute.class("text-lg font-semibold text-zinc-300 mb-3")], [
          element.text("Delete Client?"),
        ]),
        html.p([attribute.class("text-sm text-zinc-400 mb-4")], [
          element.text("Are you sure you want to delete client "),
          html.code([attribute.class("text-zinc-300")], [
            element.text(client_id),
          ]),
          element.text("? This action cannot be undone."),
        ]),
        html.div([attribute.class("flex gap-2 justify-end")], [
          html.button(
            [
              attribute.class(
                "font-mono px-4 py-2 text-sm text-zinc-400 hover:text-zinc-300 rounded transition-colors cursor-pointer",
              ),
              event.on_click(CancelDeleteClient),
            ],
            [element.text("Cancel")],
          ),
          html.button(
            [
              attribute.class(
                "font-mono px-4 py-2 text-sm text-red-400 border border-red-900 hover:bg-red-900/30 rounded transition-colors cursor-pointer",
              ),
              event.on_click(SubmitDeleteClient),
            ],
            [element.text("Delete")],
          ),
        ]),
      ]),
    ],
  )
}

/// Admin management section showing current admins and add admin form
fn admin_management_section(
  settings: get_settings.Settings,
  model: Model,
) -> Element(Msg) {
  html.div([attribute.class("bg-zinc-800/50 rounded p-6")], [
    html.h2([attribute.class("text-xl font-semibold text-zinc-300 mb-4")], [
      element.text("Admin Management"),
    ]),
    // Admin alert
    case model.admin_alert {
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
    // Current admins list
    html.div([attribute.class("mb-6")], [
      html.h3([attribute.class("text-sm font-medium text-zinc-400 mb-2")], [
        element.text("Current Admins"),
      ]),
      html.ul([attribute.class("space-y-2")], {
        list.map(settings.admin_dids, fn(did) {
          html.li(
            [
              attribute.class(
                "flex items-center justify-between bg-zinc-900/50 px-3 py-2 rounded border border-zinc-800",
              ),
            ],
            [
              html.span(
                [attribute.class("font-mono text-sm text-zinc-300 truncate")],
                [element.text(did)],
              ),
              case model.remove_confirm_did {
                Some(confirm_did) if confirm_did == did ->
                  html.div([attribute.class("flex gap-2")], [
                    html.button(
                      [
                        attribute.class(
                          "text-xs px-2 py-1 text-zinc-400 hover:text-zinc-300 cursor-pointer",
                        ),
                        event.on_click(CancelRemoveAdmin),
                      ],
                      [element.text("Cancel")],
                    ),
                    html.button(
                      [
                        attribute.class(
                          "text-xs px-2 py-1 text-red-400 hover:bg-red-900/30 rounded cursor-pointer",
                        ),
                        event.on_click(SubmitRemoveAdmin),
                      ],
                      [element.text("Confirm Remove")],
                    ),
                  ])
                _ ->
                  html.button(
                    [
                      attribute.class(
                        "text-xs px-2 py-1 text-zinc-500 hover:text-red-400 transition-colors cursor-pointer",
                      ),
                      event.on_click(ConfirmRemoveAdmin(did)),
                    ],
                    [element.text("Remove")],
                  )
              },
            ],
          )
        })
      }),
    ]),
    // Add admin form
    html.div([attribute.class("border-t border-zinc-800 pt-4")], [
      html.h3([attribute.class("text-sm font-medium text-zinc-400 mb-2")], [
        element.text("Add Admin"),
      ]),
      html.div([attribute.class("flex gap-2")], [
        html.input([
          attribute.type_("text"),
          attribute.class(
            "flex-1 font-mono px-4 py-2 text-sm text-zinc-300 bg-zinc-900 border border-zinc-800 rounded focus:outline-none focus:border-zinc-700",
          ),
          attribute.placeholder("did:plc:... or did:web:..."),
          attribute.value(model.new_admin_did),
          event.on_input(UpdateNewAdminDid),
        ]),
        button.button(
          disabled: model.new_admin_did == "",
          on_click: SubmitAddAdmin,
          text: "Add Admin",
        ),
      ]),
      html.p([attribute.class("text-xs text-zinc-500 mt-2")], [
        element.text("Enter the DID of the user you want to make an admin."),
      ]),
    ]),
  ])
}
