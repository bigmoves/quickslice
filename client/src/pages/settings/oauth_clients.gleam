/// OAuth Clients Section
///
/// Displays OAuth client list with create/edit/delete functionality.
import components/button
import generated/queries/get_o_auth_clients
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/set
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import pages/settings/helpers.{render_alert}
import pages/settings/types.{
  type Model, type Msg, CancelDeleteClient, CancelEditClient,
  ConfirmDeleteClient, StartEditClient, SubmitDeleteClient, SubmitEditClient,
  SubmitNewClient, ToggleNewClientForm, ToggleSecretVisibility,
  UpdateEditClientName, UpdateEditClientRedirectUris, UpdateEditClientScope,
  UpdateNewClientName, UpdateNewClientRedirectUris, UpdateNewClientScope,
  UpdateNewClientType,
}
import squall_cache.{type Cache}

pub fn view(cache: Cache, model: Model) -> Element(Msg) {
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
    render_alert(model.oauth_alert),
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
                      [element.text(secret)],
                    )
                  False ->
                    html.code(
                      [attribute.class("text-xs text-zinc-400 font-mono")],
                      [element.text("••••••••••••••••")],
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
