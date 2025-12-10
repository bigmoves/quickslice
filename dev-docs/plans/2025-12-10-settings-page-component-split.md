# Settings Page Component Split Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Break the 1,126-line `settings.gleam` into a main orchestrator file plus 5 section-specific view modules for better maintainability.

**Architecture:** Central state with delegated views. `settings.gleam` keeps `Model`, `Msg`, `init()`, `update()`, and a thin `view()` that composes section views. Each section file exports a single `view()` function that receives the full model and returns `Element(Msg)`.

**Tech Stack:** Gleam, Lustre (Elm-like web framework), Tailwind CSS

---

## Task 1: Create settings directory and basic section module

**Files:**
- Create: `client/src/pages/settings/basic.gleam`

**Step 1: Create the settings directory**

```bash
mkdir -p client/src/pages/settings
```

**Step 2: Create basic.gleam with the view function**

Create `client/src/pages/settings/basic.gleam`:

```gleam
/// Basic Settings Section
///
/// Displays form for domain authority, relay URL, PLC directory URL,
/// jetstream URL, and OAuth supported scopes.

import components/alert
import components/button
import generated/queries/get_settings
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import pages/settings.{
  type Model, type Msg, SubmitBasicSettings, UpdateDomainAuthorityInput,
  UpdateJetstreamUrlInput, UpdateOAuthSupportedScopesInput,
  UpdatePlcDirectoryUrlInput, UpdateRelayUrlInput,
}
import gleam/option.{None, Some}

pub fn view(
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
```

**Step 3: Verify it compiles**

```bash
cd client && gleam build
```

Expected: Compilation error because `pages/settings` doesn't export the types yet. This is expected - we'll fix it in Task 6.

**Step 4: Commit progress**

```bash
git add client/src/pages/settings/basic.gleam
git commit -m "feat: add settings/basic.gleam section module (not yet wired)"
```

---

## Task 2: Create lexicons section module

**Files:**
- Create: `client/src/pages/settings/lexicons.gleam`

**Step 1: Create lexicons.gleam**

Create `client/src/pages/settings/lexicons.gleam`:

```gleam
/// Lexicons Section
///
/// Displays file upload for lexicon ZIP files.

import components/alert
import components/button
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import pages/settings.{type Model, type Msg, SelectLexiconFile, UploadLexicons}
import gleam/option.{None, Some}

pub fn view(model: Model) -> Element(Msg) {
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
```

**Step 2: Commit progress**

```bash
git add client/src/pages/settings/lexicons.gleam
git commit -m "feat: add settings/lexicons.gleam section module (not yet wired)"
```

---

## Task 3: Create OAuth clients section module

**Files:**
- Create: `client/src/pages/settings/oauth_clients.gleam`

**Step 1: Create oauth_clients.gleam**

This is the largest section. Create `client/src/pages/settings/oauth_clients.gleam`:

```gleam
/// OAuth Clients Section
///
/// Displays OAuth client list with create/edit/delete functionality.

import components/alert
import components/button
import generated/queries/get_o_auth_clients
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import pages/settings.{
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
          button.button(disabled: False, on_click: SubmitEditClient, text: "Save"),
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
          html.code([attribute.class("text-zinc-300")], [element.text(client_id)]),
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
```

**Step 2: Commit progress**

```bash
git add client/src/pages/settings/oauth_clients.gleam
git commit -m "feat: add settings/oauth_clients.gleam section module (not yet wired)"
```

---

## Task 4: Create admin section module

**Files:**
- Create: `client/src/pages/settings/admin.gleam`

**Step 1: Create admin.gleam**

Create `client/src/pages/settings/admin.gleam`:

```gleam
/// Admin Management Section
///
/// Displays current admins list with add/remove functionality.

import components/alert
import components/button
import generated/queries/get_settings
import gleam/list
import gleam/option.{None, Some}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import pages/settings.{
  type Model, type Msg, CancelRemoveAdmin, ConfirmRemoveAdmin, SubmitAddAdmin,
  SubmitRemoveAdmin, UpdateNewAdminDid,
}

pub fn view(settings: get_settings.Settings, model: Model) -> Element(Msg) {
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
```

**Step 2: Commit progress**

```bash
git add client/src/pages/settings/admin.gleam
git commit -m "feat: add settings/admin.gleam section module (not yet wired)"
```

---

## Task 5: Create danger zone section module

**Files:**
- Create: `client/src/pages/settings/danger_zone.gleam`

**Step 1: Create danger_zone.gleam**

Create `client/src/pages/settings/danger_zone.gleam`:

```gleam
/// Danger Zone Section
///
/// Displays reset functionality with confirmation.

import components/alert
import gleam/option.{None, Some}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import pages/settings.{
  type Model, type Msg, SubmitReset, UpdateResetConfirmation,
}

pub fn view(model: Model) -> Element(Msg) {
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
```

**Step 2: Commit progress**

```bash
git add client/src/pages/settings/danger_zone.gleam
git commit -m "feat: add settings/danger_zone.gleam section module (not yet wired)"
```

---

## Task 6: Update main settings.gleam to use section modules

**Files:**
- Modify: `client/src/pages/settings.gleam`

**Step 1: Add imports for section modules at top of file (after line 104)**

Add these imports after the existing imports:

```gleam
import pages/settings/admin as admin_section
import pages/settings/basic as basic_section
import pages/settings/danger_zone as danger_zone_section
import pages/settings/lexicons as lexicons_section
import pages/settings/oauth_clients as oauth_clients_section
```

**Step 2: Replace the view function body (lines 314-322)**

Replace the section calls inside `squall_cache.Data(data) ->` block:

```gleam
          squall_cache.Data(data) ->
            html.div([attribute.class("space-y-6")], [
              basic_section.view(data.settings, model, is_saving),
              lexicons_section.view(model),
              oauth_clients_section.view(cache, model),
              admin_section.view(data.settings, model),
              danger_zone_section.view(model),
            ])
```

**Step 3: Delete the old section functions**

Remove these functions from `settings.gleam` (they're now in section modules):
- `basic_settings_section` (lines 333-469)
- `lexicons_section` (lines 471-516)
- `danger_zone_section` (lines 518-575)
- `oauth_clients_section` (lines 577-639)
- `new_client_form` (lines 641-757)
- `oauth_client_card` (lines 759-891)
- `edit_client_form` (lines 893-976)
- `delete_confirmation_dialog` (lines 978-1020)
- `admin_management_section` (lines 1022-1125)

After deletion, `settings.gleam` should end around line 332.

**Step 4: Build and verify**

```bash
cd client && gleam build
```

Expected: Build succeeds with no errors.

**Step 5: Commit the refactor**

```bash
git add client/src/pages/settings.gleam
git commit -m "refactor: wire up settings section modules in main view"
```

---

## Task 7: Run full build and test

**Step 1: Clean build**

```bash
cd client && gleam clean && gleam build
```

Expected: Build succeeds.

**Step 2: Run development server to verify UI works**

```bash
cd client && gleam run -m lustre/dev start
```

Expected: App starts, navigate to settings page, verify all sections render and function correctly.

**Step 3: Final commit**

```bash
git add -A
git commit -m "refactor: complete settings page component split

- Extract basic settings to settings/basic.gleam
- Extract lexicons upload to settings/lexicons.gleam
- Extract OAuth clients to settings/oauth_clients.gleam
- Extract admin management to settings/admin.gleam
- Extract danger zone to settings/danger_zone.gleam
- Main settings.gleam now orchestrates section views
- Reduces main file from 1126 to ~350 lines"
```

---

## Summary

| File | Lines | Purpose |
|------|-------|---------|
| `settings.gleam` | ~350 | Model, Msg, init, update, view orchestrator |
| `settings/basic.gleam` | ~120 | Domain authority, URLs, scopes form |
| `settings/lexicons.gleam` | ~60 | ZIP file upload |
| `settings/oauth_clients.gleam` | ~350 | Client CRUD with forms and cards |
| `settings/admin.gleam` | ~100 | Admin list and add/remove |
| `settings/danger_zone.gleam` | ~80 | Reset with confirmation |
