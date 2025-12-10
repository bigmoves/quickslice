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
import generated/queries/get_settings
import gleam/json
import gleam/option.{None, Some}
import gleam/set
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import pages/settings/admin as admin_section
import pages/settings/basic as basic_section
import pages/settings/danger_zone as danger_zone_section
import pages/settings/lexicons as lexicons_section
import pages/settings/oauth_clients as oauth_clients_section
import pages/settings/types.{Model}
import squall_cache.{type Cache}

// Re-export types for backwards compatibility with existing code that imports from pages/settings
pub type Msg =
  types.Msg

pub type Model =
  types.Model

pub fn set_alert(
  model: types.Model,
  kind: String,
  message: String,
) -> types.Model {
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
              basic_section.view(data.settings, model, is_saving),
              lexicons_section.view(model),
              oauth_clients_section.view(cache, model),
              admin_section.view(data.settings, model),
              danger_zone_section.view(model),
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
