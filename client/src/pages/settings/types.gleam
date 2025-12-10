/// Shared types for settings page components
///
/// This module exists to solve a circular import problem:
/// - settings.gleam needs to import section modules to call their view() functions
/// - Section modules need Model and Msg types to render views
/// - Without this module, settings.gleam ↔ section modules would be circular
///
/// By extracting types here, we get: types.gleam ← settings.gleam
///                                   types.gleam ← section modules
import gleam/option.{type Option}
import gleam/set.{type Set}

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
