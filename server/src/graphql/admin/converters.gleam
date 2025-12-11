/// Value converters for admin GraphQL API
///
/// Transform domain types to GraphQL value.Value objects
import database/types.{
  type ActivityBucket, type ActivityEntry, type Lexicon, type OAuthClient,
  client_type_to_string,
}
import gleam/list
import gleam/option.{None, Some}
import swell/value

/// Convert CurrentSession data to GraphQL value
pub fn current_session_to_value(
  did: String,
  handle: String,
  is_admin: Bool,
) -> value.Value {
  value.Object([
    #("did", value.String(did)),
    #("handle", value.String(handle)),
    #("isAdmin", value.Boolean(is_admin)),
  ])
}

/// Convert statistics counts to GraphQL value
pub fn statistics_to_value(
  record_count: Int,
  actor_count: Int,
  lexicon_count: Int,
) -> value.Value {
  value.Object([
    #("recordCount", value.Int(record_count)),
    #("actorCount", value.Int(actor_count)),
    #("lexiconCount", value.Int(lexicon_count)),
  ])
}

/// Convert ActivityBucket domain type to GraphQL value
pub fn activity_bucket_to_value(bucket: ActivityBucket) -> value.Value {
  let total = bucket.create_count + bucket.update_count + bucket.delete_count
  value.Object([
    #("timestamp", value.String(bucket.timestamp)),
    #("total", value.Int(total)),
    #("creates", value.Int(bucket.create_count)),
    #("updates", value.Int(bucket.update_count)),
    #("deletes", value.Int(bucket.delete_count)),
  ])
}

/// Convert ActivityEntry domain type to GraphQL value
pub fn activity_entry_to_value(entry: ActivityEntry) -> value.Value {
  let error_msg_value = case entry.error_message {
    Some(msg) -> value.String(msg)
    None -> value.Null
  }

  value.Object([
    #("id", value.Int(entry.id)),
    #("timestamp", value.String(entry.timestamp)),
    #("operation", value.String(entry.operation)),
    #("collection", value.String(entry.collection)),
    #("did", value.String(entry.did)),
    #("status", value.String(entry.status)),
    #("errorMessage", error_msg_value),
    #("eventJson", value.String(entry.event_json)),
  ])
}

/// Convert Settings data to GraphQL value
pub fn settings_to_value(
  domain_authority: String,
  admin_dids: List(String),
  relay_url: String,
  plc_directory_url: String,
  jetstream_url: String,
  oauth_supported_scopes: String,
) -> value.Value {
  value.Object([
    #("id", value.String("Settings:singleton")),
    #("domainAuthority", value.String(domain_authority)),
    #("adminDids", value.List(list.map(admin_dids, value.String))),
    #("relayUrl", value.String(relay_url)),
    #("plcDirectoryUrl", value.String(plc_directory_url)),
    #("jetstreamUrl", value.String(jetstream_url)),
    #("oauthSupportedScopes", value.String(oauth_supported_scopes)),
  ])
}

/// Convert OAuthClient domain type to GraphQL value
pub fn oauth_client_to_value(client: OAuthClient) -> value.Value {
  let secret_value = case client.client_secret {
    Some(s) -> value.String(s)
    None -> value.Null
  }
  let scope_value = case client.scope {
    Some(s) -> value.String(s)
    None -> value.Null
  }
  value.Object([
    #("clientId", value.String(client.client_id)),
    #("clientSecret", secret_value),
    #("clientName", value.String(client.client_name)),
    #("clientType", value.String(client_type_to_string(client.client_type))),
    #("redirectUris", value.List(list.map(client.redirect_uris, value.String))),
    #("scope", scope_value),
    #("createdAt", value.Int(client.created_at)),
  ])
}

/// Convert Lexicon domain type to GraphQL value
pub fn lexicon_to_value(lexicon: Lexicon) -> value.Value {
  value.Object([
    #("id", value.String(lexicon.id)),
    #("json", value.String(lexicon.json)),
    #("createdAt", value.String(lexicon.created_at)),
  ])
}
