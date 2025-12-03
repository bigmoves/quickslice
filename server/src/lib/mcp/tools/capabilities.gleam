import database/repositories/config
import database/repositories/lexicons
import database/repositories/records
import gleam/json
import gleam/option.{None, Some}
import gleam/result
import sqlight

/// Get server capabilities
pub fn get_server_capabilities(
  db: sqlight.Connection,
) -> Result(json.Json, String) {
  // Get counts for status
  let lexicon_count =
    lexicons.get_count(db)
    |> result.unwrap(0)
  let record_count =
    records.get_count(db)
    |> result.unwrap(0)

  // Get config values using individual getters (handles env var precedence)
  let relay_url = config.get_relay_url(db)
  let jetstream_url = config.get_jetstream_url(db)
  let plc_directory_url = config.get_plc_directory_url(db)
  let oauth_supported_scopes = config.get_oauth_supported_scopes(db)
  let admin_dids = config.get_admin_dids(db)
  let domain_authority = case config.get(db, "domain_authority") {
    Ok(value) -> Some(value)
    Error(_) -> None
  }

  Ok(
    json.object([
      #("name", json.string("quickslice")),
      #("version", json.string("0.1.0")),
      #(
        "features",
        json.array(
          ["graphql", "subscriptions", "oauth", "backfill", "lexicon_import"],
          json.string,
        ),
      ),
      #("protocols", json.array(["atproto"], json.string)),
      #(
        "status",
        json.object([
          #("recordCount", json.int(record_count)),
          #("lexiconCount", json.int(lexicon_count)),
          #("databaseConnected", json.bool(True)),
        ]),
      ),
      #(
        "config",
        json.object([
          #("relayUrl", json.string(relay_url)),
          #("jetstreamUrl", json.string(jetstream_url)),
          #("plcDirectoryUrl", json.string(plc_directory_url)),
          #("oauthSupportedScopes", json.string(oauth_supported_scopes)),
          #("adminDids", json.array(admin_dids, json.string)),
          #("domainAuthority", case domain_authority {
            Some(value) -> json.string(value)
            None -> json.null()
          }),
        ]),
      ),
    ]),
  )
}
