import database/repositories/config
import database/repositories/lexicons
import database/repositories/records
import gleam/dict
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
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

  // Get all config values in a single query
  let cfg = config.get_all(db)

  let relay_url =
    dict.get(cfg, "relay_url")
    |> result.unwrap(config.default_relay_url)
  let jetstream_url =
    dict.get(cfg, "jetstream_url")
    |> result.unwrap(config.default_jetstream_url)
  let plc_directory_url =
    dict.get(cfg, "plc_directory_url")
    |> result.unwrap(config.default_plc_directory_url)
  let oauth_supported_scopes =
    dict.get(cfg, "oauth_supported_scopes")
    |> result.unwrap(config.default_oauth_supported_scopes)
  let admin_dids = case dict.get(cfg, "admin_dids") {
    Ok(value) ->
      value
      |> string.split(",")
      |> list.map(string.trim)
      |> list.filter(fn(did) { !string.is_empty(did) })
    Error(_) -> []
  }
  let domain_authority = case dict.get(cfg, "domain_authority") {
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
