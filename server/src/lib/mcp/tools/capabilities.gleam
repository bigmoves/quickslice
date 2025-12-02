import database/repositories/lexicons
import gleam/json
import gleam/result
import sqlight

/// Get server capabilities
pub fn get_server_capabilities(
  db: sqlight.Connection,
) -> Result(json.Json, String) {
  // Get lexicon count for status
  let lexicon_count =
    lexicons.get_count(db)
    |> result.unwrap(0)

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
          #("lexiconCount", json.int(lexicon_count)),
          #("databaseConnected", json.bool(True)),
        ]),
      ),
    ]),
  )
}
