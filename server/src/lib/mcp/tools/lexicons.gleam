import database/repositories/lexicons
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/result
import sqlight

/// List all lexicons with summary info
pub fn list_lexicons(db: sqlight.Connection) -> Result(json.Json, String) {
  use lexicon_list <- result.try(
    lexicons.get_all(db)
    |> result.map_error(fn(_) { "Failed to fetch lexicons" }),
  )

  let summaries =
    list.map(lexicon_list, fn(lex) {
      // Extract type from JSON
      let lexicon_type = extract_lexicon_type(lex.json)

      json.object([
        #("nsid", json.string(lex.id)),
        #("type", json.string(lexicon_type)),
        #("createdAt", json.string(lex.created_at)),
      ])
    })

  Ok(json.object([#("lexicons", json.array(summaries, fn(x) { x }))]))
}

/// Get a single lexicon by NSID
pub fn get_lexicon(
  db: sqlight.Connection,
  nsid: String,
) -> Result(json.Json, String) {
  use lexicon_list <- result.try(
    lexicons.get(db, nsid)
    |> result.map_error(fn(_) { "Failed to fetch lexicon" }),
  )

  case lexicon_list {
    [] -> Error("Lexicon not found: " <> nsid)
    [lex, ..] -> {
      // Return the lexicon with its raw JSON definition as a string
      // The caller can parse it if needed
      Ok(
        json.object([
          #("nsid", json.string(lex.id)),
          #("definition", json.string(lex.json)),
        ]),
      )
    }
  }
}

/// Extract the main type from a lexicon JSON string
fn extract_lexicon_type(json_str: String) -> String {
  // Simple extraction - look for "type" in defs.main
  let decoder = {
    use defs <- decode.field("defs", {
      use main <- decode.field("main", {
        use t <- decode.field("type", decode.string)
        decode.success(t)
      })
      decode.success(main)
    })
    decode.success(defs)
  }

  case json.parse(json_str, decoder) {
    Ok(t) -> t
    Error(_) -> "unknown"
  }
}
