/// Backfill endpoint handler
///
/// Handles /backfill POST endpoint for triggering collection backfills
import backfill
import config
import database/repositories/lexicons
import gleam/erlang/process
import gleam/http as gleam_http
import gleam/int
import gleam/list
import gleam/option
import sqlight
import wisp

/// Handle backfill request
/// Only accepts POST method to trigger backfill
pub fn handle(
  req: wisp.Request,
  db: sqlight.Connection,
  config_subject: process.Subject(config.Message),
) -> wisp.Response {
  case req.method {
    gleam_http.Post -> {
      // Get domain authority from config
      let domain_authority = case config.get_domain_authority(config_subject) {
        option.Some(authority) -> authority
        option.None -> ""
      }

      // Get all record-type lexicons
      case lexicons.get_record_types(db) {
        Ok(lexicons) -> {
          case lexicons {
            [] -> {
              wisp.response(200)
              |> wisp.set_header("content-type", "application/json")
              |> wisp.set_body(wisp.Text(
                "{\"status\": \"no_lexicons\", \"message\": \"No record-type lexicons found\"}",
              ))
            }
            _ -> {
              // Separate lexicons by domain authority
              let #(collections, external_collections) =
                lexicons
                |> list.partition(fn(lex) {
                  backfill.nsid_matches_domain_authority(
                    lex.id,
                    domain_authority,
                  )
                })

              let collection_ids = list.map(collections, fn(lex) { lex.id })
              let external_collection_ids =
                list.map(external_collections, fn(lex) { lex.id })

              // Run backfill in background process
              let backfill_config = backfill.default_config()
              process.spawn_unlinked(fn() {
                backfill.backfill_collections(
                  [],
                  collection_ids,
                  external_collection_ids,
                  backfill_config,
                  db,
                )
              })

              wisp.response(200)
              |> wisp.set_header("content-type", "application/json")
              |> wisp.set_body(wisp.Text(
                "{\"status\": \"started\", \"collections\": "
                <> int.to_string(list.length(collection_ids))
                <> ", \"external_collections\": "
                <> int.to_string(list.length(external_collection_ids))
                <> "}",
              ))
            }
          }
        }
        Error(_) -> {
          wisp.response(500)
          |> wisp.set_header("content-type", "application/json")
          |> wisp.set_body(wisp.Text(
            "{\"error\": \"database_error\", \"message\": \"Failed to fetch lexicons\"}",
          ))
        }
      }
    }
    _ -> {
      wisp.response(405)
      |> wisp.set_header("content-type", "application/json")
      |> wisp.set_body(wisp.Text(
        "{\"error\": \"method_not_allowed\", \"message\": \"Use POST to trigger backfill\"}",
      ))
    }
  }
}
