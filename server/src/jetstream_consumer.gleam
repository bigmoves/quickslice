import backfill
import database
import envoy
import event_handler
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/string
import goose
import sqlight

/// Start the Jetstream consumer in a background process
pub fn start(db: sqlight.Connection) -> Result(Nil, String) {
  io.println("")
  io.println("🚀 Starting Jetstream consumer...")

  // Get all record-type lexicons from the database
  case database.get_record_type_lexicons(db) {
    Ok(lexicons) -> {
      // Separate lexicons by domain authority
      let #(local_lexicons, external_lexicons) =
        lexicons
        |> list.partition(fn(lex) {
          backfill.nsid_matches_domain_authority(lex.id)
        })

      let local_collection_ids = list.map(local_lexicons, fn(lex) { lex.id })
      let external_collection_ids =
        list.map(external_lexicons, fn(lex) { lex.id })

      // Combine all collections into a single list for unified consumer
      let all_collection_ids =
        list.append(local_collection_ids, external_collection_ids)

      case all_collection_ids {
        [] -> {
          io.println("⚠️  No collections found - skipping Jetstream consumer")
          io.println("   Import lexicons first")
          io.println("")
          Ok(Nil)
        }
        _ -> {
          io.println(
            "📋 Listening to "
            <> int.to_string(list.length(local_collection_ids))
            <> " local collection(s) (all DIDs):",
          )
          list.each(local_collection_ids, fn(col) { io.println("   - " <> col) })

          case external_collection_ids {
            [] -> Nil
            _ -> {
              io.println("")
              io.println(
                "📋 Tracking "
                <> int.to_string(list.length(external_collection_ids))
                <> " external collection(s) (known DIDs only, filtered client-side):",
              )
              list.each(external_collection_ids, fn(col) {
                io.println("   - " <> col)
              })
            }
          }

          // Get Jetstream URL from environment variable or use default
          let jetstream_url = case envoy.get("JETSTREAM_URL") {
            Ok(url) -> url
            Error(_) -> "wss://jetstream2.us-west.bsky.network/subscribe"
          }

          // Create unified Jetstream config for all collections (no DID filter - listen to all)
          let unified_config =
            goose.JetstreamConfig(
              endpoint: jetstream_url,
              wanted_collections: all_collection_ids,
              wanted_dids: [],
              cursor: option.None,
              max_message_size_bytes: option.None,
              compress: True,
              require_hello: False,
            )

          io.println("")
          io.println("Connecting to Jetstream...")
          io.println("   Endpoint: " <> jetstream_url)
          io.println(
            "   Collections: "
            <> int.to_string(list.length(all_collection_ids))
            <> " (all DIDs, filtered client-side for external)",
          )

          // Start the unified consumer
          process.spawn_unlinked(fn() {
            goose.start_consumer(unified_config, fn(event_json) {
              handle_jetstream_event(
                db,
                event_json,
                external_collection_ids,
              )
            })
          })

          io.println("")
          io.println("Jetstream consumer started")
          io.println("")

          Ok(Nil)
        }
      }
    }
    Error(err) -> {
      Error("Failed to fetch lexicons: " <> string.inspect(err))
    }
  }
}

/// Check if a DID exists in the actor table
fn is_known_did(db: sqlight.Connection, did: String) -> Bool {
  let sql = "SELECT 1 FROM actor WHERE did = ? LIMIT 1"

  case
    sqlight.query(
      sql,
      on: db,
      with: [sqlight.text(did)],
      expecting: decode.at([0], decode.int),
    )
  {
    Ok(results) -> results != []
    Error(_) -> False
  }
}

/// Handle a raw Jetstream event JSON string
fn handle_jetstream_event(
  db: sqlight.Connection,
  event_json: String,
  external_collection_ids: List(String),
) -> Nil {
  case goose.parse_event(event_json) {
    goose.CommitEvent(did, _time_us, commit) -> {
      // Check if this is an external collection event
      let is_external =
        list.contains(external_collection_ids, commit.collection)

      // If external, only process if DID is known
      case is_external {
        True -> {
          case is_known_did(db, did) {
            True -> event_handler.handle_commit_event(db, did, commit)
            False -> Nil
          }
        }
        False -> {
          // Local collection - always process
          event_handler.handle_commit_event(db, did, commit)
        }
      }
    }
    goose.IdentityEvent(_did, _time_us, _identity) -> {
      // Silently ignore identity events
      Nil
    }
    goose.AccountEvent(_did, _time_us, _account) -> {
      // Silently ignore account events
      Nil
    }
    goose.UnknownEvent(_raw) -> {
      // Silently ignore unknown events
      Nil
    }
  }
}
