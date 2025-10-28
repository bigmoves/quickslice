import database
import envoy
import event_handler
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import jetstream
import sqlight

/// Start the Jetstream consumer in a background process
pub fn start(db: sqlight.Connection) -> Result(Nil, String) {
  io.println("")
  io.println("🚀 Starting Jetstream consumer...")

  // Get all record-type lexicons from the database
  case database.get_record_type_lexicons(db) {
    Ok(lexicons) -> {
      let collection_ids = list.map(lexicons, fn(lex) { lex.id })

      case collection_ids {
        [] -> {
          io.println(
            "⚠️  No record-type lexicons found - skipping Jetstream consumer",
          )
          io.println("   Import lexicons first to enable real-time indexing")
          io.println("")
          Ok(Nil)
        }
        _ -> {
          io.println(
            "📋 Listening to "
            <> int.to_string(list.length(collection_ids))
            <> " collections:",
          )
          list.each(collection_ids, fn(col) { io.println("   - " <> col) })

          // Get Jetstream URL from environment variable or use default
          let jetstream_url = case envoy.get("JETSTREAM_URL") {
            Ok(url) -> url
            Error(_) -> "wss://jetstream2.us-east.bsky.network/subscribe"
          }

          // Create Jetstream config
          let config =
            jetstream.JetstreamConfig(
              endpoint: jetstream_url,
              wanted_collections: collection_ids,
              wanted_dids: [],
            )

          io.println("")
          io.println("🌐 Connecting to Jetstream...")
          io.println("   Endpoint: " <> config.endpoint)
          io.println("   DID filter: All DIDs (no filter)")
          io.println("")

          // Start the Jetstream consumer in a separate process
          // This will run independently and call our event handler callback
          process.spawn_unlinked(fn() {
            jetstream.start_consumer(config, fn(event_json) {
              handle_jetstream_event(db, event_json)
            })
          })

          io.println("✅ Jetstream consumer started")
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

/// Handle a raw Jetstream event JSON string
fn handle_jetstream_event(db: sqlight.Connection, event_json: String) -> Nil {
  case jetstream.parse_event(event_json) {
    jetstream.CommitEvent(did, _time_us, commit) -> {
      event_handler.handle_commit_event(db, did, commit)
    }
    jetstream.IdentityEvent(_did, _time_us, _identity) -> {
      // Silently ignore identity events
      Nil
    }
    jetstream.AccountEvent(_did, _time_us, _account) -> {
      // Silently ignore account events
      Nil
    }
    jetstream.UnknownEvent(_raw) -> {
      // Silently ignore unknown events
      Nil
    }
  }
}
