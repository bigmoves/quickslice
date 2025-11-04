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

      // For Jetstream, only subscribe to local collections
      // External collections will be filtered in the event handler based on known DIDs
      let wanted_collection_ids = local_collection_ids

      case wanted_collection_ids {
        [] -> {
          io.println(
            "⚠️  No local collections found - skipping Jetstream consumer",
          )
          io.println("   Import lexicons with matching domain authority first")
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
                <> " external collection(s) (known DIDs only):",
              )
              list.each(external_collection_ids, fn(col) {
                io.println("   - " <> col)
              })
            }
          }

          // Get Jetstream URL from environment variable or use default
          let jetstream_url = case envoy.get("JETSTREAM_URL") {
            Ok(url) -> url
            Error(_) -> "wss://jetstream2.us-east.bsky.network/subscribe"
          }

          // Create Jetstream config for local collections (no DID filter - listen to all)
          let local_config =
            goose.JetstreamConfig(
              endpoint: jetstream_url,
              wanted_collections: local_collection_ids,
              wanted_dids: [],
              cursor: option.None,
              max_message_size_bytes: option.None,
              compress: False,
              require_hello: False,
              max_backoff_seconds: 60,
              log_connection_events: True,
              log_retry_attempts: False,
            )

          io.println("")
          io.println("Connecting to Jetstream...")
          io.println("   Endpoint: " <> jetstream_url)
          io.println(
            "   Local collections: "
            <> int.to_string(list.length(local_collection_ids))
            <> " (all DIDs)",
          )

          // Start the local collections consumer
          process.spawn_unlinked(fn() {
            goose.start_consumer(local_config, fn(event_json) {
              handle_jetstream_event(db, event_json)
            })
          })

          // If we have external collections, start a second consumer with DID filter
          case external_collection_ids {
            [] -> Nil
            _ -> {
              // Get all known DIDs from the database
              case get_all_known_dids(db) {
                Ok(known_dids) -> {
                  case known_dids {
                    [] -> {
                      io.println(
                        "   External collections: "
                        <> int.to_string(list.length(external_collection_ids))
                        <> " (0 known DIDs - skipping)",
                      )
                      Nil
                    }
                    _ -> {
                      let external_config =
                        goose.JetstreamConfig(
                          endpoint: jetstream_url,
                          wanted_collections: external_collection_ids,
                          wanted_dids: known_dids,
                          cursor: option.None,
                          max_message_size_bytes: option.None,
                          compress: False,
                          require_hello: False,
                          max_backoff_seconds: 60,
                          log_connection_events: True,
                          log_retry_attempts: False,
                        )

                      io.println(
                        "   External collections: "
                        <> int.to_string(list.length(external_collection_ids))
                        <> " ("
                        <> int.to_string(list.length(known_dids))
                        <> " known DIDs)",
                      )

                      // Start the external collections consumer
                      process.spawn_unlinked(fn() {
                        goose.start_consumer(external_config, fn(event_json) {
                          handle_jetstream_event(db, event_json)
                        })
                      })

                      Nil
                    }
                  }
                }
                Error(_) -> {
                  io.println(
                    "   External collections: Failed to fetch known DIDs - skipping",
                  )
                  Nil
                }
              }
            }
          }

          io.println("")
          io.println("Jetstream consumer(s) started")
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

/// Get all known DIDs from the actor table
fn get_all_known_dids(db: sqlight.Connection) -> Result(List(String), String) {
  let sql = "SELECT did FROM actor"

  case
    sqlight.query(
      sql,
      on: db,
      with: [],
      expecting: decode.at([0], decode.string),
    )
  {
    Ok(dids) -> Ok(dids)
    Error(err) -> Error("Failed to fetch DIDs: " <> string.inspect(err))
  }
}

/// Handle a raw Jetstream event JSON string
fn handle_jetstream_event(db: sqlight.Connection, event_json: String) -> Nil {
  case goose.parse_event(event_json) {
    goose.CommitEvent(did, _time_us, commit) -> {
      event_handler.handle_commit_event(db, did, commit)
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
