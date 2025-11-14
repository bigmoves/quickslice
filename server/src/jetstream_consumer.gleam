import backfill
import config
import database
import envoy
import event_handler
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option
import gleam/otp/actor
import gleam/string
import goose
import logging
import sqlight

/// Messages that can be sent to the Jetstream consumer actor
pub type Message {
  Stop(reply_with: process.Subject(Nil))
  Restart(reply_with: process.Subject(Result(Nil, String)))
}

/// Messages for cursor tracker actor
pub type CursorMessage {
  UpdateCursor(time_us: Int)
  FlushCursor(reply_with: process.Subject(Nil))
}

/// Internal state of the Jetstream consumer actor
type State {
  State(
    db: sqlight.Connection,
    consumer_pid: option.Option(process.Pid),
    cursor_tracker_pid: option.Option(process.Pid),
  )
}

/// State for cursor tracker
type CursorState {
  CursorState(
    db: sqlight.Connection,
    latest_cursor: option.Option(Int),
    last_flush_time: Int,
  )
}

/// Start the Jetstream consumer actor
pub fn start(db: sqlight.Connection) -> Result(process.Subject(Message), String) {
  case start_consumer_process(db) {
    Ok(consumer_pid) -> {
      let initial_state =
        State(
          db: db,
          consumer_pid: option.Some(consumer_pid),
          cursor_tracker_pid: option.None,
        )

      let result =
        actor.new(initial_state)
        |> actor.on_message(handle_message)
        |> actor.start

      case result {
        Ok(started) -> Ok(started.data)
        Error(err) ->
          Error("Failed to start consumer actor: " <> string.inspect(err))
      }
    }
    Error(err) -> {
      // Consumer failed to start, but we still create the actor so it can be restarted later
      logging.log(logging.Warning, "[jetstream] " <> err)
      let initial_state =
        State(
          db: db,
          consumer_pid: option.None,
          cursor_tracker_pid: option.None,
        )

      let result =
        actor.new(initial_state)
        |> actor.on_message(handle_message)
        |> actor.start

      case result {
        Ok(started) -> Ok(started.data)
        Error(actor_err) ->
          Error("Failed to start consumer actor: " <> string.inspect(actor_err))
      }
    }
  }
}

/// Stop the Jetstream consumer
pub fn stop(consumer: process.Subject(Message)) -> Nil {
  actor.call(consumer, waiting: 1000, sending: Stop)
}

/// Restart the Jetstream consumer with fresh lexicon data
pub fn restart(consumer: process.Subject(Message)) -> Result(Nil, String) {
  actor.call(consumer, waiting: 5000, sending: Restart)
}

/// Handle messages sent to the consumer actor
fn handle_message(state: State, message: Message) -> actor.Next(State, Message) {
  case message {
    Stop(client) -> {
      // Stop the consumer if it's running
      case state.consumer_pid {
        option.Some(pid) -> {
          logging.log(logging.Info, "[jetstream] Stopping consumer...")
          process.kill(pid)
          process.send(client, Nil)
          actor.continue(State(..state, consumer_pid: option.None))
        }
        option.None -> {
          process.send(client, Nil)
          actor.continue(state)
        }
      }
    }

    Restart(client) -> {
      // Stop old consumer if running
      case state.consumer_pid {
        option.Some(pid) -> {
          logging.log(logging.Info, "[jetstream] Stopping old consumer...")
          process.kill(pid)
        }
        option.None -> Nil
      }

      // Start new consumer with fresh lexicon data
      case start_consumer_process(state.db) {
        Ok(new_pid) -> {
          process.send(client, Ok(Nil))
          actor.continue(State(..state, consumer_pid: option.Some(new_pid)))
        }
        Error(err) -> {
          process.send(client, Error(err))
          actor.continue(State(..state, consumer_pid: option.None))
        }
      }
    }
  }
}

/// Get current timestamp in seconds (for tracking flush intervals)
@external(erlang, "erlang", "system_time")
fn erlang_system_time(unit: Int) -> Int

fn get_current_time_seconds() -> Int {
  // Time unit: 1 = seconds
  erlang_system_time(1)
}

/// Handle cursor tracker messages
fn handle_cursor_message(
  state: CursorState,
  message: CursorMessage,
) -> actor.Next(CursorState, CursorMessage) {
  case message {
    UpdateCursor(time_us) -> {
      let current_time = get_current_time_seconds()
      let time_since_last_flush = current_time - state.last_flush_time

      // Update latest cursor
      let new_state = CursorState(..state, latest_cursor: option.Some(time_us))

      // Flush every 5 seconds
      case time_since_last_flush >= 5 {
        True -> {
          // Flush the new cursor value (time_us)
          case database.set_jetstream_cursor(state.db, time_us) {
            Ok(_) -> {
              actor.continue(
                CursorState(
                  db: state.db,
                  last_flush_time: current_time,
                  latest_cursor: option.None,
                ),
              )
            }
            Error(err) -> {
              logging.log(
                logging.Error,
                "[jetstream] Failed to update cursor: " <> string.inspect(err),
              )
              // Keep the cursor in state so we can retry on next flush
              actor.continue(new_state)
            }
          }
        }
        False -> actor.continue(new_state)
      }
    }

    FlushCursor(client) -> {
      // Force flush current cursor
      case state.latest_cursor {
        option.Some(cursor) -> {
          case database.set_jetstream_cursor(state.db, cursor) {
            Ok(_) -> {
              process.send(client, Nil)
              actor.continue(
                CursorState(
                  ..state,
                  latest_cursor: option.None,
                  last_flush_time: get_current_time_seconds(),
                ),
              )
            }
            Error(err) -> {
              logging.log(
                logging.Error,
                "[jetstream] Failed to flush cursor: " <> string.inspect(err),
              )
              process.send(client, Nil)
              actor.continue(state)
            }
          }
        }
        option.None -> {
          process.send(client, Nil)
          actor.continue(state)
        }
      }
    }
  }
}

/// Start cursor tracker actor
fn start_cursor_tracker(
  db: sqlight.Connection,
  disable_cursor: Bool,
) -> option.Option(process.Subject(CursorMessage)) {
  case disable_cursor {
    True -> option.None
    False -> {
      let initial_state =
        CursorState(
          db: db,
          latest_cursor: option.None,
          last_flush_time: get_current_time_seconds(),
        )

      case
        actor.new(initial_state)
        |> actor.on_message(handle_cursor_message)
        |> actor.start
      {
        Ok(started) -> option.Some(started.data)
        Error(err) -> {
          logging.log(
            logging.Error,
            "[jetstream] Failed to start cursor tracker: "
              <> string.inspect(err),
          )
          option.None
        }
      }
    }
  }
}

/// Start the actual consumer process (extracted from original start function)
fn start_consumer_process(db: sqlight.Connection) -> Result(process.Pid, String) {
  logging.log(logging.Info, "")
  logging.log(logging.Info, "[jetstream] Starting Jetstream consumer...")

  // Get PLC directory URL from environment variable or use default
  let plc_url = case envoy.get("PLC_DIRECTORY_URL") {
    Ok(url) -> url
    Error(_) -> "https://plc.directory"
  }

  // Start config cache actor to get domain authority
  let assert Ok(config_subject) = config.start(db)

  // Get domain authority from config
  let domain_authority = case config.get_domain_authority(config_subject) {
    option.Some(authority) -> authority
    option.None -> ""
  }

  // Get all record-type lexicons from the database
  case database.get_record_type_lexicons(db) {
    Ok(lexicons) -> {
      // Separate lexicons by domain authority
      let #(local_lexicons, external_lexicons) =
        lexicons
        |> list.partition(fn(lex) {
          backfill.nsid_matches_domain_authority(lex.id, domain_authority)
        })

      let local_collection_ids = list.map(local_lexicons, fn(lex) { lex.id })
      let external_collection_ids =
        list.map(external_lexicons, fn(lex) { lex.id })

      // Combine all collections into a single list for unified consumer
      let all_collection_ids =
        list.append(local_collection_ids, external_collection_ids)

      case all_collection_ids {
        [] -> {
          logging.log(
            logging.Warning,
            "[jetstream] No collections found - skipping Jetstream consumer",
          )
          logging.log(logging.Info, "[jetstream]    Import lexicons first")
          logging.log(logging.Info, "")
          Error("No collections found")
        }
        _ -> {
          logging.log(
            logging.Info,
            "[jetstream] Listening to "
              <> int.to_string(list.length(local_collection_ids))
              <> " local collection(s) (all DIDs):",
          )
          list.each(local_collection_ids, fn(col) {
            logging.log(logging.Info, "[jetstream]    - " <> col)
          })

          case external_collection_ids {
            [] -> Nil
            _ -> {
              logging.log(logging.Info, "")
              logging.log(
                logging.Info,
                "[jetstream] Tracking "
                  <> int.to_string(list.length(external_collection_ids))
                  <> " external collection(s) (known DIDs only, filtered client-side):",
              )
              list.each(external_collection_ids, fn(col) {
                logging.log(logging.Info, "[jetstream]    - " <> col)
              })
            }
          }

          // Get Jetstream URL from environment variable or use default
          let jetstream_url = case envoy.get("JETSTREAM_URL") {
            Ok(url) -> url
            Error(_) -> "wss://jetstream2.us-west.bsky.network/subscribe"
          }

          // Check if cursor tracking is disabled via environment variable
          let disable_cursor = case envoy.get("JETSTREAM_DISABLE_CURSOR") {
            Ok(value) ->
              case string.lowercase(value) {
                "true" | "1" | "yes" -> True
                _ -> False
              }
            Error(_) -> False
          }

          // Read cursor from database unless disabled
          let cursor = case disable_cursor {
            True -> {
              logging.log(
                logging.Info,
                "[jetstream] Cursor tracking disabled via JETSTREAM_DISABLE_CURSOR",
              )
              option.None
            }
            False -> {
              case database.get_jetstream_cursor(db) {
                Ok(option.Some(cursor)) -> {
                  logging.log(
                    logging.Info,
                    "[jetstream] Resuming from cursor: "
                      <> int.to_string(cursor),
                  )
                  option.Some(cursor)
                }
                Ok(option.None) -> {
                  logging.log(
                    logging.Info,
                    "[jetstream] No cursor found, starting from live stream",
                  )
                  option.None
                }
                Error(err) -> {
                  logging.log(
                    logging.Error,
                    "[jetstream] Failed to read cursor: "
                      <> string.inspect(err)
                      <> ", starting from live stream",
                  )
                  option.None
                }
              }
            }
          }

          // Create unified Jetstream config for all collections (no DID filter - listen to all)
          let unified_config =
            goose.JetstreamConfig(
              endpoint: jetstream_url,
              wanted_collections: all_collection_ids,
              wanted_dids: [],
              cursor: cursor,
              max_message_size_bytes: option.None,
              compress: False,
              require_hello: False,
            )

          logging.log(logging.Info, "")
          logging.log(logging.Info, "[jetstream] Connecting to Jetstream...")
          logging.log(
            logging.Info,
            "[jetstream]    Endpoint: " <> jetstream_url,
          )
          logging.log(
            logging.Info,
            "[jetstream]    Collections: "
              <> int.to_string(list.length(all_collection_ids))
              <> " (all DIDs, filtered client-side for external)",
          )

          // Start cursor tracker
          let cursor_tracker = start_cursor_tracker(db, disable_cursor)

          // Start the unified consumer
          let local_collections = local_collection_ids
          let ext_collections = external_collection_ids
          let pid =
            process.spawn_unlinked(fn() {
              goose.start_consumer(unified_config, fn(event_json) {
                // Spawn each event into its own process so they don't block each other
                let _pid =
                  process.spawn_unlinked(fn() {
                    handle_jetstream_event(
                      db,
                      event_json,
                      local_collections,
                      ext_collections,
                      plc_url,
                      cursor_tracker,
                    )
                  })
                Nil
              })
            })

          logging.log(logging.Info, "")
          logging.log(logging.Info, "[jetstream] Jetstream consumer started")
          logging.log(logging.Info, "")

          Ok(pid)
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
  collection_ids: List(String),
  external_collection_ids: List(String),
  plc_url: String,
  cursor_tracker: option.Option(process.Subject(CursorMessage)),
) -> Nil {
  case goose.parse_event(event_json) {
    goose.CommitEvent(did, time_us, commit) -> {
      // Update cursor tracker with latest time_us
      case cursor_tracker {
        option.Some(tracker) -> process.send(tracker, UpdateCursor(time_us))
        option.None -> Nil
      }

      // Check if this is an external collection event
      let is_external =
        list.contains(external_collection_ids, commit.collection)

      // If external, only process if DID is known
      case is_external {
        True -> {
          case is_known_did(db, did) {
            True ->
              event_handler.handle_commit_event(
                db,
                did,
                time_us,
                commit,
                plc_url,
                collection_ids,
                external_collection_ids,
              )
            False -> Nil
          }
        }
        False -> {
          // Local collection - always process
          event_handler.handle_commit_event(
            db,
            did,
            time_us,
            commit,
            plc_url,
            collection_ids,
            external_collection_ids,
          )
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
