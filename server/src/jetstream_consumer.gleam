import backfill
import database/executor.{type Executor}
import database/jetstream
import database/repositories/config as config_repo
import database/repositories/lexicons
import envoy
import event_handler
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option
import gleam/otp/actor
import gleam/string
import gleam/time/timestamp
import goose
import logging

// ============================================================================
// CONFIGURATION
// ============================================================================

/// How long to wait without messages before forcing a restart (in milliseconds)
const heartbeat_timeout_ms = 300_000

// 5 minutes - restart when stuck

/// How often to check for heartbeat timeouts (in milliseconds)
const heartbeat_check_interval_ms = 300_000

// 5 minutes - check at the same interval as timeout for consistent triggering

// ============================================================================
// TYPES
// ============================================================================

/// Messages that can be sent to the consumer manager
pub type ManagerMessage {
  /// Update the last seen message timestamp
  MessageReceived(timestamp: Int)
  /// Check if we should restart due to timeout
  CheckHeartbeat
  /// Update the self subject after actor starts
  UpdateSelfSubject(process.Subject(ManagerMessage))
  /// Update the consumer subject
  UpdateConsumerSubject(option.Option(process.Subject(Message)))
  /// Manual restart request
  ManualRestart(reply_with: process.Subject(Result(Nil, String)))
  /// Manual stop request
  ManualStop(reply_with: process.Subject(Nil))
}

/// State for the consumer manager actor
pub type ManagerState {
  ManagerState(
    db: Executor,
    last_message_time_ms: Int,
    consumer_subject: option.Option(process.Subject(Message)),
    self_subject: process.Subject(ManagerMessage),
  )
}

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
    db: Executor,
    consumer_pid: option.Option(process.Pid),
    cursor_tracker_pid: option.Option(process.Pid),
  )
}

/// State for cursor tracker
type CursorState {
  CursorState(
    db: Executor,
    latest_cursor: option.Option(Int),
    last_flush_time: Int,
  )
}

// ============================================================================
// CONSUMER MANAGER
// ============================================================================

/// Start the consumer manager that spawns and monitors the consumer
pub fn start(db: Executor) -> Result(process.Subject(ManagerMessage), String) {
  let temp_subject = process.new_subject()

  let state =
    ManagerState(
      db: db,
      last_message_time_ms: get_current_time_milliseconds(),
      consumer_subject: option.None,
      self_subject: temp_subject,
    )

  let result =
    actor.new(state)
    |> actor.on_message(handle_manager_message)
    |> actor.start

  case result {
    Ok(started) -> {
      // Update the actor's state with its real subject
      process.send(started.data, UpdateSelfSubject(started.data))

      // Spawn the initial consumer actor
      case start_consumer_actor(db, option.Some(started.data)) {
        Ok(consumer_subject) -> {
          // Update manager state with consumer subject
          process.send(
            started.data,
            UpdateConsumerSubject(option.Some(consumer_subject)),
          )

          // Schedule the first heartbeat check
          process.send_after(
            started.data,
            heartbeat_check_interval_ms,
            CheckHeartbeat,
          )

          Ok(started.data)
        }
        Error(err) -> {
          // Consumer failed to start, but manager is running for future restarts
          logging.log(
            logging.Warning,
            "[jetstream] Consumer failed to start: " <> err,
          )

          // Schedule heartbeat check anyway (it will attempt restart)
          process.send_after(
            started.data,
            heartbeat_check_interval_ms,
            CheckHeartbeat,
          )

          Ok(started.data)
        }
      }
    }
    Error(err) ->
      Error("Failed to start manager actor: " <> string.inspect(err))
  }
}

/// Stop the Jetstream consumer
pub fn stop(manager: process.Subject(ManagerMessage)) -> Nil {
  // Send stop request through manager
  let _ = actor.call(manager, waiting: 1000, sending: ManualStop)
  Nil
}

/// Restart the Jetstream consumer with fresh lexicon data
pub fn restart(manager: process.Subject(ManagerMessage)) -> Result(Nil, String) {
  actor.call(manager, waiting: 5000, sending: ManualRestart)
}

/// Handle messages sent to the consumer manager
fn handle_manager_message(
  state: ManagerState,
  message: ManagerMessage,
) -> actor.Next(ManagerState, ManagerMessage) {
  case message {
    UpdateSelfSubject(subject) -> {
      // Update state with the real actor subject
      let new_state = ManagerState(..state, self_subject: subject)
      actor.continue(new_state)
    }

    UpdateConsumerSubject(subject) -> {
      let new_state = ManagerState(..state, consumer_subject: subject)
      actor.continue(new_state)
    }

    MessageReceived(timestamp) -> {
      // Update the last seen message time
      let new_state = ManagerState(..state, last_message_time_ms: timestamp)
      actor.continue(new_state)
    }

    CheckHeartbeat -> {
      let current_time = get_current_time_milliseconds()
      let time_since_last_message = current_time - state.last_message_time_ms

      // Debug logging for slow message rates
      case time_since_last_message > 60_000 {
        True ->
          logging.log(
            logging.Debug,
            "[jetstream] Health check: "
              <> int.to_string(time_since_last_message / 1000)
              <> "s since last message",
          )
        False -> Nil
      }

      case time_since_last_message > heartbeat_timeout_ms {
        True -> {
          // No messages received within timeout - force restart
          logging.log(
            logging.Warning,
            "[jetstream] No messages received for "
              <> int.to_string(time_since_last_message / 1000)
              <> " seconds. Restarting consumer...",
          )

          // Stop old consumer if running
          case state.consumer_subject {
            option.Some(subject) -> {
              let _ = actor.call(subject, waiting: 1000, sending: Stop)
              Nil
            }
            option.None -> Nil
          }

          // Start new consumer
          case start_consumer_actor(state.db, option.Some(state.self_subject)) {
            Ok(new_subject) -> {
              logging.log(logging.Info, "[jetstream] Consumer restarted")

              // Reset the timer and update state
              let new_state =
                ManagerState(
                  ..state,
                  last_message_time_ms: current_time,
                  consumer_subject: option.Some(new_subject),
                )

              // Schedule next check
              process.send_after(
                state.self_subject,
                heartbeat_check_interval_ms,
                CheckHeartbeat,
              )

              actor.continue(new_state)
            }
            Error(err) -> {
              logging.log(
                logging.Error,
                "[jetstream] Failed to restart consumer: " <> err,
              )

              // Schedule next check to retry
              process.send_after(
                state.self_subject,
                heartbeat_check_interval_ms,
                CheckHeartbeat,
              )

              actor.continue(
                ManagerState(..state, consumer_subject: option.None),
              )
            }
          }
        }
        False -> {
          // Still receiving messages - schedule next check
          process.send_after(
            state.self_subject,
            heartbeat_check_interval_ms,
            CheckHeartbeat,
          )
          actor.continue(state)
        }
      }
    }

    ManualRestart(client) -> {
      logging.log(logging.Info, "[jetstream] Manual restart requested")

      // Stop old consumer if running
      case state.consumer_subject {
        option.Some(subject) -> {
          let _ = actor.call(subject, waiting: 1000, sending: Stop)
          Nil
        }
        option.None -> Nil
      }

      // Start new consumer
      case start_consumer_actor(state.db, option.Some(state.self_subject)) {
        Ok(new_subject) -> {
          process.send(client, Ok(Nil))
          actor.continue(
            ManagerState(
              ..state,
              last_message_time_ms: get_current_time_milliseconds(),
              consumer_subject: option.Some(new_subject),
            ),
          )
        }
        Error(err) -> {
          process.send(client, Error(err))
          actor.continue(ManagerState(..state, consumer_subject: option.None))
        }
      }
    }

    ManualStop(client) -> {
      logging.log(logging.Info, "[jetstream] Manual stop requested")

      // Stop consumer if running
      case state.consumer_subject {
        option.Some(subject) -> {
          let _ = actor.call(subject, waiting: 1000, sending: Stop)
          process.send(client, Nil)
        }
        option.None -> process.send(client, Nil)
      }

      actor.continue(ManagerState(..state, consumer_subject: option.None))
    }
  }
}

// ============================================================================
// CONSUMER ACTOR
// ============================================================================

/// Start the Jetstream consumer actor (called by manager)
fn start_consumer_actor(
  db: Executor,
  manager: option.Option(process.Subject(ManagerMessage)),
) -> Result(process.Subject(Message), String) {
  case start_consumer_process(db, manager) {
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
      // Note: We pass option.None for manager since restarts go through the manager
      case start_consumer_process(state.db, option.None) {
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
fn get_current_time_seconds() -> Int {
  let #(seconds, _nanoseconds) =
    timestamp.system_time()
    |> timestamp.to_unix_seconds_and_nanoseconds
  seconds
}

/// Get current timestamp in milliseconds
fn get_current_time_milliseconds() -> Int {
  let #(seconds, nanoseconds) =
    timestamp.system_time()
    |> timestamp.to_unix_seconds_and_nanoseconds
  seconds * 1000 + nanoseconds / 1_000_000
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
          case jetstream.set_cursor(state.db, time_us) {
            Ok(_) -> {
              actor.continue(CursorState(
                db: state.db,
                last_flush_time: current_time,
                latest_cursor: option.None,
              ))
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
          case jetstream.set_cursor(state.db, cursor) {
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
  db: Executor,
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
fn start_consumer_process(
  db: Executor,
  manager: option.Option(process.Subject(ManagerMessage)),
) -> Result(process.Pid, String) {
  logging.log(logging.Info, "")
  logging.log(logging.Info, "[jetstream] Starting Jetstream consumer...")

  // Get PLC directory URL from database config
  let plc_url = config_repo.get_plc_directory_url(db)

  // Get domain authority from database
  let domain_authority = case config_repo.get(db, "domain_authority") {
    Ok(authority) -> authority
    Error(_) -> ""
  }

  // Get all record-type lexicons from the database
  case lexicons.get_record_types(db) {
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

          // Get Jetstream URL from database config
          let jetstream_url = config_repo.get_jetstream_url(db)

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
              case jetstream.get_cursor(db) {
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
                      manager,
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
fn is_known_did(db: Executor, did: String) -> Bool {
  let sql = case executor.dialect(db) {
    executor.SQLite -> "SELECT 1 FROM actor WHERE did = ? LIMIT 1"
    executor.PostgreSQL -> "SELECT 1 FROM actor WHERE did = $1 LIMIT 1"
  }

  case
    executor.query(db, sql, [executor.Text(did)], decode.at([0], decode.int))
  {
    Ok(results) -> results != []
    Error(_) -> False
  }
}

/// Handle a raw Jetstream event JSON string
fn handle_jetstream_event(
  db: Executor,
  event_json: String,
  collection_ids: List(String),
  external_collection_ids: List(String),
  plc_url: String,
  cursor_tracker: option.Option(process.Subject(CursorMessage)),
  manager: option.Option(process.Subject(ManagerMessage)),
) -> Nil {
  case goose.parse_event(event_json) {
    goose.CommitEvent(did, time_us, commit) -> {
      // Send heartbeat to manager (convert microseconds to milliseconds)
      case manager {
        option.Some(mgr) -> process.send(mgr, MessageReceived(time_us / 1000))
        option.None -> Nil
      }

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
