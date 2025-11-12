import actor_validator
import backfill
import database
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option
import gleam/string
import goose
import jetstream_activity
import lexicon
import logging
import pubsub
import sqlight
import stats_pubsub

/// Convert a Dynamic value (Erlang term) to JSON string
fn dynamic_to_json(value: Dynamic) -> String {
  // Erlang's json:encode returns an iolist, we need to convert it to a string
  let iolist = do_json_encode(value)
  iolist_to_string(iolist)
}

/// Encode a dynamic value to JSON (returns iolist)
@external(erlang, "json", "encode")
fn do_json_encode(value: Dynamic) -> Dynamic

/// Convert an iolist to a string
@external(erlang, "erlang", "iolist_to_binary")
fn iolist_to_binary(iolist: Dynamic) -> Dynamic

/// Wrapper to convert iolist to string
fn iolist_to_string(iolist: Dynamic) -> String {
  let binary = iolist_to_binary(iolist)
  // The binary is already a string in Gleam's representation
  case decode.run(binary, decode.string) {
    Ok(str) -> str
    Error(_) -> {
      logging.log(
        logging.Warning,
        "[jetstream] Failed to convert iolist to string",
      )
      string.inspect(iolist)
    }
  }
}

/// Convert microseconds since Unix epoch to ISO8601 format
/// Uses the event's original timestamp for accurate indexedAt values
@external(erlang, "event_handler_ffi", "microseconds_to_iso8601")
fn microseconds_to_iso8601(time_us: Int) -> String

/// Serialize a commit event to JSON string for activity logging
fn serialize_commit_event(
  did: String,
  time_us: Int,
  commit: goose.CommitData,
) -> String {
  let record_json = case commit.record {
    option.Some(record_data) -> json.string(dynamic_to_json(record_data))
    option.None -> json.null()
  }

  let cid_json = case commit.cid {
    option.Some(cid) -> json.string(cid)
    option.None -> json.null()
  }

  json.object([
    #("did", json.string(did)),
    #("time_us", json.int(time_us)),
    #(
      "commit",
      json.object([
        #("rev", json.string(commit.rev)),
        #("operation", json.string(commit.operation)),
        #("collection", json.string(commit.collection)),
        #("rkey", json.string(commit.rkey)),
        #("record", record_json),
        #("cid", cid_json),
      ]),
    ),
  ])
  |> json.to_string
}

/// Handle a commit event (create, update, or delete)
pub fn handle_commit_event(
  db: sqlight.Connection,
  did: String,
  time_us: Int,
  commit: goose.CommitData,
  plc_url: String,
  collection_ids: List(String),
  external_collection_ids: List(String),
) -> Nil {
  let uri = "at://" <> did <> "/" <> commit.collection <> "/" <> commit.rkey

  // Log activity at entry point - serialize the commit event to JSON
  let event_json = serialize_commit_event(did, time_us, commit)
  let timestamp = microseconds_to_iso8601(time_us)

  let activity_id = case
    jetstream_activity.log_activity(
      db,
      timestamp,
      commit.operation,
      commit.collection,
      did,
      event_json,
    )
  {
    Ok(id) -> option.Some(id)
    Error(err) -> {
      logging.log(
        logging.Warning,
        "[jetstream] Failed to log activity: " <> string.inspect(err),
      )
      option.None
    }
  }

  case commit.operation {
    "create" | "update" -> {
      // Extract record and cid from options
      case commit.record, commit.cid {
        option.Some(record_data), option.Some(cid_value) -> {
          // Convert the dynamic record to JSON string using Erlang's json:encode
          let json_string = dynamic_to_json(record_data)

          // Get lexicons from database for validation
          case database.get_all_lexicons(db) {
            Ok(lexicons) -> {
              let lexicon_jsons = list.map(lexicons, fn(lex) { lex.json })

              // Validate record against lexicon
              case
                lexicon.validate_record(
                  lexicon_jsons,
                  commit.collection,
                  json_string,
                )
              {
                Ok(_) -> {
                  // Check if record already exists BEFORE inserting to determine operation type
                  let existing_record = database.get_record(db, uri)
                  let is_create = case existing_record {
                    Ok([]) -> True
                    // Empty list means record doesn't exist
                    Ok(_) -> False
                    // Non-empty list means record exists
                    Error(_) -> {
                      // Database error - log it and treat as update to be safe
                      logging.log(
                        logging.Warning,
                        "[jetstream] Error checking existing record for " <> uri,
                      )
                      False
                    }
                  }

                  // Ensure actor exists before inserting record
                  case actor_validator.ensure_actor_exists(db, did, plc_url) {
                    Ok(is_new_actor) -> {
                      // If this is a new actor, synchronously backfill all collections
                      // This ensures subscription joins have complete data immediately
                      // We're already in a spawned process per event, so blocking is fine
                      case is_new_actor {
                        True -> {
                          // Publish stats event for new actor
                          stats_pubsub.publish(stats_pubsub.ActorCreated)

                          backfill.backfill_collections_for_actor(
                            db,
                            did,
                            collection_ids,
                            external_collection_ids,
                            plc_url,
                          )
                        }
                        False -> Nil
                      }

                      // Continue with record insertion
                      // Validation passed, insert record
                      case
                        database.insert_record(
                          db,
                          uri,
                          cid_value,
                          did,
                          commit.collection,
                          json_string,
                        )
                      {
                        Ok(database.Inserted) -> {
                          logging.log(
                            logging.Info,
                            "[jetstream] "
                              <> case is_create {
                              True -> "create"
                              False -> "update"
                            }
                              <> " "
                              <> commit.collection
                              <> " ("
                              <> commit.rkey
                              <> ") "
                              <> did,
                          )

                          // Update activity status to success
                          case activity_id {
                            option.Some(id) -> {
                              case
                                jetstream_activity.update_status(
                                  db,
                                  id,
                                  "success",
                                  option.None,
                                )
                              {
                                Ok(_) ->
                                  // Publish activity event for real-time UI updates
                                  stats_pubsub.publish(
                                    stats_pubsub.ActivityLogged(
                                      id,
                                      timestamp,
                                      commit.operation,
                                      commit.collection,
                                      did,
                                      "success",
                                      option.None,
                                      event_json,
                                    ),
                                  )
                                Error(_) -> Nil
                              }
                            }
                            option.None -> Nil
                          }

                          // Publish event to PubSub for GraphQL subscriptions
                          let operation = case is_create {
                            True -> pubsub.Create
                            False -> pubsub.Update
                          }

                          // Convert event timestamp from microseconds to ISO8601
                          let indexed_at = microseconds_to_iso8601(time_us)

                          let event =
                            pubsub.RecordEvent(
                              uri: uri,
                              cid: cid_value,
                              did: did,
                              collection: commit.collection,
                              value: json_string,
                              indexed_at: indexed_at,
                              operation: operation,
                            )

                          pubsub.publish(event)

                          // Publish stats event for real-time stats updates
                          case is_create {
                            True ->
                              stats_pubsub.publish(stats_pubsub.RecordCreated)
                            False -> Nil
                          }
                        }
                        Ok(database.Skipped) -> {
                          logging.log(
                            logging.Info,
                            "[jetstream] skipped (duplicate CID) "
                              <> commit.collection
                              <> " ("
                              <> commit.rkey
                              <> ") "
                              <> did,
                          )

                          // Update activity status to success (but don't increment counters)
                          case activity_id {
                            option.Some(id) -> {
                              case
                                jetstream_activity.update_status(
                                  db,
                                  id,
                                  "success",
                                  option.Some("Skipped: duplicate CID"),
                                )
                              {
                                Ok(_) ->
                                  // Publish activity event for real-time UI updates
                                  stats_pubsub.publish(
                                    stats_pubsub.ActivityLogged(
                                      id,
                                      timestamp,
                                      commit.operation,
                                      commit.collection,
                                      did,
                                      "success",
                                      option.Some("Skipped: duplicate CID"),
                                      event_json,
                                    ),
                                  )
                                Error(_) -> Nil
                              }
                            }
                            option.None -> Nil
                          }
                          // Don't publish RecordCreated event - record wasn't actually created
                        }
                        Error(err) -> {
                          logging.log(
                            logging.Error,
                            "[jetstream] Failed to insert record "
                              <> uri
                              <> ": "
                              <> string.inspect(err),
                          )

                          // Update activity status to error
                          case activity_id {
                            option.Some(id) -> {
                              case
                                jetstream_activity.update_status(
                                  db,
                                  id,
                                  "error",
                                  option.Some(
                                    "Database insert failed: "
                                    <> string.inspect(err),
                                  ),
                                )
                              {
                                Ok(_) -> {
                                  let error_msg =
                                    "Database insert failed: "
                                    <> string.inspect(err)
                                  // Publish activity event for real-time UI updates
                                  stats_pubsub.publish(
                                    stats_pubsub.ActivityLogged(
                                      id,
                                      timestamp,
                                      commit.operation,
                                      commit.collection,
                                      did,
                                      "error",
                                      option.Some(error_msg),
                                      event_json,
                                    ),
                                  )
                                }
                                Error(_) -> Nil
                              }
                            }
                            option.None -> Nil
                          }
                        }
                      }
                    }
                    Error(actor_err) -> {
                      logging.log(
                        logging.Error,
                        "[jetstream] Failed to validate/create actor for "
                          <> uri
                          <> ": "
                          <> actor_err,
                      )

                      // Update activity status to error
                      case activity_id {
                        option.Some(id) -> {
                          case
                            jetstream_activity.update_status(
                              db,
                              id,
                              "error",
                              option.Some(
                                "Actor validation failed: " <> actor_err,
                              ),
                            )
                          {
                            Ok(_) -> {
                              let error_msg =
                                "Actor validation failed: " <> actor_err
                              // Publish activity event for real-time UI updates
                              stats_pubsub.publish(stats_pubsub.ActivityLogged(
                                id,
                                timestamp,
                                commit.operation,
                                commit.collection,
                                did,
                                "error",
                                option.Some(error_msg),
                                event_json,
                              ))
                            }
                            Error(_) -> Nil
                          }
                        }
                        option.None -> Nil
                      }
                    }
                  }
                }
                Error(validation_error) -> {
                  logging.log(
                    logging.Warning,
                    "[jetstream] Validation failed for "
                      <> uri
                      <> ": "
                      <> lexicon.describe_error(validation_error),
                  )

                  // Update activity status to validation_error
                  case activity_id {
                    option.Some(id) -> {
                      case
                        jetstream_activity.update_status(
                          db,
                          id,
                          "validation_error",
                          option.Some(lexicon.describe_error(validation_error)),
                        )
                      {
                        Ok(_) -> {
                          let error_msg =
                            lexicon.describe_error(validation_error)
                          // Publish activity event for real-time UI updates
                          stats_pubsub.publish(stats_pubsub.ActivityLogged(
                            id,
                            timestamp,
                            commit.operation,
                            commit.collection,
                            did,
                            "validation_error",
                            option.Some(error_msg),
                            event_json,
                          ))
                        }
                        Error(_) -> Nil
                      }
                    }
                    option.None -> Nil
                  }
                }
              }
            }
            Error(db_err) -> {
              logging.log(
                logging.Error,
                "[jetstream] Failed to fetch lexicons for validation: "
                  <> string.inspect(db_err),
              )

              // Update activity status to error
              case activity_id {
                option.Some(id) -> {
                  let _ =
                    jetstream_activity.update_status(
                      db,
                      id,
                      "error",
                      option.Some(
                        "Failed to fetch lexicons: " <> string.inspect(db_err),
                      ),
                    )
                  Nil
                }
                option.None -> Nil
              }
            }
          }
        }
        _, _ -> {
          logging.log(
            logging.Warning,
            "[jetstream] "
              <> commit.operation
              <> " event missing record or cid for "
              <> uri,
          )

          // Update activity status to error
          case activity_id {
            option.Some(id) -> {
              let _ =
                jetstream_activity.update_status(
                  db,
                  id,
                  "error",
                  option.Some("Event missing record or cid"),
                )
              Nil
            }
            option.None -> Nil
          }
        }
      }
    }
    "delete" -> {
      logging.log(
        logging.Info,
        "[jetstream] delete "
          <> commit.collection
          <> " ("
          <> commit.rkey
          <> ") "
          <> did,
      )

      case database.delete_record(db, uri) {
        Ok(_) -> {
          // Update activity status to success
          case activity_id {
            option.Some(id) -> {
              case
                jetstream_activity.update_status(db, id, "success", option.None)
              {
                Ok(_) ->
                  // Publish activity event for real-time UI updates
                  stats_pubsub.publish(stats_pubsub.ActivityLogged(
                    id,
                    timestamp,
                    commit.operation,
                    commit.collection,
                    did,
                    "success",
                    option.None,
                    event_json,
                  ))
                Error(_) -> Nil
              }
            }
            option.None -> Nil
          }

          // Publish delete event to PubSub for GraphQL subscriptions
          // Use the event timestamp from the Jetstream event
          let indexed_at = microseconds_to_iso8601(time_us)

          let event =
            pubsub.RecordEvent(
              uri: uri,
              cid: "",
              did: did,
              collection: commit.collection,
              value: "",
              indexed_at: indexed_at,
              operation: pubsub.Delete,
            )

          pubsub.publish(event)

          // Publish stats event for real-time stats updates
          stats_pubsub.publish(stats_pubsub.RecordDeleted)
        }
        Error(err) -> {
          logging.log(
            logging.Error,
            "[jetstream] Failed to delete: " <> string.inspect(err),
          )

          // Update activity status to error
          case activity_id {
            option.Some(id) -> {
              let _ =
                jetstream_activity.update_status(
                  db,
                  id,
                  "error",
                  option.Some("Delete failed: " <> string.inspect(err)),
                )
              Nil
            }
            option.None -> Nil
          }
        }
      }
    }
    _ -> {
      logging.log(
        logging.Warning,
        "[jetstream] Unknown operation: " <> commit.operation,
      )

      // Update activity status to error
      case activity_id {
        option.Some(id) -> {
          let _ =
            jetstream_activity.update_status(
              db,
              id,
              "error",
              option.Some("Unknown operation: " <> commit.operation),
            )
          Nil
        }
        option.None -> Nil
      }
    }
  }
}

/// Handle an identity event (update actor handle)
pub fn handle_identity_event(
  db: sqlight.Connection,
  identity: goose.IdentityData,
) -> Nil {
  case database.upsert_actor(db, identity.did, identity.handle) {
    Ok(_) -> {
      logging.log(
        logging.Info,
        "[jetstream] identity update: "
          <> identity.handle
          <> " ("
          <> identity.did
          <> ")",
      )
    }
    Error(err) -> {
      logging.log(
        logging.Error,
        "[jetstream] Failed to upsert actor "
          <> identity.did
          <> ": "
          <> string.inspect(err),
      )
    }
  }
}

/// Handle an account event
pub fn handle_account_event(
  _db: sqlight.Connection,
  account: goose.AccountData,
) -> Nil {
  // For now, just log account events - we could extend this in the future
  let status = case account.active {
    True -> "active"
    False -> "inactive"
  }
  logging.log(
    logging.Info,
    "[jetstream] account " <> status <> ": " <> account.did,
  )
}
