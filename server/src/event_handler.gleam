import database
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import logging
import gleam/list
import gleam/option
import gleam/string
import goose
import lexicon
import sqlight

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
      logging.log(logging.Warning, "[jetstream] Failed to convert iolist to string")
      string.inspect(iolist)
    }
  }
}

/// Handle a commit event (create, update, or delete)
pub fn handle_commit_event(
  db: sqlight.Connection,
  did: String,
  commit: goose.CommitData,
) -> Nil {
  let uri = "at://" <> did <> "/" <> commit.collection <> "/" <> commit.rkey

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
                    Ok(_) -> {
                      logging.log(
                        logging.Info,
                        "[jetstream] "
                        <> commit.operation
                        <> " "
                        <> commit.collection
                        <> " ("
                        <> commit.rkey
                        <> ") "
                        <> did,
                      )
                    }
                    Error(err) -> {
                      logging.log(
                        logging.Error,
                        "[jetstream] Failed to insert record "
                        <> uri
                        <> ": "
                        <> string.inspect(err),
                      )
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
                }
              }
            }
            Error(db_err) -> {
              logging.log(
                logging.Error,
                "[jetstream] Failed to fetch lexicons for validation: "
                <> string.inspect(db_err),
              )
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
        }
      }
    }
    "delete" -> {
      logging.log(
        logging.Info,
        "[jetstream] delete " <> commit.collection <> " (" <> commit.rkey <> ") " <> did,
      )

      case database.delete_record(db, uri) {
        Ok(_) -> {
          Nil
        }
        Error(err) -> {
          logging.log(logging.Error, "[jetstream] Failed to delete: " <> string.inspect(err))
        }
      }
    }
    _ -> {
      logging.log(logging.Warning, "[jetstream] Unknown operation: " <> commit.operation)
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
        "[jetstream] identity update: " <> identity.handle <> " (" <> identity.did <> ")",
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
  logging.log(logging.Info, "[jetstream] account " <> status <> ": " <> account.did)
}
