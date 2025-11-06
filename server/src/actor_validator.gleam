import backfill
import database
import gleam/list
import gleam/string
import logging
import sqlight

/// Ensures that an actor exists in the database. If the actor is not found,
/// it will be resolved from the PLC directory and added to the database.
///
/// Returns Ok(True) if a new actor was created.
/// Returns Ok(False) if the actor already existed.
/// Returns Error(String) if the actor could not be resolved or created.
pub fn ensure_actor_exists(
  db: sqlight.Connection,
  did: String,
  plc_url: String,
) -> Result(Bool, String) {
  // Check if actor already exists
  case database.get_actor(db, did) {
    Ok(actors) -> {
      case list.is_empty(actors) {
        False -> {
          // Actor exists, nothing to do
          logging.log(
            logging.Debug,
            "Actor already exists: " <> did,
          )
          Ok(False)
        }
        True -> {
          // Actor not found, need to resolve and create
          logging.log(
            logging.Info,
            "Actor not found, resolving DID: " <> did,
          )

          case backfill.resolve_did(did, plc_url) {
            Ok(atp_data) -> {
              logging.log(
                logging.Info,
                "Resolved DID " <> did <> " to handle: " <> atp_data.handle,
              )

              // Create actor in database
              case database.upsert_actor(db, atp_data.did, atp_data.handle) {
                Ok(_) -> {
                  logging.log(
                    logging.Info,
                    "Successfully created actor: " <> did,
                  )
                  Ok(True)
                }
                Error(err) -> {
                  let error_msg =
                    "Failed to create actor in database: "
                    <> did
                    <> " - "
                    <> string.inspect(err)
                  logging.log(logging.Error, error_msg)
                  Error(error_msg)
                }
              }
            }
            Error(err) -> {
              let error_msg = "Failed to resolve DID: " <> did <> " - " <> err
              logging.log(logging.Error, error_msg)
              Error(error_msg)
            }
          }
        }
      }
    }
    Error(err) -> {
      let error_msg =
        "Database error checking for actor: "
        <> did
        <> " - "
        <> string.inspect(err)
      logging.log(logging.Error, error_msg)
      Error(error_msg)
    }
  }
}
