import database
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/io
import gleam/list
import gleam/option
import gleam/string
import jetstream
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
      io.println_error("⚠️  Failed to convert iolist to string")
      string.inspect(iolist)
    }
  }
}

/// Handle a commit event (create, update, or delete)
pub fn handle_commit_event(
  db: sqlight.Connection,
  did: String,
  commit: jetstream.CommitData,
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
                      io.println(
                        "✅ "
                        <> commit.operation
                        <> " "
                        <> commit.collection
                        <> " ("
                        <> commit.rkey
                        <> ")",
                      )
                    }
                    Error(err) -> {
                      io.println_error(
                        "❌ Failed to insert record "
                        <> uri
                        <> ": "
                        <> string.inspect(err),
                      )
                    }
                  }
                }
                Error(validation_error) -> {
                  io.println_error(
                    "⚠️  Validation failed for "
                    <> uri
                    <> ": "
                    <> lexicon.describe_error(validation_error),
                  )
                }
              }
            }
            Error(db_err) -> {
              io.println_error(
                "❌ Failed to fetch lexicons for validation: "
                <> string.inspect(db_err),
              )
            }
          }
        }
        _, _ -> {
          io.println_error(
            "⚠️  "
            <> commit.operation
            <> " event missing record or cid for "
            <> uri,
          )
        }
      }
    }
    "delete" -> {
      io.println(
        "🗑️  delete " <> commit.collection <> " (" <> commit.rkey <> ")",
      )

      case database.delete_record(db, uri) {
        Ok(_) -> {
          Nil
        }
        Error(err) -> {
          io.println_error("    ❌ Failed to delete: " <> string.inspect(err))
        }
      }
    }
    _ -> {
      io.println_error("⚠️  Unknown operation: " <> commit.operation)
    }
  }
}

/// Handle an identity event (update actor handle)
pub fn handle_identity_event(
  db: sqlight.Connection,
  identity: jetstream.IdentityData,
) -> Nil {
  case database.upsert_actor(db, identity.did, identity.handle) {
    Ok(_) -> {
      io.println(
        "👤 identity update: " <> identity.handle <> " (" <> identity.did <> ")",
      )
    }
    Error(err) -> {
      io.println_error(
        "❌ Failed to upsert actor "
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
  account: jetstream.AccountData,
) -> Nil {
  // For now, just log account events - we could extend this in the future
  let status = case account.active {
    True -> "active"
    False -> "inactive"
  }
  io.println("🔐 account " <> status <> ": " <> account.did)
}
