import database/repositories/lexicons
import gleam/dict
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import honk
import logging
import simplifile
import sqlight
import zip_helper

pub type ImportStats {
  ImportStats(total: Int, imported: Int, failed: Int, errors: List(String))
}

/// Imports lexicons from a directory into the database
pub fn import_lexicons_from_directory(
  directory: String,
  db: sqlight.Connection,
) -> Result(ImportStats, String) {
  // Scan directory for JSON files
  logging.log(logging.Info, "[import] Scanning directory recursively...")
  use file_paths <- result.try(scan_directory_recursive(directory))

  logging.log(
    logging.Info,
    "[import]   Found "
      <> string.inspect(list.length(file_paths))
      <> " .json files",
  )
  logging.log(logging.Info, "")
  logging.log(logging.Info, "[import] Reading all lexicon files...")

  // Read all files first to get their content
  let file_contents =
    file_paths
    |> list.filter_map(fn(file_path) {
      case simplifile.read(file_path) {
        Ok(content) -> Ok(#(file_path, content))
        Error(_) -> Error(Nil)
      }
    })

  logging.log(logging.Info, "[import] Validating all lexicons together...")

  // Extract all JSON strings and parse them to Json objects
  let all_json_strings = list.map(file_contents, fn(pair) { pair.1 })
  let all_json_results =
    honk.parse_json_strings(all_json_strings)
    |> result.map_error(fn(_) { "Failed to parse JSON" })

  let validation_result = case all_json_results {
    Ok(all_jsons) ->
      case honk.validate(all_jsons) {
        Ok(_) -> {
          logging.log(
            logging.Info,
            "[import]   All lexicons validated successfully",
          )
          Ok(Nil)
        }
        Error(err_map) -> {
          logging.log(
            logging.Error,
            "[import]   Validation failed: "
              <> format_validation_errors(err_map),
          )
          Error("Validation failed")
        }
      }
    Error(_) -> {
      logging.log(logging.Error, "[import]   Failed to parse JSON")
      Error("Failed to parse JSON")
    }
  }

  logging.log(logging.Info, "")
  logging.log(logging.Info, "[import] Importing lexicons to database...")

  // Import each file (skip individual validation since we already validated all together)
  let results = case validation_result {
    Error(_) -> {
      // If validation failed, don't import anything
      file_paths |> list.map(fn(_) { Error("Validation failed") })
    }
    Ok(_) -> {
      // Validation succeeded, import each lexicon
      file_contents
      |> list.map(fn(pair) {
        let #(file_path, json_content) = pair
        import_validated_lexicon(db, file_path, json_content)
      })
    }
  }

  // Calculate stats
  let total = list.length(results)
  let imported =
    results
    |> list.filter(fn(r) {
      case r {
        Ok(_) -> True
        Error(_) -> False
      }
    })
    |> list.length

  let failed = total - imported

  let errors =
    results
    |> list.filter_map(fn(r) {
      case r {
        Error(err) -> Ok(err)
        Ok(_) -> Error(Nil)
      }
    })

  Ok(ImportStats(
    total: total,
    imported: imported,
    failed: failed,
    errors: errors,
  ))
}

/// Scans a directory recursively for JSON files
pub fn scan_directory_recursive(path: String) -> Result(List(String), String) {
  case simplifile.is_directory(path) {
    Ok(False) -> Error("Path is not a directory: " <> path)
    Error(_) -> Error("Failed to access directory: " <> path)
    Ok(True) -> {
      case simplifile.read_directory(path) {
        Error(_) -> Error("Failed to read directory: " <> path)
        Ok(entries) -> {
          entries
          |> list.filter_map(fn(entry) {
            // Skip macOS metadata directories and hidden files
            case
              string.starts_with(entry, "__MACOSX")
              || string.starts_with(entry, ".")
            {
              True -> Error(Nil)
              False -> {
                let entry_path = path <> "/" <> entry

                case simplifile.is_directory(entry_path) {
                  Ok(True) -> {
                    // Recursively scan subdirectory
                    case scan_directory_recursive(entry_path) {
                      Ok(paths) -> Ok(paths)
                      Error(_) -> Error(Nil)
                    }
                  }
                  _ -> {
                    // Check if it's a .json file
                    case string.ends_with(entry, ".json") {
                      True -> Ok([entry_path])
                      False -> Error(Nil)
                    }
                  }
                }
              }
            }
          })
          |> list.flatten
          |> Ok
        }
      }
    }
  }
}

/// Parses and validates a lexicon file
pub fn parse_and_validate_lexicon(
  file_path: String,
) -> Result(#(String, String), String) {
  // Read file content
  use json_content <- result.try(case simplifile.read(file_path) {
    Ok(content) -> Ok(content)
    Error(_) -> Error("Failed to read file")
  })

  // Extract lexicon ID from JSON
  use lexicon_id <- result.try(extract_lexicon_id(json_content))

  // Validate using lexicon package
  use json_obj <- result.try(
    honk.parse_json_string(json_content)
    |> result.map_error(fn(_) { "Failed to parse JSON" }),
  )

  case honk.validate([json_obj]) {
    Ok(_) -> Ok(#(lexicon_id, json_content))
    Error(err_map) ->
      Error("Validation failed: " <> format_validation_errors(err_map))
  }
}

/// Extracts the lexicon ID from JSON content
fn extract_lexicon_id(json_content: String) -> Result(String, String) {
  // Try to decode "id" field first
  let id_decoder = {
    use id <- decode.field("id", decode.string)
    decode.success(id)
  }

  // Try to decode "lexicon" field as fallback
  let lexicon_decoder = {
    use lex <- decode.field("lexicon", decode.string)
    decode.success(lex)
  }

  case json.parse(json_content, id_decoder) {
    Ok(id) -> Ok(id)
    Error(_) ->
      case json.parse(json_content, lexicon_decoder) {
        Ok(id) -> Ok(id)
        Error(_) ->
          Error("Missing 'id' or 'lexicon' field - not a valid lexicon schema")
      }
  }
}

/// Formats validation errors from error map into readable strings
fn format_validation_errors(
  error_map: dict.Dict(String, List(String)),
) -> String {
  error_map
  |> dict.to_list
  |> list.flat_map(fn(entry) {
    let #(_key, errors) = entry
    errors
  })
  |> string.join(", ")
}

/// Imports a single lexicon file (with validation)
pub fn import_single_lexicon(
  conn: sqlight.Connection,
  file_path: String,
) -> Result(String, String) {
  let file_name = case string.split(file_path, "/") |> list.last {
    Ok(name) -> name
    Error(_) -> file_path
  }

  case parse_and_validate_lexicon(file_path) {
    Ok(#(lexicon_id, json_content)) -> {
      case lexicons.insert(conn, lexicon_id, json_content) {
        Ok(_) -> {
          logging.log(logging.Info, "[import]   " <> lexicon_id)
          Ok(lexicon_id)
        }
        Error(_) -> {
          let err_msg = file_name <> ": Database insertion failed"
          logging.log(logging.Error, "[import]   " <> err_msg)
          Error(err_msg)
        }
      }
    }
    Error(err) -> {
      let err_msg = file_name <> ": " <> err
      logging.log(logging.Error, "[import]   " <> err_msg)
      Error(err_msg)
    }
  }
}

/// Imports a lexicon that has already been validated
/// Used when importing multiple lexicons that were validated together
fn import_validated_lexicon(
  conn: sqlight.Connection,
  file_path: String,
  json_content: String,
) -> Result(String, String) {
  let file_name = case string.split(file_path, "/") |> list.last {
    Ok(name) -> name
    Error(_) -> file_path
  }

  case extract_lexicon_id(json_content) {
    Ok(lexicon_id) -> {
      case lexicons.insert(conn, lexicon_id, json_content) {
        Ok(_) -> {
          logging.log(logging.Info, "[import]   " <> lexicon_id)
          Ok(lexicon_id)
        }
        Error(_) -> {
          let err_msg = file_name <> ": Database insertion failed"
          logging.log(logging.Error, "[import]   " <> err_msg)
          Error(err_msg)
        }
      }
    }
    Error(err) -> {
      let err_msg = file_name <> ": " <> err
      logging.log(logging.Error, "[import]   " <> err_msg)
      Error(err_msg)
    }
  }
}

/// Decode base64 string to bit array using Erlang FFI
@external(erlang, "base64", "decode")
fn decode_base64(base64: String) -> BitArray

/// Import lexicons from a base64-encoded ZIP file
/// Returns ImportStats on success, error message on failure
pub fn import_lexicons_from_base64_zip(
  zip_base64: String,
  db: sqlight.Connection,
) -> Result(ImportStats, String) {
  // Decode base64 to binary
  let zip_binary = decode_base64(zip_base64)

  // Create temporary directory for extraction
  let temp_dir = "/tmp/lexicons_" <> string.inspect(erlang_timestamp())
  use _ <- result.try(case simplifile.create_directory(temp_dir) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("Failed to create temporary directory")
  })

  // Write ZIP file to temp location
  let zip_path = temp_dir <> "/lexicons.zip"
  use _ <- result.try(case simplifile.write_bits(zip_path, zip_binary) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("Failed to write ZIP file")
  })

  // Extract ZIP to temp directory
  let extract_dir = temp_dir <> "/extracted"
  use _ <- result.try(case simplifile.create_directory(extract_dir) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("Failed to create extraction directory")
  })

  use _ <- result.try(zip_helper.extract_zip(zip_path, extract_dir))

  // Import lexicons from extracted directory
  let import_result = import_lexicons_from_directory(extract_dir, db)

  // Clean up temp directory
  let _ = simplifile.delete(zip_path)
  let _ = delete_directory_recursive(extract_dir)
  let _ = simplifile.delete(temp_dir)

  import_result
}

/// Recursively delete a directory and its contents
fn delete_directory_recursive(path: String) -> Result(Nil, Nil) {
  case simplifile.is_directory(path) {
    Ok(True) -> {
      case simplifile.read_directory(path) {
        Ok(entries) -> {
          // Delete all entries first
          list.each(entries, fn(entry) {
            let entry_path = path <> "/" <> entry
            let _ = delete_directory_recursive(entry_path)
            Nil
          })

          // Then delete the directory itself
          case simplifile.delete(path) {
            Ok(_) -> Ok(Nil)
            Error(_) -> Error(Nil)
          }
        }
        Error(_) -> Error(Nil)
      }
    }
    _ -> {
      // Not a directory, try to delete as file
      case simplifile.delete(path) {
        Ok(_) -> Ok(Nil)
        Error(_) -> Error(Nil)
      }
    }
  }
}

/// Get current Erlang timestamp (for temp directory naming)
@external(erlang, "erlang", "system_time")
fn erlang_timestamp() -> Int
