import database
import gleam/dynamic/decode
import gleam/io
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import lexicon
import simplifile
import sqlight

pub type ImportStats {
  ImportStats(total: Int, imported: Int, failed: Int, errors: List(String))
}

/// Imports lexicons from a directory into the database
pub fn import_lexicons_from_directory(
  directory: String,
) -> Result(ImportStats, String) {
  use db <- result.try(case database.initialize("atproto.db") {
    Ok(conn) -> Ok(conn)
    Error(_) -> Error("Failed to initialize database")
  })

  // Scan directory for JSON files
  io.println("📁 Scanning directory recursively...")
  use file_paths <- result.try(scan_directory_recursive(directory))

  io.println(
    "  ✓ Found " <> string.inspect(list.length(file_paths)) <> " .json files",
  )
  io.println("")
  io.println("📝 Validating and importing lexicons...")

  // Import each file
  let results =
    file_paths
    |> list.map(fn(file_path) { import_single_lexicon(db, file_path) })

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
  case lexicon.validate_schemas([json_content]) {
    Ok(_) -> Ok(#(lexicon_id, json_content))
    Error(err) -> Error("Validation failed: " <> format_validation_error(err))
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

/// Formats validation errors into readable strings
fn format_validation_error(error: lexicon.ValidationError) -> String {
  // Just convert to string for now - the lexicon package will provide details
  string.inspect(error)
}

/// Imports a single lexicon file
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
      case database.insert_lexicon(conn, lexicon_id, json_content) {
        Ok(_) -> {
          io.println("  ✓ " <> lexicon_id)
          Ok(lexicon_id)
        }
        Error(_) -> {
          let err_msg = file_name <> ": Database insertion failed"
          io.println("  ✗ " <> err_msg)
          Error(err_msg)
        }
      }
    }
    Error(err) -> {
      let err_msg = file_name <> ": " <> err
      io.println("  ✗ " <> err_msg)
      Error(err_msg)
    }
  }
}
