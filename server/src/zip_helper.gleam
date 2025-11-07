/// FFI wrapper around Erlang's :zip module for extracting ZIP files
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/string

/// Extract a ZIP file to a destination directory
///
/// Returns Ok(Nil) on success, Error(String) on failure
pub fn extract_zip(zip_path: String, destination: String) -> Result(Nil, String) {
  case do_extract_zip(zip_path, destination) {
    Ok(_) -> Ok(Nil)
    Error(err) -> Error(dynamic_to_string(err))
  }
}

/// Erlang FFI to unzip a file
/// Uses :zip.unzip/2 with the :cwd option to specify extraction directory
@external(erlang, "zip_helper_ffi", "unzip_file")
fn do_extract_zip(
  zip_path: String,
  destination: String,
) -> Result(Dynamic, Dynamic)

/// Convert a dynamic error to a string for error reporting
fn dynamic_to_string(value: Dynamic) -> String {
  case decode.run(value, decode.string) {
    Ok(str) -> str
    Error(_) -> {
      // Try to convert to string representation
      case string.inspect(value) {
        str -> str
      }
    }
  }
}
