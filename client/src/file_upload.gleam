/// File upload utilities via JavaScript FFI
///
/// Provides base64 encoding for file uploads through GraphQL

/// Read a file and encode it as base64
/// This is async and uses a callback (dispatch function) to return the result
@external(javascript, "./file_upload.ffi.mjs", "readFileAsBase64")
pub fn read_file_as_base64(
  file_input_id: String,
  dispatch: fn(Result(String, String)) -> Nil,
) -> Nil

/// Clear a file input element
@external(javascript, "./file_upload.ffi.mjs", "clearFileInput")
pub fn clear_file_input(file_input_id: String) -> Nil
