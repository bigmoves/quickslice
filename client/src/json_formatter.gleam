/// JSON formatting utilities via JavaScript FFI

/// Pretty-print a JSON string with indentation
@external(javascript, "./json_formatter.ffi.mjs", "prettyPrint")
pub fn pretty_print(json_string: String) -> String

/// Pretty-print a JSON string with nested JSON strings expanded
@external(javascript, "./json_formatter.ffi.mjs", "prettyPrintNested")
pub fn pretty_print_nested(json_string: String) -> String
