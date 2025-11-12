/// Number formatting utilities via JavaScript FFI
/// Format a number with locale-specific thousands separators
@external(javascript, "./number_formatter.ffi.mjs", "formatNumber")
pub fn format_number(number: Int) -> String
