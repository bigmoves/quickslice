/// Date formatting utilities via JavaScript FFI

/// Format an ISO8601 timestamp to local time (HH:MM:SS in user's timezone)
@external(javascript, "./date_formatter.ffi.mjs", "formatTimeLocal")
pub fn format_time_local(iso_timestamp: String) -> String

/// Format an ISO8601 timestamp to full local datetime
@external(javascript, "./date_formatter.ffi.mjs", "formatDateTimeLocal")
pub fn format_datetime_local(iso_timestamp: String) -> String
