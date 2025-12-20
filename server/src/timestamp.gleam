/// Timestamp utilities for ISO8601 formatting
/// Convert microseconds since Unix epoch to ISO8601 format
@external(erlang, "timestamp_ffi", "microseconds_to_iso8601")
pub fn microseconds_to_iso8601(time_us: Int) -> String

/// Get current timestamp in nanoseconds
@external(erlang, "os", "system_time")
fn system_time_native() -> Int

/// Get current time as ISO8601 string
pub fn current_iso8601() -> String {
  // os:system_time() returns nanoseconds, convert to microseconds
  microseconds_to_iso8601(system_time_native() / 1000)
}
