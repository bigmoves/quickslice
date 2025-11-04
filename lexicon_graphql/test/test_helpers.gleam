/// Test helpers for creating Dynamic values in tests
import gleam/dynamic.{type Dynamic}

/// Convert any Gleam value to Dynamic for testing purposes
/// This uses FFI to unsafely coerce values - only use in tests!
@external(erlang, "test_helpers_ffi", "to_dynamic")
pub fn to_dynamic(value: a) -> Dynamic
