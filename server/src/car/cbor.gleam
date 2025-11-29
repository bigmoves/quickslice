/// CBOR decoding wrapper for erl_cbor
///
/// Provides a Gleam-friendly interface to the erl_cbor Erlang library
import gleam/dynamic.{type Dynamic}

/// CBOR decoding errors
pub type CborError {
  DecodeError(String)
}

/// Decode a CBOR binary into a Dynamic value
/// Uses erl_cbor which handles DAG-CBOR tags as {Tag, Value} tuples
@external(erlang, "cbor_ffi", "decode")
pub fn decode(bytes: BitArray) -> Result(Dynamic, Dynamic)
