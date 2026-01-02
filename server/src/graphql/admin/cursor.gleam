/// Cursor encoding/decoding for admin GraphQL connections
///
/// Cursors are opaque base64-encoded strings in format "Type:ID"
/// Example: "Label:42" -> "TGFiZWw6NDI="
import gleam/bit_array
import gleam/int
import gleam/result
import gleam/string

/// Encode a cursor from prefix and ID
/// "Label", 42 -> "TGFiZWw6NDI="
pub fn encode(prefix: String, id: Int) -> String {
  let raw = prefix <> ":" <> int.to_string(id)
  bit_array.from_string(raw)
  |> bit_array.base64_encode(True)
}

/// Decode a cursor to prefix and ID
/// "TGFiZWw6NDI=" -> Ok(#("Label", 42))
pub fn decode(cursor: String) -> Result(#(String, Int), Nil) {
  use decoded <- result.try(
    bit_array.base64_decode(cursor)
    |> result.try(fn(bits) { bit_array.to_string(bits) })
    |> result.replace_error(Nil),
  )

  case string.split(decoded, ":") {
    [prefix, id_str] -> {
      case int.parse(id_str) {
        Ok(id) -> Ok(#(prefix, id))
        Error(_) -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}
