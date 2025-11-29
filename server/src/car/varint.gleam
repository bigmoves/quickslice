/// LEB128 unsigned integer decoding for CAR file parsing
///
/// Varints use the most significant bit as a continuation flag:
/// - If MSB is 1, more bytes follow
/// - If MSB is 0, this is the last byte
/// - Lower 7 bits contain the value chunk
import gleam/int

/// Decode a varint from the start of a BitArray
/// Returns the decoded value and remaining bytes
pub fn decode(bytes: BitArray) -> Result(#(Int, BitArray), Nil) {
  decode_loop(bytes, 0, 0)
}

fn decode_loop(
  bytes: BitArray,
  value: Int,
  shift: Int,
) -> Result(#(Int, BitArray), Nil) {
  case bytes {
    <<byte, rest:bits>> -> {
      // Extract lower 7 bits
      let chunk = int.bitwise_and(byte, 0x7F)
      // Add to value at current shift position
      let new_value =
        int.bitwise_or(value, int.bitwise_shift_left(chunk, shift))
      // Check continuation bit (MSB)
      case int.bitwise_and(byte, 0x80) {
        0 -> Ok(#(new_value, rest))
        _ -> decode_loop(rest, new_value, shift + 7)
      }
    }
    _ -> Error(Nil)
  }
}

/// Calculate the encoded size of a varint value
/// Used to compute CID byte sizes
pub fn encoded_size(n: Int) -> Int {
  case n < 128 {
    True -> 1
    False -> 1 + encoded_size(int.bitwise_shift_right(n, 7))
  }
}

/// Encode an integer as a varint (LEB128)
pub fn encode(n: Int) -> BitArray {
  encode_loop(n, <<>>)
}

fn encode_loop(n: Int, acc: BitArray) -> BitArray {
  case n < 128 {
    True -> <<acc:bits, n>>
    False -> {
      let byte = int.bitwise_or(int.bitwise_and(n, 0x7F), 0x80)
      encode_loop(int.bitwise_shift_right(n, 7), <<acc:bits, byte>>)
    }
  }
}
