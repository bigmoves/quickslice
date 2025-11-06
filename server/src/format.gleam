import gleam/int
import gleam/list
import gleam/string

/// Formats an integer with comma thousand separators
/// Example: format_number(1234567) -> "1,234,567"
pub fn format_number(num: Int) -> String {
  let str = int.to_string(num)
  let is_negative = string.starts_with(str, "-")
  let digits = case is_negative {
    True -> string.drop_start(str, 1)
    False -> str
  }

  let chars = string.to_graphemes(digits)
  let char_count = list.length(chars)

  // If 3 or fewer digits, no commas needed
  case char_count <= 3 {
    True -> str
    False -> {
      let reversed = list.reverse(chars)

      let formatted =
        reversed
        |> list.index_map(fn(char, idx) {
          case idx > 0 && idx % 3 == 0 {
            True -> [",", char]
            False -> [char]
          }
        })
        |> list.flatten
        |> list.reverse
        |> string.join("")

      case is_negative {
        True -> "-" <> formatted
        False -> formatted
      }
    }
  }
}
