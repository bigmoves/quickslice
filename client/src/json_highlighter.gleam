/// JSON formatter and syntax highlighter
import gleam/list
import gleam/string
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

@external(javascript, "./json_highlighter.ffi.mjs", "format")
pub fn format(json_string: String) -> String

/// Render JSON with syntax highlighting
pub fn highlight(json_string: String) -> Element(msg) {
  json_string
  |> string.to_graphemes
  |> highlight_chars(False, [])
  |> list.reverse
  |> element.fragment
}

fn highlight_chars(
  chars: List(String),
  in_string: Bool,
  acc: List(Element(msg)),
) -> List(Element(msg)) {
  highlight_chars_with_context(chars, in_string, False, acc)
}

fn highlight_chars_with_context(
  chars: List(String),
  in_string: Bool,
  after_colon: Bool,
  acc: List(Element(msg)),
) -> List(Element(msg)) {
  case chars {
    [] -> acc
    ["\"", ..rest] -> {
      // Quote - determine color based on context
      // Keys = sky blue, Values = green
      let #(color, is_value_string) = case in_string {
        // Exiting a string - use the current context to determine color
        True ->
          case after_colon {
            True -> #("text-green-400", True)
            False -> #("text-sky-400", False)
          }
        // Entering a string - check if we're after a colon
        False ->
          case after_colon {
            True -> #("text-green-400", True)
            False -> #("text-sky-400", False)
          }
      }
      let elem = html.span([attribute.class(color)], [element.text("\"")])
      // After closing a string, reset after_colon to False
      let new_after_colon = case in_string {
        True -> False
        False -> is_value_string
      }
      highlight_chars_with_context(rest, !in_string, new_after_colon, [
        elem,
        ..acc
      ])
    }
    ["{", ..rest] | ["}", ..rest] | ["[", ..rest] | ["]", ..rest] if !in_string -> {
      // Structural characters - dimmed white
      let char = case chars {
        [c, ..] -> c
        _ -> ""
      }
      let elem =
        html.span([attribute.class("text-zinc-400")], [element.text(char)])
      highlight_chars_with_context(rest, in_string, False, [elem, ..acc])
    }
    [":", ..rest] if !in_string -> {
      let elem =
        html.span([attribute.class("text-zinc-400")], [element.text(":")])
      highlight_chars_with_context(rest, in_string, True, [elem, ..acc])
    }
    [",", ..rest] if !in_string -> {
      let elem =
        html.span([attribute.class("text-zinc-400")], [element.text(",")])
      highlight_chars_with_context(rest, in_string, False, [elem, ..acc])
    }
    [" ", ..rest] | ["\n", ..rest] if !in_string -> {
      // Preserve whitespace and maintain after_colon state
      let elem =
        element.text(case chars {
          [" ", ..] -> " "
          ["\n", ..] -> "\n"
          _ -> ""
        })
      highlight_chars_with_context(rest, in_string, after_colon, [elem, ..acc])
    }
    [char, ..rest] if in_string -> {
      // Inside a string - keys are sky blue, values are green
      let color = case after_colon {
        True -> "text-green-400"
        False -> "text-sky-400"
      }
      let elem = html.span([attribute.class(color)], [element.text(char)])
      highlight_chars_with_context(rest, in_string, after_colon, [elem, ..acc])
    }
    ["t", "r", "u", "e", ..]
      | ["f", "a", "l", "s", "e", ..]
      | ["n", "u", "l", "l", ..]
      if !in_string
    -> {
      // Boolean or null - sunset red
      let word = case chars {
        ["t", "r", "u", "e", ..] -> "true"
        ["f", "a", "l", "s", "e", ..] -> "false"
        ["n", "u", "l", "l", ..] -> "null"
        _ -> ""
      }
      let len = string.length(word)
      let elem =
        html.span([attribute.class("text-red-400")], [element.text(word)])
      let remaining = list.drop(chars, len)
      highlight_chars_with_context(remaining, in_string, False, [elem, ..acc])
    }
    [char, ..rest] -> {
      // Numbers - sunset red, other text - light
      let color = case char {
        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "." | "-" ->
          "text-red-400"
        _ -> "text-zinc-200"
      }
      let elem = html.span([attribute.class(color)], [element.text(char)])
      highlight_chars_with_context(rest, in_string, after_colon, [elem, ..acc])
    }
  }
}
