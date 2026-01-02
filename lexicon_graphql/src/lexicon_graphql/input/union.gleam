/// Union Input Utilities
///
/// Functions for handling AT Protocol union fields in GraphQL inputs.
/// Provides case conversion between naming conventions and transformation
/// from GraphQL discriminated union format to AT Protocol $type format.
///
/// GraphQL input: { type: "SELF_LABELS", selfLabels: { values: [...] } }
/// AT Protocol output: { $type: "com.atproto.label.defs#selfLabels", values: [...] }
import gleam/list
import gleam/string
import swell/value

// ─── Case Conversion ───────────────────────────────────────────────

/// Convert camelCase to SCREAMING_SNAKE_CASE
/// "selfLabels" -> "SELF_LABELS"
/// "myVariantType" -> "MY_VARIANT_TYPE"
///
/// Note: Numbers are not treated as word boundaries.
/// "oauth2Client" -> "OAUTH2_CLIENT" (not "OAUTH2CLIENT")
pub fn camel_to_screaming_snake(s: String) -> String {
  s
  |> string.to_graphemes
  |> list.fold(#("", False), fn(acc, char) {
    let #(result, prev_was_lower) = acc
    let is_upper = is_uppercase_letter(char)
    case is_upper, prev_was_lower {
      True, True -> #(result <> "_" <> char, False)
      _, _ -> #(result <> string.uppercase(char), !is_upper)
    }
  })
  |> fn(pair) { pair.0 }
}

/// Convert SCREAMING_SNAKE_CASE to camelCase
/// "SELF_LABELS" -> "selfLabels"
/// "MY_VARIANT_TYPE" -> "myVariantType"
/// "OAUTH2_CLIENT" -> "oauth2Client"
pub fn screaming_snake_to_camel(enum_value: String) -> String {
  let parts = string.split(enum_value, "_")
  case parts {
    [first, ..rest] -> {
      let lower_first = string.lowercase(first)
      let capitalized_rest =
        list.map(rest, fn(part) {
          case string.pop_grapheme(string.lowercase(part)) {
            Ok(#(first_char, remaining)) ->
              string.uppercase(first_char) <> remaining
            Error(_) -> part
          }
        })
      lower_first <> string.concat(capitalized_rest)
    }
    [] -> enum_value
  }
}

// ─── Ref Utilities ─────────────────────────────────────────────────

/// Extract short name from a fully-qualified ref
/// "com.atproto.label.defs#selfLabels" -> "selfLabels"
/// "com.atproto.label.defs" -> "defs"
pub fn ref_to_short_name(ref: String) -> String {
  case string.split(ref, "#") {
    [_, name] -> name
    _ -> {
      case string.split(ref, ".") |> list.last {
        Ok(name) -> name
        Error(_) -> ref
      }
    }
  }
}

/// Convert a ref to SCREAMING_SNAKE_CASE enum value
/// "com.atproto.label.defs#selfLabels" -> "SELF_LABELS"
pub fn ref_to_enum_value(ref: String) -> String {
  ref_to_short_name(ref) |> camel_to_screaming_snake
}

// ─── Transformation ────────────────────────────────────────────────

/// Transform a union object from GraphQL discriminated format to AT Protocol format
///
/// Takes fields from a GraphQL input object and a list of possible refs for this union.
/// Returns the transformed value in AT Protocol format with $type field.
pub fn transform_union_object(
  fields: List(#(String, value.Value)),
  refs: List(String),
) -> value.Value {
  // Find the "type" discriminator field
  let type_field = list.key_find(fields, "type")

  case type_field {
    Ok(value.Enum(enum_value)) -> {
      transform_with_enum_value(fields, refs, enum_value)
    }
    Ok(value.String(str_value)) -> {
      // Handle string type discriminator (fallback)
      transform_with_enum_value(fields, refs, str_value)
    }
    _ -> value.Object(fields)
  }
}

// ─── Internal Helpers ──────────────────────────────────────────────

/// Check if a character is an uppercase letter (A-Z only)
/// Returns False for numbers and other characters
fn is_uppercase_letter(char: String) -> Bool {
  let upper = string.uppercase(char)
  let lower = string.lowercase(char)
  // Is uppercase AND is a letter (not number/symbol)
  upper == char && lower != char
}

/// Internal helper to transform using an enum value
fn transform_with_enum_value(
  fields: List(#(String, value.Value)),
  refs: List(String),
  enum_value: String,
) -> value.Value {
  // Convert enum value back to ref
  let matching_ref = find_ref_for_enum_value(enum_value, refs)
  case matching_ref {
    Ok(ref) -> {
      // Find the variant field (same name as the short ref name)
      let short_name = screaming_snake_to_camel(enum_value)
      case list.key_find(fields, short_name) {
        Ok(value.Object(variant_fields)) -> {
          // Build AT Protocol format: variant fields + $type
          value.Object([#("$type", value.String(ref)), ..variant_fields])
        }
        _ -> {
          // No variant data, just return $type
          value.Object([#("$type", value.String(ref))])
        }
      }
    }
    Error(_) -> value.Object(fields)
  }
}

/// Find the ref that matches an enum value
/// "SELF_LABELS" matches "com.atproto.label.defs#selfLabels"
fn find_ref_for_enum_value(
  enum_value: String,
  refs: List(String),
) -> Result(String, Nil) {
  list.find(refs, fn(ref) { ref_to_enum_value(ref) == enum_value })
}
