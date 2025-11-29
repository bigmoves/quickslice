/// Lexicon JSON Parser
///
/// Parses AT Protocol lexicon JSON documents into structured Lexicon types
/// that can be used with the schema builder.
import gleam/dict
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{None}
import gleam/result
import lexicon_graphql/types

/// Parse a lexicon JSON string into a Lexicon type
pub fn parse_lexicon(json_str: String) -> Result(types.Lexicon, String) {
  // Decode the JSON into a structured format using continuation-passing style
  let decoder = {
    use id <- decode.field("id", decode.string)
    use defs <- decode.field("defs", decode_defs())
    decode.success(types.Lexicon(id:, defs:))
  }

  json.parse(json_str, decoder)
  |> result.map_error(fn(_) { "Failed to parse lexicon JSON" })
}

/// Create a decoder for the defs object
fn decode_defs() -> decode.Decoder(types.Defs) {
  use main <- decode.optional_field(
    "main",
    None,
    decode.optional(decode_record_def()),
  )
  use defs_dict <- decode.then(decode.dict(decode.string, decode.dynamic))

  // Filter out "main" and parse remaining defs
  let others_list =
    defs_dict
    |> dict.to_list
    |> list.filter(fn(entry) { entry.0 != "main" })
    |> list.filter_map(fn(entry) {
      let #(name, def_dyn) = entry
      case decode_def(def_dyn) {
        Ok(def) -> Ok(#(name, def))
        Error(_) -> Error(Nil)
      }
    })

  let others = dict.from_list(others_list)
  decode.success(types.Defs(main:, others:))
}

/// Decode a definition (either record or object)
fn decode_def(
  dyn: decode.Dynamic,
) -> Result(types.Def, List(decode.DecodeError)) {
  // Try to decode as object first (simpler structure)
  case decode_object_def_inner(dyn) {
    Ok(obj_def) -> Ok(types.Object(obj_def))
    Error(_) -> {
      // Try as record
      case decode.run(dyn, decode_record_def()) {
        Ok(rec_def) -> Ok(types.Record(rec_def))
        Error(e) -> Error(e)
      }
    }
  }
}

/// Decode an object definition (inner, returns ObjectDef not Def)
fn decode_object_def_inner(
  dyn: decode.Dynamic,
) -> Result(types.ObjectDef, List(decode.DecodeError)) {
  let decoder = {
    use type_ <- decode.field("type", decode.string)
    use required_fields <- decode.optional_field(
      "required",
      [],
      decode.list(decode.string),
    )
    use properties_dict <- decode.field(
      "properties",
      decode.dict(decode.string, decode.dynamic),
    )

    // Convert dict to list of properties
    let properties =
      properties_dict
      |> dict.to_list
      |> list.map(fn(entry) {
        let #(name, prop_dyn) = entry
        let is_required = list.contains(required_fields, name)

        let #(prop_type, prop_format, prop_ref, prop_items) = case
          decode_property(prop_dyn)
        {
          Ok(#(t, f, r, i)) -> #(t, f, r, i)
          Error(_) -> #("string", None, None, None)
        }

        #(
          name,
          types.Property(
            prop_type,
            is_required,
            prop_format,
            prop_ref,
            prop_items,
          ),
        )
      })

    decode.success(types.ObjectDef(type_:, required_fields:, properties:))
  }
  decode.run(dyn, decoder)
}

/// Create a decoder for a record definition
fn decode_record_def() -> decode.Decoder(types.RecordDef) {
  use type_ <- decode.field("type", decode.string)
  use key <- decode.optional_field("key", None, decode.optional(decode.string))
  use record <- decode.field("record", decode_record_object())
  decode.success(types.RecordDef(type_:, key:, properties: record))
}

/// Create a decoder for the record object which contains properties
fn decode_record_object() -> decode.Decoder(List(#(String, types.Property))) {
  // This is more complex - we need to decode a dict of properties
  use properties_dict <- decode.field(
    "properties",
    decode.dict(decode.string, decode.dynamic),
  )
  use required_list <- decode.optional_field(
    "required",
    [],
    decode.list(decode.string),
  )

  // Convert dict to list of properties
  let properties =
    properties_dict
    |> dict.to_list
    |> list.map(fn(entry) {
      let #(name, prop_dyn) = entry
      let is_required = list.contains(required_list, name)

      // Extract type, format, ref, and items from the property
      let #(prop_type, prop_format, prop_ref, prop_items) = case
        decode_property(prop_dyn)
      {
        Ok(#(t, f, r, i)) -> #(t, f, r, i)
        Error(_) -> #("string", None, None, None)
        // Default fallback
      }

      #(
        name,
        types.Property(
          prop_type,
          is_required,
          prop_format,
          prop_ref,
          prop_items,
        ),
      )
    })

  decode.success(properties)
}

/// Decode a property's type, format, ref, and items fields
fn decode_property(
  dyn: decode.Dynamic,
) -> Result(
  #(
    String,
    option.Option(String),
    option.Option(String),
    option.Option(types.ArrayItems),
  ),
  List(decode.DecodeError),
) {
  let property_decoder = {
    use type_ <- decode.field("type", decode.string)
    use format <- decode.optional_field(
      "format",
      None,
      decode.optional(decode.string),
    )
    use ref <- decode.optional_field(
      "ref",
      None,
      decode.optional(decode.string),
    )
    use items_dyn <- decode.optional_field(
      "items",
      None,
      decode.optional(decode.dynamic),
    )

    // Decode items if present
    let items = case items_dyn {
      option.Some(dyn) ->
        case decode_array_items(dyn) {
          Ok(arr_items) -> option.Some(arr_items)
          Error(_) -> None
        }
      None -> None
    }

    decode.success(#(type_, format, ref, items))
  }
  decode.run(dyn, property_decoder)
}

/// Decode array items (type, ref, refs)
fn decode_array_items(
  dyn: decode.Dynamic,
) -> Result(types.ArrayItems, List(decode.DecodeError)) {
  let items_decoder = {
    use type_ <- decode.field("type", decode.string)
    use ref <- decode.optional_field(
      "ref",
      None,
      decode.optional(decode.string),
    )
    use refs <- decode.optional_field(
      "refs",
      None,
      decode.optional(decode.list(decode.string)),
    )
    decode.success(types.ArrayItems(type_:, ref:, refs:))
  }
  decode.run(dyn, items_decoder)
}
