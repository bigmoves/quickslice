/// Lexicon JSON Parser
///
/// Parses AT Protocol lexicon JSON documents into structured Lexicon types
/// that can be used with the schema builder.
import gleam/dict
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/result
import lexicon_graphql/schema_builder

/// Parse a lexicon JSON string into a Lexicon type
pub fn parse_lexicon(json_str: String) -> Result(schema_builder.Lexicon, String) {
  // Decode the JSON into a structured format using continuation-passing style
  let decoder = {
    use id <- decode.field("id", decode.string)
    use defs <- decode.field("defs", decode_defs())
    decode.success(schema_builder.Lexicon(id:, defs:))
  }

  json.parse(json_str, decoder)
  |> result.map_error(fn(_) { "Failed to parse lexicon JSON" })
}

/// Create a decoder for the defs object
fn decode_defs() -> decode.Decoder(schema_builder.Defs) {
  use main <- decode.field("main", decode_record_def())
  decode.success(schema_builder.Defs(main:))
}

/// Create a decoder for a record definition
fn decode_record_def() -> decode.Decoder(schema_builder.RecordDef) {
  use type_ <- decode.field("type", decode.string)
  use record <- decode.field("record", decode_record_object())
  decode.success(schema_builder.RecordDef(type_:, properties: record))
}

/// Create a decoder for the record object which contains properties
fn decode_record_object() -> decode.Decoder(
  List(#(String, schema_builder.Property)),
) {
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

      // Extract the type from the property
      let prop_type = case decode_property_type(prop_dyn) {
        Ok(type_) -> type_
        Error(_) -> "string"
        // Default fallback
      }

      #(name, schema_builder.Property(prop_type, is_required))
    })

  decode.success(properties)
}

/// Decode a property's type field
fn decode_property_type(
  dyn: decode.Dynamic,
) -> Result(String, List(decode.DecodeError)) {
  let type_decoder = {
    use type_ <- decode.field("type", decode.string)
    decode.success(type_)
  }
  decode.run(dyn, type_decoder)
}
