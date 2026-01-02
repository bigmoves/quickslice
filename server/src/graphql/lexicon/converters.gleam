/// Value converters for lexicon GraphQL API
///
/// Transform database records and dynamic values to GraphQL value.Value objects
import database/executor.{type Executor}
import database/repositories/actors
import database/repositories/label_definitions
import database/types
import gleam/dict
import gleam/dynamic
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/string
import swell/value

/// Convert a database Record to a GraphQL value.Value
///
/// Creates an Object with all the record metadata plus the parsed JSON value
pub fn record_to_graphql_value(
  record: types.Record,
  db: Executor,
) -> value.Value {
  // Parse the record JSON and convert to GraphQL value
  let value_object = case parse_json_to_value(record.json) {
    Ok(val) -> val
    Error(_) -> value.Object([])
    // Fallback to empty object on parse error
  }

  // Look up actor handle from actor table
  let actor_handle = case actors.get(db, record.did) {
    Ok([actor, ..]) -> value.String(actor.handle)
    _ -> value.Null
  }

  // Create the full record object with metadata and value
  value.Object([
    #("uri", value.String(record.uri)),
    #("cid", value.String(record.cid)),
    #("did", value.String(record.did)),
    #("collection", value.String(record.collection)),
    #("indexedAt", value.String(record.indexed_at)),
    #("actorHandle", actor_handle),
    #("value", value_object),
  ])
}

/// Parse a JSON string and convert it to a GraphQL value.Value
pub fn parse_json_to_value(json_str: String) -> Result(value.Value, String) {
  // Parse JSON string to dynamic value
  case json.parse(json_str, decode.dynamic) {
    Ok(dyn) -> Ok(dynamic_to_value(dyn))
    Error(_) -> Error("Failed to parse JSON")
  }
}

/// Convert a dynamic value to a GraphQL value.Value
pub fn dynamic_to_value(dyn: dynamic.Dynamic) -> value.Value {
  // Try different decoders in order
  case decode.run(dyn, decode.string) {
    Ok(s) -> value.String(s)
    Error(_) ->
      case decode.run(dyn, decode.int) {
        Ok(i) -> value.Int(i)
        Error(_) ->
          case decode.run(dyn, decode.float) {
            Ok(f) -> value.Float(f)
            Error(_) ->
              case decode.run(dyn, decode.bool) {
                Ok(b) -> value.Boolean(b)
                Error(_) ->
                  case decode.run(dyn, decode.list(decode.dynamic)) {
                    Ok(items) -> {
                      let converted_items = list.map(items, dynamic_to_value)
                      value.List(converted_items)
                    }
                    Error(_) ->
                      case
                        decode.run(
                          dyn,
                          decode.dict(decode.string, decode.dynamic),
                        )
                      {
                        Ok(dict) -> {
                          let fields =
                            dict
                            |> dict.to_list
                            |> list.map(fn(entry) {
                              let #(key, val) = entry
                              #(key, dynamic_to_value(val))
                            })
                          value.Object(fields)
                        }
                        Error(_) -> value.Null
                      }
                  }
              }
          }
      }
  }
}

/// Convert a dynamic JSON value to graphql value.Value
pub fn json_dynamic_to_value(dyn: dynamic.Dynamic) -> value.Value {
  // Try different decoders in order
  case decode.run(dyn, decode.string) {
    Ok(s) -> value.String(s)
    Error(_) ->
      case decode.run(dyn, decode.int) {
        Ok(i) -> value.Int(i)
        Error(_) ->
          case decode.run(dyn, decode.float) {
            Ok(f) -> value.Float(f)
            Error(_) ->
              case decode.run(dyn, decode.bool) {
                Ok(b) -> value.Boolean(b)
                Error(_) ->
                  // Try as a list
                  case decode.run(dyn, decode.list(decode.dynamic)) {
                    Ok(items) ->
                      value.List(list.map(items, json_dynamic_to_value))
                    Error(_) ->
                      // Try as an object (dict)
                      case
                        decode.run(
                          dyn,
                          decode.dict(decode.string, decode.dynamic),
                        )
                      {
                        Ok(d) ->
                          value.Object(
                            list.map(dict.to_list(d), fn(pair) {
                              #(pair.0, json_dynamic_to_value(pair.1))
                            }),
                          )
                        Error(_) -> value.Null
                      }
                  }
              }
          }
      }
  }
}

/// Extract a reference URI from a record's JSON
/// This handles both simple string fields (at-uri) and strongRef objects
pub fn extract_reference_uri(
  json_str: String,
  field_name: String,
) -> Result(String, Nil) {
  // Parse the JSON
  case parse_json_to_value(json_str) {
    Ok(value.Object(fields)) -> {
      // Find the field
      case list.key_find(fields, field_name) {
        Ok(value.String(uri)) -> Ok(uri)
        Ok(value.Object(ref_fields)) -> {
          // Handle strongRef: { "uri": "...", "cid": "..." }
          case list.key_find(ref_fields, "uri") {
            Ok(value.String(uri)) -> Ok(uri)
            _ -> Error(Nil)
          }
        }
        _ -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

/// Convert a LabelPreference to GraphQL value
/// Takes a label definition and the user's effective visibility setting
pub fn label_preference_to_value(
  def: label_definitions.LabelDefinition,
  visibility: String,
) -> value.Value {
  value.Object([
    #("val", value.String(def.val)),
    #("description", value.String(def.description)),
    #("severity", value.Enum(string.uppercase(def.severity))),
    #("defaultVisibility", value.Enum(string.uppercase(def.default_visibility))),
    #("visibility", value.Enum(string.uppercase(visibility))),
  ])
}
