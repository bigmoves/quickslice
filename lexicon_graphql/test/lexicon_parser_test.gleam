/// Tests for Lexicon JSON Parser
///
/// Parses AT Protocol lexicon JSON into structured Lexicon types
import gleam/dict
import gleam/list
import gleam/option
import gleeunit/should
import lexicon_graphql/internal/lexicon/parser as lexicon_parser
import lexicon_graphql/types

// Test parsing a simple record lexicon
pub fn parse_simple_record_lexicon_test() {
  let json =
    "{
      \"lexicon\": 1,
      \"id\": \"xyz.statusphere.status\",
      \"defs\": {
        \"main\": {
          \"type\": \"record\",
          \"record\": {
            \"type\": \"object\",
            \"required\": [\"text\"],
            \"properties\": {
              \"text\": {\"type\": \"string\"},
              \"createdAt\": {\"type\": \"string\"}
            }
          }
        }
      }
    }"

  let result = lexicon_parser.parse_lexicon(json)

  should.be_ok(result)

  // Verify the parsed lexicon has correct structure
  case result {
    Ok(lexicon) -> {
      should.equal(lexicon.id, "xyz.statusphere.status")
      // Verify it has properties
      case lexicon.defs.main {
        option.Some(types.RecordDef(type_: "record", key: _, properties: props)) -> {
          // Should have at least text and createdAt properties
          should.be_true(list.length(props) >= 2)
        }
        option.Some(types.RecordDef(type_: _, key: _, properties: _)) -> {
          should.fail()
        }
        option.None -> {
          should.fail()
        }
      }
    }
    Error(_) -> should.fail()
  }
}

// Test parsing lexicon with no required fields
pub fn parse_lexicon_with_optional_fields_test() {
  let json =
    "{
      \"lexicon\": 1,
      \"id\": \"xyz.statusphere.profile\",
      \"defs\": {
        \"main\": {
          \"type\": \"record\",
          \"record\": {
            \"type\": \"object\",
            \"properties\": {
              \"displayName\": {\"type\": \"string\"},
              \"bio\": {\"type\": \"string\"}
            }
          }
        }
      }
    }"

  let result = lexicon_parser.parse_lexicon(json)

  should.be_ok(result)
}

// Test parsing invalid JSON
pub fn parse_invalid_json_test() {
  let json = "{invalid json"

  let result = lexicon_parser.parse_lexicon(json)

  should.be_error(result)
}

// Test parsing JSON with missing required fields
pub fn parse_lexicon_missing_id_test() {
  let json =
    "{
      \"lexicon\": 1,
      \"defs\": {
        \"main\": {
          \"type\": \"record\"
        }
      }
    }"

  let result = lexicon_parser.parse_lexicon(json)

  should.be_error(result)
}

// Test parsing lexicon with array property containing ref items
pub fn parse_array_with_ref_items_test() {
  let json =
    "{
      \"lexicon\": 1,
      \"id\": \"fm.teal.alpha.feed.track\",
      \"defs\": {
        \"main\": {
          \"type\": \"record\",
          \"record\": {
            \"type\": \"object\",
            \"properties\": {
              \"artists\": {
                \"type\": \"array\",
                \"items\": {
                  \"type\": \"ref\",
                  \"ref\": \"fm.teal.alpha.feed.defs#artist\"
                }
              }
            }
          }
        }
      }
    }"

  let result = lexicon_parser.parse_lexicon(json)
  should.be_ok(result)

  case result {
    Ok(lexicon) -> {
      case lexicon.defs.main {
        option.Some(types.RecordDef(type_: _, key: _, properties: props)) -> {
          case list.find(props, fn(p) { p.0 == "artists" }) {
            Ok(#(_, prop)) -> {
              should.equal(prop.type_, "array")
              case prop.items {
                option.Some(items) -> {
                  should.equal(items.type_, "ref")
                  should.equal(
                    items.ref,
                    option.Some("fm.teal.alpha.feed.defs#artist"),
                  )
                }
                option.None -> should.fail()
              }
            }
            Error(_) -> should.fail()
          }
        }
        option.None -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// Test parsing lexicon with array property containing union items
pub fn parse_array_with_union_items_test() {
  let json =
    "{
      \"lexicon\": 1,
      \"id\": \"fm.teal.alpha.feed.track\",
      \"defs\": {
        \"main\": {
          \"type\": \"record\",
          \"record\": {
            \"type\": \"object\",
            \"properties\": {
              \"creators\": {
                \"type\": \"array\",
                \"items\": {
                  \"type\": \"union\",
                  \"refs\": [
                    \"fm.teal.alpha.feed.defs#artist\",
                    \"fm.teal.alpha.feed.defs#band\"
                  ]
                }
              }
            }
          }
        }
      }
    }"

  let result = lexicon_parser.parse_lexicon(json)
  should.be_ok(result)

  case result {
    Ok(lexicon) -> {
      case lexicon.defs.main {
        option.Some(types.RecordDef(type_: _, key: _, properties: props)) -> {
          case list.find(props, fn(p) { p.0 == "creators" }) {
            Ok(#(_, prop)) -> {
              should.equal(prop.type_, "array")
              case prop.items {
                option.Some(items) -> {
                  should.equal(items.type_, "union")
                  should.equal(
                    items.refs,
                    option.Some([
                      "fm.teal.alpha.feed.defs#artist",
                      "fm.teal.alpha.feed.defs#band",
                    ]),
                  )
                }
                option.None -> should.fail()
              }
            }
            Error(_) -> should.fail()
          }
        }
        option.None -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// Test parsing lexicon with union property (not array)
pub fn parse_property_union_test() {
  let json =
    "{
      \"lexicon\": 1,
      \"id\": \"app.bsky.feed.post\",
      \"defs\": {
        \"main\": {
          \"type\": \"record\",
          \"record\": {
            \"type\": \"object\",
            \"properties\": {
              \"embed\": {
                \"type\": \"union\",
                \"refs\": [
                  \"app.bsky.embed.images\",
                  \"app.bsky.embed.video\"
                ]
              }
            }
          }
        }
      }
    }"

  let result = lexicon_parser.parse_lexicon(json)
  should.be_ok(result)

  case result {
    Ok(lexicon) -> {
      case lexicon.defs.main {
        option.Some(types.RecordDef(type_: _, key: _, properties: props)) -> {
          case list.find(props, fn(p) { p.0 == "embed" }) {
            Ok(#(_, prop)) -> {
              should.equal(prop.type_, "union")
              should.equal(
                prop.refs,
                option.Some([
                  "app.bsky.embed.images",
                  "app.bsky.embed.video",
                ]),
              )
            }
            Error(_) -> should.fail()
          }
        }
        option.None -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// Test parsing lexicon WITHOUT main definition (like com.atproto.label.defs)
// These lexicons only have defs.others with object types
pub fn parse_lexicon_without_main_test() {
  let json =
    "{
      \"lexicon\": 1,
      \"id\": \"com.atproto.label.defs\",
      \"defs\": {
        \"selfLabels\": {
          \"type\": \"object\",
          \"required\": [\"values\"],
          \"properties\": {
            \"values\": {
              \"type\": \"array\",
              \"items\": {\"ref\": \"#selfLabel\", \"type\": \"ref\"},
              \"maxLength\": 10
            }
          }
        },
        \"selfLabel\": {
          \"type\": \"object\",
          \"required\": [\"val\"],
          \"properties\": {
            \"val\": {\"type\": \"string\", \"maxLength\": 128}
          }
        }
      }
    }"

  let result = lexicon_parser.parse_lexicon(json)
  should.be_ok(result)

  case result {
    Ok(lexicon) -> {
      should.equal(lexicon.id, "com.atproto.label.defs")
      // Main should be None since there's no main definition
      should.be_none(lexicon.defs.main)
      // Others should have both selfLabels and selfLabel
      let others_count = dict.size(lexicon.defs.others)
      should.equal(others_count, 2)
    }
    Error(_) -> should.fail()
  }
}

// Test parsing lexicon with array property containing string items
pub fn parse_array_with_string_items_test() {
  let json =
    "{
      \"lexicon\": 1,
      \"id\": \"fm.teal.alpha.feed.track\",
      \"defs\": {
        \"main\": {
          \"type\": \"record\",
          \"record\": {
            \"type\": \"object\",
            \"properties\": {
              \"artistNames\": {
                \"type\": \"array\",
                \"items\": {
                  \"type\": \"string\"
                }
              }
            }
          }
        }
      }
    }"

  let result = lexicon_parser.parse_lexicon(json)
  should.be_ok(result)

  case result {
    Ok(lexicon) -> {
      case lexicon.defs.main {
        option.Some(types.RecordDef(type_: _, key: _, properties: props)) -> {
          // Find artistNames property
          case list.find(props, fn(p) { p.0 == "artistNames" }) {
            Ok(#(_, prop)) -> {
              should.equal(prop.type_, "array")
              case prop.items {
                option.Some(items) -> {
                  should.equal(items.type_, "string")
                  should.equal(items.ref, option.None)
                  should.equal(items.refs, option.None)
                }
                option.None -> should.fail()
              }
            }
            Error(_) -> should.fail()
          }
        }
        option.None -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// Test parsing lexicon with main object type AND others defs (like app.bsky.richtext.facet)
pub fn parse_object_main_with_others_test() {
  let json =
    "{
      \"lexicon\": 1,
      \"id\": \"app.bsky.richtext.facet\",
      \"defs\": {
        \"main\": {
          \"type\": \"object\",
          \"required\": [\"index\", \"features\"],
          \"properties\": {
            \"index\": {\"ref\": \"#byteSlice\", \"type\": \"ref\"},
            \"features\": {
              \"type\": \"array\",
              \"items\": {
                \"refs\": [\"#mention\", \"#link\", \"#tag\"],
                \"type\": \"union\"
              }
            }
          }
        },
        \"mention\": {
          \"type\": \"object\",
          \"required\": [\"did\"],
          \"properties\": {
            \"did\": {\"type\": \"string\", \"format\": \"did\"}
          }
        },
        \"link\": {
          \"type\": \"object\",
          \"required\": [\"uri\"],
          \"properties\": {
            \"uri\": {\"type\": \"string\", \"format\": \"uri\"}
          }
        },
        \"tag\": {
          \"type\": \"object\",
          \"required\": [\"tag\"],
          \"properties\": {
            \"tag\": {\"type\": \"string\"}
          }
        },
        \"byteSlice\": {
          \"type\": \"object\",
          \"required\": [\"byteStart\", \"byteEnd\"],
          \"properties\": {
            \"byteStart\": {\"type\": \"integer\"},
            \"byteEnd\": {\"type\": \"integer\"}
          }
        }
      }
    }"

  let result = lexicon_parser.parse_lexicon(json)
  should.be_ok(result)

  case result {
    Ok(lexicon) -> {
      should.equal(lexicon.id, "app.bsky.richtext.facet")

      // Main should exist and be object type
      case lexicon.defs.main {
        option.Some(types.RecordDef(type_: "object", key: _, properties: _)) -> {
          // Good - main is object type
          Nil
        }
        _ -> should.fail()
      }

      // Others should have mention, link, tag, byteSlice
      let others_count = dict.size(lexicon.defs.others)
      should.equal(others_count, 4)

      // Verify each one exists
      should.be_true(dict.has_key(lexicon.defs.others, "mention"))
      should.be_true(dict.has_key(lexicon.defs.others, "link"))
      should.be_true(dict.has_key(lexicon.defs.others, "tag"))
      should.be_true(dict.has_key(lexicon.defs.others, "byteSlice"))
    }
    Error(_) -> should.fail()
  }
}
