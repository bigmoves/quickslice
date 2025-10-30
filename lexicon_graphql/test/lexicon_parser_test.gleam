/// Tests for Lexicon JSON Parser
///
/// Parses AT Protocol lexicon JSON into structured Lexicon types
import gleam/list
import gleeunit/should
import lexicon_graphql/lexicon_parser
import lexicon_graphql/schema_builder

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
        schema_builder.RecordDef(type_: "record", properties: props) -> {
          // Should have at least text and createdAt properties
          should.be_true(list.length(props) >= 2)
        }
        schema_builder.RecordDef(type_: _, properties: _) -> {
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
