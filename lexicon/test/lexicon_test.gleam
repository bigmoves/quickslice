import gleeunit
import gleeunit/should
import lexicon

pub fn main() {
  gleeunit.main()
}

// Test NSID validation - this is the simplest test to verify NIF is working
pub fn valid_nsid_test() {
  // Valid NSIDs
  lexicon.is_valid_nsid("com.atproto.repo.createRecord")
  |> should.be_true
}

pub fn valid_nsid_simple_test() {
  lexicon.is_valid_nsid("app.bsky.feed.post")
  |> should.be_true
}

pub fn invalid_nsid_with_space_test() {
  // Invalid NSID - contains space
  lexicon.is_valid_nsid("invalid nsid")
  |> should.be_false
}

pub fn invalid_nsid_empty_test() {
  // Invalid NSID - empty string
  lexicon.is_valid_nsid("")
  |> should.be_false
}

// Test validating a valid record against a lexicon schema
pub fn validate_valid_record_test() {
  let schema =
    "{
      \"lexicon\": 1,
      \"id\": \"com.example.post\",
      \"defs\": {
        \"main\": {
          \"type\": \"record\",
          \"key\": \"tid\",
          \"record\": {
            \"type\": \"object\",
            \"required\": [\"text\"],
            \"properties\": {
              \"text\": {
                \"type\": \"string\",
                \"maxLength\": 300
              }
            }
          }
        }
      }
    }"

  let record =
    "{
      \"text\": \"Hello, world!\"
    }"

  lexicon.validate_record([schema], "com.example.post", record)
  |> should.be_ok
}

// Test validating an invalid record (missing required field)
pub fn validate_invalid_record_missing_field_test() {
  let schema =
    "{
      \"lexicon\": 1,
      \"id\": \"com.example.post\",
      \"defs\": {
        \"main\": {
          \"type\": \"record\",
          \"key\": \"tid\",
          \"record\": {
            \"type\": \"object\",
            \"required\": [\"text\"],
            \"properties\": {
              \"text\": {
                \"type\": \"string\"
              }
            }
          }
        }
      }
    }"

  let record = "{}"

  lexicon.validate_record([schema], "com.example.post", record)
  |> should.be_error
}
