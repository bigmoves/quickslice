import gleam/dynamic/decode
import gleam/http/request.{type Request}
import gleam/json
import squall
import gleam/option.{type Option}

pub type ActivityEntry {
  ActivityEntry(
    id: Int,
    timestamp: String,
    operation: String,
    collection: String,
    did: String,
    status: String,
    error_message: Option(String),
    event_json: Option(String),
  )
}

pub fn activity_entry_decoder() -> decode.Decoder(ActivityEntry) {
  use id <- decode.field("id", decode.int)
  use timestamp <- decode.field("timestamp", decode.string)
  use operation <- decode.field("operation", decode.string)
  use collection <- decode.field("collection", decode.string)
  use did <- decode.field("did", decode.string)
  use status <- decode.field("status", decode.string)
  use error_message <- decode.field("errorMessage", decode.optional(decode.string))
  use event_json <- decode.field("eventJson", decode.optional(decode.string))
  decode.success(ActivityEntry(
    id: id,
    timestamp: timestamp,
    operation: operation,
    collection: collection,
    did: did,
    status: status,
    error_message: error_message,
    event_json: event_json,
  ))
}

pub fn activity_entry_to_json(input: ActivityEntry) -> json.Json {
  json.object(
    [
      #("id", json.int(input.id)),
      #("timestamp", json.string(input.timestamp)),
      #("operation", json.string(input.operation)),
      #("collection", json.string(input.collection)),
      #("did", json.string(input.did)),
      #("status", json.string(input.status)),
      #("errorMessage", json.nullable(input.error_message, json.string)),
      #("eventJson", json.nullable(input.event_json, json.string)),
    ],
  )
}

pub type GetRecentActivityResponse {
  GetRecentActivityResponse(recent_activity: List(ActivityEntry))
}

pub fn get_recent_activity_response_decoder() -> decode.Decoder(GetRecentActivityResponse) {
  use recent_activity <- decode.field("recentActivity", decode.list(activity_entry_decoder()))
  decode.success(GetRecentActivityResponse(recent_activity: recent_activity))
}

pub fn get_recent_activity_response_to_json(input: GetRecentActivityResponse) -> json.Json {
  json.object(
    [
      #("recentActivity", json.array(
        from: input.recent_activity,
        of: activity_entry_to_json,
      )),
    ],
  )
}

pub fn get_recent_activity(client: squall.Client, hours: Int) -> Result(Request(String), String) {
  squall.prepare_request(
    client,
    "query GetRecentActivity($hours: Int!) {\n  recentActivity(hours: $hours) {\n    id\n    timestamp\n    operation\n    collection\n    did\n    status\n    errorMessage\n    eventJson\n  }\n}",
    json.object([#("hours", json.int(hours))]),
  )
}

pub fn parse_get_recent_activity_response(body: String) -> Result(GetRecentActivityResponse, String) {
  squall.parse_response(body, get_recent_activity_response_decoder())
}
