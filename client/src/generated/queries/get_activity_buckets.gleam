import gleam/dynamic/decode
import gleam/http/request.{type Request}
import gleam/json
import squall

pub type TimeRange {
  ONEHOUR
  THREEHOURS
  SIXHOURS
  ONEDAY
  SEVENDAYS
}

pub fn time_range_to_string(value: TimeRange) -> String {
  case value {
    ONEHOUR -> "ONE_HOUR"
    THREEHOURS -> "THREE_HOURS"
    SIXHOURS -> "SIX_HOURS"
    ONEDAY -> "ONE_DAY"
    SEVENDAYS -> "SEVEN_DAYS"
  }
}

pub fn time_range_decoder() -> decode.Decoder(TimeRange) {
  decode.string
  |> decode.then(fn(str) {
    case str {
      "ONE_HOUR" -> decode.success(ONEHOUR)
      "THREE_HOURS" -> decode.success(THREEHOURS)
      "SIX_HOURS" -> decode.success(SIXHOURS)
      "ONE_DAY" -> decode.success(ONEDAY)
      "SEVEN_DAYS" -> decode.success(SEVENDAYS)
      _other -> decode.failure(ONEHOUR, "TimeRange")
    }
  })
}

pub type ActivityBucket {
  ActivityBucket(
    timestamp: String,
    total: Int,
    creates: Int,
    updates: Int,
    deletes: Int,
  )
}

pub fn activity_bucket_decoder() -> decode.Decoder(ActivityBucket) {
  use timestamp <- decode.field("timestamp", decode.string)
  use total <- decode.field("total", decode.int)
  use creates <- decode.field("creates", decode.int)
  use updates <- decode.field("updates", decode.int)
  use deletes <- decode.field("deletes", decode.int)
  decode.success(ActivityBucket(
    timestamp: timestamp,
    total: total,
    creates: creates,
    updates: updates,
    deletes: deletes,
  ))
}

pub fn activity_bucket_to_json(input: ActivityBucket) -> json.Json {
  json.object([
    #("timestamp", json.string(input.timestamp)),
    #("total", json.int(input.total)),
    #("creates", json.int(input.creates)),
    #("updates", json.int(input.updates)),
    #("deletes", json.int(input.deletes)),
  ])
}

pub type GetActivityBucketsResponse {
  GetActivityBucketsResponse(activity_buckets: List(ActivityBucket))
}

pub fn get_activity_buckets_response_decoder() -> decode.Decoder(
  GetActivityBucketsResponse,
) {
  use activity_buckets <- decode.field(
    "activityBuckets",
    decode.list(activity_bucket_decoder()),
  )
  decode.success(GetActivityBucketsResponse(activity_buckets: activity_buckets))
}

pub fn get_activity_buckets_response_to_json(
  input: GetActivityBucketsResponse,
) -> json.Json {
  json.object([
    #(
      "activityBuckets",
      json.array(from: input.activity_buckets, of: activity_bucket_to_json),
    ),
  ])
}

pub fn get_activity_buckets(
  client: squall.Client,
  range: TimeRange,
) -> Result(Request(String), String) {
  squall.prepare_request(
    client,
    "query GetActivityBuckets($range: TimeRange!) {\n  activityBuckets(range: $range) {\n    timestamp\n    total\n    creates\n    updates\n    deletes\n  }\n}",
    json.object([#("range", json.string(time_range_to_string(range)))]),
  )
}

pub fn parse_get_activity_buckets_response(
  body: String,
) -> Result(GetActivityBucketsResponse, String) {
  squall.parse_response(body, get_activity_buckets_response_decoder())
}
