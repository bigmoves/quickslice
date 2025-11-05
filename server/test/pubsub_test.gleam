import gleeunit
import gleeunit/should
import pubsub.{Create, Delete, RecordEvent, Update}

pub fn main() {
  gleeunit.main()
}

// Test: RecordEvent type creation
pub fn create_record_event_test() {
  let event =
    RecordEvent(
      uri: "at://did:plc:123/app.bsky.feed.post/abc",
      cid: "bafyreiabc123",
      did: "did:plc:123",
      collection: "app.bsky.feed.post",
      value: "{\"text\":\"Hello world\"}",
      indexed_at: "2025-01-15T12:00:00Z",
      operation: Create,
    )

  event.uri
  |> should.equal("at://did:plc:123/app.bsky.feed.post/abc")
  event.operation
  |> should.equal(Create)
}

// Test: operation types
pub fn operation_types_test() {
  let create_event =
    RecordEvent(
      uri: "at://did:plc:test/app.bsky.feed.post/1",
      cid: "bafycreate",
      did: "did:plc:test",
      collection: "app.bsky.feed.post",
      value: "{\"text\":\"created\"}",
      indexed_at: "2025-01-15T12:00:00Z",
      operation: Create,
    )

  let update_event =
    RecordEvent(
      uri: "at://did:plc:test/app.bsky.feed.post/1",
      cid: "bafyupdate",
      did: "did:plc:test",
      collection: "app.bsky.feed.post",
      value: "{\"text\":\"updated\"}",
      indexed_at: "2025-01-15T12:01:00Z",
      operation: Update,
    )

  let delete_event =
    RecordEvent(
      uri: "at://did:plc:test/app.bsky.feed.post/1",
      cid: "",
      did: "did:plc:test",
      collection: "app.bsky.feed.post",
      value: "{}",
      indexed_at: "2025-01-15T12:02:00Z",
      operation: Delete,
    )

  create_event.operation
  |> should.equal(Create)
  update_event.operation
  |> should.equal(Update)
  delete_event.operation
  |> should.equal(Delete)

  // Delete events should have empty/minimal data
  delete_event.cid
  |> should.equal("")
  delete_event.value
  |> should.equal("{}")
}
