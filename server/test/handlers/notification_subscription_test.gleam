/// Tests for notification subscription event filtering
///
/// Verifies that notification subscription events correctly match
/// mentions of the subscribed DID while excluding self-authored records.
import gleam/option.{None, Some}
import gleeunit/should
import handlers/graphql_ws
import pubsub

pub fn notification_event_matches_did_test() {
  let event =
    pubsub.RecordEvent(
      uri: "at://did:plc:author/app.bsky.feed.like/abc",
      cid: "bafy123",
      did: "did:plc:author",
      collection: "app.bsky.feed.like",
      value: "{\"subject\":{\"uri\":\"at://did:plc:target/app.bsky.feed.post/xyz\"}}",
      indexed_at: "2024-01-01T00:00:00Z",
      operation: pubsub.Create,
    )

  let matches =
    graphql_ws.event_matches_notification_subscription(
      event,
      "did:plc:target",
      None,
    )

  matches |> should.be_true()
}

pub fn notification_event_excludes_self_authored_test() {
  let event =
    pubsub.RecordEvent(
      uri: "at://did:plc:target/app.bsky.feed.post/xyz",
      cid: "bafy123",
      did: "did:plc:target",
      collection: "app.bsky.feed.post",
      value: "{\"text\":\"Hello\"}",
      indexed_at: "2024-01-01T00:00:00Z",
      operation: pubsub.Create,
    )

  let matches =
    graphql_ws.event_matches_notification_subscription(
      event,
      "did:plc:target",
      None,
    )

  matches |> should.be_false()
}

pub fn notification_event_filters_by_collection_test() {
  let event =
    pubsub.RecordEvent(
      uri: "at://did:plc:author/app.bsky.feed.like/abc",
      cid: "bafy123",
      did: "did:plc:author",
      collection: "app.bsky.feed.like",
      value: "{\"subject\":{\"uri\":\"at://did:plc:target/app.bsky.feed.post/xyz\"}}",
      indexed_at: "2024-01-01T00:00:00Z",
      operation: pubsub.Create,
    )

  // Filter to only posts, like should not match
  let matches =
    graphql_ws.event_matches_notification_subscription(
      event,
      "did:plc:target",
      Some(["app.bsky.feed.post"]),
    )

  matches |> should.be_false()
}

pub fn notification_event_accepts_matching_collection_test() {
  let event =
    pubsub.RecordEvent(
      uri: "at://did:plc:author/app.bsky.feed.like/abc",
      cid: "bafy123",
      did: "did:plc:author",
      collection: "app.bsky.feed.like",
      value: "{\"subject\":{\"uri\":\"at://did:plc:target/app.bsky.feed.post/xyz\"}}",
      indexed_at: "2024-01-01T00:00:00Z",
      operation: pubsub.Create,
    )

  // Filter includes likes, should match
  let matches =
    graphql_ws.event_matches_notification_subscription(
      event,
      "did:plc:target",
      Some(["app.bsky.feed.like", "app.bsky.graph.follow"]),
    )

  matches |> should.be_true()
}

pub fn notification_event_excludes_update_operation_test() {
  let event =
    pubsub.RecordEvent(
      uri: "at://did:plc:author/app.bsky.feed.like/abc",
      cid: "bafy123",
      did: "did:plc:author",
      collection: "app.bsky.feed.like",
      value: "{\"subject\":{\"uri\":\"at://did:plc:target/app.bsky.feed.post/xyz\"}}",
      indexed_at: "2024-01-01T00:00:00Z",
      operation: pubsub.Update,
    )

  // Only Create operations should match notifications
  let matches =
    graphql_ws.event_matches_notification_subscription(
      event,
      "did:plc:target",
      None,
    )

  matches |> should.be_false()
}

pub fn notification_event_excludes_delete_operation_test() {
  let event =
    pubsub.RecordEvent(
      uri: "at://did:plc:author/app.bsky.feed.like/abc",
      cid: "bafy123",
      did: "did:plc:author",
      collection: "app.bsky.feed.like",
      value: "{\"subject\":{\"uri\":\"at://did:plc:target/app.bsky.feed.post/xyz\"}}",
      indexed_at: "2024-01-01T00:00:00Z",
      operation: pubsub.Delete,
    )

  // Only Create operations should match notifications
  let matches =
    graphql_ws.event_matches_notification_subscription(
      event,
      "did:plc:target",
      None,
    )

  matches |> should.be_false()
}

pub fn notification_event_excludes_unrelated_did_test() {
  let event =
    pubsub.RecordEvent(
      uri: "at://did:plc:author/app.bsky.feed.post/abc",
      cid: "bafy123",
      did: "did:plc:author",
      collection: "app.bsky.feed.post",
      value: "{\"text\":\"Hello world, no mentions\"}",
      indexed_at: "2024-01-01T00:00:00Z",
      operation: pubsub.Create,
    )

  let matches =
    graphql_ws.event_matches_notification_subscription(
      event,
      "did:plc:target",
      None,
    )

  matches |> should.be_false()
}
