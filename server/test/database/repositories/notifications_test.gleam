// Test suite for get_notifications repository function
import database/repositories/records
import database/types.{type Record}
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import test_helpers

pub fn get_notifications_returns_records_mentioning_did_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_record_table(db)

  // Insert a record that mentions did:plc:target
  let assert Ok(_) =
    records.insert(
      db,
      "at://did:plc:author/app.bsky.feed.like/abc",
      "bafy123",
      "did:plc:author",
      "app.bsky.feed.like",
      "{\"subject\":{\"uri\":\"at://did:plc:target/app.bsky.feed.post/xyz\"}}",
    )

  // Insert a record by the target (should be excluded)
  let assert Ok(_) =
    records.insert(
      db,
      "at://did:plc:target/app.bsky.feed.post/xyz",
      "bafy456",
      "did:plc:target",
      "app.bsky.feed.post",
      "{\"text\":\"Hello world\"}",
    )

  // Insert a record that doesn't mention the target
  let assert Ok(_) =
    records.insert(
      db,
      "at://did:plc:other/app.bsky.feed.post/zzz",
      "bafy789",
      "did:plc:other",
      "app.bsky.feed.post",
      "{\"text\":\"Unrelated post\"}",
    )

  let assert Ok(#(results, _cursor, _has_next, _has_prev)) =
    records.get_notifications(db, "did:plc:target", None, None, None)

  // Should only return the like, not the target's own post or unrelated post
  list.length(results) |> should.equal(1)
  let assert Ok(first): Result(Record, _) = list.first(results)
  first.uri |> should.equal("at://did:plc:author/app.bsky.feed.like/abc")
  first.did |> should.equal("did:plc:author")
  first.collection |> should.equal("app.bsky.feed.like")
}

pub fn get_notifications_filters_by_collection_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_record_table(db)

  // Insert a like mentioning target
  let assert Ok(_) =
    records.insert(
      db,
      "at://did:plc:author/app.bsky.feed.like/abc",
      "bafy123",
      "did:plc:author",
      "app.bsky.feed.like",
      "{\"subject\":{\"uri\":\"at://did:plc:target/app.bsky.feed.post/xyz\"}}",
    )

  // Insert a follow mentioning target
  let assert Ok(_) =
    records.insert(
      db,
      "at://did:plc:author/app.bsky.graph.follow/def",
      "bafy456",
      "did:plc:author",
      "app.bsky.graph.follow",
      "{\"subject\":\"did:plc:target\"}",
    )

  // Filter to only likes
  let assert Ok(#(results, _, _, _)) =
    records.get_notifications(
      db,
      "did:plc:target",
      Some(["app.bsky.feed.like"]),
      None,
      None,
    )

  list.length(results) |> should.equal(1)
  let assert Ok(first): Result(Record, _) = list.first(results)
  first.collection |> should.equal("app.bsky.feed.like")
}
