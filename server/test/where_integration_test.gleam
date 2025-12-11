import database/queries/where_clause
import database/repositories/records
import database/schema/tables
import gleam/dict
import gleam/dynamic/decode
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import gleeunit
import gleeunit/should
import sqlight

pub fn main() {
  gleeunit.main()
}

// Helper to setup test database with sample records
fn setup_test_db() -> Result(sqlight.Connection, sqlight.Error) {
  use conn <- result.try(sqlight.open(":memory:"))
  use _ <- result.try(tables.create_record_table(conn))

  // Insert test records
  use _ <- result.try(records.insert(
    conn,
    "at://did:plc:1/app.bsky.feed.post/1",
    "cid1",
    "did:plc:1",
    "app.bsky.feed.post",
    "{\"text\":\"Hello World\",\"likes\":100}",
  ))

  use _ <- result.try(records.insert(
    conn,
    "at://did:plc:2/app.bsky.feed.post/2",
    "cid2",
    "did:plc:2",
    "app.bsky.feed.post",
    "{\"text\":\"Goodbye World\",\"likes\":50}",
  ))

  use _ <- result.try(records.insert(
    conn,
    "at://did:plc:3/app.bsky.feed.post/3",
    "cid3",
    "did:plc:3",
    "app.bsky.feed.post",
    "{\"text\":\"Test post\",\"likes\":200}",
  ))

  use _ <- result.try(records.insert(
    conn,
    "at://did:plc:1/app.bsky.actor.profile/1",
    "cid4",
    "did:plc:1",
    "app.bsky.actor.profile",
    "{\"displayName\":\"Alice\"}",
  ))

  Ok(conn)
}

// Test: Filter by DID (table column)
pub fn filter_by_did_test() {
  let assert Ok(conn) = setup_test_db()

  let where_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "did",
          where_clause.WhereCondition(
            eq: Some(sqlight.text("did:plc:1")),
            in_values: None,
            contains: None,
            gt: None,
            gte: None,
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let result =
    records.get_by_collection_paginated_with_where(
      conn,
      "app.bsky.feed.post",
      Some(10),
      None,
      None,
      None,
      None,
      Some(where_clause),
    )

  case result {
    Ok(#(records, _, _, _)) -> {
      list.length(records) |> should.equal(1)
      case list.first(records) {
        Ok(record) -> record.did |> should.equal("did:plc:1")
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// Test: Filter by JSON field with contains
pub fn filter_by_json_contains_test() {
  let assert Ok(conn) = setup_test_db()

  let where_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "text",
          where_clause.WhereCondition(
            eq: None,
            in_values: None,
            contains: Some("Hello"),
            gt: None,
            gte: None,
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let result =
    records.get_by_collection_paginated_with_where(
      conn,
      "app.bsky.feed.post",
      Some(10),
      None,
      None,
      None,
      None,
      Some(where_clause),
    )

  case result {
    Ok(#(records, _, _, _)) -> {
      list.length(records) |> should.equal(1)
      case list.first(records) {
        Ok(record) -> should.be_true(string.contains(record.json, "Hello"))
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// Test: Filter by JSON field comparison (gt)
pub fn filter_by_json_comparison_test() {
  let assert Ok(conn) = setup_test_db()

  let where_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "likes",
          where_clause.WhereCondition(
            eq: None,
            in_values: None,
            contains: None,
            gt: Some(sqlight.int(75)),
            gte: None,
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let result =
    records.get_by_collection_paginated_with_where(
      conn,
      "app.bsky.feed.post",
      Some(10),
      None,
      None,
      None,
      None,
      Some(where_clause),
    )

  case result {
    Ok(#(records, _, _, _)) -> {
      // Should match records with likes > 75 (100 and 200)
      list.length(records) |> should.equal(2)
    }
    Error(_) -> should.fail()
  }
}

// Test: Range query with gte and lt
pub fn filter_range_query_test() {
  let assert Ok(conn) = setup_test_db()

  let where_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "likes",
          where_clause.WhereCondition(
            eq: None,
            in_values: None,
            contains: None,
            gt: None,
            gte: Some(sqlight.int(50)),
            lt: Some(sqlight.int(150)),
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let result =
    records.get_by_collection_paginated_with_where(
      conn,
      "app.bsky.feed.post",
      Some(10),
      None,
      None,
      None,
      None,
      Some(where_clause),
    )

  case result {
    Ok(#(records, _, _, _)) -> {
      // Should match records with 50 <= likes < 150 (50 and 100)
      list.length(records) |> should.equal(2)
    }
    Error(_) -> should.fail()
  }
}

// Test: Nested AND with multiple conditions
pub fn filter_nested_and_test() {
  let assert Ok(conn) = setup_test_db()

  let did_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "did",
          where_clause.WhereCondition(
            eq: Some(sqlight.text("did:plc:1")),
            in_values: None,
            contains: None,
            gt: None,
            gte: None,
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let likes_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "likes",
          where_clause.WhereCondition(
            eq: None,
            in_values: None,
            contains: None,
            gt: Some(sqlight.int(50)),
            gte: None,
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let root_clause =
    where_clause.WhereClause(
      conditions: dict.new(),
      and: Some([did_clause, likes_clause]),
      or: None,
    )

  let result =
    records.get_by_collection_paginated_with_where(
      conn,
      "app.bsky.feed.post",
      Some(10),
      None,
      None,
      None,
      None,
      Some(root_clause),
    )

  case result {
    Ok(#(records, _, _, _)) -> {
      // Should only match did:plc:1 with likes > 50 (the first record)
      list.length(records) |> should.equal(1)
      case list.first(records) {
        Ok(record) -> {
          record.did |> should.equal("did:plc:1")
          should.be_true(string.contains(record.json, "100"))
        }
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// Test: Nested OR with multiple conditions
pub fn filter_nested_or_test() {
  let assert Ok(conn) = setup_test_db()

  let did1_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "did",
          where_clause.WhereCondition(
            eq: Some(sqlight.text("did:plc:1")),
            in_values: None,
            contains: None,
            gt: None,
            gte: None,
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let did2_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "did",
          where_clause.WhereCondition(
            eq: Some(sqlight.text("did:plc:2")),
            in_values: None,
            contains: None,
            gt: None,
            gte: None,
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let root_clause =
    where_clause.WhereClause(
      conditions: dict.new(),
      and: None,
      or: Some([did1_clause, did2_clause]),
    )

  let result =
    records.get_by_collection_paginated_with_where(
      conn,
      "app.bsky.feed.post",
      Some(10),
      None,
      None,
      None,
      None,
      Some(root_clause),
    )

  case result {
    Ok(#(records, _, _, _)) -> {
      // Should match both did:plc:1 and did:plc:2
      list.length(records) |> should.equal(2)
    }
    Error(_) -> should.fail()
  }
}

// Test: Empty where clause returns all records
pub fn filter_empty_where_test() {
  let assert Ok(conn) = setup_test_db()

  let where_clause = where_clause.empty_clause()

  let result =
    records.get_by_collection_paginated_with_where(
      conn,
      "app.bsky.feed.post",
      Some(10),
      None,
      None,
      None,
      None,
      Some(where_clause),
    )

  case result {
    Ok(#(records, _, _, _)) -> {
      // Should return all 3 posts
      list.length(records) |> should.equal(3)
    }
    Error(_) -> should.fail()
  }
}

// Test: Where clause with pagination
pub fn filter_with_pagination_test() {
  let assert Ok(conn) = setup_test_db()

  let where_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "likes",
          where_clause.WhereCondition(
            eq: None,
            in_values: None,
            contains: None,
            gt: Some(sqlight.int(25)),
            gte: None,
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  // First page: limit 2
  let result =
    records.get_by_collection_paginated_with_where(
      conn,
      "app.bsky.feed.post",
      Some(2),
      None,
      None,
      None,
      None,
      Some(where_clause),
    )

  case result {
    Ok(#(records, next_cursor, has_next, has_prev)) -> {
      list.length(records) |> should.equal(2)
      should.be_true(has_next)
      should.be_false(has_prev)
      should.be_true(option.is_some(next_cursor))
    }
    Error(_) -> should.fail()
  }
}

// Test: Numeric comparison with is_numeric=True uses INTEGER cast
pub fn filter_numeric_with_cast_test() {
  let assert Ok(conn) = setup_test_db()

  // Test that numeric comparisons work with is_numeric: True
  let where_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "likes",
          where_clause.WhereCondition(
            eq: None,
            in_values: None,
            contains: None,
            gt: Some(sqlight.int(75)),
            gte: None,
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: True,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let result =
    records.get_by_collection_paginated_with_where(
      conn,
      "app.bsky.feed.post",
      Some(10),
      None,
      None,
      None,
      None,
      Some(where_clause),
    )

  case result {
    Ok(#(records, _, _, _)) -> {
      // Should match records with likes > 75 (100 and 200)
      list.length(records) |> should.equal(2)
    }
    Error(_) -> should.fail()
  }
}

// Test: String datetime comparison without INTEGER cast
pub fn filter_datetime_string_comparison_test() {
  let assert Ok(conn) = setup_test_db()

  // Insert records with ISO datetime strings in JSON
  let assert Ok(_) =
    sqlight.query(
      "INSERT INTO record (uri, cid, did, collection, json, indexed_at)
       VALUES (?, 'cid5', 'did:plc:5', 'app.bsky.feed.post', ?, datetime('now'))",
      on: conn,
      with: [
        sqlight.text("at://did:plc:5/app.bsky.feed.post/5"),
        sqlight.text(
          "{\"text\":\"Old post\",\"playedTime\":\"2024-01-01T00:00:00Z\"}",
        ),
      ],
      expecting: decode.string,
    )

  let assert Ok(_) =
    sqlight.query(
      "INSERT INTO record (uri, cid, did, collection, json, indexed_at)
       VALUES (?, 'cid6', 'did:plc:6', 'app.bsky.feed.post', ?, datetime('now'))",
      on: conn,
      with: [
        sqlight.text("at://did:plc:6/app.bsky.feed.post/6"),
        sqlight.text(
          "{\"text\":\"New post\",\"playedTime\":\"2024-12-01T00:00:00Z\"}",
        ),
      ],
      expecting: decode.string,
    )

  // Filter for records with playedTime >= "2024-06-01" (string comparison)
  let where_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "playedTime",
          where_clause.WhereCondition(
            eq: None,
            in_values: None,
            contains: None,
            gt: None,
            gte: Some(sqlight.text("2024-06-01T00:00:00Z")),
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let result =
    records.get_by_collection_paginated_with_where(
      conn,
      "app.bsky.feed.post",
      Some(10),
      None,
      None,
      None,
      None,
      Some(where_clause),
    )

  case result {
    Ok(#(records, _, _, _)) -> {
      // Should only match the "New post" with playedTime in December
      list.length(records) |> should.equal(1)
      case list.first(records) {
        Ok(record) -> should.be_true(string.contains(record.json, "New post"))
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// ===== isNull End-to-End Tests =====

/// Test isNull: true end-to-end from GraphQL parsing through SQL execution
pub fn is_null_true_end_to_end_test() {
  let assert Ok(conn) = sqlight.open(":memory:")
  let assert Ok(_) = tables.create_record_table(conn)

  // Insert records - some with replyParent, some without
  let assert Ok(_) =
    sqlight.query(
      "INSERT INTO record (uri, cid, did, collection, json, indexed_at)
       VALUES (?, 'cid1', 'did:plc:1', 'app.bsky.feed.post', ?, datetime('now'))",
      on: conn,
      with: [
        sqlight.text("at://did:plc:1/app.bsky.feed.post/1"),
        sqlight.text("{\"text\":\"Root post\"}"),
      ],
      expecting: decode.string,
    )

  let assert Ok(_) =
    sqlight.query(
      "INSERT INTO record (uri, cid, did, collection, json, indexed_at)
       VALUES (?, 'cid2', 'did:plc:2', 'app.bsky.feed.post', ?, datetime('now'))",
      on: conn,
      with: [
        sqlight.text("at://did:plc:2/app.bsky.feed.post/2"),
        sqlight.text(
          "{\"text\":\"Reply post\",\"replyParent\":\"at://did:plc:1/app.bsky.feed.post/1\"}",
        ),
      ],
      expecting: decode.string,
    )

  let assert Ok(_) =
    sqlight.query(
      "INSERT INTO record (uri, cid, did, collection, json, indexed_at)
       VALUES (?, 'cid3', 'did:plc:3', 'app.bsky.feed.post', ?, datetime('now'))",
      on: conn,
      with: [
        sqlight.text("at://did:plc:3/app.bsky.feed.post/3"),
        sqlight.text("{\"text\":\"Another root post\"}"),
      ],
      expecting: decode.string,
    )

  // Filter for records where replyParent IS NULL (root posts only)
  let where_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "replyParent",
          where_clause.WhereCondition(
            eq: None,
            in_values: None,
            contains: None,
            gt: None,
            gte: None,
            lt: None,
            lte: None,
            is_null: Some(True),
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let result =
    records.get_by_collection_paginated_with_where(
      conn,
      "app.bsky.feed.post",
      Some(10),
      None,
      None,
      None,
      None,
      Some(where_clause),
    )

  case result {
    Ok(#(records_list, _, _, _)) -> {
      // Should only match the 2 root posts (without replyParent)
      list.length(records_list) |> should.equal(2)
      // Verify none of them are replies
      list.all(records_list, fn(record) {
        !string.contains(record.json, "replyParent")
      })
      |> should.be_true
    }
    Error(_) -> should.fail()
  }
}

/// Test isNull: false end-to-end from GraphQL parsing through SQL execution
pub fn is_null_false_end_to_end_test() {
  let assert Ok(conn) = sqlight.open(":memory:")
  let assert Ok(_) = tables.create_record_table(conn)

  // Insert records - some with replyParent, some without
  let assert Ok(_) =
    sqlight.query(
      "INSERT INTO record (uri, cid, did, collection, json, indexed_at)
       VALUES (?, 'cid1', 'did:plc:1', 'app.bsky.feed.post', ?, datetime('now'))",
      on: conn,
      with: [
        sqlight.text("at://did:plc:1/app.bsky.feed.post/1"),
        sqlight.text("{\"text\":\"Root post\"}"),
      ],
      expecting: decode.string,
    )

  let assert Ok(_) =
    sqlight.query(
      "INSERT INTO record (uri, cid, did, collection, json, indexed_at)
       VALUES (?, 'cid2', 'did:plc:2', 'app.bsky.feed.post', ?, datetime('now'))",
      on: conn,
      with: [
        sqlight.text("at://did:plc:2/app.bsky.feed.post/2"),
        sqlight.text(
          "{\"text\":\"Reply post\",\"replyParent\":\"at://did:plc:1/app.bsky.feed.post/1\"}",
        ),
      ],
      expecting: decode.string,
    )

  let assert Ok(_) =
    sqlight.query(
      "INSERT INTO record (uri, cid, did, collection, json, indexed_at)
       VALUES (?, 'cid3', 'did:plc:3', 'app.bsky.feed.post', ?, datetime('now'))",
      on: conn,
      with: [
        sqlight.text("at://did:plc:3/app.bsky.feed.post/3"),
        sqlight.text("{\"text\":\"Another root post\"}"),
      ],
      expecting: decode.string,
    )

  // Filter for records where replyParent IS NOT NULL (replies only)
  let where_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "replyParent",
          where_clause.WhereCondition(
            eq: None,
            in_values: None,
            contains: None,
            gt: None,
            gte: None,
            lt: None,
            lte: None,
            is_null: Some(False),
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let result =
    records.get_by_collection_paginated_with_where(
      conn,
      "app.bsky.feed.post",
      Some(10),
      None,
      None,
      None,
      None,
      Some(where_clause),
    )

  case result {
    Ok(#(records_list, _, _, _)) -> {
      // Should only match the 1 reply post (with replyParent)
      list.length(records_list) |> should.equal(1)
      // Verify it's a reply
      case list.first(records_list) {
        Ok(record) ->
          should.be_true(string.contains(record.json, "replyParent"))
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}
