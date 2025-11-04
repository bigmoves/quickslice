import database
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import gleeunit
import gleeunit/should
import sqlight
import where_clause

pub fn main() {
  gleeunit.main()
}

// Helper to setup test database with sample records
fn setup_test_db() -> Result(sqlight.Connection, sqlight.Error) {
  use conn <- result.try(sqlight.open(":memory:"))
  use _ <- result.try(database.create_record_table(conn))

  // Insert test records
  use _ <- result.try(database.insert_record(
    conn,
    "at://did:plc:1/app.bsky.feed.post/1",
    "cid1",
    "did:plc:1",
    "app.bsky.feed.post",
    "{\"text\":\"Hello World\",\"likes\":100}",
  ))

  use _ <- result.try(database.insert_record(
    conn,
    "at://did:plc:2/app.bsky.feed.post/2",
    "cid2",
    "did:plc:2",
    "app.bsky.feed.post",
    "{\"text\":\"Goodbye World\",\"likes\":50}",
  ))

  use _ <- result.try(database.insert_record(
    conn,
    "at://did:plc:3/app.bsky.feed.post/3",
    "cid3",
    "did:plc:3",
    "app.bsky.feed.post",
    "{\"text\":\"Test post\",\"likes\":200}",
  ))

  use _ <- result.try(database.insert_record(
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
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let result =
    database.get_records_by_collection_paginated_with_where(
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
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let result =
    database.get_records_by_collection_paginated_with_where(
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
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let result =
    database.get_records_by_collection_paginated_with_where(
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
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let result =
    database.get_records_by_collection_paginated_with_where(
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
    database.get_records_by_collection_paginated_with_where(
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
    database.get_records_by_collection_paginated_with_where(
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
    database.get_records_by_collection_paginated_with_where(
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
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  // First page: limit 2
  let result =
    database.get_records_by_collection_paginated_with_where(
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
