import database/queries/aggregates
import database/schema/tables
import database/types
import gleam/dict
import gleam/dynamic/decode
import gleam/list
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should
import sqlight
import where_clause

pub fn main() {
  gleeunit.main()
}

// Helper to create an in-memory test database using existing database functions
fn setup_test_db() -> sqlight.Connection {
  let assert Ok(conn) = sqlight.open(":memory:")

  // Use existing database creation functions
  let assert Ok(_) = tables.create_record_table(conn)
  let assert Ok(_) = tables.create_actor_table(conn)

  conn
}

// Helper to insert test records
fn insert_test_record(
  conn: sqlight.Connection,
  uri: String,
  collection: String,
  json: String,
  did: String,
) {
  let assert Ok(_) =
    sqlight.query(
      "INSERT INTO record (uri, cid, did, collection, json, indexed_at)
       VALUES (?, 'cid123', ?, ?, ?, datetime('now'))",
      on: conn,
      with: [
        sqlight.text(uri),
        sqlight.text(did),
        sqlight.text(collection),
        sqlight.text(json),
      ],
      expecting: decode.string,
    )
  Nil
}

pub fn test_simple_group_by_single_field() {
  let conn = setup_test_db()

  // Insert test data with different statuses
  insert_test_record(
    conn,
    "at://did1/xyz.statusphere.status/1",
    "xyz.statusphere.status",
    "{\"status\": \"👍\", \"text\": \"Great!\"}",
    "did:plc:user1",
  )
  insert_test_record(
    conn,
    "at://did2/xyz.statusphere.status/2",
    "xyz.statusphere.status",
    "{\"status\": \"👍\", \"text\": \"Awesome!\"}",
    "did:plc:user2",
  )
  insert_test_record(
    conn,
    "at://did3/xyz.statusphere.status/3",
    "xyz.statusphere.status",
    "{\"status\": \"👎\", \"text\": \"Not good\"}",
    "did:plc:user3",
  )

  // Aggregate by status field
  let assert Ok(results) =
    aggregates.get_aggregated_records(
      conn,
      "xyz.statusphere.status",
      [types.SimpleField("status")],
      None,
      True,
      // order by count desc
      10,
    )

  // Should have 2 groups (👍 and 👎)
  list.length(results) |> should.equal(2)

  // First group should be 👍 with count 2
  let assert [first, second] = results
  first.count |> should.equal(2)
  second.count |> should.equal(1)
}

pub fn test_group_by_multiple_fields() {
  let conn = setup_test_db()

  // Insert test data
  insert_test_record(
    conn,
    "at://did1/xyz.statusphere.status/1",
    "xyz.statusphere.status",
    "{\"status\": \"👍\", \"category\": \"work\"}",
    "did:plc:user1",
  )
  insert_test_record(
    conn,
    "at://did2/xyz.statusphere.status/2",
    "xyz.statusphere.status",
    "{\"status\": \"👍\", \"category\": \"personal\"}",
    "did:plc:user2",
  )
  insert_test_record(
    conn,
    "at://did3/xyz.statusphere.status/3",
    "xyz.statusphere.status",
    "{\"status\": \"👍\", \"category\": \"work\"}",
    "did:plc:user3",
  )

  // Aggregate by status and category
  let assert Ok(results) =
    aggregates.get_aggregated_records(
      conn,
      "xyz.statusphere.status",
      [types.SimpleField("status"), types.SimpleField("category")],
      None,
      True,
      10,
    )

  // Should have 2 groups
  list.length(results) |> should.equal(2)

  // Check that we get the expected counts
  let assert [first, ..] = results
  first.count |> should.equal(2)
}

pub fn test_group_by_with_where_filter() {
  let conn = setup_test_db()

  // Insert test data
  insert_test_record(
    conn,
    "at://did1/xyz.statusphere.status/1",
    "xyz.statusphere.status",
    "{\"status\": \"👍\", \"active\": \"true\"}",
    "did:plc:user1",
  )
  insert_test_record(
    conn,
    "at://did2/xyz.statusphere.status/2",
    "xyz.statusphere.status",
    "{\"status\": \"👍\", \"active\": \"false\"}",
    "did:plc:user2",
  )
  insert_test_record(
    conn,
    "at://did3/xyz.statusphere.status/3",
    "xyz.statusphere.status",
    "{\"status\": \"👎\", \"active\": \"true\"}",
    "did:plc:user3",
  )

  // Create WHERE clause to filter only active records
  let where_condition =
    where_clause.WhereCondition(
      eq: Some(sqlight.text("true")),
      in_values: None,
      contains: None,
      gt: None,
      gte: None,
      lt: None,
      lte: None,
      is_numeric: False,
    )

  let where_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([#("active", where_condition)]),
      and: None,
      or: None,
    )

  // Aggregate by status with WHERE filter
  let assert Ok(results) =
    aggregates.get_aggregated_records(
      conn,
      "xyz.statusphere.status",
      [types.SimpleField("status")],
      Some(where_clause),
      True,
      10,
    )

  // Should have 2 groups, each with count 1 (only active records)
  list.length(results) |> should.equal(2)
  list.all(results, fn(r) { r.count == 1 }) |> should.be_true()
}

pub fn test_group_by_table_column() {
  let conn = setup_test_db()

  // Insert test data with different DIDs
  insert_test_record(
    conn,
    "at://did1/xyz.statusphere.status/1",
    "xyz.statusphere.status",
    "{\"status\": \"👍\"}",
    "did:plc:user1",
  )
  insert_test_record(
    conn,
    "at://did1/xyz.statusphere.status/2",
    "xyz.statusphere.status",
    "{\"status\": \"👍\"}",
    "did:plc:user1",
  )
  insert_test_record(
    conn,
    "at://did2/xyz.statusphere.status/3",
    "xyz.statusphere.status",
    "{\"status\": \"👍\"}",
    "did:plc:user2",
  )

  // Aggregate by DID (table column)
  let assert Ok(results) =
    aggregates.get_aggregated_records(
      conn,
      "xyz.statusphere.status",
      [types.SimpleField("did")],
      None,
      True,
      10,
    )

  // Should have 2 groups (2 different DIDs)
  list.length(results) |> should.equal(2)

  // First group should have count 2
  let assert [first, ..] = results
  first.count |> should.equal(2)
}

pub fn test_order_by_count_ascending() {
  let conn = setup_test_db()

  // Insert test data
  insert_test_record(
    conn,
    "at://did1/xyz.statusphere.status/1",
    "xyz.statusphere.status",
    "{\"status\": \"👍\"}",
    "did:plc:user1",
  )
  insert_test_record(
    conn,
    "at://did2/xyz.statusphere.status/2",
    "xyz.statusphere.status",
    "{\"status\": \"👎\"}",
    "did:plc:user2",
  )
  insert_test_record(
    conn,
    "at://did3/xyz.statusphere.status/3",
    "xyz.statusphere.status",
    "{\"status\": \"👎\"}",
    "did:plc:user3",
  )

  // Aggregate with ascending order
  let assert Ok(results) =
    aggregates.get_aggregated_records(
      conn,
      "xyz.statusphere.status",
      [types.SimpleField("status")],
      None,
      False,
      // order by count asc
      10,
    )

  list.length(results) |> should.equal(2)

  // First result should have count 1 (ascending order)
  let assert [first, ..] = results
  first.count |> should.equal(1)
}

pub fn test_limit() {
  let conn = setup_test_db()

  // Insert test data with many different statuses
  insert_test_record(
    conn,
    "at://did1/xyz.statusphere.status/1",
    "xyz.statusphere.status",
    "{\"status\": \"A\"}",
    "did:plc:user1",
  )
  insert_test_record(
    conn,
    "at://did2/xyz.statusphere.status/2",
    "xyz.statusphere.status",
    "{\"status\": \"B\"}",
    "did:plc:user2",
  )
  insert_test_record(
    conn,
    "at://did3/xyz.statusphere.status/3",
    "xyz.statusphere.status",
    "{\"status\": \"C\"}",
    "did:plc:user3",
  )
  insert_test_record(
    conn,
    "at://did4/xyz.statusphere.status/4",
    "xyz.statusphere.status",
    "{\"status\": \"D\"}",
    "did:plc:user4",
  )

  // Aggregate with limit of 2
  let assert Ok(results) =
    aggregates.get_aggregated_records(
      conn,
      "xyz.statusphere.status",
      [types.SimpleField("status")],
      None,
      True,
      2,
    )

  // Should only return 2 results due to limit
  list.length(results) |> should.equal(2)
}

pub fn test_date_truncation_day() {
  let conn = setup_test_db()

  // Insert records with indexed_at timestamps on different days
  let assert Ok(_) =
    sqlight.query(
      "INSERT INTO record (uri, cid, did, collection, json, indexed_at)
       VALUES (?, 'cid1', 'did1', 'xyz.statusphere.status', '{}', '2024-01-15 10:30:00')",
      on: conn,
      with: [sqlight.text("at://did1/xyz.statusphere.status/1")],
      expecting: decode.string,
    )

  let assert Ok(_) =
    sqlight.query(
      "INSERT INTO record (uri, cid, did, collection, json, indexed_at)
       VALUES (?, 'cid2', 'did2', 'xyz.statusphere.status', '{}', '2024-01-15 15:45:00')",
      on: conn,
      with: [sqlight.text("at://did2/xyz.statusphere.status/2")],
      expecting: decode.string,
    )

  let assert Ok(_) =
    sqlight.query(
      "INSERT INTO record (uri, cid, did, collection, json, indexed_at)
       VALUES (?, 'cid3', 'did3', 'xyz.statusphere.status', '{}', '2024-01-16 09:00:00')",
      on: conn,
      with: [sqlight.text("at://did3/xyz.statusphere.status/3")],
      expecting: decode.string,
    )

  // Aggregate by indexed_at truncated to day
  let assert Ok(results) =
    aggregates.get_aggregated_records(
      conn,
      "xyz.statusphere.status",
      [types.TruncatedField("indexed_at", types.Day)],
      None,
      True,
      10,
    )

  // Should have 2 groups (2 different days)
  list.length(results) |> should.equal(2)

  // One day should have 2 records
  let assert [first, ..] = results
  first.count |> should.equal(2)
}

pub fn test_empty_result() {
  let conn = setup_test_db()

  // No records inserted

  // Try to aggregate
  let assert Ok(results) =
    aggregates.get_aggregated_records(
      conn,
      "xyz.statusphere.status",
      [types.SimpleField("status")],
      None,
      True,
      10,
    )

  // Should return empty list
  list.length(results) |> should.equal(0)
}
