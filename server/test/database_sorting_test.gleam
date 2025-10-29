/// Database sorting integration tests
///
/// Tests that SQL ORDER BY clauses are generated correctly and
/// that sorting works properly with the database
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import database
import sqlight

// Helper to create test database with records
fn create_test_db_with_records() -> sqlight.Connection {
  // Create in-memory database
  let assert Ok(conn) = sqlight.open(":memory:")

  // Create schema using the database module
  let assert Ok(_) = database.create_record_table(conn)

  // Insert test records with different dates
  let records = [
    #(
      "at://did:plc:1/xyz.statusphere.status/1",
      "cid1",
      "did:plc:1",
      "xyz.statusphere.status",
      "{\"status\":\"😊\",\"createdAt\":\"2025-01-15T10:00:00Z\"}",
      "2025-01-15T10:00:00Z",
    ),
    #(
      "at://did:plc:2/xyz.statusphere.status/2",
      "cid2",
      "did:plc:2",
      "xyz.statusphere.status",
      "{\"status\":\"🎉\",\"createdAt\":\"2025-01-20T10:00:00Z\"}",
      "2025-01-20T10:00:00Z",
    ),
    #(
      "at://did:plc:3/xyz.statusphere.status/3",
      "cid3",
      "did:plc:3",
      "xyz.statusphere.status",
      "{\"status\":\"🤔\",\"createdAt\":\"2025-01-10T10:00:00Z\"}",
      "2025-01-10T10:00:00Z",
    ),
    // Record with NULL createdAt
    #(
      "at://did:plc:4/xyz.statusphere.status/4",
      "cid4",
      "did:plc:4",
      "xyz.statusphere.status",
      "{\"status\":\"😴\",\"createdAt\":null}",
      "2025-01-25T10:00:00Z",
    ),
    // Record with invalid createdAt
    #(
      "at://did:plc:5/xyz.statusphere.status/5",
      "cid5",
      "did:plc:5",
      "xyz.statusphere.status",
      "{\"status\":\"🤷\",\"createdAt\":\"wowzers\"}",
      "2025-01-18T10:00:00Z",
    ),
  ]

  list.each(records, fn(record) {
    let #(uri, cid, did, collection, json, indexed_at) = record
    let insert_sql =
      "INSERT INTO record (uri, cid, did, collection, json, indexed_at)
       VALUES ('"
      <> uri
      <> "', '"
      <> cid
      <> "', '"
      <> did
      <> "', '"
      <> collection
      <> "', '"
      <> json
      <> "', '"
      <> indexed_at
      <> "')"

    let assert Ok(_) = sqlight.exec(insert_sql, conn)
    Nil
  })

  conn
}

// Test: Sort by indexedAt DESC (default)
pub fn test_sort_by_indexed_at_desc() {
  let conn = create_test_db_with_records()

  let result =
    database.get_records_by_collection_paginated(
      conn,
      "xyz.statusphere.status",
      Some(10),
      None,
      None,
      None,
      Some([#("indexed_at", "desc")]),
    )

  case result {
    Ok(#(records, _, _, _)) -> {
      // First record should be most recent (2025-01-25)
      case list.first(records) {
        Ok(first) ->
          should.equal(first.indexed_at, "2025-01-25T10:00:00Z")
        Error(_) -> should.be_true(False)
      }

      // Verify order: 2025-01-25, 2025-01-20, 2025-01-18, 2025-01-15, 2025-01-10
      let dates = list.map(records, fn(r) { r.indexed_at })
      should.equal(dates, [
        "2025-01-25T10:00:00Z",
        "2025-01-20T10:00:00Z",
        "2025-01-18T10:00:00Z",
        "2025-01-15T10:00:00Z",
        "2025-01-10T10:00:00Z",
      ])
    }
    Error(_) -> should.be_true(False)
  }
}

// Test: Sort by indexedAt ASC
pub fn test_sort_by_indexed_at_asc() {
  let conn = create_test_db_with_records()

  let result =
    database.get_records_by_collection_paginated(
      conn,
      "xyz.statusphere.status",
      Some(10),
      None,
      None,
      None,
      Some([#("indexed_at", "asc")]),
    )

  case result {
    Ok(#(records, _, _, _)) -> {
      // First record should be oldest (2025-01-10)
      case list.first(records) {
        Ok(first) ->
          should.equal(first.indexed_at, "2025-01-10T10:00:00Z")
        Error(_) -> should.be_true(False)
      }

      // Verify ascending order
      let dates = list.map(records, fn(r) { r.indexed_at })
      should.equal(dates, [
        "2025-01-10T10:00:00Z",
        "2025-01-15T10:00:00Z",
        "2025-01-18T10:00:00Z",
        "2025-01-20T10:00:00Z",
        "2025-01-25T10:00:00Z",
      ])
    }
    Error(_) -> should.be_true(False)
  }
}

// Test: Sort by JSON field (createdAt) DESC with NULLS LAST
pub fn test_sort_by_json_field_desc_nulls_last() {
  let conn = create_test_db_with_records()

  let result =
    database.get_records_by_collection_paginated(
      conn,
      "xyz.statusphere.status",
      Some(10),
      None,
      None,
      None,
      Some([#("createdAt", "desc")]),
    )

  case result {
    Ok(#(records, _, _, _)) -> {
      // First record should have newest createdAt (2025-01-20)
      case list.first(records) {
        Ok(first) ->
          should.equal(first.indexed_at, "2025-01-20T10:00:00Z")
        Error(_) -> should.be_true(False)
      }

      // Last two records should be NULL and invalid date (NULLS LAST)
      case list.reverse(records) {
        [last, second_last, ..] -> {
          // These should be the records with null or invalid dates
          let last_indexed = [last.indexed_at, second_last.indexed_at]
          // Should contain both the null and "wowzers" records
          should.be_true(
            list.contains(last_indexed, "2025-01-25T10:00:00Z")
            || list.contains(last_indexed, "2025-01-18T10:00:00Z"),
          )
        }
        _ -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}

// Test: Sort by JSON field (createdAt) ASC with NULLS LAST
pub fn test_sort_by_json_field_asc_nulls_last() {
  let conn = create_test_db_with_records()

  let result =
    database.get_records_by_collection_paginated(
      conn,
      "xyz.statusphere.status",
      Some(10),
      None,
      None,
      None,
      Some([#("createdAt", "asc")]),
    )

  case result {
    Ok(#(records, _, _, _)) -> {
      // First record should have oldest valid createdAt (2025-01-10)
      case list.first(records) {
        Ok(first) ->
          should.equal(first.indexed_at, "2025-01-10T10:00:00Z")
        Error(_) -> should.be_true(False)
      }

      // Last two should still be NULL/invalid (NULLS LAST even with ASC)
      case list.reverse(records) {
        [last, second_last, ..] -> {
          let last_indexed = [last.indexed_at, second_last.indexed_at]
          should.be_true(
            list.contains(last_indexed, "2025-01-25T10:00:00Z")
            || list.contains(last_indexed, "2025-01-18T10:00:00Z"),
          )
        }
        _ -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}

// Test: Pagination with sorting (first N records)
pub fn test_pagination_with_sorting() {
  let conn = create_test_db_with_records()

  // Get first 2 records sorted by createdAt DESC
  let result =
    database.get_records_by_collection_paginated(
      conn,
      "xyz.statusphere.status",
      Some(2),
      None,
      None,
      None,
      Some([#("createdAt", "desc")]),
    )

  case result {
    Ok(#(records, _, has_next, _)) -> {
      // Should get exactly 2 records
      should.equal(list.length(records), 2)

      // Should have next page
      should.be_true(has_next)

      // First should be 2025-01-20, second should be 2025-01-15
      case records {
        [first, second] -> {
          should.equal(first.indexed_at, "2025-01-20T10:00:00Z")
          should.equal(second.indexed_at, "2025-01-15T10:00:00Z")
        }
        _ -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}

// Test: Invalid date strings are treated as NULL
pub fn test_invalid_dates_treated_as_null() {
  let conn = create_test_db_with_records()

  let result =
    database.get_records_by_collection_paginated(
      conn,
      "xyz.statusphere.status",
      Some(10),
      None,
      None,
      None,
      Some([#("createdAt", "desc")]),
    )

  case result {
    Ok(#(records, _, _, _)) -> {
      // The record with "wowzers" should be near the end (treated as NULL)
      // Find the "wowzers" record by its indexed_at
      let wowzers_position =
        list.index_map(records, fn(r: database.Record, idx) {
          case r.indexed_at == "2025-01-18T10:00:00Z" {
            True -> Some(idx)
            False -> None
          }
        })
        |> list.filter_map(fn(x) { option.to_result(x, Nil) })
        |> list.first

      case wowzers_position {
        Ok(pos) -> {
          // Should be in last 2 positions (index 3 or 4 out of 5 records)
          should.be_true(pos >= 3)
        }
        Error(_) -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}

// Test: Cursor-based pagination works correctly
pub fn test_cursor_pagination() {
  let conn = create_test_db_with_records()

  // Get first page of 2 records
  let first_page =
    database.get_records_by_collection_paginated(
      conn,
      "xyz.statusphere.status",
      Some(2),
      None,
      None,
      None,
      Some([#("indexed_at", "desc")]),
    )

  case first_page {
    Ok(#(first_records, Some(end_cursor), has_next, _)) -> {
      // Should get exactly 2 records
      should.equal(list.length(first_records), 2)

      // Should have next page
      should.be_true(has_next)

      // First page should be most recent (2025-01-25 and 2025-01-20)
      case first_records {
        [first, second] -> {
          should.equal(first.indexed_at, "2025-01-25T10:00:00Z")
          should.equal(second.indexed_at, "2025-01-20T10:00:00Z")

          // Now get second page using the cursor
          let second_page =
            database.get_records_by_collection_paginated(
              conn,
              "xyz.statusphere.status",
              Some(2),
              Some(end_cursor),
              None,
              None,
              Some([#("indexed_at", "desc")]),
            )

          case second_page {
            Ok(#(second_records, _, second_has_next, _)) -> {
              // Should get exactly 2 records
              should.equal(list.length(second_records), 2)

              // Should have next page (1 record remaining)
              should.be_true(second_has_next)

              // Second page should be next two (2025-01-18 and 2025-01-15)
              case second_records {
                [third, fourth] -> {
                  should.equal(third.indexed_at, "2025-01-18T10:00:00Z")
                  should.equal(fourth.indexed_at, "2025-01-15T10:00:00Z")

                  // Verify no overlap - records should be different
                  should.not_equal(first.uri, third.uri)
                  should.not_equal(first.uri, fourth.uri)
                  should.not_equal(second.uri, third.uri)
                  should.not_equal(second.uri, fourth.uri)
                }
                _ -> should.be_true(False)
              }
            }
            Error(_) -> should.be_true(False)
          }
        }
        _ -> should.be_true(False)
      }
    }
    _ -> should.be_true(False)
  }
}

// Test: Cursor pagination with no next page
pub fn test_cursor_pagination_last_page() {
  let conn = create_test_db_with_records()

  // Get first 4 records, leaving only 1
  let first_page =
    database.get_records_by_collection_paginated(
      conn,
      "xyz.statusphere.status",
      Some(4),
      None,
      None,
      None,
      Some([#("indexed_at", "desc")]),
    )

  case first_page {
    Ok(#(_, Some(end_cursor), has_next, _)) -> {
      // Should have next page
      should.be_true(has_next)

      // Get last page
      let last_page =
        database.get_records_by_collection_paginated(
          conn,
          "xyz.statusphere.status",
          Some(2),
          Some(end_cursor),
          None,
          None,
          Some([#("indexed_at", "desc")]),
        )

      case last_page {
        Ok(#(last_records, _, last_has_next, _)) -> {
          // Should get exactly 1 record (only 1 remaining)
          should.equal(list.length(last_records), 1)

          // Should NOT have next page
          should.be_false(last_has_next)

          // Should be the oldest record
          case list.first(last_records) {
            Ok(last) -> should.equal(last.indexed_at, "2025-01-10T10:00:00Z")
            Error(_) -> should.be_true(False)
          }
        }
        Error(_) -> should.be_true(False)
      }
    }
    _ -> should.be_true(False)
  }
}
