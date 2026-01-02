/// Integration tests for admin connection pagination
///
/// Tests that labels and reports return proper connection structure
/// with edges, pageInfo, and totalCount
import database/repositories/labels
import database/repositories/reports
import gleam/list
import gleam/option
import gleeunit/should
import test_helpers

// =============================================================================
// Labels Connection Pagination Tests
// =============================================================================

pub fn labels_get_paginated_returns_correct_structure_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  let src = "did:plc:admin123"

  // Insert 5 labels
  let assert Ok(_) =
    labels.insert(
      db,
      src,
      "at://did:plc:user1/app.bsky.feed.post/1",
      option.None,
      "spam",
      option.None,
    )
  let assert Ok(_) =
    labels.insert(
      db,
      src,
      "at://did:plc:user2/app.bsky.feed.post/2",
      option.None,
      "spam",
      option.None,
    )
  let assert Ok(_) =
    labels.insert(
      db,
      src,
      "at://did:plc:user3/app.bsky.feed.post/3",
      option.None,
      "spam",
      option.None,
    )
  let assert Ok(_) =
    labels.insert(
      db,
      src,
      "at://did:plc:user4/app.bsky.feed.post/4",
      option.None,
      "!warn",
      option.None,
    )
  let assert Ok(_) =
    labels.insert(
      db,
      src,
      "at://did:plc:user5/app.bsky.feed.post/5",
      option.None,
      "spam",
      option.None,
    )

  // Get first 2 labels
  let assert Ok(paginated) =
    labels.get_paginated(db, option.None, option.None, 2, option.None)

  // Should return 2 labels
  paginated.labels |> list.length() |> should.equal(2)
  // Should have next page (3 more labels)
  paginated.has_next_page |> should.equal(True)
  // Total count should be 5
  paginated.total_count |> should.equal(5)
}

pub fn labels_get_paginated_with_filter_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  let src = "did:plc:admin123"

  // Insert labels with different vals
  let assert Ok(_) =
    labels.insert(
      db,
      src,
      "at://did:plc:user1/app.bsky.feed.post/1",
      option.None,
      "spam",
      option.None,
    )
  let assert Ok(_) =
    labels.insert(
      db,
      src,
      "at://did:plc:user2/app.bsky.feed.post/2",
      option.None,
      "!warn",
      option.None,
    )
  let assert Ok(_) =
    labels.insert(
      db,
      src,
      "at://did:plc:user3/app.bsky.feed.post/3",
      option.None,
      "spam",
      option.None,
    )

  // Filter by val = "spam"
  let assert Ok(paginated) =
    labels.get_paginated(db, option.None, option.Some("spam"), 10, option.None)

  // Should return 2 spam labels
  paginated.labels |> list.length() |> should.equal(2)
  // No next page
  paginated.has_next_page |> should.equal(False)
  // Total count of spam labels is 2
  paginated.total_count |> should.equal(2)
}

pub fn labels_get_paginated_with_cursor_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  let src = "did:plc:admin123"

  // Insert 4 labels
  let assert Ok(_) =
    labels.insert(
      db,
      src,
      "at://did:plc:user1/app.bsky.feed.post/1",
      option.None,
      "spam",
      option.None,
    )
  let assert Ok(_) =
    labels.insert(
      db,
      src,
      "at://did:plc:user2/app.bsky.feed.post/2",
      option.None,
      "spam",
      option.None,
    )
  let assert Ok(_) =
    labels.insert(
      db,
      src,
      "at://did:plc:user3/app.bsky.feed.post/3",
      option.None,
      "spam",
      option.None,
    )
  let assert Ok(_) =
    labels.insert(
      db,
      src,
      "at://did:plc:user4/app.bsky.feed.post/4",
      option.None,
      "spam",
      option.None,
    )

  // Get first page (2 items)
  let assert Ok(page1) =
    labels.get_paginated(db, option.None, option.None, 2, option.None)

  page1.labels |> list.length() |> should.equal(2)
  page1.has_next_page |> should.equal(True)
  page1.total_count |> should.equal(4)

  // Get last item from first page to use as cursor
  let assert Ok(last_label) = list.last(page1.labels)

  // Get second page using cursor
  let assert Ok(page2) =
    labels.get_paginated(
      db,
      option.None,
      option.None,
      2,
      option.Some(last_label.id),
    )

  page2.labels |> list.length() |> should.equal(2)
  page2.has_next_page |> should.equal(False)
  // Total count is still 4 (cursor doesn't affect total)
  page2.total_count |> should.equal(4)

  // Verify second page has different labels (lower IDs)
  let assert Ok(first_label_page2) = list.first(page2.labels)
  { first_label_page2.id < last_label.id } |> should.equal(True)
}

pub fn labels_get_paginated_empty_result_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  // Get paginated with no labels
  let assert Ok(paginated) =
    labels.get_paginated(db, option.None, option.None, 10, option.None)

  paginated.labels |> list.length() |> should.equal(0)
  paginated.has_next_page |> should.equal(False)
  paginated.total_count |> should.equal(0)
}

// =============================================================================
// Reports Connection Pagination Tests
// =============================================================================

pub fn reports_get_paginated_returns_correct_structure_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  let reporter = "did:plc:reporter123"

  // Insert 4 reports
  let assert Ok(_) =
    reports.insert(db, reporter, "at://did/post/1", "spam", option.None)
  let assert Ok(_) =
    reports.insert(db, reporter, "at://did/post/2", "violation", option.None)
  let assert Ok(_) =
    reports.insert(db, reporter, "at://did/post/3", "rude", option.None)
  let assert Ok(_) =
    reports.insert(db, reporter, "at://did/post/4", "spam", option.None)

  // Get first 2 reports
  let assert Ok(paginated) =
    reports.get_paginated(db, option.None, 2, option.None)

  paginated.reports |> list.length() |> should.equal(2)
  paginated.has_next_page |> should.equal(True)
  paginated.total_count |> should.equal(4)
}

pub fn reports_get_paginated_with_status_filter_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  let reporter = "did:plc:reporter123"
  let admin = "did:plc:admin123"

  // Insert reports
  let assert Ok(report1) =
    reports.insert(db, reporter, "at://did/post/1", "spam", option.None)
  let assert Ok(_) =
    reports.insert(db, reporter, "at://did/post/2", "violation", option.None)
  let assert Ok(_) =
    reports.insert(db, reporter, "at://did/post/3", "rude", option.None)

  // Resolve first report
  let assert Ok(_) = reports.resolve(db, report1.id, "resolved", admin)

  // Filter by status = "pending"
  let assert Ok(paginated) =
    reports.get_paginated(db, option.Some("pending"), 10, option.None)

  paginated.reports |> list.length() |> should.equal(2)
  paginated.has_next_page |> should.equal(False)
  paginated.total_count |> should.equal(2)
}

pub fn reports_get_paginated_with_cursor_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  let reporter = "did:plc:reporter123"

  // Insert 4 reports
  let assert Ok(_) =
    reports.insert(db, reporter, "at://did/post/1", "spam", option.None)
  let assert Ok(_) =
    reports.insert(db, reporter, "at://did/post/2", "spam", option.None)
  let assert Ok(_) =
    reports.insert(db, reporter, "at://did/post/3", "spam", option.None)
  let assert Ok(_) =
    reports.insert(db, reporter, "at://did/post/4", "spam", option.None)

  // Get first page
  let assert Ok(page1) = reports.get_paginated(db, option.None, 2, option.None)

  page1.reports |> list.length() |> should.equal(2)
  page1.has_next_page |> should.equal(True)

  // Get last item from first page
  let assert Ok(last_report) = list.last(page1.reports)

  // Get second page using cursor
  let assert Ok(page2) =
    reports.get_paginated(db, option.None, 2, option.Some(last_report.id))

  page2.reports |> list.length() |> should.equal(2)
  page2.has_next_page |> should.equal(False)
  page2.total_count |> should.equal(4)
}
