/// Integration tests for labels and reports
///
/// Tests the moderation system including:
/// - Label creation and retrieval
/// - Report creation and retrieval
/// - Takedown filtering in record queries
/// - Self-labels parsing and merging
import database/repositories/labels
import database/repositories/reports
import gleam/dict
import gleam/list
import gleam/option
import gleeunit/should
import graphql/lexicon/fetchers
import swell/value
import test_helpers

// =============================================================================
// Label Repository Tests
// =============================================================================

pub fn label_insert_and_get_by_uri_test() {
  // Setup test database
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  let src = "did:plc:admin123"
  let uri = "at://did:plc:user1/app.bsky.feed.post/abc123"
  let val = "spam"

  // Insert a label
  let assert Ok(label) =
    labels.insert(db, src, uri, option.None, val, option.None)

  // Verify label fields
  label.src |> should.equal(src)
  label.uri |> should.equal(uri)
  label.val |> should.equal(val)
  label.neg |> should.equal(False)

  // Get by URI should return the label
  let assert Ok(found_labels) = labels.get_by_uris(db, [uri])
  found_labels |> list.length() |> should.equal(1)

  let assert [found] = found_labels
  found.val |> should.equal(val)
}

pub fn label_negation_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  let src = "did:plc:admin123"
  let uri = "at://did:plc:user1/app.bsky.feed.post/abc123"

  // Insert a label
  let assert Ok(_) =
    labels.insert(db, src, uri, option.None, "spam", option.None)

  // Insert negation
  let assert Ok(neg_label) = labels.insert_negation(db, src, uri, "spam")
  neg_label.neg |> should.equal(True)

  // get_by_uris should only return active (non-negated) labels
  let assert Ok(found_labels) = labels.get_by_uris(db, [uri])
  // The negation retracts the original label, so get_by_uris should return 0
  // (The original is excluded because a later negation exists for it)
  found_labels |> list.length() |> should.equal(0)
}

pub fn label_get_all_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  let src = "did:plc:admin123"

  // Insert multiple labels
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

  // Get all with no filters
  let assert Ok(all_labels) =
    labels.get_all(db, option.None, option.None, 100, option.None)
  all_labels |> list.length() |> should.equal(3)

  // Filter by val
  let assert Ok(spam_labels) =
    labels.get_all(db, option.None, option.Some("spam"), 100, option.None)
  spam_labels |> list.length() |> should.equal(2)
}

// =============================================================================
// Takedown Filtering Tests
// =============================================================================

pub fn takedown_uris_returns_takedown_labels_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  let src = "did:plc:admin123"
  let takedown_uri = "at://did:plc:user1/app.bsky.feed.post/takedown"
  let normal_uri = "at://did:plc:user2/app.bsky.feed.post/normal"
  let suspend_uri = "at://did:plc:user3/app.bsky.feed.post/suspend"

  // Insert labels - takedown, normal, and suspend
  let assert Ok(_) =
    labels.insert(db, src, takedown_uri, option.None, "!takedown", option.None)
  let assert Ok(_) =
    labels.insert(db, src, normal_uri, option.None, "spam", option.None)
  let assert Ok(_) =
    labels.insert(db, src, suspend_uri, option.None, "!suspend", option.None)

  // get_takedown_uris should return only !takedown and !suspend URIs
  let assert Ok(takedown_list) =
    labels.get_takedown_uris(db, [takedown_uri, normal_uri, suspend_uri])

  takedown_list |> list.length() |> should.equal(2)
  takedown_list |> list.contains(takedown_uri) |> should.equal(True)
  takedown_list |> list.contains(suspend_uri) |> should.equal(True)
  takedown_list |> list.contains(normal_uri) |> should.equal(False)
}

// =============================================================================
// Report Repository Tests
// =============================================================================

pub fn report_insert_and_get_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  let reporter_did = "did:plc:reporter123"
  let subject_uri = "at://did:plc:user1/app.bsky.feed.post/abc123"

  // Insert a report
  let assert Ok(report) =
    reports.insert(
      db,
      reporter_did,
      subject_uri,
      "spam",
      option.Some("This is spam content"),
    )

  report.reporter_did |> should.equal(reporter_did)
  report.subject_uri |> should.equal(subject_uri)
  report.reason_type |> should.equal("spam")
  report.status |> should.equal("pending")

  // Get by ID
  let assert Ok(option.Some(found)) = reports.get(db, report.id)
  found.id |> should.equal(report.id)
}

pub fn report_get_all_by_status_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  let reporter = "did:plc:reporter123"

  // Create multiple reports
  let assert Ok(_) =
    reports.insert(db, reporter, "at://did/post/1", "spam", option.None)
  let assert Ok(_) =
    reports.insert(db, reporter, "at://did/post/2", "violation", option.None)
  let assert Ok(_) =
    reports.insert(db, reporter, "at://did/post/3", "rude", option.None)

  // Get all pending reports
  let assert Ok(pending) =
    reports.get_all(db, option.Some("pending"), 100, option.None)
  pending |> list.length() |> should.equal(3)
}

pub fn report_resolve_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  let admin = "did:plc:admin123"
  let reporter = "did:plc:reporter123"

  // Create reports
  let assert Ok(report1) =
    reports.insert(db, reporter, "at://did/post/1", "spam", option.None)
  let assert Ok(report2) =
    reports.insert(db, reporter, "at://did/post/2", "violation", option.None)

  // Resolve first report
  let assert Ok(resolved) = reports.resolve(db, report1.id, "resolved", admin)
  resolved.status |> should.equal("resolved")
  resolved.resolved_by |> should.equal(option.Some(admin))

  // Dismiss second report (status = "dismissed")
  let assert Ok(dismissed) = reports.resolve(db, report2.id, "dismissed", admin)
  dismissed.status |> should.equal("dismissed")

  // Get pending should return 0
  let assert Ok(pending) =
    reports.get_all(db, option.Some("pending"), 100, option.None)
  pending |> list.length() |> should.equal(0)

  // Get resolved should return 1
  let assert Ok(resolved_list) =
    reports.get_all(db, option.Some("resolved"), 100, option.None)
  resolved_list |> list.length() |> should.equal(1)
}

// =============================================================================
// Integration with Records (Takedown Filtering)
// =============================================================================

pub fn takedown_label_identified_for_filtering_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_core_tables(db)
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  let uri1 = "at://did:plc:user1/app.bsky.feed.post/abc123"
  let uri2 = "at://did:plc:user2/app.bsky.feed.post/def456"

  // Apply takedown label to first record
  let assert Ok(_) =
    labels.insert(
      db,
      "did:plc:admin",
      uri1,
      option.None,
      "!takedown",
      option.None,
    )

  // Check that get_takedown_uris returns the takedown URI
  let assert Ok(takedown_uris) = labels.get_takedown_uris(db, [uri1, uri2])
  takedown_uris |> list.length() |> should.equal(1)
  takedown_uris |> list.contains(uri1) |> should.equal(True)
  takedown_uris |> list.contains(uri2) |> should.equal(False)
}

// =============================================================================
// Self-Labels Parsing Tests
// =============================================================================

pub fn self_labels_parsing_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  let uri = "at://did:plc:user1/app.bsky.feed.post/abc123"
  let json_with_self_labels =
    "{\"text\": \"hello\", \"labels\": {\"$type\": \"com.atproto.label.defs#selfLabels\", \"values\": [{\"val\": \"porn\"}]}}"

  // Create labels fetcher
  let fetcher = fetchers.labels_fetcher(db)

  // Call fetcher with JSON containing self-labels
  let assert Ok(results) = fetcher([#(uri, option.Some(json_with_self_labels))])

  // Should have self-label
  let assert Ok(labels_list) = dict.get(results, uri)
  labels_list |> list.length() |> should.equal(1)

  // Check the label has correct val and src (author DID)
  let assert [label] = labels_list
  case label {
    value.Object(fields) -> {
      let assert Ok(value.String(val)) = list.key_find(fields, "val")
      val |> should.equal("porn")

      let assert Ok(value.String(src)) = list.key_find(fields, "src")
      src |> should.equal("did:plc:user1")
    }
    _ -> should.fail()
  }
}

pub fn self_labels_without_labels_field_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  let uri = "at://did:plc:user1/app.bsky.feed.post/abc123"
  let json_without_labels = "{\"text\": \"hello world\"}"

  // Create labels fetcher
  let fetcher = fetchers.labels_fetcher(db)

  // Call fetcher with JSON without labels
  let assert Ok(results) = fetcher([#(uri, option.Some(json_without_labels))])

  // Should have no labels
  case dict.get(results, uri) {
    Ok(labels_list) -> labels_list |> list.length() |> should.equal(0)
    Error(_) -> Nil
    // Also acceptable - no entry means no labels
  }
}

pub fn merged_labels_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  let uri = "at://did:plc:user1/app.bsky.feed.post/abc123"
  let json_with_self_labels =
    "{\"text\": \"hello\", \"labels\": {\"$type\": \"com.atproto.label.defs#selfLabels\", \"values\": [{\"val\": \"porn\"}]}}"

  // Add moderator label
  let assert Ok(_) =
    labels.insert(db, "did:plc:admin", uri, option.None, "spam", option.None)

  // Create labels fetcher
  let fetcher = fetchers.labels_fetcher(db)

  // Call fetcher with JSON containing self-labels
  let assert Ok(results) = fetcher([#(uri, option.Some(json_with_self_labels))])

  // Should have both labels (1 self + 1 moderator)
  let assert Ok(labels_list) = dict.get(results, uri)
  labels_list |> list.length() |> should.equal(2)

  // Check we have both "porn" (self) and "spam" (moderator)
  let vals =
    list.filter_map(labels_list, fn(label) {
      case label {
        value.Object(fields) -> {
          case list.key_find(fields, "val") {
            Ok(value.String(v)) -> Ok(v)
            _ -> Error(Nil)
          }
        }
        _ -> Error(Nil)
      }
    })
  vals |> list.contains("porn") |> should.equal(True)
  vals |> list.contains("spam") |> should.equal(True)
}

pub fn multiple_self_labels_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  let uri = "at://did:plc:user1/app.bsky.feed.post/abc123"
  let json_with_multiple_labels =
    "{\"text\": \"adult content\", \"labels\": {\"$type\": \"com.atproto.label.defs#selfLabels\", \"values\": [{\"val\": \"porn\"}, {\"val\": \"sexual\"}]}}"

  // Create labels fetcher
  let fetcher = fetchers.labels_fetcher(db)

  // Call fetcher with JSON containing multiple self-labels
  let assert Ok(results) =
    fetcher([#(uri, option.Some(json_with_multiple_labels))])

  // Should have both self-labels
  let assert Ok(labels_list) = dict.get(results, uri)
  labels_list |> list.length() |> should.equal(2)
}
