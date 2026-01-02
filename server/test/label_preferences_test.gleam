/// Integration tests for label preferences
///
/// Tests the label preference system including:
/// - Setting label preferences
/// - Getting label preferences for a user
/// - Non-system label filtering
/// - Validation of visibility values
import database/repositories/label_definitions
import database/repositories/label_preferences
import gleam/list
import gleam/option
import gleam/string
import gleeunit/should
import test_helpers

// =============================================================================
// Label Definitions Repository Tests (with default_visibility)
// =============================================================================

pub fn label_definitions_get_includes_default_visibility_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  // Get a label definition
  let assert Ok(option.Some(def)) = label_definitions.get(db, "spam")

  // Should have default_visibility field
  def.val |> should.equal("spam")
  def.default_visibility |> should.equal("warn")
}

pub fn label_definitions_get_non_system_excludes_system_labels_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  // Get non-system labels
  let assert Ok(defs) = label_definitions.get_non_system(db)

  // Should have at least some labels
  list.length(defs) |> should.not_equal(0)

  // None should start with !
  list.all(defs, fn(def) { !string.starts_with(def.val, "!") })
  |> should.be_true()
}

pub fn label_definitions_get_non_system_includes_content_labels_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  // Get non-system labels
  let assert Ok(defs) = label_definitions.get_non_system(db)

  // Should include porn, spam, etc.
  let vals = list.map(defs, fn(d) { d.val })

  vals |> list.contains("porn") |> should.be_true()
  vals |> list.contains("spam") |> should.be_true()
  vals |> list.contains("sexual") |> should.be_true()
  vals |> list.contains("nudity") |> should.be_true()
}

// =============================================================================
// Label Preferences Repository Tests
// =============================================================================

pub fn label_preferences_set_and_get_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  let did = "did:plc:user123"
  let label_val = "spam"
  let visibility = "hide"

  // Set a preference
  let assert Ok(pref) = label_preferences.set(db, did, label_val, visibility)

  pref.did |> should.equal(did)
  pref.label_val |> should.equal(label_val)
  pref.visibility |> should.equal(visibility)

  // Get the preference
  let assert Ok(option.Some(found)) = label_preferences.get(db, did, label_val)

  found.visibility |> should.equal(visibility)
}

pub fn label_preferences_get_by_did_returns_all_user_prefs_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  let did = "did:plc:user123"

  // Set multiple preferences
  let assert Ok(_) = label_preferences.set(db, did, "spam", "hide")
  let assert Ok(_) = label_preferences.set(db, did, "porn", "warn")
  let assert Ok(_) = label_preferences.set(db, did, "nudity", "ignore")

  // Get all preferences for user
  let assert Ok(prefs) = label_preferences.get_by_did(db, did)

  prefs |> list.length() |> should.equal(3)

  // Verify each preference exists
  let vals = list.map(prefs, fn(p) { p.label_val })
  vals |> list.contains("spam") |> should.be_true()
  vals |> list.contains("porn") |> should.be_true()
  vals |> list.contains("nudity") |> should.be_true()
}

pub fn label_preferences_set_updates_existing_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  let did = "did:plc:user123"
  let label_val = "spam"

  // Set initial preference
  let assert Ok(_) = label_preferences.set(db, did, label_val, "warn")

  // Update preference
  let assert Ok(updated) = label_preferences.set(db, did, label_val, "hide")

  updated.visibility |> should.equal("hide")

  // Verify only one preference exists
  let assert Ok(prefs) = label_preferences.get_by_did(db, did)
  prefs |> list.length() |> should.equal(1)
}

pub fn label_preferences_delete_removes_preference_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  let did = "did:plc:user123"
  let label_val = "spam"

  // Set a preference
  let assert Ok(_) = label_preferences.set(db, did, label_val, "hide")

  // Delete the preference
  let assert Ok(_) = label_preferences.delete(db, did, label_val)

  // Should no longer exist
  let assert Ok(option.None) = label_preferences.get(db, did, label_val)
}

pub fn label_preferences_different_users_have_separate_prefs_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  let user1 = "did:plc:user1"
  let user2 = "did:plc:user2"

  // Set different preferences for different users
  let assert Ok(_) = label_preferences.set(db, user1, "spam", "hide")
  let assert Ok(_) = label_preferences.set(db, user2, "spam", "warn")

  // Get user1's preference
  let assert Ok(option.Some(pref1)) = label_preferences.get(db, user1, "spam")
  pref1.visibility |> should.equal("hide")

  // Get user2's preference
  let assert Ok(option.Some(pref2)) = label_preferences.get(db, user2, "spam")
  pref2.visibility |> should.equal("warn")
}

pub fn label_preferences_get_returns_none_when_not_set_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  // Get a preference that doesn't exist
  let assert Ok(option.None) =
    label_preferences.get(db, "did:plc:user123", "spam")
}

pub fn label_preferences_get_by_did_returns_empty_for_new_user_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  // Get preferences for a user with no preferences
  let assert Ok(prefs) = label_preferences.get_by_did(db, "did:plc:newuser")

  prefs |> list.length() |> should.equal(0)
}
