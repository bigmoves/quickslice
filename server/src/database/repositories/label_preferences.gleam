/// Repository for actor label preferences
import database/executor.{type DbError, type Executor, Text}
import gleam/dynamic/decode
import gleam/option.{type Option, None, Some}

/// A label preference record
pub type LabelPreference {
  LabelPreference(
    did: String,
    label_val: String,
    visibility: String,
    created_at: String,
  )
}

/// Get all preferences for a user
pub fn get_by_did(
  exec: Executor,
  did: String,
) -> Result(List(LabelPreference), DbError) {
  let p1 = executor.placeholder(exec, 1)
  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "SELECT did, label_val, visibility, created_at FROM actor_label_preference WHERE did = "
      <> p1
    executor.PostgreSQL ->
      "SELECT did, label_val, visibility, created_at::text FROM actor_label_preference WHERE did = "
      <> p1
  }

  executor.query(exec, sql, [Text(did)], preference_decoder())
}

/// Get a specific preference
pub fn get(
  exec: Executor,
  did: String,
  label_val: String,
) -> Result(Option(LabelPreference), DbError) {
  let p1 = executor.placeholder(exec, 1)
  let p2 = executor.placeholder(exec, 2)
  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "SELECT did, label_val, visibility, created_at FROM actor_label_preference WHERE did = "
      <> p1
      <> " AND label_val = "
      <> p2
    executor.PostgreSQL ->
      "SELECT did, label_val, visibility, created_at::text FROM actor_label_preference WHERE did = "
      <> p1
      <> " AND label_val = "
      <> p2
  }

  case
    executor.query(
      exec,
      sql,
      [Text(did), Text(label_val)],
      preference_decoder(),
    )
  {
    Ok([pref]) -> Ok(Some(pref))
    Ok(_) -> Ok(None)
    Error(e) -> Error(e)
  }
}

/// Set a preference (upsert)
pub fn set(
  exec: Executor,
  did: String,
  label_val: String,
  visibility: String,
) -> Result(LabelPreference, DbError) {
  let p1 = executor.placeholder(exec, 1)
  let p2 = executor.placeholder(exec, 2)
  let p3 = executor.placeholder(exec, 3)

  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "INSERT INTO actor_label_preference (did, label_val, visibility) VALUES ("
      <> p1
      <> ", "
      <> p2
      <> ", "
      <> p3
      <> ") ON CONFLICT(did, label_val) DO UPDATE SET visibility = excluded.visibility RETURNING did, label_val, visibility, created_at"
    executor.PostgreSQL ->
      "INSERT INTO actor_label_preference (did, label_val, visibility) VALUES ("
      <> p1
      <> ", "
      <> p2
      <> ", "
      <> p3
      <> ") ON CONFLICT(did, label_val) DO UPDATE SET visibility = excluded.visibility RETURNING did, label_val, visibility, created_at::text"
  }

  case
    executor.query(
      exec,
      sql,
      [Text(did), Text(label_val), Text(visibility)],
      preference_decoder(),
    )
  {
    Ok([pref]) -> Ok(pref)
    Ok(_) -> Error(executor.QueryError("Set did not return preference"))
    Error(e) -> Error(e)
  }
}

/// Delete a preference (reset to default)
pub fn delete(
  exec: Executor,
  did: String,
  label_val: String,
) -> Result(Nil, DbError) {
  let p1 = executor.placeholder(exec, 1)
  let p2 = executor.placeholder(exec, 2)
  let sql =
    "DELETE FROM actor_label_preference WHERE did = "
    <> p1
    <> " AND label_val = "
    <> p2

  executor.exec(exec, sql, [Text(did), Text(label_val)])
}

/// Decoder for LabelPreference
fn preference_decoder() -> decode.Decoder(LabelPreference) {
  use did <- decode.field(0, decode.string)
  use label_val <- decode.field(1, decode.string)
  use visibility <- decode.field(2, decode.string)
  use created_at <- decode.field(3, decode.string)
  decode.success(LabelPreference(did:, label_val:, visibility:, created_at:))
}
