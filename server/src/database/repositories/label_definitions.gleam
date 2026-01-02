/// Repository for label definitions
import database/executor.{type DbError, type Executor, Text}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// Valid visibility values for label preferences
pub const valid_visibilities = ["ignore", "show", "warn", "hide"]

/// Validate a visibility value
/// Returns Ok(visibility) if valid, Error with message if invalid
pub fn validate_visibility(visibility: String) -> Result(String, String) {
  case list.contains(valid_visibilities, visibility) {
    True -> Ok(visibility)
    False ->
      Error(
        "Invalid visibility. Must be one of: "
        <> string.join(valid_visibilities, ", "),
      )
  }
}

/// Label definition domain type
pub type LabelDefinition {
  LabelDefinition(
    val: String,
    description: String,
    severity: String,
    default_visibility: String,
    created_at: String,
  )
}

/// Get all label definitions
pub fn get_all(exec: Executor) -> Result(List(LabelDefinition), DbError) {
  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "SELECT val, description, severity, default_visibility, created_at FROM label_definition ORDER BY val"
    executor.PostgreSQL ->
      "SELECT val, description, severity, default_visibility, created_at::text FROM label_definition ORDER BY val"
  }
  executor.query(exec, sql, [], label_definition_decoder())
}

/// Get all non-system label definitions (excludes labels starting with !)
pub fn get_non_system(exec: Executor) -> Result(List(LabelDefinition), DbError) {
  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "SELECT val, description, severity, default_visibility, created_at FROM label_definition WHERE val NOT LIKE '!%' ORDER BY val"
    executor.PostgreSQL ->
      "SELECT val, description, severity, default_visibility, created_at::text FROM label_definition WHERE val NOT LIKE '!%' ORDER BY val"
  }
  executor.query(exec, sql, [], label_definition_decoder())
}

/// Get a label definition by value
pub fn get(
  exec: Executor,
  val: String,
) -> Result(Option(LabelDefinition), DbError) {
  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "SELECT val, description, severity, default_visibility, created_at FROM label_definition WHERE val = "
      <> executor.placeholder(exec, 1)
    executor.PostgreSQL ->
      "SELECT val, description, severity, default_visibility, created_at::text FROM label_definition WHERE val = "
      <> executor.placeholder(exec, 1)
  }
  case executor.query(exec, sql, [Text(val)], label_definition_decoder()) {
    Ok([def]) -> Ok(Some(def))
    Ok(_) -> Ok(None)
    Error(e) -> Error(e)
  }
}

/// Insert a new label definition
pub fn insert(
  exec: Executor,
  val: String,
  description: String,
  severity: String,
  default_visibility: String,
) -> Result(Nil, DbError) {
  let p1 = executor.placeholder(exec, 1)
  let p2 = executor.placeholder(exec, 2)
  let p3 = executor.placeholder(exec, 3)
  let p4 = executor.placeholder(exec, 4)
  let sql =
    "INSERT INTO label_definition (val, description, severity, default_visibility) VALUES ("
    <> p1
    <> ", "
    <> p2
    <> ", "
    <> p3
    <> ", "
    <> p4
    <> ")"
  executor.exec(exec, sql, [
    Text(val),
    Text(description),
    Text(severity),
    Text(default_visibility),
  ])
}

/// Check if a label value exists
pub fn exists(exec: Executor, val: String) -> Result(Bool, DbError) {
  case get(exec, val) {
    Ok(Some(_)) -> Ok(True)
    Ok(None) -> Ok(False)
    Error(e) -> Error(e)
  }
}

/// Decoder for LabelDefinition
fn label_definition_decoder() -> decode.Decoder(LabelDefinition) {
  use val <- decode.field(0, decode.string)
  use description <- decode.field(1, decode.string)
  use severity <- decode.field(2, decode.string)
  use default_visibility <- decode.field(3, decode.string)
  use created_at <- decode.field(4, decode.string)
  decode.success(LabelDefinition(
    val:,
    description:,
    severity:,
    default_visibility:,
    created_at:,
  ))
}
