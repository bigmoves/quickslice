// server/src/database/sqlite/executor.gleam

import database/executor.{
  type DbError, type Executor, type Value, Blob, Bool, ConstraintError, Float,
  Int, Null, QueryError, SQLite, Text,
}
import gleam/dynamic/decode
import gleam/list
import gleam/result
import gleam/string
import sqlight

/// Create an Executor for SQLite from an open connection
pub fn new(conn: sqlight.Connection) -> Executor {
  executor.new(
    SQLite,
    fn(sql, params) {
      sqlight.query(
        sql,
        on: conn,
        with: to_sqlight_values(params),
        expecting: decode.dynamic,
      )
      |> result.map_error(sqlight_error_to_db_error)
    },
    fn(sql, params) {
      sqlight.query(
        sql,
        on: conn,
        with: to_sqlight_values(params),
        expecting: decode.dynamic,
      )
      |> result.map(fn(_) { Nil })
      |> result.map_error(sqlight_error_to_db_error)
    },
    fn(_index) { "?" },
    fn(column, field) { "json_extract(" <> column <> ", '$." <> field <> "')" },
    fn(column, path) {
      let path_str = string.join(path, ".")
      "json_extract(" <> column <> ", '$." <> path_str <> "')"
    },
    fn() { "datetime('now')" },
  )
}

/// Convert our Value type to sqlight.Value
fn to_sqlight_values(values: List(Value)) -> List(sqlight.Value) {
  list.map(values, fn(v) {
    case v {
      Text(s) -> sqlight.text(s)
      Int(i) -> sqlight.int(i)
      Float(f) -> sqlight.float(f)
      Bool(b) ->
        case b {
          True -> sqlight.int(1)
          False -> sqlight.int(0)
        }
      Null -> sqlight.null()
      Blob(b) -> sqlight.blob(b)
    }
  })
}

/// Convert sqlight.Error to our DbError type
fn sqlight_error_to_db_error(err: sqlight.Error) -> DbError {
  case err {
    sqlight.SqlightError(code, message, _) ->
      case code {
        sqlight.ConstraintCheck
        | sqlight.ConstraintCommithook
        | sqlight.ConstraintDatatype
        | sqlight.ConstraintForeignkey
        | sqlight.ConstraintFunction
        | sqlight.ConstraintNotnull
        | sqlight.ConstraintPinned
        | sqlight.ConstraintPrimarykey
        | sqlight.ConstraintRowid
        | sqlight.ConstraintTrigger
        | sqlight.ConstraintUnique
        | sqlight.ConstraintVtab -> ConstraintError(message)
        _ -> QueryError(message)
      }
  }
}
