// server/src/database/postgres/executor.gleam

import database/executor.{
  type DbError, type Executor, type Value, Blob, Bool, ConnectionError,
  ConstraintError, DecodeError, Float, Int, Null, PostgreSQL, QueryError, Text,
}
import gleam/dynamic/decode
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import pog

/// Create an Executor for PostgreSQL from a connection pool
pub fn new(pool: pog.Connection) -> Executor {
  executor.new(
    PostgreSQL,
    fn(sql, params) {
      pog.query(sql)
      |> set_pog_params(params)
      |> pog.returning(decode.dynamic)
      |> pog.execute(pool)
      |> result.map(fn(returned) { returned.rows })
      |> result.map_error(pog_error_to_db_error)
    },
    fn(sql, params) {
      pog.query(sql)
      |> set_pog_params(params)
      |> pog.execute(pool)
      |> result.map(fn(_) { Nil })
      |> result.map_error(pog_error_to_db_error)
    },
    fn(index) { "$" <> int.to_string(index) },
    fn(column, field) { column <> "->>'" <> field <> "'" },
    fn(column, path) {
      case path {
        [] -> column
        [single] -> column <> "->>'" <> single <> "'"
        _ -> {
          // All but last use ->, last uses ->>
          let path_parts =
            list.index_map(path, fn(part, i) {
              case i == list.length(path) - 1 {
                True -> "->>'" <> part <> "'"
                False -> "->'" <> part <> "'"
              }
            })
          column <> string.join(path_parts, "")
        }
      }
    },
    fn() { "NOW()" },
  )
}

/// Set parameters on a pog query
fn set_pog_params(query: pog.Query(a), params: List(Value)) -> pog.Query(a) {
  list.fold(params, query, fn(q, param) {
    case param {
      Text(s) -> pog.parameter(q, pog.text(s))
      Int(i) -> pog.parameter(q, pog.int(i))
      Float(f) -> pog.parameter(q, pog.float(f))
      Bool(b) -> pog.parameter(q, pog.bool(b))
      Null -> pog.parameter(q, pog.null())
      Blob(b) -> pog.parameter(q, pog.bytea(b))
    }
  })
}

/// Convert pog.QueryError to our DbError type
fn pog_error_to_db_error(err: pog.QueryError) -> DbError {
  case err {
    pog.ConstraintViolated(message, constraint, _detail) ->
      ConstraintError(message <> ": " <> constraint)
    pog.PostgresqlError(code, name, message) ->
      QueryError("[" <> code <> " " <> name <> "] " <> message)
    pog.UnexpectedArgumentCount(expected, got) ->
      QueryError(
        "Expected "
        <> int.to_string(expected)
        <> " arguments, got "
        <> int.to_string(got),
      )
    pog.UnexpectedArgumentType(expected, got) ->
      QueryError("Expected argument type " <> expected <> ", got " <> got)
    pog.UnexpectedResultType(errors) ->
      DecodeError("Failed to decode result: " <> string.inspect(errors))
    pog.ConnectionUnavailable ->
      ConnectionError("Database connection unavailable")
    pog.QueryTimeout -> QueryError("Query timeout")
  }
}
