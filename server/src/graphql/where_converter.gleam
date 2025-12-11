/// Converts GraphQL where input types to SQL where clause types
///
/// This module bridges the gap between the GraphQL layer (lexicon_graphql/input/where)
/// and the database layer (where_clause with database Value types).
import database/executor.{type Value, Bool, Int, Text}
import database/queries/where_clause
import gleam/dict
import gleam/list
import gleam/option.{type Option}
import lexicon_graphql/input/where

/// Convert a where.WhereValue to a database Value
fn convert_value(value: where.WhereValue) -> Value {
  case value {
    where.StringValue(s) -> Text(s)
    where.IntValue(i) -> Int(i)
    where.BoolValue(b) -> Bool(b)
  }
}

/// Check if a WhereValue is numeric (Int)
fn is_numeric_value(value: where.WhereValue) -> Bool {
  case value {
    where.IntValue(_) -> True
    where.StringValue(_) -> False
    where.BoolValue(_) -> False
  }
}

/// Check if any comparison value in the condition is numeric
fn has_numeric_comparison(cond: where.WhereCondition) -> Bool {
  let check_opt = fn(opt: Option(where.WhereValue)) -> Bool {
    case opt {
      option.Some(v) -> is_numeric_value(v)
      option.None -> False
    }
  }
  check_opt(cond.gt)
  || check_opt(cond.gte)
  || check_opt(cond.lt)
  || check_opt(cond.lte)
}

/// Convert a where.WhereCondition to a where_clause.WhereCondition
fn convert_condition(cond: where.WhereCondition) -> where_clause.WhereCondition {
  where_clause.WhereCondition(
    eq: option.map(cond.eq, convert_value),
    in_values: option.map(cond.in_values, fn(values) {
      list.map(values, convert_value)
    }),
    contains: cond.contains,
    gt: option.map(cond.gt, convert_value),
    gte: option.map(cond.gte, convert_value),
    lt: option.map(cond.lt, convert_value),
    lte: option.map(cond.lte, convert_value),
    is_null: cond.is_null,
    is_numeric: has_numeric_comparison(cond),
  )
}

/// Convert a where.WhereClause to a where_clause.WhereClause
pub fn convert_where_clause(
  clause: where.WhereClause,
) -> where_clause.WhereClause {
  where_clause.WhereClause(
    conditions: dict.map_values(clause.conditions, fn(_key, value) {
      convert_condition(value)
    }),
    and: option.map(clause.and, fn(clauses) {
      list.map(clauses, convert_where_clause)
    }),
    or: option.map(clause.or, fn(clauses) {
      list.map(clauses, convert_where_clause)
    }),
  )
}
