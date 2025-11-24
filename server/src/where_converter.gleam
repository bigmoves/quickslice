/// Converts GraphQL where input types to SQL where clause types
///
/// This module bridges the gap between the GraphQL layer (lexicon_graphql/input/where)
/// and the database layer (where_clause with sqlight types).
import gleam/dict
import gleam/list
import gleam/option
import lexicon_graphql/input/where
import sqlight
import where_clause

/// Convert a where.WhereValue to a sqlight.Value
fn convert_value(value: where.WhereValue) -> sqlight.Value {
  case value {
    where.StringValue(s) -> sqlight.text(s)
    where.IntValue(i) -> sqlight.int(i)
    where.BoolValue(b) -> sqlight.bool(b)
  }
}

/// Convert a where.WhereCondition to a where_clause.WhereCondition
fn convert_condition(
  cond: where.WhereCondition,
) -> where_clause.WhereCondition {
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
