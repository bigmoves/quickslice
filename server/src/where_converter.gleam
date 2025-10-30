/// Converts GraphQL where input types to SQL where clause types
///
/// This module bridges the gap between the GraphQL layer (lexicon_graphql/where_input)
/// and the database layer (where_clause with sqlight types).
import gleam/dict
import gleam/list
import gleam/option
import lexicon_graphql/where_input
import sqlight
import where_clause

/// Convert a where_input.WhereValue to a sqlight.Value
fn convert_value(value: where_input.WhereValue) -> sqlight.Value {
  case value {
    where_input.StringValue(s) -> sqlight.text(s)
    where_input.IntValue(i) -> sqlight.int(i)
    where_input.BoolValue(b) -> sqlight.bool(b)
  }
}

/// Convert a where_input.WhereCondition to a where_clause.WhereCondition
fn convert_condition(
  cond: where_input.WhereCondition,
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

/// Convert a where_input.WhereClause to a where_clause.WhereClause
pub fn convert_where_clause(
  clause: where_input.WhereClause,
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
