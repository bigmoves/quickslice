/// GraphQL Where Input Parser
///
/// Provides parsing functions to convert GraphQL values to intermediate where clause types.
/// These are simple value types that can be passed to the database layer for SQL generation.
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import graphql/value

/// Simple value type that can represent strings, ints, or other primitives
pub type WhereValue {
  StringValue(String)
  IntValue(Int)
  BoolValue(Bool)
}

/// Intermediate representation of a where condition (no SQL types)
pub type WhereCondition {
  WhereCondition(
    eq: Option(WhereValue),
    in_values: Option(List(WhereValue)),
    contains: Option(String),
    gt: Option(WhereValue),
    gte: Option(WhereValue),
    lt: Option(WhereValue),
    lte: Option(WhereValue),
  )
}

/// Intermediate representation of a where clause (no SQL types)
pub type WhereClause {
  WhereClause(
    conditions: Dict(String, WhereCondition),
    and: Option(List(WhereClause)),
    or: Option(List(WhereClause)),
  )
}

/// Parse a GraphQL filter value into a WhereCondition
pub fn parse_condition(filter_value: value.Value) -> WhereCondition {
  case filter_value {
    value.Object(fields) -> {
      let eq = case list.key_find(fields, "eq") {
        Ok(value.String(s)) -> Some(StringValue(s))
        Ok(value.Int(i)) -> Some(IntValue(i))
        Ok(value.Boolean(b)) -> Some(BoolValue(b))
        _ -> None
      }

      let in_values = case list.key_find(fields, "in") {
        Ok(value.List(items)) -> {
          let values =
            list.filter_map(items, fn(item) {
              case item {
                value.String(s) -> Ok(StringValue(s))
                value.Int(i) -> Ok(IntValue(i))
                value.Boolean(b) -> Ok(BoolValue(b))
                _ -> Error(Nil)
              }
            })
          case values {
            [] -> None
            _ -> Some(values)
          }
        }
        _ -> None
      }

      let contains = case list.key_find(fields, "contains") {
        Ok(value.String(s)) -> Some(s)
        _ -> None
      }

      let gt = case list.key_find(fields, "gt") {
        Ok(value.String(s)) -> Some(StringValue(s))
        Ok(value.Int(i)) -> Some(IntValue(i))
        _ -> None
      }

      let gte = case list.key_find(fields, "gte") {
        Ok(value.String(s)) -> Some(StringValue(s))
        Ok(value.Int(i)) -> Some(IntValue(i))
        _ -> None
      }

      let lt = case list.key_find(fields, "lt") {
        Ok(value.String(s)) -> Some(StringValue(s))
        Ok(value.Int(i)) -> Some(IntValue(i))
        _ -> None
      }

      let lte = case list.key_find(fields, "lte") {
        Ok(value.String(s)) -> Some(StringValue(s))
        Ok(value.Int(i)) -> Some(IntValue(i))
        _ -> None
      }

      WhereCondition(
        eq: eq,
        in_values: in_values,
        contains: contains,
        gt: gt,
        gte: gte,
        lt: lt,
        lte: lte,
      )
    }
    _ ->
      WhereCondition(
        eq: None,
        in_values: None,
        contains: None,
        gt: None,
        gte: None,
        lt: None,
        lte: None,
      )
  }
}

/// Parse a GraphQL where object into a WhereClause
pub fn parse_where_clause(where_value: value.Value) -> WhereClause {
  case where_value {
    value.Object(fields) -> {
      // Extract field conditions (not and/or)
      let field_conditions =
        list.filter_map(fields, fn(field) {
          let #(field_name, field_value) = field
          case field_name {
            "and" | "or" -> Error(Nil)
            _ -> Ok(#(field_name, parse_condition(field_value)))
          }
        })

      // Extract nested AND clauses
      let and_clauses = case list.key_find(fields, "and") {
        Ok(value.List(items)) -> {
          let clauses = list.map(items, parse_where_clause)
          case clauses {
            [] -> None
            _ -> Some(clauses)
          }
        }
        _ -> None
      }

      // Extract nested OR clauses
      let or_clauses = case list.key_find(fields, "or") {
        Ok(value.List(items)) -> {
          let clauses = list.map(items, parse_where_clause)
          case clauses {
            [] -> None
            _ -> Some(clauses)
          }
        }
        _ -> None
      }

      WhereClause(
        conditions: dict.from_list(field_conditions),
        and: and_clauses,
        or: or_clauses,
      )
    }
    _ -> WhereClause(conditions: dict.new(), and: None, or: None)
  }
}

/// Check if a where clause is empty
pub fn is_clause_empty(clause: WhereClause) -> Bool {
  dict.is_empty(clause.conditions) && clause.and == None && clause.or == None
}
