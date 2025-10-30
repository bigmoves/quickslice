import gleam/option.{type Option, None, Some}
import gleam/dict.{type Dict}
import gleam/list
import gleam/string
import gleam/result
import sqlight

/// Represents a single condition on a field with various comparison operators
pub type WhereCondition {
  WhereCondition(
    eq: Option(sqlight.Value),
    in_values: Option(List(sqlight.Value)),
    contains: Option(String),
    gt: Option(sqlight.Value),
    gte: Option(sqlight.Value),
    lt: Option(sqlight.Value),
    lte: Option(sqlight.Value),
  )
}

/// Represents a complete where clause with support for nested AND/OR logic
pub type WhereClause {
  WhereClause(
    /// Field-level conditions (combined with AND)
    conditions: Dict(String, WhereCondition),
    /// Nested AND clauses - all must be true
    and: Option(List(WhereClause)),
    /// Nested OR clauses - at least one must be true
    or: Option(List(WhereClause)),
  )
}

/// Creates an empty WhereCondition with all operators set to None
pub fn empty_condition() -> WhereCondition {
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

/// Creates an empty WhereClause with no conditions
pub fn empty_clause() -> WhereClause {
  WhereClause(conditions: dict.new(), and: None, or: None)
}

/// Checks if a WhereCondition has any operators set
pub fn is_condition_empty(condition: WhereCondition) -> Bool {
  case condition {
    WhereCondition(
      eq: None,
      in_values: None,
      contains: None,
      gt: None,
      gte: None,
      lt: None,
      lte: None,
    ) -> True
    _ -> False
  }
}

/// Checks if a WhereClause is empty (no conditions at all)
pub fn is_clause_empty(clause: WhereClause) -> Bool {
  dict.is_empty(clause.conditions)
  && clause.and == None
  && clause.or == None
}

/// Checks if a WhereClause requires a join with the actor table
pub fn requires_actor_join(clause: WhereClause) -> Bool {
  // Check if actorHandle is in the conditions
  let has_actor_handle = dict.has_key(clause.conditions, "actorHandle")

  // Check nested AND clauses
  let has_actor_in_and = case clause.and {
    Some(and_clauses) -> list.any(and_clauses, requires_actor_join)
    None -> False
  }

  // Check nested OR clauses
  let has_actor_in_or = case clause.or {
    Some(or_clauses) -> list.any(or_clauses, requires_actor_join)
    None -> False
  }

  has_actor_handle || has_actor_in_and || has_actor_in_or
}

// Table columns that should not use json_extract
const table_columns = ["uri", "cid", "did", "collection", "indexed_at"]

/// Determines if a field is a table column or a JSON field
fn is_table_column(field: String) -> Bool {
  list.contains(table_columns, field)
}

/// Builds the SQL reference for a field (either table column or JSON path)
/// If use_table_prefix is true, table columns are prefixed with "record."
fn build_field_ref(field: String, use_table_prefix: Bool) -> String {
  case field {
    "actorHandle" -> "actor.handle"
    _ ->
      case is_table_column(field) {
        True ->
          case use_table_prefix {
            True -> "record." <> field
            False -> field
          }
        False -> {
          let table_name = case use_table_prefix {
            True -> "record."
            False -> ""
          }
          "json_extract(" <> table_name <> "json, '$." <> field <> "')"
        }
      }
  }
}

/// Builds SQL for a single condition on a field
/// Returns a list of SQL strings and accumulated parameters
fn build_single_condition(
  field: String,
  condition: WhereCondition,
  use_table_prefix: Bool,
) -> #(List(String), List(sqlight.Value)) {
  let field_ref = build_field_ref(field, use_table_prefix)
  let mut_sql_parts = []
  let mut_params = []

  // eq operator
  let #(sql_parts, params) = case condition.eq {
    Some(value) -> {
      #([field_ref <> " = ?", ..mut_sql_parts], [value, ..mut_params])
    }
    None -> #(mut_sql_parts, mut_params)
  }
  let mut_sql_parts = sql_parts
  let mut_params = params

  // in operator
  let #(sql_parts, params) = case condition.in_values {
    Some(values) -> {
      case values {
        [] -> #(mut_sql_parts, mut_params)
        // Empty list - skip this condition
        _ -> {
          let placeholders =
            list.repeat("?", list.length(values))
            |> string.join(", ")
          let sql = field_ref <> " IN (" <> placeholders <> ")"
          #([sql, ..mut_sql_parts], list.append(values, mut_params))
        }
      }
    }
    None -> #(mut_sql_parts, mut_params)
  }
  let mut_sql_parts = sql_parts
  let mut_params = params

  // gt operator
  let #(sql_parts, params) = case condition.gt {
    Some(value) -> {
      #([field_ref <> " > ?", ..mut_sql_parts], [value, ..mut_params])
    }
    None -> #(mut_sql_parts, mut_params)
  }
  let mut_sql_parts = sql_parts
  let mut_params = params

  // gte operator
  let #(sql_parts, params) = case condition.gte {
    Some(value) -> {
      #([field_ref <> " >= ?", ..mut_sql_parts], [value, ..mut_params])
    }
    None -> #(mut_sql_parts, mut_params)
  }
  let mut_sql_parts = sql_parts
  let mut_params = params

  // lt operator
  let #(sql_parts, params) = case condition.lt {
    Some(value) -> {
      #([field_ref <> " < ?", ..mut_sql_parts], [value, ..mut_params])
    }
    None -> #(mut_sql_parts, mut_params)
  }
  let mut_sql_parts = sql_parts
  let mut_params = params

  // lte operator
  let #(sql_parts, params) = case condition.lte {
    Some(value) -> {
      #([field_ref <> " <= ?", ..mut_sql_parts], [value, ..mut_params])
    }
    None -> #(mut_sql_parts, mut_params)
  }
  let mut_sql_parts = sql_parts
  let mut_params = params

  // contains operator (case-insensitive LIKE)
  let #(sql_parts, params) = case condition.contains {
    Some(search_text) -> {
      let sql = field_ref <> " LIKE '%' || ? || '%' COLLATE NOCASE"
      #([sql, ..mut_sql_parts], [sqlight.text(search_text), ..mut_params])
    }
    None -> #(mut_sql_parts, mut_params)
  }

  // Reverse to maintain correct order (we built backwards)
  #(list.reverse(sql_parts), list.reverse(params))
}

/// Builds WHERE clause SQL from a WhereClause
/// Returns tuple of (sql_string, parameters)
/// use_table_prefix: if True, prefixes table columns with "record." for joins
pub fn build_where_sql(
  clause: WhereClause,
  use_table_prefix: Bool,
) -> #(String, List(sqlight.Value)) {
  case is_clause_empty(clause) {
    True -> #("", [])
    False -> {
      let #(sql_parts, params) =
        build_where_clause_internal(clause, use_table_prefix)
      let sql = string.join(sql_parts, " AND ")
      #(sql, params)
    }
  }
}

/// Internal recursive function to build where clause parts
fn build_where_clause_internal(
  clause: WhereClause,
  use_table_prefix: Bool,
) -> #(List(String), List(sqlight.Value)) {
  let mut_sql_parts = []
  let mut_params = []

  // Build conditions from field-level conditions
  let #(field_sql_parts, field_params) =
    dict.fold(
      clause.conditions,
      #([], []),
      fn(acc, field, condition) {
        let #(acc_sql, acc_params) = acc
        let #(cond_sql_parts, cond_params) =
          build_single_condition(field, condition, use_table_prefix)
        #(
          list.append(acc_sql, cond_sql_parts),
          list.append(acc_params, cond_params),
        )
      },
    )

  let mut_sql_parts = list.append(mut_sql_parts, field_sql_parts)
  let mut_params = list.append(mut_params, field_params)

  // Handle nested AND clauses
  let #(and_sql_parts, and_params) = case clause.and {
    Some(and_clauses) -> {
      list.fold(and_clauses, #([], []), fn(acc, nested_clause) {
        let #(acc_sql, acc_params) = acc
        case is_clause_empty(nested_clause) {
          True -> acc
          False -> {
            let #(nested_sql_parts, nested_params) =
              build_where_clause_internal(nested_clause, use_table_prefix)
            // Wrap nested clause in parentheses if it has multiple parts
            let nested_sql = case list.length(nested_sql_parts) {
              0 -> ""
              1 -> list.first(nested_sql_parts) |> result.unwrap("")
              _ -> "(" <> string.join(nested_sql_parts, " AND ") <> ")"
            }
            let new_sql = case nested_sql {
              "" -> acc_sql
              _ -> [nested_sql, ..acc_sql]
            }
            #(new_sql, list.append(nested_params, acc_params))
          }
        }
      })
    }
    None -> #([], [])
  }

  let mut_sql_parts = list.append(mut_sql_parts, and_sql_parts)
  let mut_params = list.append(mut_params, and_params)

  // Handle nested OR clauses
  let #(or_sql_parts, or_params) = case clause.or {
    Some(or_clauses) -> {
      list.fold(or_clauses, #([], []), fn(acc, nested_clause) {
        let #(acc_sql, acc_params) = acc
        case is_clause_empty(nested_clause) {
          True -> acc
          False -> {
            let #(nested_sql_parts, nested_params) =
              build_where_clause_internal(nested_clause, use_table_prefix)
            // Wrap nested clause in parentheses if it has multiple parts
            let nested_sql = case list.length(nested_sql_parts) {
              0 -> ""
              1 -> list.first(nested_sql_parts) |> result.unwrap("")
              _ -> "(" <> string.join(nested_sql_parts, " AND ") <> ")"
            }
            let new_sql = case nested_sql {
              "" -> acc_sql
              _ -> [nested_sql, ..acc_sql]
            }
            #(new_sql, list.append(nested_params, acc_params))
          }
        }
      })
    }
    None -> #([], [])
  }

  // If we have OR parts, wrap them in parentheses and join with OR
  let #(final_sql_parts, final_params) = case list.length(or_sql_parts) {
    0 -> #(mut_sql_parts, mut_params)
    _ -> {
      // Reverse the OR parts since we built them backwards
      let reversed_or = list.reverse(or_sql_parts)
      let or_combined = "(" <> string.join(reversed_or, " OR ") <> ")"
      #([or_combined, ..mut_sql_parts], list.append(or_params, mut_params))
    }
  }

  #(final_sql_parts, final_params)
}
