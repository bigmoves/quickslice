import database/executor.{type Executor, type Value, Text}
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// Represents a single condition on a field with various comparison operators
pub type WhereCondition {
  WhereCondition(
    eq: Option(Value),
    in_values: Option(List(Value)),
    contains: Option(String),
    gt: Option(Value),
    gte: Option(Value),
    lt: Option(Value),
    lte: Option(Value),
    is_null: Option(Bool),
    /// Whether the comparison values are numeric (affects JSON field casting)
    is_numeric: Bool,
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
    is_null: None,
    is_numeric: False,
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
      is_null: None,
      is_numeric: _,
    ) -> True
    _ -> False
  }
}

/// Checks if a WhereClause is empty (no conditions at all)
pub fn is_clause_empty(clause: WhereClause) -> Bool {
  dict.is_empty(clause.conditions) && clause.and == None && clause.or == None
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
fn build_field_ref(
  exec: Executor,
  field: String,
  use_table_prefix: Bool,
) -> String {
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
          executor.json_extract(exec, table_name <> "json", field)
        }
      }
  }
}

/// Helper to determine if we should cast to numeric
/// Uses the is_numeric flag set during value conversion
fn should_cast_numeric(condition: WhereCondition) -> Bool {
  // Only cast if we have numeric comparison operators AND the values are numeric
  // String values (like ISO dates) should not be cast
  condition.is_numeric
  && {
    option.is_some(condition.gt)
    || option.is_some(condition.gte)
    || option.is_some(condition.lt)
    || option.is_some(condition.lte)
  }
}

/// Builds field reference with optional numeric cast for JSON fields
fn build_field_ref_with_cast(
  exec: Executor,
  field: String,
  use_table_prefix: Bool,
  cast_numeric: Bool,
) -> String {
  let field_ref = build_field_ref(exec, field, use_table_prefix)

  // If it's a JSON field and we need numeric cast, wrap in CAST
  case is_table_column(field) || !cast_numeric {
    True -> field_ref
    False -> {
      // Use dialect-specific cast syntax
      case executor.dialect(exec) {
        executor.SQLite -> "CAST(" <> field_ref <> " AS INTEGER)"
        executor.PostgreSQL -> "(" <> field_ref <> ")::INTEGER"
      }
    }
  }
}

/// Get the LIKE collation syntax for case-insensitive search
fn case_insensitive_like(exec: Executor) -> String {
  case executor.dialect(exec) {
    executor.SQLite -> " COLLATE NOCASE"
    executor.PostgreSQL -> ""
    // PostgreSQL ILIKE is case-insensitive by default
  }
}

/// Get the LIKE operator for case-insensitive search
fn like_operator(exec: Executor) -> String {
  case executor.dialect(exec) {
    executor.SQLite -> " LIKE "
    executor.PostgreSQL -> " ILIKE "
  }
}

/// Builds SQL for a single condition on a field
/// Returns a list of SQL strings and accumulated parameters
/// param_offset is the starting parameter index (1-based)
fn build_single_condition(
  exec: Executor,
  field: String,
  condition: WhereCondition,
  use_table_prefix: Bool,
  param_offset: Int,
) -> #(List(String), List(Value), Int) {
  // Check if numeric casting is needed (for gt/gte/lt/lte operators)
  let has_numeric_comparison = should_cast_numeric(condition)

  let field_ref =
    build_field_ref_with_cast(
      exec,
      field,
      use_table_prefix,
      has_numeric_comparison,
    )

  // For isNull, we need the field ref without numeric cast
  let field_ref_no_cast = build_field_ref(exec, field, use_table_prefix)

  let mut_sql_parts = []
  let mut_params = []
  let mut_offset = param_offset

  // eq operator
  let #(sql_parts, params, offset) = case condition.eq {
    Some(value) -> {
      let placeholder = executor.placeholder(exec, mut_offset)
      #(
        [field_ref <> " = " <> placeholder, ..mut_sql_parts],
        [value, ..mut_params],
        mut_offset + 1,
      )
    }
    None -> #(mut_sql_parts, mut_params, mut_offset)
  }
  let mut_sql_parts = sql_parts
  let mut_params = params
  let mut_offset = offset

  // in operator
  let #(sql_parts, params, offset) = case condition.in_values {
    Some(values) -> {
      case values {
        [] -> #(mut_sql_parts, mut_params, mut_offset)
        // Empty list - skip this condition
        _ -> {
          let placeholders =
            list.index_map(values, fn(_, i) {
              executor.placeholder(exec, mut_offset + i)
            })
            |> string.join(", ")
          let sql = field_ref <> " IN (" <> placeholders <> ")"
          #(
            [sql, ..mut_sql_parts],
            list.append(values, mut_params),
            mut_offset + list.length(values),
          )
        }
      }
    }
    None -> #(mut_sql_parts, mut_params, mut_offset)
  }
  let mut_sql_parts = sql_parts
  let mut_params = params
  let mut_offset = offset

  // gt operator
  let #(sql_parts, params, offset) = case condition.gt {
    Some(value) -> {
      let placeholder = executor.placeholder(exec, mut_offset)
      #(
        [field_ref <> " > " <> placeholder, ..mut_sql_parts],
        [value, ..mut_params],
        mut_offset + 1,
      )
    }
    None -> #(mut_sql_parts, mut_params, mut_offset)
  }
  let mut_sql_parts = sql_parts
  let mut_params = params
  let mut_offset = offset

  // gte operator
  let #(sql_parts, params, offset) = case condition.gte {
    Some(value) -> {
      let placeholder = executor.placeholder(exec, mut_offset)
      #(
        [field_ref <> " >= " <> placeholder, ..mut_sql_parts],
        [value, ..mut_params],
        mut_offset + 1,
      )
    }
    None -> #(mut_sql_parts, mut_params, mut_offset)
  }
  let mut_sql_parts = sql_parts
  let mut_params = params
  let mut_offset = offset

  // lt operator
  let #(sql_parts, params, offset) = case condition.lt {
    Some(value) -> {
      let placeholder = executor.placeholder(exec, mut_offset)
      #(
        [field_ref <> " < " <> placeholder, ..mut_sql_parts],
        [value, ..mut_params],
        mut_offset + 1,
      )
    }
    None -> #(mut_sql_parts, mut_params, mut_offset)
  }
  let mut_sql_parts = sql_parts
  let mut_params = params
  let mut_offset = offset

  // lte operator
  let #(sql_parts, params, offset) = case condition.lte {
    Some(value) -> {
      let placeholder = executor.placeholder(exec, mut_offset)
      #(
        [field_ref <> " <= " <> placeholder, ..mut_sql_parts],
        [value, ..mut_params],
        mut_offset + 1,
      )
    }
    None -> #(mut_sql_parts, mut_params, mut_offset)
  }
  let mut_sql_parts = sql_parts
  let mut_params = params
  let mut_offset = offset

  // contains operator (case-insensitive LIKE)
  let #(sql_parts, params, offset) = case condition.contains {
    Some(search_text) -> {
      let placeholder = executor.placeholder(exec, mut_offset)
      let like_op = like_operator(exec)
      let collation = case_insensitive_like(exec)
      let sql =
        field_ref
        <> like_op
        <> "'%' || "
        <> placeholder
        <> " || '%'"
        <> collation
      #(
        [sql, ..mut_sql_parts],
        [Text(search_text), ..mut_params],
        mut_offset + 1,
      )
    }
    None -> #(mut_sql_parts, mut_params, mut_offset)
  }
  let mut_sql_parts = sql_parts
  let mut_params = params
  let mut_offset = offset

  // isNull operator (no parameters needed)
  let #(sql_parts, params, offset) = case condition.is_null {
    Some(True) -> {
      let sql = field_ref_no_cast <> " IS NULL"
      #([sql, ..mut_sql_parts], mut_params, mut_offset)
    }
    Some(False) -> {
      let sql = field_ref_no_cast <> " IS NOT NULL"
      #([sql, ..mut_sql_parts], mut_params, mut_offset)
    }
    None -> #(mut_sql_parts, mut_params, mut_offset)
  }

  // Reverse to maintain correct order (we built backwards)
  #(list.reverse(sql_parts), list.reverse(params), offset)
}

/// Builds WHERE clause SQL from a WhereClause
/// Returns tuple of (sql_string, parameters)
/// use_table_prefix: if True, prefixes table columns with "record." for joins
pub fn build_where_sql(
  exec: Executor,
  clause: WhereClause,
  use_table_prefix: Bool,
) -> #(String, List(Value)) {
  case is_clause_empty(clause) {
    True -> #("", [])
    False -> {
      let #(sql_parts, params, _) =
        build_where_clause_internal(exec, clause, use_table_prefix, 1)
      let sql = string.join(sql_parts, " AND ")
      #(sql, params)
    }
  }
}

/// Internal recursive function to build where clause parts
fn build_where_clause_internal(
  exec: Executor,
  clause: WhereClause,
  use_table_prefix: Bool,
  param_offset: Int,
) -> #(List(String), List(Value), Int) {
  let mut_sql_parts = []
  let mut_params = []
  let mut_offset = param_offset

  // Build conditions from field-level conditions
  let #(field_sql_parts, field_params, new_offset) =
    dict.fold(
      clause.conditions,
      #([], [], mut_offset),
      fn(acc, field, condition) {
        let #(acc_sql, acc_params, acc_offset) = acc
        let #(cond_sql_parts, cond_params, new_offset) =
          build_single_condition(
            exec,
            field,
            condition,
            use_table_prefix,
            acc_offset,
          )
        #(
          list.append(acc_sql, cond_sql_parts),
          list.append(acc_params, cond_params),
          new_offset,
        )
      },
    )

  let mut_sql_parts = list.append(mut_sql_parts, field_sql_parts)
  let mut_params = list.append(mut_params, field_params)
  let mut_offset = new_offset

  // Handle nested AND clauses
  let #(and_sql_parts, and_params, new_offset) = case clause.and {
    Some(and_clauses) -> {
      list.fold(and_clauses, #([], [], mut_offset), fn(acc, nested_clause) {
        let #(acc_sql, acc_params, acc_offset) = acc
        case is_clause_empty(nested_clause) {
          True -> acc
          False -> {
            let #(nested_sql_parts, nested_params, new_offset) =
              build_where_clause_internal(
                exec,
                nested_clause,
                use_table_prefix,
                acc_offset,
              )
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
            #(new_sql, list.append(nested_params, acc_params), new_offset)
          }
        }
      })
    }
    None -> #([], [], mut_offset)
  }

  let mut_sql_parts = list.append(mut_sql_parts, and_sql_parts)
  let mut_params = list.append(mut_params, and_params)
  let mut_offset = new_offset

  // Handle nested OR clauses
  let #(or_sql_parts, or_params, new_offset) = case clause.or {
    Some(or_clauses) -> {
      list.fold(or_clauses, #([], [], mut_offset), fn(acc, nested_clause) {
        let #(acc_sql, acc_params, acc_offset) = acc
        case is_clause_empty(nested_clause) {
          True -> acc
          False -> {
            let #(nested_sql_parts, nested_params, new_offset) =
              build_where_clause_internal(
                exec,
                nested_clause,
                use_table_prefix,
                acc_offset,
              )
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
            #(new_sql, list.append(nested_params, acc_params), new_offset)
          }
        }
      })
    }
    None -> #([], [], mut_offset)
  }

  // If we have OR parts, wrap them in parentheses and join with OR
  let #(final_sql_parts, final_params, final_offset) = case
    list.length(or_sql_parts)
  {
    0 -> #(mut_sql_parts, mut_params, new_offset)
    _ -> {
      // Reverse the OR parts since we built them backwards
      let reversed_or = list.reverse(or_sql_parts)
      let or_combined = "(" <> string.join(reversed_or, " OR ") <> ")"
      #(
        [or_combined, ..mut_sql_parts],
        list.append(or_params, mut_params),
        new_offset,
      )
    }
  }

  #(final_sql_parts, final_params, final_offset)
}
