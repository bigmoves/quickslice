import database/types.{
  type DateInterval, type GroupByField, Day, Hour, Month, SimpleField,
  TruncatedField, Week,
}
import gleam/dict
import gleam/dynamic/decode
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import lexicon_graphql/output/aggregate
import sqlight
import where_clause

// ===== Aggregation Support =====

/// Get aggregated records grouped by specified fields
pub fn get_aggregated_records(
  conn: sqlight.Connection,
  collection: String,
  group_by: List(GroupByField),
  where: Option(where_clause.WhereClause),
  order_by_count_desc: Bool,
  limit: Int,
) -> Result(List(aggregate.AggregateResult), sqlight.Error) {
  // Build SELECT clause with grouped fields
  let select_parts =
    group_by
    |> list.index_map(fn(field, index) {
      let field_name = "field_" <> int.to_string(index)
      case field {
        SimpleField(f) -> build_field_select(f, field_name)
        TruncatedField(f, interval) ->
          build_date_truncate_select(f, interval, field_name)
      }
    })
    |> list.append(["COUNT(*) as count"])
    |> string.join(", ")

  // Build GROUP BY clause
  let group_by_clause =
    list.range(0, list.length(group_by) - 1)
    |> list.map(fn(i) { "field_" <> int.to_string(i) })
    |> string.join(", ")

  // Check if we need to join with actor table
  let needs_actor_join = case where {
    Some(wc) -> where_clause.requires_actor_join(wc)
    None -> False
  }

  // Build FROM clause with optional LEFT JOIN
  let from_clause = case needs_actor_join {
    True -> "record LEFT JOIN actor ON record.did = actor.did"
    False -> "record"
  }

  // Build WHERE clause parts - start with collection filter
  let mut_where_parts = ["record.collection = ?"]
  let mut_bind_values = [sqlight.text(collection)]

  // Add where clause conditions if provided
  // Note: Always use table prefix (True) because the FROM clause uses "record" as the table name
  let #(where_parts, bind_values) = case where {
    Some(wc) -> {
      case where_clause.is_clause_empty(wc) {
        True -> #(mut_where_parts, mut_bind_values)
        False -> {
          let #(where_sql, where_params) =
            where_clause.build_where_sql(wc, True)
          let new_where = list.append(mut_where_parts, [where_sql])
          let new_binds = list.append(mut_bind_values, where_params)
          #(new_where, new_binds)
        }
      }
    }
    None -> #(mut_where_parts, mut_bind_values)
  }

  // Build ORDER BY clause
  let order_by = case order_by_count_desc {
    True -> "count DESC"
    False -> "count ASC"
  }

  // Build the SQL query
  let sql = "
    SELECT " <> select_parts <> "
    FROM " <> from_clause <> "
    WHERE " <> string.join(where_parts, " AND ") <> "
    GROUP BY " <> group_by_clause <> "
    ORDER BY " <> order_by <> "
    LIMIT " <> int.to_string(limit)

  // Create decoder - we need to build it dynamically based on number of fields
  let num_fields = list.length(group_by)

  // Decode as list of dynamics, then post-process
  let decoder = decode.list(decode.dynamic)

  // Execute query and map results
  sqlight.query(sql, on: conn, with: bind_values, expecting: decoder)
  |> result.map(fn(rows) {
    rows
    |> list.map(fn(row_values) {
      // Take first N as group fields, last as count
      let group_values = list.take(row_values, num_fields)
      let count = case list.last(row_values) {
        Ok(count_dynamic) ->
          case decode.run(count_dynamic, decode.int) {
            Ok(n) -> n
            Error(_) -> 0
          }
        Error(_) -> 0
      }

      // Build dict from field names to values
      let field_names =
        list.range(0, num_fields - 1)
        |> list.map(fn(i) { "field_" <> int.to_string(i) })
      let field_dict = dict.from_list(list.zip(field_names, group_values))

      aggregate.AggregateResult(field_dict, count)
    })
  })
}

/// Build SELECT expression for a field (table column or JSON field)
fn build_field_select(field: String, alias: String) -> String {
  case is_table_column_for_aggregate(field) {
    True -> "record." <> field <> " as " <> alias
    False -> "json_extract(record.json, '$." <> field <> "') as " <> alias
  }
}

/// Build SELECT expression for date truncation
fn build_date_truncate_select(
  field: String,
  interval: DateInterval,
  alias: String,
) -> String {
  let field_ref = case is_table_column_for_aggregate(field) {
    True -> "record." <> field
    False -> "json_extract(record.json, '$." <> field <> "')"
  }

  case interval {
    Hour -> "strftime('%Y-%m-%d %H:00:00', " <> field_ref <> ") as " <> alias
    Day -> "strftime('%Y-%m-%d', " <> field_ref <> ") as " <> alias
    Week -> "strftime('%Y-W%W', " <> field_ref <> ") as " <> alias
    Month -> "strftime('%Y-%m', " <> field_ref <> ") as " <> alias
  }
}

/// Check if field is a table column (for aggregation context)
fn is_table_column_for_aggregate(field: String) -> Bool {
  case field {
    "uri" | "cid" | "did" | "collection" | "indexed_at" -> True
    _ -> False
  }
}
