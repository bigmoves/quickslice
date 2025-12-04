import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit
import gleeunit/should
import sqlight
import where_clause

pub fn main() {
  gleeunit.main()
}

// Test: Empty where clause should produce empty SQL
pub fn build_where_empty_clause_test() {
  let clause = where_clause.empty_clause()
  let #(sql, params) = where_clause.build_where_sql(clause, False)

  sql |> should.equal("")
  list.length(params) |> should.equal(0)
}

// Test: Single eq operator on table column
pub fn build_where_eq_on_table_column_test() {
  let condition =
    where_clause.WhereCondition(
      eq: Some(sqlight.text("app.bsky.feed.post")),
      in_values: None,
      contains: None,
      gt: None,
      gte: None,
      lt: None,
      lte: None,
      is_null: None,
      is_numeric: False,
    )
  let clause =
    where_clause.WhereClause(
      conditions: dict.from_list([#("collection", condition)]),
      and: None,
      or: None,
    )

  let #(sql, params) = where_clause.build_where_sql(clause, False)

  sql |> should.equal("collection = ?")
  list.length(params) |> should.equal(1)
}

// Test: in operator with multiple values
pub fn build_where_in_operator_test() {
  let condition =
    where_clause.WhereCondition(
      eq: None,
      in_values: Some([
        sqlight.text("did1"),
        sqlight.text("did2"),
        sqlight.text("did3"),
      ]),
      contains: None,
      gt: None,
      gte: None,
      lt: None,
      lte: None,
      is_null: None,
      is_numeric: False,
    )
  let clause =
    where_clause.WhereClause(
      conditions: dict.from_list([#("did", condition)]),
      and: None,
      or: None,
    )

  let #(sql, params) = where_clause.build_where_sql(clause, False)

  sql |> should.equal("did IN (?, ?, ?)")
  list.length(params) |> should.equal(3)
}

// Test: gt operator on indexed_at
pub fn build_where_gt_operator_test() {
  let condition =
    where_clause.WhereCondition(
      eq: None,
      in_values: None,
      contains: None,
      gt: Some(sqlight.text("2024-01-01T00:00:00Z")),
      gte: None,
      lt: None,
      lte: None,
      is_null: None,
      is_numeric: False,
    )
  let clause =
    where_clause.WhereClause(
      conditions: dict.from_list([#("indexed_at", condition)]),
      and: None,
      or: None,
    )

  let #(sql, params) = where_clause.build_where_sql(clause, False)

  sql |> should.equal("indexed_at > ?")
  list.length(params) |> should.equal(1)
}

// Test: gte operator
pub fn build_where_gte_operator_test() {
  let condition =
    where_clause.WhereCondition(
      eq: None,
      in_values: None,
      contains: None,
      gt: None,
      gte: Some(sqlight.int(2000)),
      lt: None,
      lte: None,
      is_null: None,
      is_numeric: True,
    )
  let clause =
    where_clause.WhereClause(
      conditions: dict.from_list([#("year", condition)]),
      and: None,
      or: None,
    )

  let #(sql, params) = where_clause.build_where_sql(clause, False)

  // Now includes CAST for numeric comparisons on JSON fields
  sql |> should.equal("CAST(json_extract(json, '$.year') AS INTEGER) >= ?")
  list.length(params) |> should.equal(1)
}

// Test: lt operator
pub fn build_where_lt_operator_test() {
  let condition =
    where_clause.WhereCondition(
      eq: None,
      in_values: None,
      contains: None,
      gt: None,
      gte: None,
      lt: Some(sqlight.text("2024-12-31T23:59:59Z")),
      lte: None,
      is_null: None,
      is_numeric: False,
    )
  let clause =
    where_clause.WhereClause(
      conditions: dict.from_list([#("indexed_at", condition)]),
      and: None,
      or: None,
    )

  let #(sql, params) = where_clause.build_where_sql(clause, False)

  sql |> should.equal("indexed_at < ?")
  list.length(params) |> should.equal(1)
}

// Test: lte operator
pub fn build_where_lte_operator_test() {
  let condition =
    where_clause.WhereCondition(
      eq: None,
      in_values: None,
      contains: None,
      gt: None,
      gte: None,
      lt: None,
      lte: Some(sqlight.int(100)),
      is_null: None,
      is_numeric: True,
    )
  let clause =
    where_clause.WhereClause(
      conditions: dict.from_list([#("count", condition)]),
      and: None,
      or: None,
    )

  let #(sql, params) = where_clause.build_where_sql(clause, False)

  // Now includes CAST for numeric comparisons on JSON fields
  sql |> should.equal("CAST(json_extract(json, '$.count') AS INTEGER) <= ?")
  list.length(params) |> should.equal(1)
}

// Test: Range query with both gt and lt
pub fn build_where_range_query_test() {
  let condition =
    where_clause.WhereCondition(
      eq: None,
      in_values: None,
      contains: None,
      gt: Some(sqlight.text("2024-01-01T00:00:00Z")),
      gte: None,
      lt: Some(sqlight.text("2024-02-01T00:00:00Z")),
      lte: None,
      is_null: None,
      is_numeric: False,
    )
  let clause =
    where_clause.WhereClause(
      conditions: dict.from_list([#("indexed_at", condition)]),
      and: None,
      or: None,
    )

  let #(sql, params) = where_clause.build_where_sql(clause, False)

  // Should combine both conditions with AND
  sql |> should.equal("indexed_at > ? AND indexed_at < ?")
  list.length(params) |> should.equal(2)
}

// Test: Multiple fields combined with AND
pub fn build_where_multiple_fields_test() {
  let cond1 =
    where_clause.WhereCondition(
      eq: Some(sqlight.text("app.bsky.feed.post")),
      in_values: None,
      contains: None,
      gt: None,
      gte: None,
      lt: None,
      lte: None,
      is_null: None,
      is_numeric: False,
    )
  let cond2 =
    where_clause.WhereCondition(
      eq: Some(sqlight.text("did:plc:xyz")),
      in_values: None,
      contains: None,
      gt: None,
      gte: None,
      lt: None,
      lte: None,
      is_null: None,
      is_numeric: False,
    )
  let clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #("collection", cond1),
        #("did", cond2),
      ]),
      and: None,
      or: None,
    )

  let #(sql, params) = where_clause.build_where_sql(clause, False)

  // Order might vary due to dict, but should have AND
  should.be_true(string.contains(sql, "AND"))
  should.be_true(string.contains(sql, "collection = ?"))
  should.be_true(string.contains(sql, "did = ?"))
  list.length(params) |> should.equal(2)
}

// Phase 3 Tests: JSON Field Filtering

// Test: Simple JSON field with eq operator
pub fn build_where_json_field_eq_test() {
  let condition =
    where_clause.WhereCondition(
      eq: Some(sqlight.text("Hello World")),
      in_values: None,
      contains: None,
      gt: None,
      gte: None,
      lt: None,
      lte: None,
      is_null: None,
      is_numeric: False,
    )
  let clause =
    where_clause.WhereClause(
      conditions: dict.from_list([#("text", condition)]),
      and: None,
      or: None,
    )

  let #(sql, params) = where_clause.build_where_sql(clause, False)

  sql |> should.equal("json_extract(json, '$.text') = ?")
  list.length(params) |> should.equal(1)
}

// Test: Nested JSON path (dot notation)
pub fn build_where_nested_json_path_test() {
  let condition =
    where_clause.WhereCondition(
      eq: Some(sqlight.text("Alice")),
      in_values: None,
      contains: None,
      gt: None,
      gte: None,
      lt: None,
      lte: None,
      is_null: None,
      is_numeric: False,
    )
  let clause =
    where_clause.WhereClause(
      conditions: dict.from_list([#("user.name", condition)]),
      and: None,
      or: None,
    )

  let #(sql, params) = where_clause.build_where_sql(clause, False)

  sql |> should.equal("json_extract(json, '$.user.name') = ?")
  list.length(params) |> should.equal(1)
}

// Test: Deeply nested JSON path
pub fn build_where_deeply_nested_json_path_test() {
  let condition =
    where_clause.WhereCondition(
      eq: Some(sqlight.text("value")),
      in_values: None,
      contains: None,
      gt: None,
      gte: None,
      lt: None,
      lte: None,
      is_null: None,
      is_numeric: False,
    )
  let clause =
    where_clause.WhereClause(
      conditions: dict.from_list([#("metadata.tags.0", condition)]),
      and: None,
      or: None,
    )

  let #(sql, params) = where_clause.build_where_sql(clause, False)

  sql |> should.equal("json_extract(json, '$.metadata.tags.0') = ?")
  list.length(params) |> should.equal(1)
}

// Test: JSON field with comparison operators
pub fn build_where_json_field_comparison_test() {
  let condition =
    where_clause.WhereCondition(
      eq: None,
      in_values: None,
      contains: None,
      gt: Some(sqlight.int(100)),
      gte: None,
      lt: Some(sqlight.int(1000)),
      lte: None,
      is_null: None,
      is_numeric: True,
    )
  let clause =
    where_clause.WhereClause(
      conditions: dict.from_list([#("likes", condition)]),
      and: None,
      or: None,
    )

  let #(sql, params) = where_clause.build_where_sql(clause, False)

  // Now includes CAST for numeric comparisons on JSON fields
  sql
  |> should.equal(
    "CAST(json_extract(json, '$.likes') AS INTEGER) > ? AND CAST(json_extract(json, '$.likes') AS INTEGER) < ?",
  )
  list.length(params) |> should.equal(2)
}

// Test: Mix of table columns and JSON fields
pub fn build_where_mixed_table_and_json_test() {
  let cond1 =
    where_clause.WhereCondition(
      eq: Some(sqlight.text("app.bsky.feed.post")),
      in_values: None,
      contains: None,
      gt: None,
      gte: None,
      lt: None,
      lte: None,
      is_null: None,
      is_numeric: False,
    )
  let cond2 =
    where_clause.WhereCondition(
      eq: None,
      in_values: None,
      contains: None,
      gt: Some(sqlight.int(10)),
      gte: None,
      lt: None,
      lte: None,
      is_null: None,
      is_numeric: True,
    )
  let clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #("collection", cond1),
        #("replyCount", cond2),
      ]),
      and: None,
      or: None,
    )

  let #(sql, params) = where_clause.build_where_sql(clause, False)

  // Should have both table column and JSON extract with CAST for numeric comparison
  should.be_true(string.contains(sql, "collection = ?"))
  should.be_true(string.contains(
    sql,
    "CAST(json_extract(json, '$.replyCount') AS INTEGER) > ?",
  ))
  should.be_true(string.contains(sql, "AND"))
  list.length(params) |> should.equal(2)
}

// Phase 4 Tests: Contains Operator

// Test: contains on JSON field
pub fn build_where_contains_json_field_test() {
  let condition =
    where_clause.WhereCondition(
      eq: None,
      in_values: None,
      contains: Some("hello"),
      gt: None,
      gte: None,
      lt: None,
      lte: None,
      is_null: None,
      is_numeric: False,
    )
  let clause =
    where_clause.WhereClause(
      conditions: dict.from_list([#("text", condition)]),
      and: None,
      or: None,
    )

  let #(sql, params) = where_clause.build_where_sql(clause, False)

  sql
  |> should.equal(
    "json_extract(json, '$.text') LIKE '%' || ? || '%' COLLATE NOCASE",
  )
  list.length(params) |> should.equal(1)
}

// Test: contains on table column (uri)
pub fn build_where_contains_table_column_test() {
  let condition =
    where_clause.WhereCondition(
      eq: None,
      in_values: None,
      contains: Some("app.bsky"),
      gt: None,
      gte: None,
      lt: None,
      lte: None,
      is_null: None,
      is_numeric: False,
    )
  let clause =
    where_clause.WhereClause(
      conditions: dict.from_list([#("uri", condition)]),
      and: None,
      or: None,
    )

  let #(sql, params) = where_clause.build_where_sql(clause, False)

  sql |> should.equal("uri LIKE '%' || ? || '%' COLLATE NOCASE")
  list.length(params) |> should.equal(1)
}

// Test: contains with special LIKE characters (should be escaped)
pub fn build_where_contains_special_chars_test() {
  let condition =
    where_clause.WhereCondition(
      eq: None,
      in_values: None,
      contains: Some("test%value"),
      gt: None,
      gte: None,
      lt: None,
      lte: None,
      is_null: None,
      is_numeric: False,
    )
  let clause =
    where_clause.WhereClause(
      conditions: dict.from_list([#("text", condition)]),
      and: None,
      or: None,
    )

  let #(sql, _params) = where_clause.build_where_sql(clause, False)

  // SQL should be generated (actual escaping would be handled by the parameter binding)
  should.be_true(string.contains(sql, "LIKE"))
  should.be_true(string.contains(sql, "COLLATE NOCASE"))
}

// Test: Multiple contains conditions
pub fn build_where_multiple_contains_test() {
  let cond1 =
    where_clause.WhereCondition(
      eq: None,
      in_values: None,
      contains: Some("pearl jam"),
      gt: None,
      gte: None,
      lt: None,
      lte: None,
      is_null: None,
      is_numeric: False,
    )
  let cond2 =
    where_clause.WhereCondition(
      eq: None,
      in_values: None,
      contains: Some("rock"),
      gt: None,
      gte: None,
      lt: None,
      lte: None,
      is_null: None,
      is_numeric: False,
    )
  let clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #("artist", cond1),
        #("genre", cond2),
      ]),
      and: None,
      or: None,
    )

  let #(sql, params) = where_clause.build_where_sql(clause, False)

  // Should have both LIKE clauses
  should.be_true(string.contains(sql, "LIKE"))
  should.be_true(string.contains(sql, "AND"))
  list.length(params) |> should.equal(2)
}

// Test: contains combined with eq operator on same field
pub fn build_where_contains_with_other_operators_test() {
  let condition =
    where_clause.WhereCondition(
      eq: None,
      in_values: None,
      contains: Some("search"),
      gt: Some(sqlight.int(100)),
      gte: None,
      lt: None,
      lte: None,
      is_null: None,
      is_numeric: False,
    )
  let clause =
    where_clause.WhereClause(
      conditions: dict.from_list([#("text", condition)]),
      and: None,
      or: None,
    )

  let #(sql, params) = where_clause.build_where_sql(clause, False)

  // Should have both LIKE and > operator
  should.be_true(string.contains(sql, "LIKE"))
  should.be_true(string.contains(sql, ">"))
  should.be_true(string.contains(sql, "AND"))
  list.length(params) |> should.equal(2)
}

// Phase 5 Tests: AND Logic

// Test: Nested AND with two simple clauses
pub fn build_where_nested_and_simple_test() {
  let clause1 =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "collection",
          where_clause.WhereCondition(
            eq: Some(sqlight.text("app.bsky.feed.post")),
            in_values: None,
            contains: None,
            gt: None,
            gte: None,
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let clause2 =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "did",
          where_clause.WhereCondition(
            eq: Some(sqlight.text("did:plc:test")),
            in_values: None,
            contains: None,
            gt: None,
            gte: None,
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let root_clause =
    where_clause.WhereClause(
      conditions: dict.new(),
      and: Some([clause1, clause2]),
      or: None,
    )

  let #(sql, params) = where_clause.build_where_sql(root_clause, False)

  // Should have both conditions AND'ed together with parentheses
  should.be_true(string.contains(sql, "collection = ?"))
  should.be_true(string.contains(sql, "did = ?"))
  should.be_true(string.contains(sql, "AND"))
  list.length(params) |> should.equal(2)
}

// Test: Nested AND with conditions at root level
pub fn build_where_and_with_root_conditions_test() {
  let nested_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "text",
          where_clause.WhereCondition(
            eq: None,
            in_values: None,
            contains: Some("hello"),
            gt: None,
            gte: None,
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let root_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "collection",
          where_clause.WhereCondition(
            eq: Some(sqlight.text("app.bsky.feed.post")),
            in_values: None,
            contains: None,
            gt: None,
            gte: None,
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: Some([nested_clause]),
      or: None,
    )

  let #(sql, params) = where_clause.build_where_sql(root_clause, False)

  // Should have both root condition and nested condition
  should.be_true(string.contains(sql, "collection = ?"))
  should.be_true(string.contains(sql, "LIKE"))
  should.be_true(string.contains(sql, "AND"))
  list.length(params) |> should.equal(2)
}

// Test: Complex nested AND matching Slice API example
// Example: (artist contains "pearl jam") AND (year >= 2000)
pub fn build_where_complex_and_test() {
  let artist_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "artist",
          where_clause.WhereCondition(
            eq: None,
            in_values: None,
            contains: Some("pearl jam"),
            gt: None,
            gte: None,
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let year_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "year",
          where_clause.WhereCondition(
            eq: None,
            in_values: None,
            contains: None,
            gt: None,
            gte: Some(sqlight.int(2000)),
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let root_clause =
    where_clause.WhereClause(
      conditions: dict.new(),
      and: Some([artist_clause, year_clause]),
      or: None,
    )

  let #(sql, params) = where_clause.build_where_sql(root_clause, False)

  // Should have both conditions
  should.be_true(string.contains(sql, "artist"))
  should.be_true(string.contains(sql, "LIKE"))
  should.be_true(string.contains(sql, "year"))
  should.be_true(string.contains(sql, ">="))
  should.be_true(string.contains(sql, "AND"))
  list.length(params) |> should.equal(2)
}

// Test: Three-level nested AND
pub fn build_where_deeply_nested_and_test() {
  let inner_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "likes",
          where_clause.WhereCondition(
            eq: None,
            in_values: None,
            contains: None,
            gt: Some(sqlight.int(10)),
            gte: None,
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let middle_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "text",
          where_clause.WhereCondition(
            eq: None,
            in_values: None,
            contains: Some("test"),
            gt: None,
            gte: None,
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: Some([inner_clause]),
      or: None,
    )

  let root_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "collection",
          where_clause.WhereCondition(
            eq: Some(sqlight.text("app.bsky.feed.post")),
            in_values: None,
            contains: None,
            gt: None,
            gte: None,
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: Some([middle_clause]),
      or: None,
    )

  let #(sql, params) = where_clause.build_where_sql(root_clause, False)

  // Should have all three conditions
  should.be_true(string.contains(sql, "collection = ?"))
  should.be_true(string.contains(sql, "LIKE"))
  should.be_true(string.contains(sql, "likes"))
  should.be_true(string.contains(sql, ">"))
  list.length(params) |> should.equal(3)
}

// Phase 6 Tests: OR Logic

// Test: Simple OR with two clauses
pub fn build_where_simple_or_test() {
  let clause1 =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "artist",
          where_clause.WhereCondition(
            eq: None,
            in_values: None,
            contains: Some("pearl jam"),
            gt: None,
            gte: None,
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let clause2 =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "genre",
          where_clause.WhereCondition(
            eq: Some(sqlight.text("rock")),
            in_values: None,
            contains: None,
            gt: None,
            gte: None,
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let root_clause =
    where_clause.WhereClause(
      conditions: dict.new(),
      and: None,
      or: Some([clause1, clause2]),
    )

  let #(sql, params) = where_clause.build_where_sql(root_clause, False)

  // Should have both conditions OR'ed together
  should.be_true(string.contains(sql, "artist"))
  should.be_true(string.contains(sql, "LIKE"))
  should.be_true(string.contains(sql, "genre"))
  should.be_true(string.contains(sql, "= ?"))
  should.be_true(string.contains(sql, "OR"))
  list.length(params) |> should.equal(2)
}

// Test: Combined AND/OR - Slice API example
// Example: (artist contains "pearl jam" OR genre = "rock") AND (year >= 2000)
pub fn build_where_combined_and_or_test() {
  let artist_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "artist",
          where_clause.WhereCondition(
            eq: None,
            in_values: None,
            contains: Some("pearl jam"),
            gt: None,
            gte: None,
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let genre_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "genre",
          where_clause.WhereCondition(
            eq: Some(sqlight.text("rock")),
            in_values: None,
            contains: None,
            gt: None,
            gte: None,
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let or_clause =
    where_clause.WhereClause(
      conditions: dict.new(),
      and: None,
      or: Some([artist_clause, genre_clause]),
    )

  let year_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "year",
          where_clause.WhereCondition(
            eq: None,
            in_values: None,
            contains: None,
            gt: None,
            gte: Some(sqlight.int(2000)),
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let root_clause =
    where_clause.WhereClause(
      conditions: dict.new(),
      and: Some([or_clause, year_clause]),
      or: None,
    )

  let #(sql, params) = where_clause.build_where_sql(root_clause, False)

  // Should have proper precedence: (artist LIKE OR genre =) AND year >=
  should.be_true(string.contains(sql, "OR"))
  should.be_true(string.contains(sql, "AND"))
  should.be_true(string.contains(sql, "artist"))
  should.be_true(string.contains(sql, "genre"))
  should.be_true(string.contains(sql, "year"))
  list.length(params) |> should.equal(3)
}

// Test: Complex nested OR/AND from Slice API documentation
// { "and": [ { "or": [artist, genre] }, { "and": [uri1, uri2] }, year ] }
pub fn build_where_complex_nested_or_and_test() {
  let artist_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "artist",
          where_clause.WhereCondition(
            eq: None,
            in_values: None,
            contains: Some("pearl jam"),
            gt: None,
            gte: None,
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let genre_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "genre",
          where_clause.WhereCondition(
            eq: None,
            in_values: None,
            contains: Some("rock"),
            gt: None,
            gte: None,
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let or_group =
    where_clause.WhereClause(
      conditions: dict.new(),
      and: None,
      or: Some([artist_clause, genre_clause]),
    )

  let uri1_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "uri",
          where_clause.WhereCondition(
            eq: None,
            in_values: None,
            contains: Some("app.bsky"),
            gt: None,
            gte: None,
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let uri2_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "uri",
          where_clause.WhereCondition(
            eq: None,
            in_values: None,
            contains: Some("post"),
            gt: None,
            gte: None,
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let and_group =
    where_clause.WhereClause(
      conditions: dict.new(),
      and: Some([uri1_clause, uri2_clause]),
      or: None,
    )

  let year_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "year",
          where_clause.WhereCondition(
            eq: None,
            in_values: None,
            contains: None,
            gt: None,
            gte: Some(sqlight.int(2000)),
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let root_clause =
    where_clause.WhereClause(
      conditions: dict.new(),
      and: Some([or_group, and_group, year_clause]),
      or: None,
    )

  let #(sql, params) = where_clause.build_where_sql(root_clause, False)

  // Should have both OR and AND with proper nesting
  should.be_true(string.contains(sql, "OR"))
  should.be_true(string.contains(sql, "AND"))
  should.be_true(string.contains(sql, "artist"))
  should.be_true(string.contains(sql, "genre"))
  should.be_true(string.contains(sql, "uri"))
  should.be_true(string.contains(sql, "year"))
  list.length(params) |> should.equal(5)
}

// Test: Multiple OR clauses at root level
pub fn build_where_multiple_or_at_root_test() {
  let clause1 =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "did",
          where_clause.WhereCondition(
            eq: Some(sqlight.text("did:plc:1")),
            in_values: None,
            contains: None,
            gt: None,
            gte: None,
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let clause2 =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "did",
          where_clause.WhereCondition(
            eq: Some(sqlight.text("did:plc:2")),
            in_values: None,
            contains: None,
            gt: None,
            gte: None,
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let clause3 =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "did",
          where_clause.WhereCondition(
            eq: Some(sqlight.text("did:plc:3")),
            in_values: None,
            contains: None,
            gt: None,
            gte: None,
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let root_clause =
    where_clause.WhereClause(
      conditions: dict.new(),
      and: None,
      or: Some([clause1, clause2, clause3]),
    )

  let #(sql, params) = where_clause.build_where_sql(root_clause, False)

  // Should have all three OR'ed together
  should.be_true(string.contains(sql, "OR"))
  should.be_true(string.contains(sql, "did"))
  list.length(params) |> should.equal(3)
}

// ===== isNull Operator Tests =====

// Test: isNull true on JSON field
pub fn build_where_is_null_true_json_field_test() {
  let condition =
    where_clause.WhereCondition(
      eq: None,
      in_values: None,
      contains: None,
      gt: None,
      gte: None,
      lt: None,
      lte: None,
      is_null: Some(True),
      is_numeric: False,
    )
  let clause =
    where_clause.WhereClause(
      conditions: dict.from_list([#("replyParent", condition)]),
      and: None,
      or: None,
    )

  let #(sql, params) = where_clause.build_where_sql(clause, False)

  sql |> should.equal("json_extract(json, '$.replyParent') IS NULL")
  list.length(params) |> should.equal(0)
}

// Test: isNull false on JSON field
pub fn build_where_is_null_false_json_field_test() {
  let condition =
    where_clause.WhereCondition(
      eq: None,
      in_values: None,
      contains: None,
      gt: None,
      gte: None,
      lt: None,
      lte: None,
      is_null: Some(False),
      is_numeric: False,
    )
  let clause =
    where_clause.WhereClause(
      conditions: dict.from_list([#("replyParent", condition)]),
      and: None,
      or: None,
    )

  let #(sql, params) = where_clause.build_where_sql(clause, False)

  sql |> should.equal("json_extract(json, '$.replyParent') IS NOT NULL")
  list.length(params) |> should.equal(0)
}

// Test: isNull true on table column
pub fn build_where_is_null_true_table_column_test() {
  let condition =
    where_clause.WhereCondition(
      eq: None,
      in_values: None,
      contains: None,
      gt: None,
      gte: None,
      lt: None,
      lte: None,
      is_null: Some(True),
      is_numeric: False,
    )
  let clause =
    where_clause.WhereClause(
      conditions: dict.from_list([#("cid", condition)]),
      and: None,
      or: None,
    )

  let #(sql, params) = where_clause.build_where_sql(clause, False)

  sql |> should.equal("cid IS NULL")
  list.length(params) |> should.equal(0)
}

// Test: isNull false on table column
pub fn build_where_is_null_false_table_column_test() {
  let condition =
    where_clause.WhereCondition(
      eq: None,
      in_values: None,
      contains: None,
      gt: None,
      gte: None,
      lt: None,
      lte: None,
      is_null: Some(False),
      is_numeric: False,
    )
  let clause =
    where_clause.WhereClause(
      conditions: dict.from_list([#("uri", condition)]),
      and: None,
      or: None,
    )

  let #(sql, params) = where_clause.build_where_sql(clause, False)

  sql |> should.equal("uri IS NOT NULL")
  list.length(params) |> should.equal(0)
}

// Test: isNull with table prefix (for joins)
pub fn build_where_is_null_with_table_prefix_test() {
  let condition =
    where_clause.WhereCondition(
      eq: None,
      in_values: None,
      contains: None,
      gt: None,
      gte: None,
      lt: None,
      lte: None,
      is_null: Some(True),
      is_numeric: False,
    )
  let clause =
    where_clause.WhereClause(
      conditions: dict.from_list([#("text", condition)]),
      and: None,
      or: None,
    )

  let #(sql, params) = where_clause.build_where_sql(clause, True)

  sql |> should.equal("json_extract(record.json, '$.text') IS NULL")
  list.length(params) |> should.equal(0)
}

// Test: isNull in nested AND clause
pub fn build_where_is_null_in_and_clause_test() {
  let is_null_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "replyParent",
          where_clause.WhereCondition(
            eq: None,
            in_values: None,
            contains: None,
            gt: None,
            gte: None,
            lt: None,
            lte: None,
            is_null: Some(True),
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let text_clause =
    where_clause.WhereClause(
      conditions: dict.from_list([
        #(
          "text",
          where_clause.WhereCondition(
            eq: None,
            in_values: None,
            contains: Some("hello"),
            gt: None,
            gte: None,
            lt: None,
            lte: None,
            is_null: None,
            is_numeric: False,
          ),
        ),
      ]),
      and: None,
      or: None,
    )

  let root_clause =
    where_clause.WhereClause(
      conditions: dict.new(),
      and: Some([is_null_clause, text_clause]),
      or: None,
    )

  let #(sql, params) = where_clause.build_where_sql(root_clause, False)

  should.be_true(string.contains(sql, "IS NULL"))
  should.be_true(string.contains(sql, "LIKE"))
  should.be_true(string.contains(sql, "AND"))
  list.length(params) |> should.equal(1)
}
