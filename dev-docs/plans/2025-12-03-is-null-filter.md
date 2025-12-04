# isNull Filter Operator Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add `isNull: Boolean` operator to where clause field conditions to filter on JSON null values or missing fields.

**Architecture:** Add `isNull` field to the `WhereCondition` type across all layers (GraphQL schema → parsing → SQL generation). The operator generates `IS NULL` / `IS NOT NULL` SQL without bound parameters.

**Tech Stack:** Gleam, GraphQL (swell), SQLite

---

## Task 1: Add isNull to GraphQL Schema

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/input/connection.gleam:86-114`

**Step 1: Write the failing test**

Create test file `lexicon_graphql/test/where_is_null_schema_test.gleam`:

```gleam
/// Tests for isNull field in where condition schema
import gleeunit
import gleeunit/should
import lexicon_graphql/input/connection
import swell/schema

pub fn main() {
  gleeunit.main()
}

pub fn where_condition_has_is_null_field_test() {
  // Build a where condition type
  let condition_type =
    connection.build_where_condition_input_type("Test", schema.string_type())

  // Get the input object fields
  case condition_type {
    schema.InputObject(_, _, fields) -> {
      // Find the isNull field
      let has_is_null =
        fields
        |> list.any(fn(field) {
          case field {
            schema.InputField(name, _, _, _) -> name == "isNull"
          }
        })
      has_is_null |> should.be_true
    }
    _ -> should.fail()
  }
}
```

**Step 2: Run test to verify it fails**

Run: `cd lexicon_graphql && gleam test -- --filter="where_is_null_schema"`
Expected: FAIL - test can't find `isNull` field

**Step 3: Add isNull field to schema builder**

In `lexicon_graphql/src/lexicon_graphql/input/connection.gleam`, update `build_where_condition_input_type`:

```gleam
/// Builds a WhereConditionInput type for filtering a specific field type
/// Supports: eq, in, contains, gt, gte, lt, lte, isNull operators
pub fn build_where_condition_input_type(
  type_name: String,
  field_type: schema.Type,
) -> schema.Type {
  let condition_type_name = type_name <> "FieldCondition"

  schema.input_object_type(
    condition_type_name,
    "Filter operators for " <> type_name <> " fields",
    [
      schema.input_field("eq", field_type, "Exact match (equals)", None),
      schema.input_field(
        "in",
        schema.list_type(schema.non_null(field_type)),
        "Match any value in the list",
        None,
      ),
      schema.input_field(
        "contains",
        schema.string_type(),
        "Case-insensitive substring match (string fields only)",
        None,
      ),
      schema.input_field("gt", field_type, "Greater than", None),
      schema.input_field("gte", field_type, "Greater than or equal to", None),
      schema.input_field("lt", field_type, "Less than", None),
      schema.input_field("lte", field_type, "Less than or equal to", None),
      schema.input_field(
        "isNull",
        schema.boolean_type(),
        "Filter for null or missing values (true) or non-null values (false)",
        None,
      ),
    ],
  )
}
```

**Step 4: Run test to verify it passes**

Run: `cd lexicon_graphql && gleam test -- --filter="where_is_null_schema"`
Expected: PASS

**Step 5: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/input/connection.gleam lexicon_graphql/test/where_is_null_schema_test.gleam
git commit -m "feat(where): add isNull field to GraphQL schema"
```

---

## Task 2: Add isNull to GraphQL Parsing Layer

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/input/where.gleam:19-29, 41-121`
- Test: `lexicon_graphql/test/where_input_test.gleam`

**Step 1: Write the failing test**

Add to `lexicon_graphql/test/where_input_test.gleam`:

```gleam
// ===== isNull Operator Tests =====

pub fn parse_is_null_true_test() {
  // { field: { isNull: true } }
  let condition_value = value.Object([#("isNull", value.Boolean(True))])
  let where_value = value.Object([#("field", condition_value)])

  let result = where_input.parse_where_clause(where_value)

  case dict.get(result.conditions, "field") {
    Ok(condition) -> {
      case condition.is_null {
        Some(True) -> should.be_true(True)
        _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_is_null_false_test() {
  // { field: { isNull: false } }
  let condition_value = value.Object([#("isNull", value.Boolean(False))])
  let where_value = value.Object([#("field", condition_value)])

  let result = where_input.parse_where_clause(where_value)

  case dict.get(result.conditions, "field") {
    Ok(condition) -> {
      case condition.is_null {
        Some(False) -> should.be_true(True)
        _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}
```

**Step 2: Run test to verify it fails**

Run: `cd lexicon_graphql && gleam test -- --filter="parse_is_null"`
Expected: FAIL - `is_null` field doesn't exist on WhereCondition

**Step 3: Add isNull to WhereCondition type and parsing**

In `lexicon_graphql/src/lexicon_graphql/input/where.gleam`:

Update the `WhereCondition` type (around line 19):

```gleam
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
    is_null: Option(Bool),
  )
}
```

Update `parse_condition` function (around line 41) to parse `isNull`:

```gleam
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

      // For comparison operators, try to parse string values as integers
      // This allows numeric comparisons even when the GraphQL schema uses String types
      let gt = case list.key_find(fields, "gt") {
        Ok(value.String(s)) -> Some(parse_string_or_int(s))
        Ok(value.Int(i)) -> Some(IntValue(i))
        _ -> None
      }

      let gte = case list.key_find(fields, "gte") {
        Ok(value.String(s)) -> Some(parse_string_or_int(s))
        Ok(value.Int(i)) -> Some(IntValue(i))
        _ -> None
      }

      let lt = case list.key_find(fields, "lt") {
        Ok(value.String(s)) -> Some(parse_string_or_int(s))
        Ok(value.Int(i)) -> Some(IntValue(i))
        _ -> None
      }

      let lte = case list.key_find(fields, "lte") {
        Ok(value.String(s)) -> Some(parse_string_or_int(s))
        Ok(value.Int(i)) -> Some(IntValue(i))
        _ -> None
      }

      let is_null = case list.key_find(fields, "isNull") {
        Ok(value.Boolean(b)) -> Some(b)
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
        is_null: is_null,
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
        is_null: None,
      )
  }
}
```

**Step 4: Run test to verify it passes**

Run: `cd lexicon_graphql && gleam test -- --filter="parse_is_null"`
Expected: PASS

**Step 5: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/input/where.gleam lexicon_graphql/test/where_input_test.gleam
git commit -m "feat(where): add isNull parsing in GraphQL layer"
```

---

## Task 3: Add isNull to SQL Where Clause Type

**Files:**
- Modify: `server/src/where_clause.gleam:9-21, 36-47, 55-69`
- Test: `server/test/where_clause_test.gleam`

**Step 1: Write the failing test**

Add to `server/test/where_clause_test.gleam`:

```gleam
// Test is_condition_empty with is_null operator set
pub fn is_condition_empty_false_with_is_null_test() {
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
  where_clause.is_condition_empty(condition) |> should.be_false
}
```

**Step 2: Run test to verify it fails**

Run: `cd server && gleam test -- --filter="is_condition_empty_false_with_is_null"`
Expected: FAIL - `is_null` field doesn't exist

**Step 3: Add isNull to WhereCondition type**

In `server/src/where_clause.gleam`:

Update the `WhereCondition` type (around line 9):

```gleam
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
    is_null: Option(Bool),
    /// Whether the comparison values are numeric (affects JSON field casting)
    is_numeric: Bool,
  )
}
```

Update `empty_condition` function (around line 36):

```gleam
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
```

Update `is_condition_empty` function (around line 55):

```gleam
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
```

**Step 4: Run test to verify it passes**

Run: `cd server && gleam test -- --filter="is_condition_empty_false_with_is_null"`
Expected: PASS

**Step 5: Commit**

```bash
git add server/src/where_clause.gleam server/test/where_clause_test.gleam
git commit -m "feat(where): add isNull to SQL WhereCondition type"
```

---

## Task 4: Add isNull to Where Converter

**Files:**
- Modify: `server/src/where_converter.gleam:45-58`

**Step 1: Write the failing test**

Add test file `server/test/where_converter_test.gleam`:

```gleam
/// Tests for where clause conversion
import gleam/dict
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should
import lexicon_graphql/input/where as where_input
import where_clause
import where_converter

pub fn main() {
  gleeunit.main()
}

pub fn convert_is_null_true_test() {
  let input_condition =
    where_input.WhereCondition(
      eq: None,
      in_values: None,
      contains: None,
      gt: None,
      gte: None,
      lt: None,
      lte: None,
      is_null: Some(True),
    )

  let input_clause =
    where_input.WhereClause(
      conditions: dict.from_list([#("field", input_condition)]),
      and: None,
      or: None,
    )

  let result = where_converter.convert_where_clause(input_clause)

  case dict.get(result.conditions, "field") {
    Ok(condition) -> {
      condition.is_null |> should.equal(Some(True))
    }
    Error(_) -> should.fail()
  }
}

pub fn convert_is_null_false_test() {
  let input_condition =
    where_input.WhereCondition(
      eq: None,
      in_values: None,
      contains: None,
      gt: None,
      gte: None,
      lt: None,
      lte: None,
      is_null: Some(False),
    )

  let input_clause =
    where_input.WhereClause(
      conditions: dict.from_list([#("field", input_condition)]),
      and: None,
      or: None,
    )

  let result = where_converter.convert_where_clause(input_clause)

  case dict.get(result.conditions, "field") {
    Ok(condition) -> {
      condition.is_null |> should.equal(Some(False))
    }
    Error(_) -> should.fail()
  }
}
```

**Step 2: Run test to verify it fails**

Run: `cd server && gleam test -- --filter="convert_is_null"`
Expected: FAIL - missing is_null field in conversion

**Step 3: Add isNull to converter**

In `server/src/where_converter.gleam`, update `convert_condition` (around line 45):

```gleam
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
```

**Step 4: Run test to verify it passes**

Run: `cd server && gleam test -- --filter="convert_is_null"`
Expected: PASS

**Step 5: Commit**

```bash
git add server/src/where_converter.gleam server/test/where_converter_test.gleam
git commit -m "feat(where): add isNull to where converter"
```

---

## Task 5: Add isNull SQL Generation

**Files:**
- Modify: `server/src/where_clause.gleam:156-252`
- Test: `server/test/where_sql_builder_test.gleam`

**Step 1: Write the failing tests**

Add to `server/test/where_sql_builder_test.gleam`:

```gleam
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
```

**Step 2: Run tests to verify they fail**

Run: `cd server && gleam test -- --filter="build_where_is_null"`
Expected: FAIL - isNull SQL generation not implemented

**Step 3: Add isNull SQL generation**

In `server/src/where_clause.gleam`, update `build_single_condition` function (around line 156).

Find the function and add `is_null` handling after the `contains` operator handling:

```gleam
/// Builds SQL for a single condition on a field
/// Returns a list of SQL strings and accumulated parameters
fn build_single_condition(
  field: String,
  condition: WhereCondition,
  use_table_prefix: Bool,
) -> #(List(String), List(sqlight.Value)) {
  // Check if numeric casting is needed (for gt/gte/lt/lte operators)
  let has_numeric_comparison = should_cast_numeric(condition)

  let field_ref =
    build_field_ref_with_cast(field, use_table_prefix, has_numeric_comparison)

  // For isNull, we need the field ref without numeric cast
  let field_ref_no_cast = build_field_ref(field, use_table_prefix)

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
  let mut_sql_parts = sql_parts
  let mut_params = params

  // isNull operator (no parameters needed)
  let #(sql_parts, params) = case condition.is_null {
    Some(True) -> {
      let sql = field_ref_no_cast <> " IS NULL"
      #([sql, ..mut_sql_parts], mut_params)
    }
    Some(False) -> {
      let sql = field_ref_no_cast <> " IS NOT NULL"
      #([sql, ..mut_sql_parts], mut_params)
    }
    None -> #(mut_sql_parts, mut_params)
  }

  // Reverse to maintain correct order (we built backwards)
  #(list.reverse(sql_parts), list.reverse(params))
}
```

**Step 4: Run tests to verify they pass**

Run: `cd server && gleam test -- --filter="build_where_is_null"`
Expected: PASS

**Step 5: Commit**

```bash
git add server/src/where_clause.gleam server/test/where_sql_builder_test.gleam
git commit -m "feat(where): add isNull SQL generation"
```

---

## Task 6: Fix Existing Tests

**Files:**
- Modify: Various test files that construct WhereCondition

After adding `is_null` field, existing tests that construct `WhereCondition` manually will fail because they're missing the new field.

**Step 1: Run all tests to find failures**

Run: `cd server && gleam test`
Run: `cd lexicon_graphql && gleam test`

**Step 2: Update all WhereCondition constructors in tests**

Search for `WhereCondition(` in test files and add `is_null: None,` to each one.

Files likely needing updates:
- `server/test/where_clause_test.gleam`
- `server/test/where_sql_builder_test.gleam`
- `server/test/where_edge_cases_test.gleam`
- `server/test/where_integration_test.gleam`

Example fix for each occurrence:
```gleam
// Before
where_clause.WhereCondition(
  eq: Some(sqlight.text("value")),
  in_values: None,
  contains: None,
  gt: None,
  gte: None,
  lt: None,
  lte: None,
  is_numeric: False,
)

// After
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
```

Similarly for `lexicon_graphql/input/where.gleam` tests:
```gleam
// Before (if any manual constructions exist)
where_input.WhereCondition(
  eq: None,
  in_values: None,
  contains: None,
  gt: None,
  gte: None,
  lt: None,
  lte: None,
)

// After
where_input.WhereCondition(
  eq: None,
  in_values: None,
  contains: None,
  gt: None,
  gte: None,
  lt: None,
  lte: None,
  is_null: None,
)
```

**Step 3: Run all tests to verify they pass**

Run: `cd server && gleam test`
Run: `cd lexicon_graphql && gleam test`
Expected: All PASS

**Step 4: Commit**

```bash
git add -A
git commit -m "fix(tests): update WhereCondition constructors with is_null field"
```

---

## Task 7: Full Integration Test

**Files:**
- Test: `server/test/where_integration_test.gleam` or new file

**Step 1: Write integration test**

Add to `server/test/where_integration_test.gleam` (or create if needed):

```gleam
pub fn is_null_end_to_end_test() {
  // Test the full flow: GraphQL input -> parsing -> conversion -> SQL

  // Simulate GraphQL input: { replyParent: { isNull: true } }
  let condition_value = value.Object([#("isNull", value.Boolean(True))])
  let where_value = value.Object([#("replyParent", condition_value)])

  // Parse
  let parsed = where_input.parse_where_clause(where_value)

  // Convert
  let converted = where_converter.convert_where_clause(parsed)

  // Generate SQL
  let #(sql, params) = where_clause.build_where_sql(converted, False)

  sql |> should.equal("json_extract(json, '$.replyParent') IS NULL")
  list.length(params) |> should.equal(0)
}

pub fn is_not_null_end_to_end_test() {
  // Simulate GraphQL input: { replyParent: { isNull: false } }
  let condition_value = value.Object([#("isNull", value.Boolean(False))])
  let where_value = value.Object([#("replyParent", condition_value)])

  // Parse
  let parsed = where_input.parse_where_clause(where_value)

  // Convert
  let converted = where_converter.convert_where_clause(parsed)

  // Generate SQL
  let #(sql, params) = where_clause.build_where_sql(converted, False)

  sql |> should.equal("json_extract(json, '$.replyParent') IS NOT NULL")
  list.length(params) |> should.equal(0)
}
```

**Step 2: Run integration tests**

Run: `cd server && gleam test -- --filter="is_null_end_to_end"`
Expected: PASS

**Step 3: Commit**

```bash
git add server/test/where_integration_test.gleam
git commit -m "test(where): add isNull integration tests"
```

---

## Task 8: Final Verification

**Step 1: Run full test suite**

```bash
cd lexicon_graphql && gleam test
cd ../server && gleam test
```

Expected: All tests PASS

**Step 2: Build the project**

```bash
gleam build
```

Expected: Build succeeds with no errors

**Step 3: Final commit (if any remaining changes)**

```bash
git status
# If clean, done. Otherwise:
git add -A
git commit -m "chore: final cleanup for isNull feature"
```
