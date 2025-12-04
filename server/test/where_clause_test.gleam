import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should
import sqlight
import where_clause

pub fn main() {
  gleeunit.main()
}

// Test empty condition creation
pub fn empty_condition_test() {
  let condition = where_clause.empty_condition()

  condition.eq |> should.equal(None)
  condition.in_values |> should.equal(None)
  condition.contains |> should.equal(None)
  condition.gt |> should.equal(None)
  condition.gte |> should.equal(None)
  condition.lt |> should.equal(None)
  condition.lte |> should.equal(None)
}

// Test empty clause creation
pub fn empty_clause_test() {
  let clause = where_clause.empty_clause()

  clause.conditions |> dict.is_empty |> should.be_true
  clause.and |> should.equal(None)
  clause.or |> should.equal(None)
}

// Test is_condition_empty with empty condition
pub fn is_condition_empty_true_test() {
  let condition = where_clause.empty_condition()
  where_clause.is_condition_empty(condition) |> should.be_true
}

// Test is_condition_empty with eq operator set
pub fn is_condition_empty_false_with_eq_test() {
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
  where_clause.is_condition_empty(condition) |> should.be_false
}

// Test is_condition_empty with contains operator set
pub fn is_condition_empty_false_with_contains_test() {
  let condition =
    where_clause.WhereCondition(
      eq: None,
      in_values: None,
      contains: Some("search"),
      gt: None,
      gte: None,
      lt: None,
      lte: None,
      is_null: None,
      is_numeric: False,
    )
  where_clause.is_condition_empty(condition) |> should.be_false
}

// Test is_clause_empty with empty clause
pub fn is_clause_empty_true_test() {
  let clause = where_clause.empty_clause()
  where_clause.is_clause_empty(clause) |> should.be_true
}

// Test is_clause_empty with conditions
pub fn is_clause_empty_false_with_conditions_test() {
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
      conditions: dict.from_list([#("field", condition)]),
      and: None,
      or: None,
    )
  where_clause.is_clause_empty(clause) |> should.be_false
}

// Test is_clause_empty with nested AND
pub fn is_clause_empty_false_with_and_test() {
  let nested_clause = where_clause.empty_clause()
  let clause =
    where_clause.WhereClause(
      conditions: dict.new(),
      and: Some([nested_clause]),
      or: None,
    )
  where_clause.is_clause_empty(clause) |> should.be_false
}

// Test is_clause_empty with nested OR
pub fn is_clause_empty_false_with_or_test() {
  let nested_clause = where_clause.empty_clause()
  let clause =
    where_clause.WhereClause(
      conditions: dict.new(),
      and: None,
      or: Some([nested_clause]),
    )
  where_clause.is_clause_empty(clause) |> should.be_false
}

// Test constructing a condition with multiple operators
pub fn condition_with_multiple_operators_test() {
  let condition =
    where_clause.WhereCondition(
      eq: None,
      in_values: None,
      contains: None,
      gt: Some(sqlight.int(10)),
      gte: None,
      lt: Some(sqlight.int(100)),
      lte: None,
      is_null: None,
      is_numeric: False,
    )

  condition.gt |> should.equal(Some(sqlight.int(10)))
  condition.lt |> should.equal(Some(sqlight.int(100)))
  where_clause.is_condition_empty(condition) |> should.be_false
}

// Test constructing a clause with nested AND/OR
pub fn clause_with_nested_and_or_test() {
  let and_clause1 =
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

  let or_clause =
    where_clause.WhereClause(
      conditions: dict.new(),
      and: None,
      or: Some([and_clause1]),
    )

  let root_clause =
    where_clause.WhereClause(
      conditions: dict.new(),
      and: Some([or_clause]),
      or: None,
    )

  case root_clause.and {
    Some(and_list) -> list.length(and_list) |> should.equal(1)
    None -> should.fail()
  }
}

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
