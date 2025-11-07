/// Tests for GraphQL where input parsing
///
/// Tests the parsing of GraphQL values into WhereClause structures
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should
import lexicon_graphql/where_input
import swell/value

pub fn main() {
  gleeunit.main()
}

// ===== Basic Operator Tests =====

pub fn parse_eq_operator_test() {
  // { field: { eq: "value" } }
  let condition_value = value.Object([#("eq", value.String("test_value"))])
  let where_value = value.Object([#("field", condition_value)])

  let result = where_input.parse_where_clause(where_value)

  // Check that we got a condition for "field"
  case dict.get(result.conditions, "field") {
    Ok(condition) -> {
      case condition.eq {
        Some(where_input.StringValue("test_value")) -> should.be_true(True)
        _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_in_operator_test() {
  // { status: { in: ["active", "pending"] } }
  let condition_value =
    value.Object([
      #("in", value.List([value.String("active"), value.String("pending")])),
    ])
  let where_value = value.Object([#("status", condition_value)])

  let result = where_input.parse_where_clause(where_value)

  case dict.get(result.conditions, "status") {
    Ok(condition) -> {
      case condition.in_values {
        Some(values) -> {
          list.length(values) |> should.equal(2)
          // Check first value
          case list.first(values) {
            Ok(where_input.StringValue("active")) -> should.be_true(True)
            _ -> should.fail()
          }
        }
        None -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_contains_operator_test() {
  // { text: { contains: "hello" } }
  let condition_value = value.Object([#("contains", value.String("hello"))])
  let where_value = value.Object([#("text", condition_value)])

  let result = where_input.parse_where_clause(where_value)

  case dict.get(result.conditions, "text") {
    Ok(condition) -> {
      case condition.contains {
        Some("hello") -> should.be_true(True)
        _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_gt_operator_test() {
  // { age: { gt: 18 } }
  let condition_value = value.Object([#("gt", value.Int(18))])
  let where_value = value.Object([#("age", condition_value)])

  let result = where_input.parse_where_clause(where_value)

  case dict.get(result.conditions, "age") {
    Ok(condition) -> {
      case condition.gt {
        Some(where_input.IntValue(18)) -> should.be_true(True)
        _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_gte_operator_test() {
  // { age: { gte: 21 } }
  let condition_value = value.Object([#("gte", value.Int(21))])
  let where_value = value.Object([#("age", condition_value)])

  let result = where_input.parse_where_clause(where_value)

  case dict.get(result.conditions, "age") {
    Ok(condition) -> {
      case condition.gte {
        Some(where_input.IntValue(21)) -> should.be_true(True)
        _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_lt_operator_test() {
  // { price: { lt: 100 } }
  let condition_value = value.Object([#("lt", value.Int(100))])
  let where_value = value.Object([#("price", condition_value)])

  let result = where_input.parse_where_clause(where_value)

  case dict.get(result.conditions, "price") {
    Ok(condition) -> {
      case condition.lt {
        Some(where_input.IntValue(100)) -> should.be_true(True)
        _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_lte_operator_test() {
  // { count: { lte: 50 } }
  let condition_value = value.Object([#("lte", value.Int(50))])
  let where_value = value.Object([#("count", condition_value)])

  let result = where_input.parse_where_clause(where_value)

  case dict.get(result.conditions, "count") {
    Ok(condition) -> {
      case condition.lte {
        Some(where_input.IntValue(50)) -> should.be_true(True)
        _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// ===== Multiple Operators on Same Field =====

pub fn parse_range_query_test() {
  // { age: { gte: 18, lte: 65 } }
  let condition_value =
    value.Object([#("gte", value.Int(18)), #("lte", value.Int(65))])
  let where_value = value.Object([#("age", condition_value)])

  let result = where_input.parse_where_clause(where_value)

  case dict.get(result.conditions, "age") {
    Ok(condition) -> {
      // Check both operators are present
      case condition.gte, condition.lte {
        Some(where_input.IntValue(18)), Some(where_input.IntValue(65)) ->
          should.be_true(True)
        _, _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// ===== Multiple Fields =====

pub fn parse_multiple_fields_test() {
  // { name: { eq: "alice" }, age: { gt: 18 } }
  let name_condition = value.Object([#("eq", value.String("alice"))])
  let age_condition = value.Object([#("gt", value.Int(18))])

  let where_value =
    value.Object([#("name", name_condition), #("age", age_condition)])

  let result = where_input.parse_where_clause(where_value)

  // Check we have 2 conditions
  dict.size(result.conditions) |> should.equal(2)

  // Check name condition
  case dict.get(result.conditions, "name") {
    Ok(condition) -> {
      case condition.eq {
        Some(where_input.StringValue("alice")) -> should.be_true(True)
        _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }

  // Check age condition
  case dict.get(result.conditions, "age") {
    Ok(condition) -> {
      case condition.gt {
        Some(where_input.IntValue(18)) -> should.be_true(True)
        _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// ===== AND Logic Tests =====

pub fn parse_simple_and_test() {
  // { and: [{ name: { eq: "alice" } }, { age: { gt: 18 } }] }
  let name_condition = value.Object([#("eq", value.String("alice"))])
  let age_condition = value.Object([#("gt", value.Int(18))])

  let name_clause = value.Object([#("name", name_condition)])
  let age_clause = value.Object([#("age", age_condition)])

  let where_value =
    value.Object([#("and", value.List([name_clause, age_clause]))])

  let result = where_input.parse_where_clause(where_value)

  // Check AND is present
  case result.and {
    Some(and_clauses) -> {
      list.length(and_clauses) |> should.equal(2)
    }
    None -> should.fail()
  }
}

pub fn parse_nested_and_test() {
  // { and: [{ and: [{ field: { eq: "value" } }] }] }
  let inner_condition = value.Object([#("eq", value.String("value"))])
  let inner_clause = value.Object([#("field", inner_condition)])
  let middle_clause = value.Object([#("and", value.List([inner_clause]))])
  let outer_clause = value.Object([#("and", value.List([middle_clause]))])

  let result = where_input.parse_where_clause(outer_clause)

  // Check nested structure
  case result.and {
    Some(outer_and) -> {
      list.length(outer_and) |> should.equal(1)
      // Check first clause has nested AND
      case list.first(outer_and) {
        Ok(middle) -> {
          case middle.and {
            Some(inner_and) -> {
              list.length(inner_and) |> should.equal(1)
            }
            None -> should.fail()
          }
        }
        Error(_) -> should.fail()
      }
    }
    None -> should.fail()
  }
}

// ===== OR Logic Tests =====

pub fn parse_simple_or_test() {
  // { or: [{ status: { eq: "active" } }, { status: { eq: "pending" } }] }
  let active_condition = value.Object([#("eq", value.String("active"))])
  let pending_condition = value.Object([#("eq", value.String("pending"))])

  let active_clause = value.Object([#("status", active_condition)])
  let pending_clause = value.Object([#("status", pending_condition)])

  let where_value =
    value.Object([#("or", value.List([active_clause, pending_clause]))])

  let result = where_input.parse_where_clause(where_value)

  // Check OR is present
  case result.or {
    Some(or_clauses) -> {
      list.length(or_clauses) |> should.equal(2)
    }
    None -> should.fail()
  }
}

pub fn parse_nested_or_test() {
  // { or: [{ or: [{ field: { eq: "value" } }] }] }
  let inner_condition = value.Object([#("eq", value.String("value"))])
  let inner_clause = value.Object([#("field", inner_condition)])
  let middle_clause = value.Object([#("or", value.List([inner_clause]))])
  let outer_clause = value.Object([#("or", value.List([middle_clause]))])

  let result = where_input.parse_where_clause(outer_clause)

  // Check nested structure
  case result.or {
    Some(outer_or) -> {
      list.length(outer_or) |> should.equal(1)
      // Check first clause has nested OR
      case list.first(outer_or) {
        Ok(middle) -> {
          case middle.or {
            Some(inner_or) -> {
              list.length(inner_or) |> should.equal(1)
            }
            None -> should.fail()
          }
        }
        Error(_) -> should.fail()
      }
    }
    None -> should.fail()
  }
}

// ===== Mixed AND/OR Tests =====

pub fn parse_and_or_mixed_test() {
  // {
  //   and: [
  //     { or: [{ a: { eq: "1" } }, { b: { eq: "2" } }] },
  //     { c: { eq: "3" } }
  //   ]
  // }
  let a_condition = value.Object([#("eq", value.String("1"))])
  let b_condition = value.Object([#("eq", value.String("2"))])
  let c_condition = value.Object([#("eq", value.String("3"))])

  let a_clause = value.Object([#("a", a_condition)])
  let b_clause = value.Object([#("b", b_condition)])
  let c_clause = value.Object([#("c", c_condition)])

  let or_clause = value.Object([#("or", value.List([a_clause, b_clause]))])
  let where_value = value.Object([#("and", value.List([or_clause, c_clause]))])

  let result = where_input.parse_where_clause(where_value)

  // Check structure
  case result.and {
    Some(and_clauses) -> {
      list.length(and_clauses) |> should.equal(2)

      // First clause should have OR
      case list.first(and_clauses) {
        Ok(first_clause) -> {
          case first_clause.or {
            Some(or_clauses) -> {
              list.length(or_clauses) |> should.equal(2)
            }
            None -> should.fail()
          }
        }
        Error(_) -> should.fail()
      }
    }
    None -> should.fail()
  }
}

// ===== Edge Cases =====

pub fn parse_empty_object_test() {
  let where_value = value.Object([])
  let result = where_input.parse_where_clause(where_value)

  where_input.is_clause_empty(result) |> should.be_true
}

pub fn parse_empty_and_list_test() {
  let where_value = value.Object([#("and", value.List([]))])
  let result = where_input.parse_where_clause(where_value)

  // Empty AND list should result in None or empty list
  case result.and {
    None -> should.be_true(True)
    Some(clauses) -> list.is_empty(clauses) |> should.be_true
  }
}

pub fn parse_empty_or_list_test() {
  let where_value = value.Object([#("or", value.List([]))])
  let result = where_input.parse_where_clause(where_value)

  // Empty OR list should result in None or empty list
  case result.or {
    None -> should.be_true(True)
    Some(clauses) -> list.is_empty(clauses) |> should.be_true
  }
}

pub fn parse_invalid_value_test() {
  // Pass a non-object value
  let where_value = value.String("not an object")
  let result = where_input.parse_where_clause(where_value)

  // Should return empty clause
  where_input.is_clause_empty(result) |> should.be_true
}

pub fn parse_boolean_value_test() {
  // { active: { eq: true } }
  let condition_value = value.Object([#("eq", value.Boolean(True))])
  let where_value = value.Object([#("active", condition_value)])

  let result = where_input.parse_where_clause(where_value)

  case dict.get(result.conditions, "active") {
    Ok(condition) -> {
      case condition.eq {
        Some(where_input.BoolValue(True)) -> should.be_true(True)
        _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_mixed_types_in_values_test() {
  // { ids: { in: [1, 2, 3] } }
  let condition_value =
    value.Object([
      #("in", value.List([value.Int(1), value.Int(2), value.Int(3)])),
    ])
  let where_value = value.Object([#("ids", condition_value)])

  let result = where_input.parse_where_clause(where_value)

  case dict.get(result.conditions, "ids") {
    Ok(condition) -> {
      case condition.in_values {
        Some(values) -> {
          list.length(values) |> should.equal(3)
        }
        None -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// ===== Complex Real-World Examples =====

pub fn parse_complex_user_filter_test() {
  // Real-world example: find users matching complex criteria
  // {
  //   and: [
  //     { or: [{ status: { eq: "active" } }, { status: { eq: "premium" } }] },
  //     { age: { gte: 18, lte: 65 } },
  //     { name: { contains: "smith" } }
  //   ]
  // }

  let active_cond = value.Object([#("eq", value.String("active"))])
  let premium_cond = value.Object([#("eq", value.String("premium"))])
  let status_active = value.Object([#("status", active_cond)])
  let status_premium = value.Object([#("status", premium_cond)])
  let or_status =
    value.Object([#("or", value.List([status_active, status_premium]))])

  let age_cond =
    value.Object([#("gte", value.Int(18)), #("lte", value.Int(65))])
  let age_clause = value.Object([#("age", age_cond)])

  let name_cond = value.Object([#("contains", value.String("smith"))])
  let name_clause = value.Object([#("name", name_cond)])

  let where_value =
    value.Object([#("and", value.List([or_status, age_clause, name_clause]))])

  let result = where_input.parse_where_clause(where_value)

  // Verify structure
  case result.and {
    Some(and_clauses) -> {
      list.length(and_clauses) |> should.equal(3)
    }
    None -> should.fail()
  }
}
