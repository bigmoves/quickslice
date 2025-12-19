# GraphQL Security Hardening Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement OWASP-recommended GraphQL security protections across swell (executor) and lexicon_graphql (schema generation) libraries.

**Architecture:** Security validations run pre-execution in swell (depth, cost, introspection, batching). Custom scalars and constraint validation live in lexicon_graphql. Rate limiting stays in quickslice server using ETS-backed sliding window counters.

**Tech Stack:** Gleam, OTP (process timeouts, ETS), swell GraphQL executor, lexicon_graphql schema generator

---

## Part 1: Swell Security Configuration

### Task 1: Add SecurityConfig Type

**Files:**
- Create: `/Users/chadmiller/code/swell/src/swell/security.gleam`
- Test: `/Users/chadmiller/code/swell/test/security_test.gleam`

**Step 1: Write the failing test**

```gleam
// test/security_test.gleam
import gleeunit/should
import swell/security

pub fn default_security_config_test() {
  let config = security.default_config()

  config.max_depth |> should.equal(10)
  config.max_cost |> should.equal(10_000)
  config.default_list_cost |> should.equal(25)
  config.timeout_ms |> should.equal(30_000)
}

pub fn production_security_config_test() {
  let config = security.production_config()

  config.max_depth |> should.equal(10)
  config.max_cost |> should.equal(5000)
  config.timeout_ms |> should.equal(15_000)
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/swell && gleam test`
Expected: FAIL with "module swell/security not found"

**Step 3: Write minimal implementation**

```gleam
// src/swell/security.gleam
import gleam/option.{type Option, None, Some}
import swell/schema

/// Security configuration for GraphQL execution
pub type SecurityConfig {
  SecurityConfig(
    /// Maximum query nesting depth (default: 10)
    max_depth: Int,
    /// Maximum query cost before rejection (default: 10_000)
    max_cost: Int,
    /// Default cost multiplier for list fields without explicit limit (default: 25)
    default_list_cost: Int,
    /// Execution timeout in milliseconds (default: 30_000)
    timeout_ms: Int,
    /// Callback to determine if introspection is allowed (default: always true)
    allow_introspection: fn(schema.Context) -> Bool,
    /// Batching policy (default: Limited(10))
    batch_policy: BatchPolicy,
  )
}

/// Batching policy for multi-operation requests
pub type BatchPolicy {
  /// Reject all batched requests
  Disabled
  /// Allow up to N operations per request
  Limited(max: Int)
}

/// Security error types
pub type SecurityError {
  QueryTooDeep(depth: Int, max: Int, path: List(String))
  QueryTooExpensive(cost: Int, max: Int)
  ExecutionTimeout(timeout_ms: Int)
  IntrospectionDisabled
  BatchingDisabled
  BatchTooLarge(count: Int, max: Int)
}

/// Default security config for development (permissive)
pub fn default_config() -> SecurityConfig {
  SecurityConfig(
    max_depth: 10,
    max_cost: 10_000,
    default_list_cost: 25,
    timeout_ms: 30_000,
    allow_introspection: fn(_) { True },
    batch_policy: Limited(10),
  )
}

/// Production security config (stricter)
pub fn production_config() -> SecurityConfig {
  SecurityConfig(
    max_depth: 10,
    max_cost: 5000,
    default_list_cost: 25,
    timeout_ms: 15_000,
    allow_introspection: fn(_) { False },
    batch_policy: Limited(5),
  )
}

/// Convert security error to human-readable message
pub fn error_message(error: SecurityError) -> String {
  case error {
    QueryTooDeep(depth, max, _path) ->
      "Query depth " <> int_to_string(depth) <> " exceeds maximum " <> int_to_string(max)
    QueryTooExpensive(cost, max) ->
      "Query cost " <> int_to_string(cost) <> " exceeds maximum " <> int_to_string(max)
    ExecutionTimeout(timeout) ->
      "Query execution timed out after " <> int_to_string(timeout) <> "ms"
    IntrospectionDisabled ->
      "Introspection is disabled"
    BatchingDisabled ->
      "Batched queries are not allowed"
    BatchTooLarge(count, max) ->
      "Batch contains " <> int_to_string(count) <> " operations, maximum is " <> int_to_string(max)
  }
}

/// Convert security error to extension code for GraphQL response
pub fn error_code(error: SecurityError) -> String {
  case error {
    QueryTooDeep(_, _, _) -> "QUERY_TOO_DEEP"
    QueryTooExpensive(_, _) -> "QUERY_TOO_EXPENSIVE"
    ExecutionTimeout(_) -> "EXECUTION_TIMEOUT"
    IntrospectionDisabled -> "INTROSPECTION_DISABLED"
    BatchingDisabled -> "BATCHING_DISABLED"
    BatchTooLarge(_, _) -> "BATCH_TOO_LARGE"
  }
}

fn int_to_string(n: Int) -> String {
  case n {
    0 -> "0"
    _ -> do_int_to_string(n, "")
  }
}

fn do_int_to_string(n: Int, acc: String) -> String {
  case n {
    0 -> acc
    _ -> {
      let digit = n % 10
      let char = case digit {
        0 -> "0"
        1 -> "1"
        2 -> "2"
        3 -> "3"
        4 -> "4"
        5 -> "5"
        6 -> "6"
        7 -> "7"
        8 -> "8"
        _ -> "9"
      }
      do_int_to_string(n / 10, char <> acc)
    }
  }
}
```

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/swell && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
cd /Users/chadmiller/code/swell
git add src/swell/security.gleam test/security_test.gleam
git commit -m "feat(security): add SecurityConfig type and default configs"
```

---

### Task 2: Implement Query Depth Validation

**Files:**
- Modify: `/Users/chadmiller/code/swell/src/swell/security.gleam`
- Test: `/Users/chadmiller/code/swell/test/security_test.gleam`

**Step 1: Write the failing test**

```gleam
// Add to test/security_test.gleam
import swell/parser

pub fn validate_depth_shallow_query_test() {
  let query = "{ user { name } }"
  let assert Ok(doc) = parser.parse(query)
  let config = security.default_config()

  security.validate_depth(doc, config)
  |> should.be_ok()
}

pub fn validate_depth_deep_query_fails_test() {
  // Query with depth 4: a -> b -> c -> d -> e
  let query = "{ a { b { c { d { e } } } } }"
  let assert Ok(doc) = parser.parse(query)
  let config = security.SecurityConfig(..security.default_config(), max_depth: 3)

  case security.validate_depth(doc, config) {
    Error(security.QueryTooDeep(depth, max, _)) -> {
      depth |> should.equal(5)
      max |> should.equal(3)
    }
    _ -> should.fail()
  }
}

pub fn validate_depth_with_fragments_test() {
  let query = "
    query { user { ...UserFields } }
    fragment UserFields on User { posts { title } }
  "
  let assert Ok(doc) = parser.parse(query)
  let config = security.SecurityConfig(..security.default_config(), max_depth: 3)

  // user(1) -> posts(2) -> title(3) = depth 3, should pass
  security.validate_depth(doc, config)
  |> should.be_ok()
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/swell && gleam test`
Expected: FAIL with "validate_depth not found"

**Step 3: Write minimal implementation**

```gleam
// Add to src/swell/security.gleam
import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import swell/parser

/// Validate query depth against config limit
pub fn validate_depth(
  document: parser.Document,
  config: SecurityConfig,
) -> Result(Nil, SecurityError) {
  let parser.Document(operations) = document

  // Build fragments dictionary
  let fragments = build_fragments_dict(operations)

  // Check each executable operation
  list.try_each(operations, fn(op) {
    case op {
      parser.FragmentDefinition(_, _, _) -> Ok(Nil)
      _ -> validate_operation_depth(op, config.max_depth, fragments)
    }
  })
}

fn build_fragments_dict(
  operations: List(parser.Operation),
) -> Dict(String, parser.Operation) {
  operations
  |> list.filter_map(fn(op) {
    case op {
      parser.FragmentDefinition(name, _, _) -> Ok(#(name, op))
      _ -> Error(Nil)
    }
  })
  |> dict.from_list()
}

fn validate_operation_depth(
  operation: parser.Operation,
  max_depth: Int,
  fragments: Dict(String, parser.Operation),
) -> Result(Nil, SecurityError) {
  let selection_set = case operation {
    parser.Query(ss) -> ss
    parser.NamedQuery(_, _, ss) -> ss
    parser.Mutation(ss) -> ss
    parser.NamedMutation(_, _, ss) -> ss
    parser.Subscription(ss) -> ss
    parser.NamedSubscription(_, _, ss) -> ss
    parser.FragmentDefinition(_, _, ss) -> ss
  }

  validate_selection_set_depth(selection_set, 1, max_depth, fragments, [])
}

fn validate_selection_set_depth(
  selection_set: parser.SelectionSet,
  current_depth: Int,
  max_depth: Int,
  fragments: Dict(String, parser.Operation),
  path: List(String),
) -> Result(Nil, SecurityError) {
  case current_depth > max_depth {
    True -> Error(QueryTooDeep(current_depth, max_depth, list.reverse(path)))
    False -> {
      let parser.SelectionSet(selections) = selection_set
      list.try_each(selections, fn(selection) {
        validate_selection_depth(selection, current_depth, max_depth, fragments, path)
      })
    }
  }
}

fn validate_selection_depth(
  selection: parser.Selection,
  current_depth: Int,
  max_depth: Int,
  fragments: Dict(String, parser.Operation),
  path: List(String),
) -> Result(Nil, SecurityError) {
  case selection {
    parser.Field(name, _, _, nested) -> {
      case nested {
        [] -> Ok(Nil)
        _ -> {
          let nested_set = parser.SelectionSet(nested)
          validate_selection_set_depth(
            nested_set,
            current_depth + 1,
            max_depth,
            fragments,
            [name, ..path],
          )
        }
      }
    }
    parser.FragmentSpread(name) -> {
      case dict.get(fragments, name) {
        Ok(parser.FragmentDefinition(_, _, ss)) ->
          validate_selection_set_depth(ss, current_depth, max_depth, fragments, path)
        _ -> Ok(Nil)
      }
    }
    parser.InlineFragment(_, nested) -> {
      let nested_set = parser.SelectionSet(nested)
      validate_selection_set_depth(nested_set, current_depth, max_depth, fragments, path)
    }
  }
}
```

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/swell && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
cd /Users/chadmiller/code/swell
git add src/swell/security.gleam test/security_test.gleam
git commit -m "feat(security): implement query depth validation"
```

---

### Task 3: Implement Query Cost Analysis

**Files:**
- Modify: `/Users/chadmiller/code/swell/src/swell/security.gleam`
- Test: `/Users/chadmiller/code/swell/test/security_test.gleam`

**Step 1: Write the failing test**

```gleam
// Add to test/security_test.gleam

pub fn calculate_cost_simple_query_test() {
  let query = "{ user { name email } }"
  let assert Ok(doc) = parser.parse(query)
  let config = security.default_config()

  case security.calculate_cost(doc, config) {
    Ok(cost) -> cost |> should.equal(1)  // user = 1, scalars = 0
    Error(_) -> should.fail()
  }
}

pub fn calculate_cost_nested_lists_test() {
  // users(first:100) { posts(first:10) { comments(first:5) { text } } }
  let query = "{ users(first: 100) { posts(first: 10) { comments(first: 5) { text } } } }"
  let assert Ok(doc) = parser.parse(query)
  let config = security.default_config()

  case security.calculate_cost(doc, config) {
    // users: 100, posts: 100*10=1000, comments: 1000*5=5000, total: 6100
    Ok(cost) -> cost |> should.equal(6100)
    Error(_) -> should.fail()
  }
}

pub fn validate_cost_rejects_expensive_query_test() {
  let query = "{ users(first: 1000) { posts(first: 100) { title } } }"
  let assert Ok(doc) = parser.parse(query)
  let config = security.SecurityConfig(..security.default_config(), max_cost: 10_000)

  case security.validate_cost(doc, config) {
    // users: 1000, posts: 1000*100=100000, total: 101000 > 10000
    Error(security.QueryTooExpensive(cost, max)) -> {
      cost |> should.equal(101_000)
      max |> should.equal(10_000)
    }
    _ -> should.fail()
  }
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/swell && gleam test`
Expected: FAIL with "calculate_cost not found"

**Step 3: Write minimal implementation**

```gleam
// Add to src/swell/security.gleam
import gleam/int

/// Calculate query cost
pub fn calculate_cost(
  document: parser.Document,
  config: SecurityConfig,
) -> Result(Int, SecurityError) {
  let parser.Document(operations) = document
  let fragments = build_fragments_dict(operations)

  // Sum cost of all executable operations
  list.try_fold(operations, 0, fn(total, op) {
    case op {
      parser.FragmentDefinition(_, _, _) -> Ok(total)
      _ -> {
        case calculate_operation_cost(op, config, fragments, 1) {
          Ok(cost) -> Ok(total + cost)
          Error(e) -> Error(e)
        }
      }
    }
  })
}

/// Validate query cost against config limit
pub fn validate_cost(
  document: parser.Document,
  config: SecurityConfig,
) -> Result(Nil, SecurityError) {
  case calculate_cost(document, config) {
    Ok(cost) if cost > config.max_cost ->
      Error(QueryTooExpensive(cost, config.max_cost))
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(e)
  }
}

fn calculate_operation_cost(
  operation: parser.Operation,
  config: SecurityConfig,
  fragments: Dict(String, parser.Operation),
  multiplier: Int,
) -> Result(Int, SecurityError) {
  let selection_set = case operation {
    parser.Query(ss) -> ss
    parser.NamedQuery(_, _, ss) -> ss
    parser.Mutation(ss) -> ss
    parser.NamedMutation(_, _, ss) -> ss
    parser.Subscription(ss) -> ss
    parser.NamedSubscription(_, _, ss) -> ss
    parser.FragmentDefinition(_, _, ss) -> ss
  }

  calculate_selection_set_cost(selection_set, config, fragments, multiplier)
}

fn calculate_selection_set_cost(
  selection_set: parser.SelectionSet,
  config: SecurityConfig,
  fragments: Dict(String, parser.Operation),
  multiplier: Int,
) -> Result(Int, SecurityError) {
  let parser.SelectionSet(selections) = selection_set

  list.try_fold(selections, 0, fn(total, selection) {
    case calculate_selection_cost(selection, config, fragments, multiplier) {
      Ok(cost) -> Ok(total + cost)
      Error(e) -> Error(e)
    }
  })
}

fn calculate_selection_cost(
  selection: parser.Selection,
  config: SecurityConfig,
  fragments: Dict(String, parser.Operation),
  multiplier: Int,
) -> Result(Int, SecurityError) {
  case selection {
    parser.Field(_, _, arguments, nested) -> {
      case nested {
        [] -> Ok(0)  // Scalar field, no cost
        _ -> {
          // Object/list field - calculate cost based on limit argument
          let limit = get_limit_from_arguments(arguments, config.default_list_cost)
          let field_cost = multiplier * limit
          let nested_set = parser.SelectionSet(nested)
          case calculate_selection_set_cost(nested_set, config, fragments, field_cost) {
            Ok(nested_cost) -> Ok(field_cost + nested_cost)
            Error(e) -> Error(e)
          }
        }
      }
    }
    parser.FragmentSpread(name) -> {
      case dict.get(fragments, name) {
        Ok(parser.FragmentDefinition(_, _, ss)) ->
          calculate_selection_set_cost(ss, config, fragments, multiplier)
        _ -> Ok(0)
      }
    }
    parser.InlineFragment(_, nested) -> {
      let nested_set = parser.SelectionSet(nested)
      calculate_selection_set_cost(nested_set, config, fragments, multiplier)
    }
  }
}

fn get_limit_from_arguments(
  arguments: List(parser.Argument),
  default: Int,
) -> Int {
  // Look for first, last, limit, or take arguments
  list.find_map(arguments, fn(arg) {
    case arg {
      parser.Argument("first", parser.IntValue(val)) -> parse_int(val)
      parser.Argument("last", parser.IntValue(val)) -> parse_int(val)
      parser.Argument("limit", parser.IntValue(val)) -> parse_int(val)
      parser.Argument("take", parser.IntValue(val)) -> parse_int(val)
      _ -> Error(Nil)
    }
  })
  |> result.unwrap(default)
}

fn parse_int(s: String) -> Result(Int, Nil) {
  int.parse(s)
}
```

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/swell && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
cd /Users/chadmiller/code/swell
git add src/swell/security.gleam test/security_test.gleam
git commit -m "feat(security): implement query cost analysis"
```

---

### Task 4: Implement Introspection Control

**Files:**
- Modify: `/Users/chadmiller/code/swell/src/swell/security.gleam`
- Test: `/Users/chadmiller/code/swell/test/security_test.gleam`

**Step 1: Write the failing test**

```gleam
// Add to test/security_test.gleam
import swell/schema

pub fn validate_introspection_allowed_test() {
  let query = "{ __schema { types { name } } }"
  let assert Ok(doc) = parser.parse(query)
  let config = security.default_config()  // allows introspection by default
  let ctx = schema.context(option.None)

  security.validate_introspection(doc, ctx, config)
  |> should.be_ok()
}

pub fn validate_introspection_blocked_test() {
  let query = "{ __schema { types { name } } }"
  let assert Ok(doc) = parser.parse(query)
  let config = security.SecurityConfig(
    ..security.default_config(),
    allow_introspection: fn(_) { False },
  )
  let ctx = schema.context(option.None)

  case security.validate_introspection(doc, ctx, config) {
    Error(security.IntrospectionDisabled) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_introspection_typename_allowed_test() {
  // __typename should always be allowed
  let query = "{ user { __typename name } }"
  let assert Ok(doc) = parser.parse(query)
  let config = security.SecurityConfig(
    ..security.default_config(),
    allow_introspection: fn(_) { False },
  )
  let ctx = schema.context(option.None)

  security.validate_introspection(doc, ctx, config)
  |> should.be_ok()
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/swell && gleam test`
Expected: FAIL with "validate_introspection not found"

**Step 3: Write minimal implementation**

```gleam
// Add to src/swell/security.gleam

/// Validate introspection access
pub fn validate_introspection(
  document: parser.Document,
  ctx: schema.Context,
  config: SecurityConfig,
) -> Result(Nil, SecurityError) {
  case has_introspection_fields(document) {
    False -> Ok(Nil)
    True -> {
      case config.allow_introspection(ctx) {
        True -> Ok(Nil)
        False -> Error(IntrospectionDisabled)
      }
    }
  }
}

fn has_introspection_fields(document: parser.Document) -> Bool {
  let parser.Document(operations) = document
  list.any(operations, fn(op) {
    case op {
      parser.Query(ss) -> selection_set_has_introspection(ss)
      parser.NamedQuery(_, _, ss) -> selection_set_has_introspection(ss)
      parser.Mutation(ss) -> selection_set_has_introspection(ss)
      parser.NamedMutation(_, _, ss) -> selection_set_has_introspection(ss)
      parser.Subscription(ss) -> selection_set_has_introspection(ss)
      parser.NamedSubscription(_, _, ss) -> selection_set_has_introspection(ss)
      parser.FragmentDefinition(_, _, _) -> False
    }
  })
}

fn selection_set_has_introspection(selection_set: parser.SelectionSet) -> Bool {
  let parser.SelectionSet(selections) = selection_set
  list.any(selections, fn(selection) {
    case selection {
      parser.Field(name, _, _, _) ->
        // __schema and __type are introspection queries
        // __typename is allowed everywhere
        name == "__schema" || name == "__type"
      parser.FragmentSpread(_) -> False
      parser.InlineFragment(_, nested) ->
        selection_set_has_introspection(parser.SelectionSet(nested))
    }
  })
}
```

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/swell && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
cd /Users/chadmiller/code/swell
git add src/swell/security.gleam test/security_test.gleam
git commit -m "feat(security): implement introspection control"
```

---

### Task 5: Implement Batch Validation

**Files:**
- Modify: `/Users/chadmiller/code/swell/src/swell/security.gleam`
- Test: `/Users/chadmiller/code/swell/test/security_test.gleam`

**Step 1: Write the failing test**

```gleam
// Add to test/security_test.gleam

pub fn validate_batch_within_limit_test() {
  let operations = [
    parser.Query(parser.SelectionSet([])),
    parser.Query(parser.SelectionSet([])),
  ]
  let config = security.SecurityConfig(..security.default_config(), batch_policy: security.Limited(10))

  security.validate_batch(operations, config)
  |> should.be_ok()
}

pub fn validate_batch_exceeds_limit_test() {
  let operations = list.repeat(parser.Query(parser.SelectionSet([])), 15)
  let config = security.SecurityConfig(..security.default_config(), batch_policy: security.Limited(10))

  case security.validate_batch(operations, config) {
    Error(security.BatchTooLarge(count, max)) -> {
      count |> should.equal(15)
      max |> should.equal(10)
    }
    _ -> should.fail()
  }
}

pub fn validate_batch_disabled_test() {
  let operations = [
    parser.Query(parser.SelectionSet([])),
    parser.Query(parser.SelectionSet([])),
  ]
  let config = security.SecurityConfig(..security.default_config(), batch_policy: security.Disabled)

  case security.validate_batch(operations, config) {
    Error(security.BatchingDisabled) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_batch_single_allowed_when_disabled_test() {
  let operations = [parser.Query(parser.SelectionSet([]))]
  let config = security.SecurityConfig(..security.default_config(), batch_policy: security.Disabled)

  security.validate_batch(operations, config)
  |> should.be_ok()
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/swell && gleam test`
Expected: FAIL with "validate_batch not found"

**Step 3: Write minimal implementation**

```gleam
// Add to src/swell/security.gleam

/// Validate batch size against policy
pub fn validate_batch(
  operations: List(parser.Operation),
  config: SecurityConfig,
) -> Result(Nil, SecurityError) {
  // Filter out fragment definitions - they don't count as operations
  let executable_ops = list.filter(operations, fn(op) {
    case op {
      parser.FragmentDefinition(_, _, _) -> False
      _ -> True
    }
  })

  let count = list.length(executable_ops)

  case config.batch_policy {
    Disabled -> {
      case count <= 1 {
        True -> Ok(Nil)
        False -> Error(BatchingDisabled)
      }
    }
    Limited(max) -> {
      case count <= max {
        True -> Ok(Nil)
        False -> Error(BatchTooLarge(count, max))
      }
    }
  }
}
```

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/swell && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
cd /Users/chadmiller/code/swell
git add src/swell/security.gleam test/security_test.gleam
git commit -m "feat(security): implement batch validation"
```

---

### Task 6: Add Unified validate_all Function

**Files:**
- Modify: `/Users/chadmiller/code/swell/src/swell/security.gleam`
- Test: `/Users/chadmiller/code/swell/test/security_test.gleam`

**Step 1: Write the failing test**

```gleam
// Add to test/security_test.gleam

pub fn validate_all_passes_valid_query_test() {
  let query = "{ user { name } }"
  let assert Ok(doc) = parser.parse(query)
  let config = security.default_config()
  let ctx = schema.context(option.None)

  security.validate_all(doc, ctx, config)
  |> should.be_ok()
}

pub fn validate_all_fails_on_first_error_test() {
  // Too deep query
  let query = "{ a { b { c { d { e { f { g { h { i { j { k { l } } } } } } } } } } } }"
  let assert Ok(doc) = parser.parse(query)
  let config = security.SecurityConfig(..security.default_config(), max_depth: 5)
  let ctx = schema.context(option.None)

  case security.validate_all(doc, ctx, config) {
    Error(security.QueryTooDeep(_, _, _)) -> Nil
    _ -> should.fail()
  }
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/swell && gleam test`
Expected: FAIL with "validate_all not found"

**Step 3: Write minimal implementation**

```gleam
// Add to src/swell/security.gleam

/// Run all security validations
pub fn validate_all(
  document: parser.Document,
  ctx: schema.Context,
  config: SecurityConfig,
) -> Result(Nil, SecurityError) {
  let parser.Document(operations) = document

  // Validate batch size first (cheapest check)
  use _ <- result.try(validate_batch(operations, config))

  // Validate introspection access
  use _ <- result.try(validate_introspection(document, ctx, config))

  // Validate depth (before cost, since deep queries are often expensive)
  use _ <- result.try(validate_depth(document, config))

  // Validate cost
  use _ <- result.try(validate_cost(document, config))

  Ok(Nil)
}
```

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/swell && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
cd /Users/chadmiller/code/swell
git add src/swell/security.gleam test/security_test.gleam
git commit -m "feat(security): add unified validate_all function"
```

---

### Task 7: Integrate Security into Executor

**Files:**
- Modify: `/Users/chadmiller/code/swell/src/swell/executor.gleam`
- Test: `/Users/chadmiller/code/swell/test/executor_security_test.gleam`

**Step 1: Write the failing test**

```gleam
// test/executor_security_test.gleam
import gleeunit/should
import gleam/option
import swell/executor
import swell/schema
import swell/security

pub fn execute_with_security_rejects_deep_query_test() {
  // Build a simple schema
  let user_type = schema.object_type("User", [
    schema.field("name", schema.string(), fn(_) { Ok(swell/value.String("test")) }),
  ])
  let query_type = schema.object_type("Query", [
    schema.field("user", user_type, fn(_) { Ok(swell/value.Object([#("name", swell/value.String("test"))])) }),
  ])
  let test_schema = schema.schema(query_type)

  // Deep query that exceeds limit
  let query = "{ user { user { user { user { user { name } } } } } }"
  let ctx = schema.context(option.None)
  let config = security.SecurityConfig(..security.default_config(), max_depth: 3)

  case executor.execute_with_security(query, test_schema, ctx, config) {
    Error(msg) -> {
      msg |> should.equal("Query depth 5 exceeds maximum 3")
    }
    Ok(_) -> should.fail()
  }
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/swell && gleam test`
Expected: FAIL with "execute_with_security not found"

**Step 3: Write minimal implementation**

```gleam
// Add to src/swell/executor.gleam after imports
import swell/security

/// Execute a GraphQL query with security validation
pub fn execute_with_security(
  query: String,
  graphql_schema: schema.Schema,
  ctx: schema.Context,
  security_config: security.SecurityConfig,
) -> Result(Response, String) {
  // Parse the query
  case parser.parse(query) {
    Error(parse_error) ->
      Error("Parse error: " <> format_parse_error(parse_error))
    Ok(document) -> {
      // Run security validations
      case security.validate_all(document, ctx, security_config) {
        Error(security_error) ->
          Error(security.error_message(security_error))
        Ok(_) -> {
          // Build canonical type registry for union resolution
          let type_registry = build_type_registry(graphql_schema)

          // Execute the document
          case execute_document(document, graphql_schema, ctx, type_registry) {
            Ok(#(data, errors)) -> Ok(Response(data, errors))
            Error(err) -> Error(err)
          }
        }
      }
    }
  }
}
```

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/swell && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
cd /Users/chadmiller/code/swell
git add src/swell/executor.gleam test/executor_security_test.gleam
git commit -m "feat(executor): integrate security validation into execution"
```

---

## Part 2: Lexicon GraphQL Custom Scalars

### Task 8: Create AtUri Scalar

**Files:**
- Create: `/Users/chadmiller/code/quickslice/lexicon_graphql/src/lexicon_graphql/scalar/at_uri.gleam`
- Test: `/Users/chadmiller/code/quickslice/lexicon_graphql/test/scalar/at_uri_test.gleam`

**Step 1: Write the failing test**

```gleam
// test/scalar/at_uri_test.gleam
import gleeunit/should
import lexicon_graphql/scalar/at_uri
import swell/value

pub fn parse_valid_at_uri_test() {
  let input = value.String("at://did:plc:abc123/app.bsky.feed.post/xyz")

  case at_uri.parse(input) {
    Ok(value.String(s)) -> s |> should.equal("at://did:plc:abc123/app.bsky.feed.post/xyz")
    _ -> should.fail()
  }
}

pub fn parse_invalid_at_uri_test() {
  let input = value.String("https://example.com")

  case at_uri.parse(input) {
    Error(msg) -> msg |> should.equal("AtUri must start with 'at://'")
    _ -> should.fail()
  }
}

pub fn parse_malformed_at_uri_test() {
  let input = value.String("at://notadid/collection")

  case at_uri.parse(input) {
    Error(msg) -> msg |> should.equal("AtUri has invalid DID format")
    _ -> should.fail()
  }
}

pub fn scalar_definition_test() {
  let scalar = at_uri.scalar()

  scalar.name |> should.equal("AtUri")
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`
Expected: FAIL with "module lexicon_graphql/scalar/at_uri not found"

**Step 3: Write minimal implementation**

```gleam
// src/lexicon_graphql/scalar/at_uri.gleam
import gleam/string
import swell/schema
import swell/value

/// AT Protocol URI scalar type
/// Format: at://did/collection/rkey
pub fn scalar() -> schema.Scalar {
  schema.Scalar(
    name: "AtUri",
    description: "AT Protocol URI (at://did/collection/rkey)",
    parse: parse,
    serialize: serialize,
  )
}

pub fn parse(input: value.Value) -> Result(value.Value, String) {
  case input {
    value.String(s) -> validate_at_uri(s)
    value.Null -> Ok(value.Null)
    _ -> Error("AtUri must be a string")
  }
}

pub fn serialize(val: value.Value) -> value.Value {
  val
}

fn validate_at_uri(s: String) -> Result(value.Value, String) {
  case string.starts_with(s, "at://") {
    False -> Error("AtUri must start with 'at://'")
    True -> {
      // Extract the part after "at://"
      let rest = string.drop_start(s, 5)

      // Check for valid DID (must start with "did:")
      case string.starts_with(rest, "did:") {
        False -> Error("AtUri has invalid DID format")
        True -> {
          // Check for at least one path segment after DID
          case string.contains(rest, "/") {
            False -> Error("AtUri missing collection path")
            True -> Ok(value.String(s))
          }
        }
      }
    }
  }
}
```

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql
git add src/lexicon_graphql/scalar/at_uri.gleam test/scalar/at_uri_test.gleam
git commit -m "feat(scalar): add AtUri custom scalar with validation"
```

---

### Task 9: Create Did Scalar

**Files:**
- Create: `/Users/chadmiller/code/quickslice/lexicon_graphql/src/lexicon_graphql/scalar/did.gleam`
- Test: `/Users/chadmiller/code/quickslice/lexicon_graphql/test/scalar/did_test.gleam`

**Step 1: Write the failing test**

```gleam
// test/scalar/did_test.gleam
import gleeunit/should
import lexicon_graphql/scalar/did
import swell/value

pub fn parse_valid_did_plc_test() {
  let input = value.String("did:plc:abc123xyz")

  case did.parse(input) {
    Ok(value.String(s)) -> s |> should.equal("did:plc:abc123xyz")
    _ -> should.fail()
  }
}

pub fn parse_valid_did_web_test() {
  let input = value.String("did:web:example.com")

  case did.parse(input) {
    Ok(value.String(s)) -> s |> should.equal("did:web:example.com")
    _ -> should.fail()
  }
}

pub fn parse_invalid_did_test() {
  let input = value.String("not-a-did")

  case did.parse(input) {
    Error(msg) -> msg |> should.equal("Did must start with 'did:'")
    _ -> should.fail()
  }
}

pub fn parse_did_missing_method_test() {
  let input = value.String("did:")

  case did.parse(input) {
    Error(msg) -> msg |> should.equal("Did missing method")
    _ -> should.fail()
  }
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`
Expected: FAIL with "module lexicon_graphql/scalar/did not found"

**Step 3: Write minimal implementation**

```gleam
// src/lexicon_graphql/scalar/did.gleam
import gleam/string
import swell/schema
import swell/value

/// Decentralized Identifier scalar type
/// Format: did:method:identifier
pub fn scalar() -> schema.Scalar {
  schema.Scalar(
    name: "Did",
    description: "Decentralized Identifier (did:method:identifier)",
    parse: parse,
    serialize: serialize,
  )
}

pub fn parse(input: value.Value) -> Result(value.Value, String) {
  case input {
    value.String(s) -> validate_did(s)
    value.Null -> Ok(value.Null)
    _ -> Error("Did must be a string")
  }
}

pub fn serialize(val: value.Value) -> value.Value {
  val
}

fn validate_did(s: String) -> Result(value.Value, String) {
  case string.starts_with(s, "did:") {
    False -> Error("Did must start with 'did:'")
    True -> {
      let rest = string.drop_start(s, 4)
      case string.length(rest) > 0 {
        False -> Error("Did missing method")
        True -> {
          // Check for method:identifier pattern
          case string.contains(rest, ":") {
            False -> Error("Did missing identifier")
            True -> Ok(value.String(s))
          }
        }
      }
    }
  }
}
```

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql
git add src/lexicon_graphql/scalar/did.gleam test/scalar/did_test.gleam
git commit -m "feat(scalar): add Did custom scalar with validation"
```

---

### Task 10: Create Handle Scalar

**Files:**
- Create: `/Users/chadmiller/code/quickslice/lexicon_graphql/src/lexicon_graphql/scalar/handle.gleam`
- Test: `/Users/chadmiller/code/quickslice/lexicon_graphql/test/scalar/handle_test.gleam`

**Step 1: Write the failing test**

```gleam
// test/scalar/handle_test.gleam
import gleeunit/should
import lexicon_graphql/scalar/handle
import swell/value

pub fn parse_valid_handle_test() {
  let input = value.String("alice.bsky.social")

  case handle.parse(input) {
    Ok(value.String(s)) -> s |> should.equal("alice.bsky.social")
    _ -> should.fail()
  }
}

pub fn parse_valid_custom_domain_test() {
  let input = value.String("alice.example.com")

  case handle.parse(input) {
    Ok(value.String(s)) -> s |> should.equal("alice.example.com")
    _ -> should.fail()
  }
}

pub fn parse_invalid_no_dot_test() {
  let input = value.String("alice")

  case handle.parse(input) {
    Error(msg) -> msg |> should.equal("Handle must contain at least one dot")
    _ -> should.fail()
  }
}

pub fn parse_invalid_starts_with_dot_test() {
  let input = value.String(".alice.bsky.social")

  case handle.parse(input) {
    Error(msg) -> msg |> should.equal("Handle cannot start with a dot")
    _ -> should.fail()
  }
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`
Expected: FAIL with "module lexicon_graphql/scalar/handle not found"

**Step 3: Write minimal implementation**

```gleam
// src/lexicon_graphql/scalar/handle.gleam
import gleam/string
import swell/schema
import swell/value

/// AT Protocol Handle scalar type
/// Format: username.domain.tld
pub fn scalar() -> schema.Scalar {
  schema.Scalar(
    name: "Handle",
    description: "AT Protocol handle (username.domain.tld)",
    parse: parse,
    serialize: serialize,
  )
}

pub fn parse(input: value.Value) -> Result(value.Value, String) {
  case input {
    value.String(s) -> validate_handle(s)
    value.Null -> Ok(value.Null)
    _ -> Error("Handle must be a string")
  }
}

pub fn serialize(val: value.Value) -> value.Value {
  val
}

fn validate_handle(s: String) -> Result(value.Value, String) {
  case string.starts_with(s, ".") {
    True -> Error("Handle cannot start with a dot")
    False -> {
      case string.ends_with(s, ".") {
        True -> Error("Handle cannot end with a dot")
        False -> {
          case string.contains(s, ".") {
            False -> Error("Handle must contain at least one dot")
            True -> {
              // Basic length check (handles are max 253 chars per DNS)
              case string.length(s) > 253 {
                True -> Error("Handle exceeds maximum length")
                False -> Ok(value.String(s))
              }
            }
          }
        }
      }
    }
  }
}
```

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql
git add src/lexicon_graphql/scalar/handle.gleam test/scalar/handle_test.gleam
git commit -m "feat(scalar): add Handle custom scalar with validation"
```

---

### Task 11: Create DateTime Scalar

**Files:**
- Create: `/Users/chadmiller/code/quickslice/lexicon_graphql/src/lexicon_graphql/scalar/datetime.gleam`
- Test: `/Users/chadmiller/code/quickslice/lexicon_graphql/test/scalar/datetime_test.gleam`

**Step 1: Write the failing test**

```gleam
// test/scalar/datetime_test.gleam
import gleeunit/should
import lexicon_graphql/scalar/datetime
import swell/value

pub fn parse_valid_datetime_z_test() {
  let input = value.String("2024-01-15T10:30:00Z")

  case datetime.parse(input) {
    Ok(value.String(s)) -> s |> should.equal("2024-01-15T10:30:00Z")
    _ -> should.fail()
  }
}

pub fn parse_valid_datetime_offset_test() {
  let input = value.String("2024-01-15T10:30:00+05:00")

  case datetime.parse(input) {
    Ok(value.String(s)) -> s |> should.equal("2024-01-15T10:30:00+05:00")
    _ -> should.fail()
  }
}

pub fn parse_valid_datetime_millis_test() {
  let input = value.String("2024-01-15T10:30:00.123Z")

  case datetime.parse(input) {
    Ok(value.String(s)) -> s |> should.equal("2024-01-15T10:30:00.123Z")
    _ -> should.fail()
  }
}

pub fn parse_invalid_datetime_test() {
  let input = value.String("not-a-date")

  case datetime.parse(input) {
    Error(msg) -> msg |> should.equal("DateTime must be in ISO 8601 format")
    _ -> should.fail()
  }
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`
Expected: FAIL with "module lexicon_graphql/scalar/datetime not found"

**Step 3: Write minimal implementation**

```gleam
// src/lexicon_graphql/scalar/datetime.gleam
import gleam/regex
import gleam/string
import swell/schema
import swell/value

/// ISO 8601 DateTime scalar type
pub fn scalar() -> schema.Scalar {
  schema.Scalar(
    name: "DateTime",
    description: "ISO 8601 datetime string",
    parse: parse,
    serialize: serialize,
  )
}

pub fn parse(input: value.Value) -> Result(value.Value, String) {
  case input {
    value.String(s) -> validate_datetime(s)
    value.Null -> Ok(value.Null)
    _ -> Error("DateTime must be a string")
  }
}

pub fn serialize(val: value.Value) -> value.Value {
  val
}

fn validate_datetime(s: String) -> Result(value.Value, String) {
  // ISO 8601 pattern: YYYY-MM-DDTHH:MM:SS[.sss](Z|+HH:MM|-HH:MM)
  let assert Ok(pattern) = regex.from_string(
    "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(\\.\\d+)?(Z|[+-]\\d{2}:\\d{2})$"
  )

  case regex.check(pattern, s) {
    True -> Ok(value.String(s))
    False -> Error("DateTime must be in ISO 8601 format")
  }
}
```

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql
git add src/lexicon_graphql/scalar/datetime.gleam test/scalar/datetime_test.gleam
git commit -m "feat(scalar): add DateTime custom scalar with ISO 8601 validation"
```

---

### Task 12: Integrate Scalars into Type Mapper

**Files:**
- Modify: `/Users/chadmiller/code/quickslice/lexicon_graphql/src/lexicon_graphql/internal/graphql/type_mapper.gleam`
- Test: `/Users/chadmiller/code/quickslice/lexicon_graphql/test/type_mapper_scalar_test.gleam`

**Step 1: Write the failing test**

```gleam
// test/type_mapper_scalar_test.gleam
import gleeunit/should
import lexicon_graphql/types
import lexicon_graphql/internal/graphql/type_mapper
import gleam/option.{None, Some}

pub fn map_at_uri_format_test() {
  let property = types.Property(
    type_: "string",
    required: True,
    format: Some("at-uri"),
    ref: None,
    refs: None,
    items: None,
  )

  let result = type_mapper.property_to_graphql_type(property)
  // Should return AtUri scalar type
  result.name |> should.equal("AtUri")
}

pub fn map_did_format_test() {
  let property = types.Property(
    type_: "string",
    required: True,
    format: Some("did"),
    ref: None,
    refs: None,
    items: None,
  )

  let result = type_mapper.property_to_graphql_type(property)
  result.name |> should.equal("Did")
}

pub fn map_handle_format_test() {
  let property = types.Property(
    type_: "string",
    required: True,
    format: Some("handle"),
    ref: None,
    refs: None,
    items: None,
  )

  let result = type_mapper.property_to_graphql_type(property)
  result.name |> should.equal("Handle")
}

pub fn map_datetime_format_test() {
  let property = types.Property(
    type_: "string",
    required: True,
    format: Some("datetime"),
    ref: None,
    refs: None,
    items: None,
  )

  let result = type_mapper.property_to_graphql_type(property)
  result.name |> should.equal("DateTime")
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`
Expected: FAIL (test may pass if mapping exists, or fail with wrong type name)

**Step 3: Write minimal implementation**

Add to type_mapper.gleam's string type mapping section:

```gleam
// In type_mapper.gleam, update the string type mapping function
import lexicon_graphql/scalar/at_uri
import lexicon_graphql/scalar/did
import lexicon_graphql/scalar/handle
import lexicon_graphql/scalar/datetime

fn map_string_type(property: types.Property) -> schema.Type {
  case property.format {
    option.Some("at-uri") -> schema.scalar_type(at_uri.scalar())
    option.Some("did") -> schema.scalar_type(did.scalar())
    option.Some("handle") -> schema.scalar_type(handle.scalar())
    option.Some("datetime") -> schema.scalar_type(datetime.scalar())
    option.Some("uri") -> schema.string()  // Regular URI, no validation
    option.Some("cid-link") -> schema.string()  // CID, no validation
    _ -> schema.string()
  }
}
```

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql
git add src/lexicon_graphql/internal/graphql/type_mapper.gleam test/type_mapper_scalar_test.gleam
git commit -m "feat(type_mapper): integrate custom scalars for AT Protocol formats"
```

---

## Part 3: Constraint Validation in lexicon_graphql

### Task 13: Extend Property Type with Constraints

**Files:**
- Modify: `/Users/chadmiller/code/quickslice/lexicon_graphql/src/lexicon_graphql/types.gleam`
- Test: `/Users/chadmiller/code/quickslice/lexicon_graphql/test/types_test.gleam`

**Step 1: Write the failing test**

```gleam
// test/types_test.gleam
import gleeunit/should
import lexicon_graphql/types
import gleam/option.{None, Some}

pub fn property_with_max_length_test() {
  let prop = types.Property(
    type_: "string",
    required: True,
    format: None,
    ref: None,
    refs: None,
    items: None,
    max_length: Some(128),
    min_length: None,
    max_graphemes: None,
    min_graphemes: None,
    pattern: None,
  )

  prop.max_length |> should.equal(Some(128))
}

pub fn property_with_pattern_test() {
  let prop = types.Property(
    type_: "string",
    required: True,
    format: None,
    ref: None,
    refs: None,
    items: None,
    max_length: None,
    min_length: None,
    max_graphemes: None,
    min_graphemes: None,
    pattern: Some("^[a-z]+$"),
  )

  prop.pattern |> should.equal(Some("^[a-z]+$"))
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`
Expected: FAIL with "Property does not have field max_length"

**Step 3: Write minimal implementation**

```gleam
// Update src/lexicon_graphql/types.gleam
import gleam/option.{type Option}

/// Property definition with constraint fields
pub type Property {
  Property(
    type_: String,
    required: Bool,
    format: Option(String),
    ref: Option(String),
    refs: Option(List(String)),
    items: Option(ArrayItems),
    // Constraint fields
    max_length: Option(Int),
    min_length: Option(Int),
    max_graphemes: Option(Int),
    min_graphemes: Option(Int),
    pattern: Option(String),
  )
}

/// Create a Property with default constraint values (all None)
pub fn property(
  type_: String,
  required: Bool,
  format: Option(String),
  ref: Option(String),
  refs: Option(List(String)),
  items: Option(ArrayItems),
) -> Property {
  Property(
    type_: type_,
    required: required,
    format: format,
    ref: ref,
    refs: refs,
    items: items,
    max_length: option.None,
    min_length: option.None,
    max_graphemes: option.None,
    min_graphemes: option.None,
    pattern: option.None,
  )
}
```

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`
Expected: PASS (after fixing all Property constructors in codebase)

**Step 5: Commit**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql
git add src/lexicon_graphql/types.gleam test/types_test.gleam
git commit -m "feat(types): add constraint fields to Property type"
```

---

### Task 14: Update Parser to Extract Constraints

**Files:**
- Modify: `/Users/chadmiller/code/quickslice/lexicon_graphql/src/lexicon_graphql/internal/lexicon/parser.gleam`
- Test: `/Users/chadmiller/code/quickslice/lexicon_graphql/test/parser_constraints_test.gleam`

**Step 1: Write the failing test**

```gleam
// test/parser_constraints_test.gleam
import gleeunit/should
import lexicon_graphql/internal/lexicon/parser
import gleam/option.{Some}

pub fn parse_property_with_max_length_test() {
  let json = "{\"type\": \"string\", \"maxLength\": 128}"

  case parser.parse_property_json(json) {
    Ok(prop) -> prop.max_length |> should.equal(Some(128))
    Error(_) -> should.fail()
  }
}

pub fn parse_property_with_pattern_test() {
  let json = "{\"type\": \"string\", \"pattern\": \"^[a-z]+$\"}"

  case parser.parse_property_json(json) {
    Ok(prop) -> prop.pattern |> should.equal(Some("^[a-z]+$"))
    Error(_) -> should.fail()
  }
}

pub fn parse_property_with_multiple_constraints_test() {
  let json = "{\"type\": \"string\", \"minLength\": 1, \"maxLength\": 100, \"maxGraphemes\": 50}"

  case parser.parse_property_json(json) {
    Ok(prop) -> {
      prop.min_length |> should.equal(Some(1))
      prop.max_length |> should.equal(Some(100))
      prop.max_graphemes |> should.equal(Some(50))
    }
    Error(_) -> should.fail()
  }
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`
Expected: FAIL (constraints not being parsed)

**Step 3: Write minimal implementation**

Update parser.gleam to extract constraint fields:

```gleam
// In parser.gleam, update parse_property function
fn parse_property(json: Dynamic) -> Result(types.Property, List(dynamic.DecodeError)) {
  use type_ <- result.try(dynamic.field("type", dynamic.string)(json))
  use format <- result.try(get_optional_string(json, "format"))
  use ref <- result.try(get_optional_string(json, "ref"))
  use refs <- result.try(get_optional_string_list(json, "refs"))
  use items <- result.try(get_optional_items(json))

  // Parse constraint fields
  use max_length <- result.try(get_optional_int(json, "maxLength"))
  use min_length <- result.try(get_optional_int(json, "minLength"))
  use max_graphemes <- result.try(get_optional_int(json, "maxGraphemes"))
  use min_graphemes <- result.try(get_optional_int(json, "minGraphemes"))
  use pattern <- result.try(get_optional_string(json, "pattern"))

  Ok(types.Property(
    type_: type_,
    required: False,  // Set based on required array later
    format: format,
    ref: ref,
    refs: refs,
    items: items,
    max_length: max_length,
    min_length: min_length,
    max_graphemes: max_graphemes,
    min_graphemes: min_graphemes,
    pattern: pattern,
  ))
}

fn get_optional_int(
  json: Dynamic,
  field_name: String,
) -> Result(Option(Int), List(dynamic.DecodeError)) {
  case dynamic.field(field_name, dynamic.int)(json) {
    Ok(n) -> Ok(option.Some(n))
    Error(_) -> Ok(option.None)
  }
}
```

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql
git add src/lexicon_graphql/internal/lexicon/parser.gleam test/parser_constraints_test.gleam
git commit -m "feat(parser): extract constraint fields from lexicon JSON"
```

---

### Task 15: Create Input Validator Module

**Files:**
- Create: `/Users/chadmiller/code/quickslice/lexicon_graphql/src/lexicon_graphql/validation/input.gleam`
- Test: `/Users/chadmiller/code/quickslice/lexicon_graphql/test/validation/input_test.gleam`

**Step 1: Write the failing test**

```gleam
// test/validation/input_test.gleam
import gleeunit/should
import lexicon_graphql/validation/input
import lexicon_graphql/types
import swell/value
import gleam/option.{None, Some}

pub fn validate_max_length_passes_test() {
  let prop = types.Property(
    type_: "string",
    required: True,
    format: None,
    ref: None,
    refs: None,
    items: None,
    max_length: Some(10),
    min_length: None,
    max_graphemes: None,
    min_graphemes: None,
    pattern: None,
  )

  input.validate(value.String("hello"), prop, "name")
  |> should.be_ok()
}

pub fn validate_max_length_fails_test() {
  let prop = types.Property(
    type_: "string",
    required: True,
    format: None,
    ref: None,
    refs: None,
    items: None,
    max_length: Some(5),
    min_length: None,
    max_graphemes: None,
    min_graphemes: None,
    pattern: None,
  )

  case input.validate(value.String("hello world"), prop, "name") {
    Error(msg) -> msg |> should.equal("name exceeds maximum length of 5")
    Ok(_) -> should.fail()
  }
}

pub fn validate_pattern_passes_test() {
  let prop = types.Property(
    type_: "string",
    required: True,
    format: None,
    ref: None,
    refs: None,
    items: None,
    max_length: None,
    min_length: None,
    max_graphemes: None,
    min_graphemes: None,
    pattern: Some("^[a-z]+$"),
  )

  input.validate(value.String("hello"), prop, "name")
  |> should.be_ok()
}

pub fn validate_pattern_fails_test() {
  let prop = types.Property(
    type_: "string",
    required: True,
    format: None,
    ref: None,
    refs: None,
    items: None,
    max_length: None,
    min_length: None,
    max_graphemes: None,
    min_graphemes: None,
    pattern: Some("^[a-z]+$"),
  )

  case input.validate(value.String("Hello123"), prop, "name") {
    Error(msg) -> msg |> should.equal("name does not match required pattern")
    Ok(_) -> should.fail()
  }
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`
Expected: FAIL with "module lexicon_graphql/validation/input not found"

**Step 3: Write minimal implementation**

```gleam
// src/lexicon_graphql/validation/input.gleam
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/regex
import gleam/result
import gleam/string
import lexicon_graphql/types
import swell/value

/// Validate an input value against property constraints
pub fn validate(
  input: value.Value,
  property: types.Property,
  field_name: String,
) -> Result(value.Value, String) {
  case input {
    value.String(s) -> validate_string(s, property, field_name)
    value.Null -> Ok(value.Null)
    _ -> Ok(input)  // Non-string types pass through
  }
}

fn validate_string(
  s: String,
  property: types.Property,
  field_name: String,
) -> Result(value.Value, String) {
  // Check max_length
  use _ <- result.try(validate_max_length(s, property.max_length, field_name))

  // Check min_length
  use _ <- result.try(validate_min_length(s, property.min_length, field_name))

  // Check pattern
  use _ <- result.try(validate_pattern(s, property.pattern, field_name))

  Ok(value.String(s))
}

fn validate_max_length(
  s: String,
  max: Option(Int),
  field_name: String,
) -> Result(Nil, String) {
  case max {
    None -> Ok(Nil)
    Some(max_len) -> {
      case string.length(s) > max_len {
        True -> Error(field_name <> " exceeds maximum length of " <> int.to_string(max_len))
        False -> Ok(Nil)
      }
    }
  }
}

fn validate_min_length(
  s: String,
  min: Option(Int),
  field_name: String,
) -> Result(Nil, String) {
  case min {
    None -> Ok(Nil)
    Some(min_len) -> {
      case string.length(s) < min_len {
        True -> Error(field_name <> " must be at least " <> int.to_string(min_len) <> " characters")
        False -> Ok(Nil)
      }
    }
  }
}

fn validate_pattern(
  s: String,
  pattern: Option(String),
  field_name: String,
) -> Result(Nil, String) {
  case pattern {
    None -> Ok(Nil)
    Some(pattern_str) -> {
      case regex.from_string(pattern_str) {
        Error(_) -> Ok(Nil)  // Invalid pattern, skip validation
        Ok(re) -> {
          case regex.check(re, s) {
            True -> Ok(Nil)
            False -> Error(field_name <> " does not match required pattern")
          }
        }
      }
    }
  }
}
```

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql
git add src/lexicon_graphql/validation/input.gleam test/validation/input_test.gleam
git commit -m "feat(validation): add input constraint validation module"
```

---

## Part 4: Server-Level Rate Limiting

### Task 16: Create ETS Rate Limiter Module

**Files:**
- Create: `/Users/chadmiller/code/quickslice/server/src/rate_limiter.gleam`
- Test: `/Users/chadmiller/code/quickslice/server/test/rate_limiter_test.gleam`

**Step 1: Write the failing test**

```gleam
// test/rate_limiter_test.gleam
import gleeunit/should
import rate_limiter

pub fn check_rate_passes_under_limit_test() {
  let assert Ok(_) = rate_limiter.start()

  // First request should pass
  rate_limiter.check("192.168.1.1", 100)
  |> should.be_ok()

  rate_limiter.stop()
}

pub fn check_rate_fails_over_limit_test() {
  let assert Ok(_) = rate_limiter.start()

  // Make 101 requests - last one should fail
  let results = list.range(1, 101)
    |> list.map(fn(_) { rate_limiter.check("192.168.1.1", 100) })

  let last = list.last(results)
  case last {
    Ok(Error(rate_limiter.RateLimited(_, _))) -> Nil
    _ -> should.fail()
  }

  rate_limiter.stop()
}

pub fn check_rate_different_keys_independent_test() {
  let assert Ok(_) = rate_limiter.start()

  // IP 1 at limit
  list.range(1, 100) |> list.each(fn(_) { rate_limiter.check("ip1", 100) })

  // IP 2 should still pass
  rate_limiter.check("ip2", 100)
  |> should.be_ok()

  rate_limiter.stop()
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: FAIL with "module rate_limiter not found"

**Step 3: Write minimal implementation**

```gleam
// src/rate_limiter.gleam
import gleam/erlang/atom
import gleam/erlang/process
import gleam/int
import gleam/result

/// Rate limiter error
pub type RateLimitError {
  RateLimited(count: Int, limit: Int)
  NotStarted
}

/// ETS table name
const table_name = "rate_limiter"

/// Start the rate limiter (create ETS table)
pub fn start() -> Result(Nil, String) {
  case create_ets_table() {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(e)
  }
}

/// Stop the rate limiter (delete ETS table)
pub fn stop() -> Result(Nil, String) {
  case delete_ets_table() {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(e)
  }
}

/// Check if request is within rate limit
/// Uses sliding window counter with 1-minute buckets
pub fn check(key: String, limit: Int) -> Result(Nil, RateLimitError) {
  let now = current_minute()
  let count = increment_and_get(key, now)

  case count > limit {
    True -> Error(RateLimited(count, limit))
    False -> Ok(Nil)
  }
}

/// Get current count for a key (for monitoring)
pub fn get_count(key: String) -> Int {
  let now = current_minute()
  get_counter(key, now)
}

// FFI for ETS operations
@external(erlang, "rate_limiter_ffi", "create_table")
fn create_ets_table() -> Result(Nil, String)

@external(erlang, "rate_limiter_ffi", "delete_table")
fn delete_ets_table() -> Result(Nil, String)

@external(erlang, "rate_limiter_ffi", "increment_counter")
fn increment_and_get(key: String, minute: Int) -> Int

@external(erlang, "rate_limiter_ffi", "get_counter")
fn get_counter(key: String, minute: Int) -> Int

@external(erlang, "rate_limiter_ffi", "current_minute")
fn current_minute() -> Int
```

Create FFI file:

```erlang
%% src/rate_limiter_ffi.erl
-module(rate_limiter_ffi).
-export([create_table/0, delete_table/0, increment_counter/2, get_counter/2, current_minute/0]).

create_table() ->
    try
        ets:new(rate_limiter, [named_table, public, set, {write_concurrency, true}]),
        {ok, nil}
    catch
        error:badarg -> {ok, nil}  % Table already exists
    end.

delete_table() ->
    try
        ets:delete(rate_limiter),
        {ok, nil}
    catch
        error:badarg -> {ok, nil}
    end.

increment_counter(Key, Minute) ->
    FullKey = {Key, Minute},
    try
        ets:update_counter(rate_limiter, FullKey, {2, 1}, {FullKey, 0})
    catch
        error:badarg -> 1
    end.

get_counter(Key, Minute) ->
    FullKey = {Key, Minute},
    case ets:lookup(rate_limiter, FullKey) of
        [{_, Count}] -> Count;
        [] -> 0
    end.

current_minute() ->
    erlang:system_time(seconds) div 60.
```

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
cd /Users/chadmiller/code/quickslice/server
git add src/rate_limiter.gleam src/rate_limiter_ffi.erl test/rate_limiter_test.gleam
git commit -m "feat(server): add ETS-backed rate limiter"
```

---

### Task 17: Integrate Rate Limiting into GraphQL Handler

**Files:**
- Modify: `/Users/chadmiller/code/quickslice/server/src/handlers/graphql.gleam`
- Test: Manual testing via curl

**Step 1: Identify integration point**

Read the current graphql handler to find where to add rate limiting check.

**Step 2: Add rate limiting check**

```gleam
// In graphql.gleam handler, add rate limit check before processing
import rate_limiter

pub fn handle(req: Request) -> Response {
  // Extract client IP
  let client_ip = get_client_ip(req)

  // Check rate limit (100 requests per minute)
  case rate_limiter.check(client_ip, 100) {
    Error(rate_limiter.RateLimited(count, limit)) -> {
      wisp.response(429)
      |> wisp.set_header("X-RateLimit-Limit", int.to_string(limit))
      |> wisp.set_header("X-RateLimit-Remaining", "0")
      |> wisp.set_header("Retry-After", "60")
      |> wisp.json_body(json.object([
        #("error", json.string("Rate limit exceeded")),
        #("limit", json.int(limit)),
        #("count", json.int(count)),
      ]))
    }
    Ok(_) -> {
      // Continue with normal handling
      handle_graphql_request(req)
    }
    Error(rate_limiter.NotStarted) -> {
      // Rate limiter not running, allow request
      handle_graphql_request(req)
    }
  }
}

fn get_client_ip(req: Request) -> String {
  // Check X-Forwarded-For header first (for proxied requests)
  case wisp.get_header(req, "x-forwarded-for") {
    Some(ips) -> {
      // Take first IP (client's original IP)
      case string.split(ips, ",") {
        [ip, ..] -> string.trim(ip)
        [] -> "unknown"
      }
    }
    None -> {
      // Fall back to direct connection IP
      case wisp.get_header(req, "x-real-ip") {
        Some(ip) -> ip
        None -> "unknown"
      }
    }
  }
}
```

**Step 3: Initialize rate limiter in server startup**

Add to server.gleam main function:

```gleam
// In server startup
let assert Ok(_) = rate_limiter.start()
```

**Step 4: Test manually**

```bash
# Start server
cd /Users/chadmiller/code/quickslice/server && gleam run

# In another terminal, make rapid requests
for i in {1..105}; do
  curl -s -o /dev/null -w "%{http_code}\n" http://localhost:4000/graphql -d '{"query":"{ __typename }"}'
done | tail -10
# Should see 429 responses after 100 requests
```

**Step 5: Commit**

```bash
cd /Users/chadmiller/code/quickslice/server
git add src/handlers/graphql.gleam src/server.gleam
git commit -m "feat(server): integrate rate limiting into GraphQL handler"
```

---

### Task 18: Add Query Size Limit Middleware

**Files:**
- Modify: `/Users/chadmiller/code/quickslice/server/src/handlers/graphql.gleam`

**Step 1: Add size check before parsing**

```gleam
// Add constant for max query size
const max_query_size = 100_000  // 100KB

pub fn handle(req: Request) -> Response {
  // Read body
  let body = wisp.read_body(req)

  // Check size limit
  case string.byte_size(body) > max_query_size {
    True -> {
      wisp.response(413)
      |> wisp.json_body(json.object([
        #("error", json.string("Query too large")),
        #("max_size", json.int(max_query_size)),
      ]))
    }
    False -> {
      // Continue with rate limiting and processing
      check_rate_limit_and_process(req, body)
    }
  }
}
```

**Step 2: Test manually**

```bash
# Create a large query file
python3 -c "print('{ ' + 'user { name } ' * 10000 + '}')" > /tmp/large_query.txt
wc -c /tmp/large_query.txt  # Should be > 100KB

# Try to send it
curl -X POST http://localhost:4000/graphql \
  -H "Content-Type: application/json" \
  -d @/tmp/large_query.txt
# Should get 413 response
```

**Step 3: Commit**

```bash
cd /Users/chadmiller/code/quickslice/server
git add src/handlers/graphql.gleam
git commit -m "feat(server): add query size limit (100KB)"
```

---

## Part 5: Integration and Final Testing

### Task 19: Update Quickslice to Use Security Config

**Files:**
- Modify: `/Users/chadmiller/code/quickslice/server/src/handlers/graphql.gleam`

**Step 1: Import and configure security**

```gleam
import swell/security

// Create security config for production use
fn get_security_config(ctx: schema.Context) -> security.SecurityConfig {
  security.SecurityConfig(
    max_depth: 10,
    max_cost: 10_000,
    default_list_cost: 25,
    timeout_ms: 30_000,
    allow_introspection: fn(c) {
      // Allow introspection for authenticated users only
      case c.data {
        option.Some(_) -> True  // Has auth context
        option.None -> False
      }
    },
    batch_policy: security.Limited(5),
  )
}
```

**Step 2: Use execute_with_security**

Replace `executor.execute` calls with `executor.execute_with_security`:

```gleam
let security_config = get_security_config(ctx)
case executor.execute_with_security(query, schema, ctx, security_config) {
  Ok(response) -> format_response(response)
  Error(err) -> format_error(err)
}
```

**Step 3: Commit**

```bash
cd /Users/chadmiller/code/quickslice/server
git add src/handlers/graphql.gleam
git commit -m "feat(server): enable GraphQL security validations in production"
```

---

### Task 20: Write Integration Tests

**Files:**
- Create: `/Users/chadmiller/code/quickslice/server/test/security_integration_test.gleam`

**Step 1: Write integration tests**

```gleam
// test/security_integration_test.gleam
import gleeunit/should
import gleam/http/request
import gleam/json
import server/test_helpers

pub fn deep_query_rejected_test() {
  let query = "{ a { b { c { d { e { f { g { h { i { j { k } } } } } } } } } } }"

  let response = test_helpers.graphql_request(query)

  response.status |> should.equal(200)
  // Check error in response body
  let assert Ok(body) = json.decode(response.body)
  body |> json.field("errors") |> should.be_some()
}

pub fn expensive_query_rejected_test() {
  let query = "{ users(first: 1000) { posts(first: 100) { comments(first: 100) { text } } } }"

  let response = test_helpers.graphql_request(query)

  response.status |> should.equal(200)
  let assert Ok(body) = json.decode(response.body)
  body |> json.field("errors") |> should.be_some()
}

pub fn introspection_requires_auth_test() {
  let query = "{ __schema { types { name } } }"

  // Without auth
  let response = test_helpers.graphql_request(query)
  let assert Ok(body) = json.decode(response.body)
  body |> json.field("errors") |> should.be_some()

  // With auth
  let auth_response = test_helpers.graphql_request_with_auth(query, "test-token")
  let assert Ok(auth_body) = json.decode(auth_response.body)
  auth_body |> json.field("data") |> should.be_some()
}

pub fn rate_limiting_works_test() {
  // Make 101 requests quickly
  let responses = list.range(1, 101)
    |> list.map(fn(_) { test_helpers.graphql_request("{ __typename }") })

  // At least one should be rate limited
  let rate_limited = list.any(responses, fn(r) { r.status == 429 })
  rate_limited |> should.be_true()
}
```

**Step 2: Run tests**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: PASS

**Step 3: Commit**

```bash
cd /Users/chadmiller/code/quickslice/server
git add test/security_integration_test.gleam
git commit -m "test(server): add GraphQL security integration tests"
```

---

### Task 21: Update Documentation

**Files:**
- Create: `/Users/chadmiller/code/swell/docs/security.md`
- Create: `/Users/chadmiller/code/quickslice/lexicon_graphql/docs/validation.md`

**Step 1: Write swell security docs**

```markdown
# Swell Security Configuration

Swell provides built-in security features to protect your GraphQL API.

## Quick Start

```gleam
import swell/executor
import swell/security

// Use production defaults
let config = security.production_config()

// Or customize
let config = security.SecurityConfig(
  max_depth: 10,
  max_cost: 10_000,
  default_list_cost: 25,
  timeout_ms: 30_000,
  allow_introspection: fn(ctx) { ctx.user != None },
  batch_policy: security.Limited(5),
)

// Execute with security
executor.execute_with_security(query, schema, ctx, config)
```

## Configuration Options

| Option | Default (dev) | Default (prod) | Description |
|--------|---------------|----------------|-------------|
| max_depth | 10 | 10 | Maximum nesting depth |
| max_cost | 10,000 | 5,000 | Maximum query cost |
| default_list_cost | 25 | 25 | Cost multiplier for unbounded lists |
| timeout_ms | 30,000 | 15,000 | Execution timeout |
| allow_introspection | Always | Never | Callback for introspection access |
| batch_policy | Limited(10) | Limited(5) | Batch operation limit |

## Error Handling

Security errors are returned as strings with corresponding codes:

- `QUERY_TOO_DEEP` - Query exceeds max_depth
- `QUERY_TOO_EXPENSIVE` - Query exceeds max_cost
- `EXECUTION_TIMEOUT` - Query timed out
- `INTROSPECTION_DISABLED` - Introspection not allowed
- `BATCHING_DISABLED` - Batching not allowed
- `BATCH_TOO_LARGE` - Too many operations in batch
```

**Step 2: Commit documentation**

```bash
cd /Users/chadmiller/code/swell
git add docs/security.md
git commit -m "docs: add security configuration guide"
```

---

## Summary

This plan implements comprehensive GraphQL security hardening across three layers:

1. **Swell (executor)**: Query depth, cost analysis, introspection control, batching limits, timeouts
2. **lexicon_graphql (schema)**: Custom scalars (AtUri, Did, Handle, DateTime), constraint validation
3. **Quickslice server**: Rate limiting (ETS-backed), query size limits

All changes are backwards-compatible with sensible defaults. Existing code works unchanged; stricter settings can be enabled for production.
