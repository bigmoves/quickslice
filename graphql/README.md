# GraphQL

A GraphQL implementation in Gleam providing query parsing, execution, and introspection support.

## Features

### Core GraphQL Functionality
- **Query Parsing**: GraphQL query language support including:
  - Field selection
  - Arguments
  - Aliases
  - Fragments (inline and named)

- **Schema Definition**: Type-safe schema builder with:
  - Object types
  - Scalar types (String, Int, Float, Boolean, ID)
  - List types
  - Non-null types
  - Field resolvers with context-based data access

- **Query Execution**: Execution engine with:
  - Recursive field resolution
  - Nested object support
  - List handling with proper field filtering
  - Fragment spreading and inline fragments
  - Error collection and reporting
  - Path tracking for error context

- **Introspection**: GraphQL introspection support
  - Schema introspection queries
  - Type introspection
  - Field introspection
  - Compatible with GraphiQL and other GraphQL clients

## Architecture

The package is organized into several modules:

- `graphql/lexer.gleam` - Tokenizes GraphQL query strings
- `graphql/parser.gleam` - Parses tokens into an AST
- `graphql/schema.gleam` - Schema definition and type system
- `graphql/executor.gleam` - Query execution engine
- `graphql/value.gleam` - GraphQL value types
- `graphql/introspection.gleam` - Schema introspection

## Usage

### Defining a Schema

```gleam
import graphql/schema
import graphql/value

// Define a simple User type
let user_type = schema.object_type(
  "User",
  "A user in the system",
  [
    schema.field("id", schema.id_type(), "User ID", fn(ctx) {
      // Extract id from context
      case ctx.data {
        option.Some(value.Object(fields)) -> {
          case list.key_find(fields, "id") {
            Ok(id_val) -> Ok(id_val)
            Error(_) -> Ok(value.Null)
          }
        }
        _ -> Ok(value.Null)
      }
    }),
    schema.field("name", schema.string_type(), "User name", fn(ctx) {
      // Extract name from context
      // ... resolver implementation
    }),
  ]
)

// Define root query type
let query_type = schema.object_type(
  "Query",
  "Root query type",
  [
    schema.field("user", user_type, "Get a user", fn(_ctx) {
      Ok(value.Object([
        #("id", value.String("1")),
        #("name", value.String("Alice")),
      ]))
    }),
  ]
)

// Create schema
let my_schema = schema.new(query_type)
```

### Executing Queries

```gleam
import graphql/executor
import graphql/schema

let query = "{ user { id name } }"
let result = executor.execute(query, my_schema, schema.Context(None))

case result {
  Ok(executor.Response(data: data, errors: [])) -> {
    // Query succeeded
    io.println("Data: " <> string.inspect(data))
  }
  Ok(executor.Response(data: data, errors: errors)) -> {
    // Query executed with errors
    io.println("Data: " <> string.inspect(data))
    io.println("Errors: " <> string.inspect(errors))
  }
  Error(err) -> {
    // Query failed to parse or execute
    io.println("Error: " <> err)
  }
}
```

## Test Coverage

The package includes tests covering:
- Parsing
- Execution
- Schema
- Introspection
- Edge cases

## Known Limitations

- Mutations not yet implemented
- Subscriptions not yet implemented
- Directives not yet implemented
- Variables not yet implemented
- Custom scalar types limited to built-in types

## Dependencies

- `gleam_stdlib` >= 0.44.0

## Development

Run tests:
```sh
cd graphql
gleam test
```

Build:
```sh
gleam build
```
