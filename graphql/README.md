# GraphQL

A GraphQL implementation in Gleam providing query parsing, execution, and introspection support.

## Features

### Core GraphQL Functionality
- **Query Parsing**: GraphQL query language support including:
  - Field selection
  - Arguments
  - Aliases
  - Fragments (inline and named)
  - Mutations (named and anonymous)
  - Subscriptions (named and anonymous)

- **Schema Definition**: Type-safe schema builder with:
  - Object types
  - Input object types
  - Scalar types (String, Int, Float, Boolean, ID)
  - List types
  - Non-null types
  - Field resolvers with context-based data access
  - Mutation support with argument handling

- **Query Execution**: Execution engine with:
  - Recursive field resolution
  - Nested object support
  - List handling with proper field filtering
  - Fragment spreading and inline fragments
  - Error collection and reporting
  - Path tracking for error context

- **Mutation Execution**: Mutation engine with:
  - Named mutations (`mutation CreateUser { ... }`)
  - Anonymous mutations (`mutation { ... }`)
  - Input type validation
  - Context-based authentication and authorization
  - Error handling and reporting

- **Subscription Execution**: Subscription engine with:
  - Named subscriptions (`subscription OnMessage { ... }`)
  - Anonymous subscriptions (`subscription { ... }`)
  - Real-time event streaming via WebSocket
  - Field selection and nested queries
  - Context-based event data passing
  - Full support for joins and complex resolvers

- **Introspection**: Full GraphQL introspection support
  - Schema introspection queries
  - Type introspection (including mutation types)
  - Field introspection
  - Input type introspection
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

// Define input type for creating users
let create_user_input = schema.input_object_type(
  "CreateUserInput",
  "Input for creating a user",
  [
    schema.input_field("name", schema.non_null(schema.string_type()), "User name", option.None),
  ]
)

// Define mutation type
let mutation_type = schema.object_type(
  "Mutation",
  "Root mutation type",
  [
    schema.field_with_args(
      "createUser",
      user_type,
      "Create a new user",
      [schema.argument("input", schema.non_null(create_user_input), "User data", option.None)],
      fn(ctx) {
        // Extract input from arguments and create user
        case schema.get_argument(ctx, "input") {
          option.Some(input) -> {
            Ok(value.Object([
              #("id", value.String("2")),
              #("name", value.String("Bob")),
            ]))
          }
          option.None -> Error("Missing input argument")
        }
      }
    ),
  ]
)

// Create schema with mutations
let my_schema = schema.new(query_type)
  |> schema.with_mutation(mutation_type)
```

### Executing Queries

```gleam
import graphql/executor
import graphql/schema

let query = "{ user { id name } }"
let ctx = schema.context(option.None)
let result = executor.execute(query, my_schema, ctx)

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

### Executing Mutations

```gleam
import graphql/executor
import graphql/schema
import graphql/value

let mutation = "
  mutation {
    createUser(input: { name: \"Bob\" }) {
      id
      name
    }
  }
"

// Create context with optional data (e.g., auth token)
let ctx_data = value.Object([
  #("auth_token", value.String("some_token"))
])
let ctx = schema.context(option.Some(ctx_data))

let result = executor.execute(mutation, my_schema, ctx)

case result {
  Ok(executor.Response(data: data, errors: [])) -> {
    // Mutation succeeded
    io.println("Created user: " <> string.inspect(data))
  }
  Ok(executor.Response(data: data, errors: errors)) -> {
    // Mutation executed with errors
    io.println("Errors: " <> string.inspect(errors))
  }
  Error(err) -> {
    io.println("Error: " <> err)
  }
}
```

### Defining and Using Subscriptions

```gleam
import graphql/schema
import graphql/executor
import graphql/value

// Define a Message type
let message_type = schema.object_type(
  "Message",
  "A chat message",
  [
    schema.field("id", schema.id_type(), "Message ID", fn(ctx) {
      // Extract from context data
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
    schema.field("content", schema.string_type(), "Message content", fn(ctx) {
      case ctx.data {
        option.Some(value.Object(fields)) -> {
          case list.key_find(fields, "content") {
            Ok(content) -> Ok(content)
            Error(_) -> Ok(value.Null)
          }
        }
        _ -> Ok(value.Null)
      }
    }),
  ]
)

// Define subscription type
let subscription_type = schema.object_type(
  "Subscription",
  "Root subscription type",
  [
    schema.field(
      "messageAdded",
      message_type,
      "Subscribe to new messages",
      fn(ctx) {
        // For subscriptions, the event data is passed via ctx.data
        // Return it directly - the executor will handle field selection
        case ctx.data {
          option.Some(data) -> Ok(data)
          option.None -> Error("Subscription called without event data")
        }
      }
    ),
  ]
)

// Create schema with subscriptions
let my_schema = schema.schema_with_subscriptions(
  query_type,
  option.Some(mutation_type),
  option.Some(subscription_type)
)
```

### Executing Subscriptions

```gleam
import graphql/executor
import graphql/schema
import graphql/value

// Subscription query with field selection
let subscription = "
  subscription {
    messageAdded {
      id
      content
    }
  }
"

// When an event occurs, create context with the event data
let event_data = value.Object([
  #("id", value.String("msg_123")),
  #("content", value.String("Hello, world!")),
  #("timestamp", value.String("2024-01-01T00:00:00Z")),
])

let ctx = schema.context(option.Some(event_data))

// Execute the subscription query with the event data
let result = executor.execute(subscription, my_schema, ctx)

case result {
  Ok(executor.Response(data: data, errors: [])) -> {
    // Subscription resolved successfully
    // Only requested fields (id, content) will be in the response
    // timestamp is filtered out by field selection
    io.println("Event: " <> string.inspect(data))
  }
  Ok(executor.Response(data: data, errors: errors)) -> {
    io.println("Errors: " <> string.inspect(errors))
  }
  Error(err) -> {
    io.println("Error: " <> err)
  }
}
```

**Key Points for Subscriptions:**

1. **Event Data via Context**: Unlike queries/mutations, subscription field resolvers receive event data through `ctx.data` and should return it directly.

2. **Field Selection Works**: The executor automatically handles field selection - you pass the full event object, and only requested fields are returned.

3. **Nested Queries Supported**: Subscriptions support the same nested queries, joins, and complex resolvers as regular queries.

4. **WebSocket Integration**: In production, subscriptions are typically used with WebSocket connections where:
   - Client subscribes via `subscription { ... }`
   - Server listens for events (e.g., PubSub)
   - When events occur, execute the subscription query with event data
   - Send results back to client over WebSocket

## Test Coverage

The package includes comprehensive tests covering:
- Query parsing (queries, mutations, subscriptions)
- Query execution with field selection
- Mutation execution with input types
- Subscription execution with event data
- Schema definition and validation
- Introspection (queries, mutations, and subscription types)
- Input type handling
- Fragment support (inline and named)
- Error handling and edge cases
- Snapshot tests for parser validation

## Known Limitations

- Directives not yet implemented
- Variables not yet fully implemented
- Custom scalar types limited to built-in types
- Union types not yet fully implemented
- Interface types not yet implemented

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
