# Lexicon GraphQL

Automatic GraphQL schema generation from AT Protocol Lexicon definitions. This package bridges AT Protocol's Lexicon schema system with GraphQL, enabling automatic GraphQL API generation for ATProto records.

## Features

### Automatic Schema Generation
- **Lexicon Parsing**: Parses AT Protocol lexicon JSON files
- **Type Mapping**: Automatically maps Lexicon types to GraphQL types
- **Database Integration**: Generates GraphQL schemas that query database records
- **Field Resolvers**: Auto-generated resolvers for lexicon properties

### Supported Lexicon Types
- `string` → GraphQL String
- `integer` → GraphQL Int
- `boolean` → GraphQL Boolean
- `datetime` → GraphQL String (ISO 8601 format)
- Objects and nested properties
- Arrays/lists

### Database Schema Builder
Generates GraphQL schemas for database-stored ATProto records with:
- Automatic field extraction from database records
- Support for nested lexicon properties
- Proper JSON parsing and field access
- Metadata fields (uri, cid, did, collection, indexedAt)

## Architecture

The package consists of several modules:

- `lexicon_graphql/lexicon_parser.gleam` - Parses lexicon JSON files
- `lexicon_graphql/type_mapper.gleam` - Maps lexicon types to GraphQL types
- `lexicon_graphql/schema_builder.gleam` - Builds GraphQL schemas from lexicons
- `lexicon_graphql/db_schema_builder.gleam` - Database-specific schema generation
- `lexicon_graphql/ref_resolver.gleam` - Resolves lexicon references
- `lexicon_graphql/nsid.gleam` - NSID (Namespaced Identifier) utilities

## Usage

### Creating a Schema from a Lexicon

```gleam
import lexicon_graphql
import lexicon_graphql/db_schema_builder
import swell/schema

// Parse a lexicon file
let lexicon_json = "{ \"lexicon\": 1, \"id\": \"xyz.statusphere.status\", ... }"
let assert Ok(lexicon) = lexicon_graphql.parse_lexicon(lexicon_json)

// Generate GraphQL schema for database queries
let collection_name = "xyz.statusphere.status"
let graphql_type = db_schema_builder.build_db_record_type(
  collection_name,
  lexicon,
  get_records_fn
)
```

### Example: Status Record Schema

For a lexicon like:
```json
{
  "lexicon": 1,
  "id": "xyz.statusphere.status",
  "defs": {
    "main": {
      "type": "record",
      "record": {
        "type": "object",
        "properties": {
          "status": { "type": "string" },
          "createdAt": { "type": "string", "format": "datetime" }
        }
      }
    }
  }
}
```

The package automatically generates a GraphQL type with:
```graphql
type XyzStatusphereStatus {
  uri: String!
  cid: String!
  did: String!
  collection: String!
  indexedAt: String!
  status: String
  createdAt: String
}
```

## Database Integration

### Record Structure

Records in the database have the following structure:
- `uri`: AT URI of the record
- `cid`: Content identifier
- `did`: DID of the record owner
- `collection`: Lexicon collection name
- `json`: **JSON string** containing the record value
- `indexed_at`: When the record was indexed

### JSON Storage Format

**IMPORTANT**: The `json` field MUST be stored as a proper JSON string, not Gleam/Erlang term syntax.

CORRECT: `{"$type":"xyz.statusphere.status","status":"..","createdAt":"2025-10-28T20:00:00Z"}`

INCORRECT: `dict.from_list([#("status", ".."), #("createdAt", "2025-10-28T20:00:00Z")])`

### Data Conversion

When storing records from Jetstream or backfill operations, always use proper JSON encoding:

```gleam
import gleam/dynamic.{type Dynamic}

// Convert Dynamic (Erlang term) to JSON string
fn dynamic_to_json(value: Dynamic) -> String {
  let iolist = json_encode(value)
  iolist_to_string(iolist)
}

@external(erlang, "json", "encode")
fn json_encode(value: Dynamic) -> Dynamic
```

**Do NOT use** `string.inspect(value)` as it produces Gleam syntax, not JSON.

## Field Resolution

The `db_schema_builder` module provides helper functions for extracting fields from context:

```gleam
// Get a top-level field from context
get_field_from_context(ctx, "fieldName")

// Get a nested field from context
get_nested_field_from_context(ctx, "parent", "child")
```

These functions handle:
- Safe field access with Result types
- Null handling
- Type checking
- Nested object traversal

## Testing

The package uses the `swell` package's test suite to verify schema generation and execution.

## Dependencies

- `gleam_stdlib` >= 0.44.0
- `gleam_json` >= 3.0.0
- `swell` >= 1.0.0

## Integration Example

```gleam
import lexicon_graphql/db_schema_builder
import database
import swell/schema
import swell/executor

// Load lexicon
let lexicon_json = load_lexicon("priv/lexicons/xyz/statusphere/status.json")
let assert Ok(lexicon) = lexicon_graphql.parse_lexicon(lexicon_json)

// Define record fetcher
fn get_records() {
  database.get_records_by_collection(db, "xyz.statusphere.status")
  |> result.map(fn(records) {
    list.map(records, record_to_graphql_value)
  })
}

// Build GraphQL type
let status_type = db_schema_builder.build_db_record_type(
  "xyz.statusphere.status",
  lexicon,
  get_records
)

// Create query type
let query_type = schema.object_type("Query", "Root query", [
  schema.field(
    "statuses",
    schema.list_type(status_type),
    "Get all statuses",
    fn(_) { get_records() }
  )
])

// Create and use schema
let graphql_schema = schema.new(query_type)
executor.execute("{ statuses { status } }", graphql_schema, schema.Context(None))
```

## NSID Support

The package includes utilities for working with NSIDs (Namespaced Identifiers):

```gleam
import lexicon_graphql/nsid

// Convert NSID to GraphQL type name
nsid.to_graphql_name("xyz.statusphere.status")
// → "XyzStatusphereStatus"

// Convert NSID to field name
nsid.to_graphql_field_name("xyz.statusphere.status")
// → "xyzStatusphereStatus"
```

## Development

Run tests:
```sh
cd lexicon_graphql
gleam test
```

Build:
```sh
gleam build
```

## Future Enhancements

- Support for lexicon references ($ref)
- Union types
- Custom validation rules
- Mutation support for creating/updating records
- Subscription support for real-time updates
