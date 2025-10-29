# QuickSlice

GraphQL API for AT Protocol records with automatic schema generation from Lexicon definitions.

## Warning: Work in Progress

This project is currently under active development. APIs are subject to change without notice. Do not use in production.

## Overview

QuickSlice provides a GraphQL interface to query AT Protocol records stored in a SQLite database. It automatically generates GraphQL schemas from Lexicon definitions and implements the Relay Cursor Connections specification for pagination.

## Features

- Automatic GraphQL schema generation from AT Protocol Lexicons
- Relay-compliant cursor-based pagination
- Sorting support with custom sort fields per record type
- Database-backed record storage and querying
- Real-time ingestion via Jetstream

## GraphQL API

### Query Structure

All record types are exposed as connection fields on the root Query type. Each connection supports:

- Forward pagination (first/after)
- Backward pagination (last/before)
- Custom sorting (sortBy)

### Example Query

```graphql
query {
  xyzStatusphereStatus(
    first: 10
    after: "cursor_value"
    sortBy: [
      { field: "createdAt", direction: DESC }
    ]
  ) {
    edges {
      node {
        uri
        cid
        did
        collection
        indexedAt
        status
        createdAt
      }
      cursor
    }
    pageInfo {
      hasNextPage
      hasPreviousPage
      startCursor
      endCursor
    }
  }
}
```

### Connection Type

All record queries return a Connection type following the Relay specification:

```graphql
type XyzStatusphereStatusConnection {
  edges: [XyzStatusphereStatusEdge!]!
  pageInfo: PageInfo!
}

type XyzStatusphereStatusEdge {
  node: XyzStatusphereStatus!
  cursor: String!
}

type PageInfo {
  hasNextPage: Boolean!
  hasPreviousPage: Boolean!
  startCursor: String
  endCursor: String
}
```

### Record Type

Each Lexicon generates a GraphQL type with standard AT Protocol fields plus custom fields from the Lexicon:

```graphql
type XyzStatusphereStatus {
  # Standard AT Protocol fields
  uri: String!
  cid: String!
  did: String!
  collection: String!
  indexedAt: String!

  # Custom fields from Lexicon
  status: String
  createdAt: String
}
```

### Pagination Arguments

All connection fields support these arguments:

```graphql
type Query {
  xyzStatusphereStatus(
    # Forward pagination
    first: Int
    after: String

    # Backward pagination
    last: Int
    before: String

    # Sorting
    sortBy: [SortFieldInput!]
  ): XyzStatusphereStatusConnection!
}
```

### Sorting

Each record type has a custom sort field enum with all available sortable fields:

```graphql
enum XyzStatusphereStatusSortField {
  uri
  cid
  did
  collection
  indexedAt
  status
  createdAt
}

input SortFieldInput {
  field: XyzStatusphereStatusSortField!
  direction: SortDirection!
}

enum SortDirection {
  ASC
  DESC
}
```

#### Sort Behavior

- Default sort: `indexed_at DESC` (most recent first)
- Multiple sort fields supported
- NULL and invalid values always appear last (NULLS LAST)
- Date fields validated using SQLite datetime() function

#### Sort Examples

Sort by creation date, newest first:
```graphql
{
  xyzStatusphereStatus(sortBy: [{ field: "createdAt", direction: DESC }]) {
    edges {
      node { status createdAt }
    }
  }
}
```

Sort by multiple fields:
```graphql
{
  xyzStatusphereStatus(
    sortBy: [
      { field: "did", direction: ASC }
      { field: "createdAt", direction: DESC }
    ]
  ) {
    edges {
      node { did status createdAt }
    }
  }
}
```

### Pagination Examples

#### Forward Pagination

Get first 10 records:
```graphql
{
  xyzStatusphereStatus(first: 10) {
    edges {
      node { status }
      cursor
    }
    pageInfo {
      hasNextPage
      endCursor
    }
  }
}
```

Get next page:
```graphql
{
  xyzStatusphereStatus(first: 10, after: "previous_end_cursor") {
    edges {
      node { status }
      cursor
    }
    pageInfo {
      hasNextPage
      endCursor
    }
  }
}
```

#### Backward Pagination

Get last 10 records:
```graphql
{
  xyzStatusphereStatus(last: 10) {
    edges {
      node { status }
      cursor
    }
    pageInfo {
      hasPreviousPage
      startCursor
    }
  }
}
```

Get previous page:
```graphql
{
  xyzStatusphereStatus(last: 10, before: "previous_start_cursor") {
    edges {
      node { status }
      cursor
    }
    pageInfo {
      hasPreviousPage
      startCursor
    }
  }
}
```

### Introspection

The API supports full GraphQL introspection for schema discovery:

```graphql
{
  __schema {
    queryType {
      fields {
        name
        type {
          name
          kind
        }
      }
    }
  }
}
```

Query available sort fields for a record type:
```graphql
{
  __type(name: "XyzStatusphereStatusSortField") {
    enumValues {
      name
      description
    }
  }
}
```

## Architecture

### Packages

- `graphql/` - Core GraphQL implementation with Relay Connection support
- `lexicon_graphql/` - Automatic schema generation from Lexicons
- `server/` - Database layer and record storage

### Database Schema

Records are stored in SQLite with the following structure:

```sql
CREATE TABLE record (
  uri TEXT PRIMARY KEY,
  cid TEXT NOT NULL,
  did TEXT NOT NULL,
  collection TEXT NOT NULL,
  json TEXT NOT NULL,
  indexed_at TEXT NOT NULL
);
```

The `json` field contains the full record value as a JSON string.

### Schema Generation

1. Load Lexicon definitions from JSON files
2. Parse Lexicon to extract record types and properties
3. Generate GraphQL types with fields for each property
4. Create connection types for pagination
5. Build custom sort field enums per record type
6. Register resolvers that query the database

## Development

### Running Tests

```sh
# Test GraphQL package
cd graphql
gleam test

# Test Lexicon GraphQL package
cd lexicon_graphql
gleam test

# Test server package
cd server
gleam test
```

### Building

```sh
gleam build
```

### Running the Server

```sh
cd server
gleam run
```

## Implementation Details

### Cursor Format

Cursors are opaque base64-encoded strings containing:
- Sort field values
- Primary key (uri)

Clients should treat cursors as opaque and not attempt to decode or construct them.

### NULL Handling

- NULL values always sorted last regardless of sort direction
- Invalid date strings (e.g., "wowzers", "0001-01-01T00:00:00Z") treated as NULL
- SQLite datetime() function used to validate date fields

### Type Safety

- Sort fields validated at query time against available fields
- Each record type has its own sort field enum
- Prevents typos and invalid field references

## Future Enhancements

- Mutations for creating/updating records
- Subscriptions for real-time updates
- Full-text search support
- Filtering by field values
- Aggregation queries (count, sum, etc.)
- Support for more Lexicon features (unions, references)
