# QuickSlice

GraphQL API for AT Protocol records with automatic schema generation from Lexicon definitions.

Work in progress. APIs subject to change.

## Features

- Automatic GraphQL schema generation from AT Protocol Lexicons
- Relay-compliant cursor-based pagination
- Real-time ingestion via Jetstream
- Backfill support for historical data
- SQLite storage

## Example

```graphql
query {
  xyzStatusphereStatus(
    first: 50
    sortBy: [
      { field: "createdAt", direction: DESC }
    ]
    where: {
      status: { contains: "👍" }
    }
  ) {
    edges {
      node {
        uri
        did
        status
        createdAt
        appBskyActorProfileByDid {
          displayName
          avatar {
            url
          }
          description
        }
      }
      cursor
    }
    pageInfo {
      hasNextPage
      endCursor
    }
  }
}
```

## Documentation

- [Queries](docs/queries.md)
- [Joins](docs/joins.md)
- [Mutations](docs/mutations.md)
- [Subscriptions](docs/subscriptions.md)
- [Blobs](docs/blobs.md)
- [Variables](docs/variables.md)

## Architecture

- `graphql/` - Core GraphQL implementation
- `lexicon_graphql/` - Schema generation from Lexicons
- `server/` - Database layer and record storage
