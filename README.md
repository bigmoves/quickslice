<div align="center">
  <img src="slices_and_lucy.png" alt="Slices and Lucy" width="201">
</div>

# quickslice

GraphQL API for AT Protocol records with automatic schema generation from Lexicon definitions.

Work in progress. APIs subject to change.

## Features

- Automatic GraphQL schema generation from AT Protocol Lexicons
- Relay-compliant cursor-based pagination
- Real-time ingestion via Jetstream
- Backfill support for historical data
- SQLite storage

## Example

The status lexicon (`xyz.statusphere.status`):

```json
{
  "lexicon": 1,
  "id": "xyz.statusphere.status",
  "defs": {
    "main": {
      "type": "record",
      "key": "tid",
      "record": {
        "type": "object",
        "required": ["status", "createdAt"],
        "properties": {
          "status": {
            "type": "string",
            "minLength": 1,
            "maxGraphemes": 1,
            "maxLength": 32
          },
          "createdAt": { "type": "string", "format": "datetime" }
        }
      }
    }
  }
}
```

Querying the status records:

```graphql
query {
  xyzStatusphereStatus(
    first: 50
    sortBy: [
      { field: createdAt, direction: DESC }
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

## Quick Start

### Docker (Fastest)

Run Quickslice locally with Docker:

```bash
docker compose up --build
```

Open http://localhost:8080 and login with your Bluesky handle.

For PostgreSQL instead of SQLite:

```bash
docker compose -f docker-compose.postgres.yml up --build
```

### Native Development

For development without Docker:

**Prerequisites:**
- [Gleam](https://gleam.run/getting-started/installing/) v1.13+
- [dbmate](https://github.com/amacneil/dbmate) for migrations
- Node.js 18+ (for client build)

**Setup:**

```bash
# Server
cd server
cp .env.example .env
make db-setup-sqlite
gleam run

# Client (rebuild after changes)
cd client
npm install
gleam run -m lustre/dev build
```

See [server/README.md](server/README.md) for detailed configuration.

## Documentation

- [Queries](docs/guides/queries.md)
- [Joins](docs/guides/joins.md)
- [Mutations](docs/guides/mutations.md)
- [Subscriptions](docs/reference/subscriptions.md)
- [Blobs](docs/reference/blobs.md)
- [Variables](docs/reference/variables.md)
- [Deployment](docs/guides/deployment.md)

## Structure

- `server/` - Main server with database layer, GraphQL API, and Jetstream ingestion
- `lexicon_graphql/` - GraphQL schema generation from AT Protocol Lexicons
- `atproto_car/` - CAR (Content Addressable aRchive) file parsing for backfills
- `client/` - Web-based GraphQL playground and admin UI
- `quickslice-client-js/` - JavaScript client library
- `www/` - Documentation website
