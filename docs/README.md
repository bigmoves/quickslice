# quickslice

An AppView (backend service) for AT Protocol apps.

## What is quickslice?

Quickslice gives you a GraphQL API and OAuth authentication for AT Protocol data. Point it at any Lexicon schemas, and it automatically generates a complete backend: queries, mutations, joins, subscriptions, and user authentication via AT Protocol identity.

Instead of building your own data layer and auth system, you get:
- **GraphQL API** generated from your Lexicon definitions
- **OAuth proxy** that authenticates users via their AT Protocol identity (Bluesky, etc.)
- **Real-time ingestion** of records from the AT Protocol network

## Core Concepts

### GraphQL API

Your Lexicon schemas become GraphQL types automatically. Query records with filtering, sorting, and pagination:

```graphql
query {
  xyzStatusphereStatus(first: 10, sortBy: [{ field: createdAt, direction: DESC }]) {
    edges {
      node {
        uri
        did
        status
        createdAt
      }
    }
  }
}
```

### OAuth Authentication

Users authenticate with their AT Protocol identity (e.g., their Bluesky account). Quickslice acts as an OAuth proxy—your app never handles credentials directly. Authenticated users can create, update, and delete their own records.

### Records

Records are the data units in AT Protocol, defined by Lexicon schemas. Quickslice ingests records from the network and stores them locally for fast querying. Mutations write records back to the user's repository (PDS).

## Getting Started

**Endpoints:**
- `/graphql` - GraphQL API
- `/graphiql` - Interactive GraphQL explorer
- `/oauth/authorize` - OAuth authorization endpoint

Queries are public. Mutations require authentication via Bearer token.
