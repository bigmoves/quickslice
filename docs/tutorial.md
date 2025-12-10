# Tutorial: Build Statusphere with Quickslice

Let's build Statusphere, an app where users share their current status as an emoji. This is the same app from the [AT Protocol docs](https://atproto.com/guides/applications), but using Quickslice as the AppView.

Along the way, we'll show what you'd write manually versus what Quickslice handles automatically.

## What We're Building

Statusphere lets users:
- Log in with their AT Protocol identity
- Set their status as an emoji
- See a feed of everyone's statuses with profile information

By the end of this tutorial, you'll understand how Quickslice eliminates the boilerplate of building an AppView.

## Step 1: Project Setup and Importing Lexicons

Every AT Protocol app starts with Lexicons. Here's the Lexicon for a status record:

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

When you import this Lexicon into Quickslice, several things happen automatically:

1. **Jetstream registration**: Quickslice starts tracking `xyz.statusphere.status` records from the network
2. **Database schema**: A normalized table is created with proper columns and indexes
3. **GraphQL types**: Query, mutation, and subscription types are generated

| Without Quickslice | With Quickslice |
|---|---|
| Write Jetstream connection code | Import your Lexicon |
| Filter events for your collection | `xyz.statusphere.status` |
| Validate incoming records | |
| Design database schema | Quickslice handles the rest. |
| Write ingestion logic | |

## Step 2: Querying Status Records

Once records are indexed, you can query them with GraphQL. Quickslice generates a query for each Lexicon type using Relay-style connections:

```graphql
query GetStatuses {
  xyzStatusphereStatus(
    first: 20
    sortBy: [{ field: "createdAt", direction: DESC }]
  ) {
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

The `edges` and `nodes` pattern comes from [Relay](https://relay.dev/graphql/connections.htm), a GraphQL specification for paginated data. Each `edge` contains a `node` (the actual record) and a `cursor` for pagination.

You can filter with `where` clauses:

```graphql
query RecentStatuses {
  xyzStatusphereStatus(
    first: 10
    where: { status: { eq: "👍" } }
  ) {
    edges {
      node {
        did
        status
      }
    }
  }
}
```

| Without Quickslice | With Quickslice |
|---|---|
| Design query API | Query is auto-generated: |
| Write database queries | |
| Handle pagination logic | `xyzStatusphereStatus { edges { node { status } } }` |
| Build filtering and sorting | |

## Step 3: Joining Profile Data

This is where Quickslice really shines. Every status record has a `did` field identifying its author. We want to show the author's display name alongside their status.

In Bluesky, profile information lives in `app.bsky.actor.profile` records. With Quickslice, you can join directly from a status to its author's profile:

```graphql
query StatusesWithProfiles {
  xyzStatusphereStatus(first: 20) {
    edges {
      node {
        status
        createdAt
        appBskyActorProfileByDid {
          displayName
          avatar { url }
        }
      }
    }
  }
}
```

The `appBskyActorProfileByDid` field is a **DID join**. It follows the `did` on the status record to find the corresponding profile record authored by the same identity.

Behind the scenes, Quickslice:
- Collects all DIDs from the status records
- Batches them into a single database query (DataLoader pattern)
- Joins the profile data efficiently

| Without Quickslice | With Quickslice |
|---|---|
| Collect DIDs from status records | Add join to your query: |
| Batch resolve DIDs to profiles | |
| Handle N+1 query problem | `appBskyActorProfileByDid { displayName }` |
| Write batching logic | |
| Join data in API response | |

### Other Join Types

DID joins are just one option. Quickslice also supports:

- **Forward joins**: Follow a URI or strong ref to another record
- **Reverse joins**: Find all records that reference a given record

See the [Joins Guide](guides/joins.md) for complete documentation.

## Step 4: Writing a Status (Mutations)

When a user wants to set their status, your app calls a mutation:

```graphql
mutation CreateStatus($status: String!, $createdAt: DateTime!) {
  createXyzStatusphereStatus(
    input: { status: $status, createdAt: $createdAt }
  ) {
    uri
    status
    createdAt
  }
}
```

When this mutation runs, Quickslice:

1. **Writes to the user's PDS**: The record is created in their personal data repository
2. **Optimistically indexes locally**: The record appears in queries immediately, without waiting for Jetstream
3. **Handles OAuth**: The authenticated session is used to sign the write

| Without Quickslice | With Quickslice |
|---|---|
| Get OAuth session/agent | Call the mutation: |
| Construct record with $type | |
| Call putRecord XRPC on the PDS | `createXyzStatusphereStatus(input: { status: "👍" })` |
| Optimistically update local DB | |
| Handle errors | |

## Step 5: Authentication

Quickslice bridges AT Protocol OAuth. Your frontend initiates login, and Quickslice manages the authorization flow:

1. User enters their handle (e.g., `alice.bsky.social`)
2. Your app redirects to Quickslice's OAuth endpoint
3. Quickslice redirects to the user's PDS for authorization
4. User approves the app
5. PDS redirects back to Quickslice with an auth code
6. Quickslice exchanges the code for tokens and establishes a session

For authenticated queries and mutations, include the appropriate auth headers. The exact headers depend on which OAuth flow you're using (DPoP or Bearer token). See the [Authentication Guide](guides/authentication.md) for setup details.

## Step 6: Deploying to Railway

The fastest way to deploy is with Railway:

1. Click the deploy button in the [Quickstart Guide](guides/deployment.md)
2. Generate an OAuth signing key with `goat key generate -t p256`
3. Paste the key into the `OAUTH_SIGNING_KEY` environment variable
4. Generate a domain and redeploy
5. Create your admin account by logging in
6. Upload your Lexicons

See [Deployment Guide](guides/deployment.md) for detailed instructions.

## What You Didn't Write

By using Quickslice, you skipped writing:

- **Jetstream connection**: Connecting to the firehose, filtering events, handling reconnection
- **Record validation**: Checking incoming records against Lexicon schemas
- **Database schema**: Designing tables, migrations, indexes
- **Query API**: Endpoints for filtering, sorting, and pagination
- **Batching**: Efficient resolution of related records
- **Optimistic updates**: Indexing records before Jetstream confirmation
- **OAuth flow**: Token exchange, session management, DPoP proofs

With Quickslice, you focus on your application logic instead of infrastructure.

## Next Steps

- [Queries Guide](guides/queries.md): Filtering, sorting, and pagination
- [Joins Guide](guides/joins.md): Forward, reverse, and DID joins
- [Mutations Guide](guides/mutations.md): Creating, updating, and deleting records
- [Authentication Guide](guides/authentication.md): Setting up OAuth
- [Deployment Guide](guides/deployment.md): Production configuration
