# Tutorial: Build Statusphere with Quickslice

Let's build Statusphere, an app where users share their current status as an emoji. This is the same app from the [AT Protocol docs](https://atproto.com/guides/applications), but using Quickslice as the AppView.

Along the way, we'll show what you'd write manually versus what Quickslice handles automatically.

**Try it live:** A working example is running at [StackBlitz](https://stackblitz.com/edit/stackblitz-starters-g3uwhweu?file=index.html), connected to a slice at [xyzstatusphere.slices.network](https://xyzstatusphere.slices.network) with the `xyz.statusphere.status` lexicon.

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

Importing this Lexicon into Quickslice triggers three automatic steps:

1. **Jetstream registration**: Quickslice tracks `xyz.statusphere.status` records from the network
2. **Database schema**: Quickslice creates a normalized table with proper columns and indexes
3. **GraphQL types**: Quickslice generates query, mutation, and subscription types

| Without Quickslice | With Quickslice |
|---|---|
| Write Jetstream connection code | Import your Lexicon |
| Filter events for your collection | `xyz.statusphere.status` |
| Validate incoming records | |
| Design database schema | Quickslice handles the rest. |
| Write ingestion logic | |

## Step 2: Querying Status Records

Query indexed records with GraphQL. Quickslice generates a query for each Lexicon type using Relay-style connections:

```graphql
query GetStatuses {
  xyzStatusphereStatus(
    first: 20
    sortBy: [{ field: createdAt, direction: DESC }]
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

The `edges` and `nodes` pattern comes from [Relay](https://relay.dev/graphql/connections.htm), a GraphQL pagination specification. Each `edge` contains a `node` (the record) and a `cursor` for pagination.

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

Here Quickslice shines. Every status record has a `did` field identifying its author. In Bluesky, profile information lives in `app.bsky.actor.profile` records. Join directly from a status to its author's profile:

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

The `appBskyActorProfileByDid` field is a **DID join**. It follows the `did` on the status record to find the profile authored by that identity.

Quickslice:
- Collects DIDs from the status records
- Batches them into a single database query (DataLoader pattern)
- Joins profile data efficiently

| Without Quickslice | With Quickslice |
|---|---|
| Collect DIDs from status records | Add join to your query: |
| Batch resolve DIDs to profiles | |
| Handle N+1 query problem | `appBskyActorProfileByDid { displayName }` |
| Write batching logic | |
| Join data in API response | |

### Other Join Types

Quickslice also supports:

- **Forward joins**: Follow a URI or strong ref to another record
- **Reverse joins**: Find all records that reference a given record

See the [Joins Guide](guides/joins.md) for complete documentation.

## Step 4: Writing a Status (Mutations)

To set a user's status, call a mutation:

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

Quickslice:

1. **Writes to the user's PDS**: Creates the record in their personal data repository
2. **Indexes optimistically**: The record appears in queries immediately, before Jetstream confirmation
3. **Handles OAuth**: Uses the authenticated session to sign the write

| Without Quickslice | With Quickslice |
|---|---|
| Get OAuth session/agent | Call the mutation: |
| Construct record with $type | |
| Call putRecord XRPC on the PDS | `createXyzStatusphereStatus(input: { status: "👍" })` |
| Optimistically update local DB | |
| Handle errors | |

## Step 5: Authentication

Quickslice bridges AT Protocol OAuth. Your frontend initiates login; Quickslice manages the authorization flow:

1. User enters their handle (e.g., `alice.bsky.social`)
2. Your app redirects to Quickslice's OAuth endpoint
3. Quickslice redirects to the user's PDS for authorization
4. User approves the app
5. PDS redirects back to Quickslice with an auth code
6. Quickslice exchanges the code for tokens and establishes a session

For authenticated queries and mutations, include auth headers. The exact headers depend on your OAuth flow (DPoP or Bearer token). See the [Authentication Guide](guides/authentication.md) for details.

## Step 6: Deploying to Railway

Deploy quickly with Railway:

1. Click the deploy button in the [Quickstart Guide](guides/deployment.md)
2. Generate an OAuth signing key with `goat key generate -t p256`
3. Paste the key into the `OAUTH_SIGNING_KEY` environment variable
4. Generate a domain and redeploy
5. Create your admin account by logging in
6. Upload your Lexicons

See [Deployment Guide](guides/deployment.md) for detailed instructions.

## What Quickslice Handled

Quickslice handled:

- **Jetstream connection**: firehose connection, event filtering, reconnection
- **Record validation**: schema checking against Lexicons
- **Database schema**: tables, migrations, indexes
- **Query API**: filtering, sorting, pagination endpoints
- **Batching**: efficient related-record resolution
- **Optimistic updates**: indexing before Jetstream confirmation
- **OAuth flow**: token exchange, session management, DPoP proofs

Focus on your application logic; Quickslice handles infrastructure.

## Next Steps

- [Queries Guide](guides/queries.md): Filtering, sorting, and pagination
- [Joins Guide](guides/joins.md): Forward, reverse, and DID joins
- [Mutations Guide](guides/mutations.md): Creating, updating, and deleting records
- [Authentication Guide](guides/authentication.md): Setting up OAuth
- [Deployment Guide](guides/deployment.md): Production configuration
