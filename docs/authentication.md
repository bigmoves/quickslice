# Authentication

Queries are public and require no authentication. Mutations require a valid access token. The `viewer` query returns the authenticated user's information, or `null` if not authenticated.

## How it works

Quickslice acts as an OAuth proxy between your app and the user's PDS (Personal Data Server). When a user logs in:

1. Your app redirects to quickslice's OAuth authorize endpoint with the user's handle
2. Quickslice resolves the user's DID and locates their PDS
3. The user authenticates with their PDS
4. Quickslice issues your app an access token

Your app never handles AT Protocol credentials directly. The access token authorizes mutations that write records to the user's repository on their PDS.

## OAuth Endpoints

- `GET /oauth/authorize` - Start the OAuth flow (redirect users here)
- `POST /oauth/token` - Exchange authorization code for tokens
- `GET /.well-known/oauth-authorization-server` - Server metadata
- `GET /oauth/oauth-client-metadata.json` - Client metadata (used by AT Protocol for client discovery)

### Prerequisites

You need an OAuth client ID. Create one in the quickslice admin UI at `/settings`.

## Using Tokens

Include the access token in the `Authorization` header:

```bash
curl -X POST https://your-quickslice.com/graphql \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer YOUR_ACCESS_TOKEN" \
  -d '{"query": "{ viewer { did handle } }"}'
```

## Viewer Query

The `viewer` query returns the authenticated user:

```graphql
query {
  viewer {
    did
    handle
  }
}
```

- `did` - The user's decentralized identifier (e.g., `did:plc:abc123...`)
- `handle` - The user's handle (e.g., `alice.bsky.social`)
- Returns `null` when not authenticated (no error)

You can use the viewer's DID to reverse join to any record type in your system. For example, if you have a profile lexicon:

```graphql
query {
  viewer {
    did
    handle
    myAppProfileByDid {
      displayName
      bio
    }
  }
}
```
