# Authentication

Quickslice acts as an OAuth proxy between your app and users' Personal Data Servers (PDS). Your app never handles AT Protocol credentials directly.

## How It Works

1. Your app redirects users to Quickslice's authorize endpoint
2. Quickslice resolves the user's DID and locates their PDS
3. The user authenticates with their PDS
4. Quickslice issues your app an access token

The access token authorizes mutations that write records to the user's repository.

## Setting Up OAuth

### Generate a Signing Key

Quickslice needs a private key to sign OAuth tokens. Generate one with `goat`:

```bash
brew install goat
goat key generate -t p256
```

Set the output as your `OAUTH_SIGNING_KEY` environment variable.

### Register an OAuth Client

1. Open your Quickslice instance and navigate to **Settings**
2. Scroll to **OAuth Clients** and click **Register New Client**
3. Fill in the form:
   - **Client Name**: Your app's name
   - **Client Type**: Public (browser apps) or Confidential (server apps)
   - **Redirect URIs**: Where users return after auth (e.g., `http://localhost:3000`)
   - **Scope**: Leave as `atproto transition:generic`
4. Copy the **Client ID**

### Public vs Confidential Clients

| Type | Use Case | Secret |
|------|----------|--------|
| **Public** | Browser apps, mobile apps | No secret (can't be kept secure on client) |
| **Confidential** | Server-side apps, backend services | Has secret (stored securely on server) |

## Using the Client SDK

The Quickslice client SDK handles OAuth, PKCE, DPoP, token refresh, and GraphQL requests.

### Install

```bash
npm install quickslice-client
```

Or via CDN:

```html
<script src="https://unpkg.com/quickslice-client/dist/quickslice-client.min.js"></script>
```

### Initialize

```javascript
import { createQuicksliceClient } from 'quickslice-client';

const client = await createQuicksliceClient({
  server: "https://yourapp.slices.network",
  clientId: "YOUR_CLIENT_ID",
});
```

### Login

```javascript
await client.loginWithRedirect({
  handle: "alice.bsky.social",
});
```

### Handle the Callback

After authentication, the user returns to your redirect URI:

```javascript
if (window.location.search.includes("code=")) {
  await client.handleRedirectCallback();
}
```

### Check Authentication State

```javascript
const isLoggedIn = await client.isAuthenticated();

if (isLoggedIn) {
  const user = client.getUser();
  console.log(user.did);  // "did:plc:abc123..."
}
```

### Logout

```javascript
await client.logout();
```

## Making Authenticated Requests

### With the SDK

The SDK handles authentication headers automatically:

```javascript
// Public query (no auth needed)
const data = await client.publicQuery(`
  query { xyzStatusphereStatus { edges { node { status } } } }
`);

// Authenticated query
const viewer = await client.query(`
  query { viewer { did handle } }
`);

// Mutation (requires auth)
const result = await client.mutate(`
  mutation { createXyzStatusphereStatus(input: { status: "🎉", createdAt: "${new Date().toISOString()}" }) { uri } }
`);
```

### Without the SDK

If you're not using the SDK, include the appropriate headers based on your OAuth flow:

**DPoP flow** (public clients):
```
Authorization: DPoP <access_token>
DPoP: <dpop_proof>
```

**Bearer token flow** (confidential clients):
```
Authorization: Bearer <access_token>
```

## The Viewer Query

The `viewer` query returns the authenticated user:

```graphql
query {
  viewer {
    did
    handle
    appBskyActorProfileByDid {
      displayName
      avatar { url }
    }
  }
}
```

Returns `null` when not authenticated (no error thrown).

## Security: PKCE and DPoP

The SDK implements two security mechanisms for browser apps:

**PKCE (Proof Key for Code Exchange)** prevents authorization code interception. Before redirecting, the SDK generates a random secret and sends only its hash to the server. When exchanging the code for tokens, the SDK proves it initiated the request.

**DPoP (Demonstrating Proof-of-Possession)** binds tokens to a cryptographic key in your browser. Each request includes a signed proof. Even if an attacker steals your access token, they can't use it without the key.

## OAuth Endpoints

- `GET /oauth/authorize` - Start the OAuth flow
- `POST /oauth/token` - Exchange authorization code for tokens
- `GET /.well-known/oauth-authorization-server` - Server metadata
- `GET /oauth/oauth-client-metadata.json` - Client metadata
