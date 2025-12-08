# Building Apps

You have a Quickslice instance running. Now build an app that authenticates users and interacts with its GraphQL API.

## Prerequisites

- **Running Quickslice instance** - Follow the [Quickstart](/quickstart) if you haven't deployed one yet
- **Your instance URL** - e.g., `https://yourapp.slices.network`

> **Live Example**
> See the complete working app on [StackBlitz](https://stackblitz.com/edit/stackblitz-starters-g3uwhweu) or browse the [source code](https://github.com/quickslice/quickslice/tree/main/examples/01-statusphere).

## How Authentication Works

Quickslice acts as an OAuth proxy between your app and users' Personal Data Servers (PDS):

1. Your app redirects users to Quickslice's authorize endpoint
2. Quickslice resolves the user's DID and locates their PDS
3. The user authenticates with their PDS (e.g., Bluesky)
4. Quickslice issues your app an access token

Your app never handles AT Protocol credentials directly. The access token authorizes GraphQL mutations that write records to the user's repository.

### Security: PKCE and DPoP

The Quickslice client SDK implements two security mechanisms that protect browser-based apps:

**PKCE (Proof Key for Code Exchange)** prevents authorization code interception attacks. Before redirecting to login, the SDK generates a random secret (code verifier) and sends only its hash (code challenge) to the server. When exchanging the authorization code for tokens, the SDK proves it initiated the request by providing the original verifier.

**DPoP (Demonstrating Proof-of-Possession)** binds tokens to a cryptographic key stored in your browser. Each request includes a signed proof that the token holder possesses the private key. Even if an attacker steals your access token, they can't use it without the key.

Together, these mechanisms provide security comparable to confidential clients—without requiring server-side secrets.

## 1. Create an OAuth Client

Before implementing authentication, register an OAuth client in your Quickslice instance.

### Public vs Confidential Clients

Choose based on where your code runs:

| Type | Use Case | Secret |
|------|----------|--------|
| **Public** | Browser apps (SPAs), mobile apps | No secret — can't be kept secure on client |
| **Confidential** | Server-side apps, backend services | Has secret — stored securely on server |

Most browser-based apps use **Public** clients. If you're building a server-rendered app or using a Backend-for-Frontend pattern, use **Confidential**.

### Register Your Client

1. Open your Quickslice instance and navigate to **Settings**
2. Scroll to **OAuth Clients** and click **Register New Client**
3. Fill in the form:
   - **Client Name**: Your app's name (e.g., "My Status App")
   - **Client Type**: Select "Public" for browser apps
   - **Redirect URIs**: Where users return after auth (e.g., `http://localhost:3000`)
   - **Scope**: Leave as `atproto transition:generic`
4. Click **Create**
5. Copy the **Client ID** — you'll need this in your app

## 2. Install the SDK

The Quickslice client SDK handles OAuth, PKCE, DPoP, token refresh, and GraphQL requests.

### npm

```bash
npm install quickslice-client
```

```javascript
import { createQuicksliceClient } from 'quickslice-client';
```

### CDN

```html
<script src="https://unpkg.com/quickslice-client/dist/quickslice-client.min.js"></script>
```

The SDK exposes `QuicksliceClient` and `createQuicksliceClient` globally.

## 3. Implement Authentication

### Initialize the Client

Create a client instance with your server URL and OAuth client ID:

```javascript
const client = await createQuicksliceClient({
  server: "https://yourapp.slices.network",
  clientId: "YOUR_CLIENT_ID",
});
```

### Login

Redirect the user to authenticate with their AT Protocol identity:

```javascript
await client.loginWithRedirect({
  handle: "alice.bsky.social",  // User's handle
});
```

### Handle the Callback

After authentication, the user returns to your app. Handle the callback to complete login:

```javascript
// Check if this is an OAuth callback
if (window.location.search.includes("code=")) {
  await client.handleRedirectCallback();
}
```

The SDK automatically exchanges the authorization code for tokens using PKCE and DPoP, stores them securely, and cleans up the URL.

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

## 4. Query the GraphQL API

### Public Queries

Use `publicQuery()` for unauthenticated requests. All queries are public by default:

```javascript
const data = await client.publicQuery(`
  query {
    xyzStatusphereStatus(
      first: 20
      sortBy: [{ field: "createdAt", direction: DESC }]
    ) {
      edges {
        node {
          status
          createdAt
          did
        }
      }
    }
  }
`);
```

### Authenticated Queries

Use `query()` for requests that require authentication. The SDK automatically includes the access token with DPoP proof:

```javascript
const data = await client.query(`
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
`);

console.log(data.viewer.handle); // "alice.bsky.social"
```

## 5. Create Records with Mutations

Use `mutate()` to create records in the user's AT Protocol repository:

```javascript
const data = await client.mutate(`
  mutation CreateStatus($status: String!, $createdAt: DateTime!) {
    createXyzStatusphereStatus(
      input: { status: $status, createdAt: $createdAt }
    ) {
      uri
      status
      createdAt
    }
  }
`, {
  status: "🎉",
  createdAt: new Date().toISOString(),
});
```

The mutation returns the created record, including its `uri` (the AT Protocol record identifier).

## 6. Confidential Clients

Use a confidential client when your app has a secure backend that can store secrets.

### When to Use

- Server-rendered applications (Node.js, Python, etc.)
- Backend-for-Frontend (BFF) pattern
- Mobile apps with a backend proxy

### Differences from Public Clients

1. Select **Confidential** when registering the client
2. You receive both a **Client ID** and **Client Secret**
3. Include the secret in the token exchange

### Token Exchange

The only code difference is adding `client_secret` to the token request:

```javascript
const response = await fetch("https://yourapp.slices.network/oauth/token", {
  method: "POST",
  headers: { "Content-Type": "application/x-www-form-urlencoded" },
  body: new URLSearchParams({
    grant_type: "authorization_code",
    code: code,
    redirect_uri: redirectUri,
    client_id: "YOUR_CLIENT_ID",
    client_secret: "YOUR_CLIENT_SECRET",  // Added for confidential clients
    code_verifier: codeVerifier,
  }),
});
```

> **Security Note**: Never expose the client secret in browser code. This request must happen on your server.
