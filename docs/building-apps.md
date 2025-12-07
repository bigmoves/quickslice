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

## 2. Implement the OAuth Flow

Public clients use PKCE (Proof Key for Code Exchange) to secure the OAuth flow without a client secret.

### Overview

The flow works like this:

1. Generate a random code verifier and its SHA-256 hash (challenge)
2. Redirect user to `/oauth/authorize` with the challenge
3. User authenticates with their PDS
4. Handle callback with authorization code
5. Exchange code + verifier for access token

### PKCE Helpers

Add these utility functions to your app:

```javascript
function base64UrlEncode(buffer) {
  const bytes = new Uint8Array(buffer);
  let binary = "";
  for (let i = 0; i < bytes.length; i++) {
    binary += String.fromCharCode(bytes[i]);
  }
  return btoa(binary)
    .replace(/\+/g, "-")
    .replace(/\//g, "_")
    .replace(/=+$/, "");
}

async function generateCodeVerifier() {
  const randomBytes = new Uint8Array(32);
  crypto.getRandomValues(randomBytes);
  return base64UrlEncode(randomBytes);
}

async function generateCodeChallenge(verifier) {
  const encoder = new TextEncoder();
  const data = encoder.encode(verifier);
  const hash = await crypto.subtle.digest("SHA-256", data);
  return base64UrlEncode(hash);
}

function generateState() {
  const randomBytes = new Uint8Array(16);
  crypto.getRandomValues(randomBytes);
  return base64UrlEncode(randomBytes);
}
```

### Initiating Login

When a user clicks "Login", generate the PKCE values and redirect:

```javascript
async function initiateLogin(handle) {
  const codeVerifier = await generateCodeVerifier();
  const codeChallenge = await generateCodeChallenge(codeVerifier);
  const state = generateState();

  // Store for callback verification
  sessionStorage.setItem("code_verifier", codeVerifier);
  sessionStorage.setItem("oauth_state", state);

  const params = new URLSearchParams({
    client_id: "YOUR_CLIENT_ID",
    redirect_uri: window.location.origin,
    response_type: "code",
    code_challenge: codeChallenge,
    code_challenge_method: "S256",
    state: state,
    login_hint: handle,  // User's AT Protocol handle
  });

  window.location.href = `https://yourapp.slices.network/oauth/authorize?${params}`;
}
```

### Handling the Callback

After authentication, the user returns to your redirect URI with `code` and `state` parameters:

```javascript
async function handleOAuthCallback() {
  const params = new URLSearchParams(window.location.search);
  const code = params.get("code");
  const state = params.get("state");

  // Verify state to prevent CSRF
  const storedState = sessionStorage.getItem("oauth_state");
  if (state !== storedState) {
    throw new Error("State mismatch — possible CSRF attack");
  }

  // Exchange code for tokens
  const codeVerifier = sessionStorage.getItem("code_verifier");

  const response = await fetch("https://yourapp.slices.network/oauth/token", {
    method: "POST",
    headers: { "Content-Type": "application/x-www-form-urlencoded" },
    body: new URLSearchParams({
      grant_type: "authorization_code",
      code: code,
      redirect_uri: window.location.origin,
      client_id: "YOUR_CLIENT_ID",
      code_verifier: codeVerifier,
    }),
  });

  const tokens = await response.json();

  // Store access token
  sessionStorage.setItem("access_token", tokens.access_token);

  // Clean up
  sessionStorage.removeItem("code_verifier");
  sessionStorage.removeItem("oauth_state");

  // Remove code from URL
  window.history.replaceState({}, document.title, window.location.pathname);
}
```

### Token Storage

The example above uses `sessionStorage` for simplicity. This works for demos but has limitations:

- **sessionStorage**: Cleared when tab closes, vulnerable to XSS
- **localStorage**: Persists across sessions, also vulnerable to XSS

For production apps, consider more secure approaches:

- **Silent refresh with Web Workers**: Store tokens in memory, use a worker to refresh silently (similar to [Auth0's approach](https://auth0.com/docs/secure/tokens/refresh-tokens/refresh-token-rotation))
- **Backend-for-Frontend (BFF)**: Use a confidential client on your server, store tokens server-side, use HTTP-only cookies for sessions

## 3. Query the GraphQL API

### Unauthenticated Queries

All queries are public and require no authentication:

```javascript
async function graphqlQuery(query, variables = {}) {
  const response = await fetch("https://yourapp.slices.network/graphql", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ query, variables }),
  });

  const result = await response.json();

  if (result.errors?.length > 0) {
    throw new Error(result.errors[0].message);
  }

  return result.data;
}
```

Example — fetch recent statuses:

```javascript
const data = await graphqlQuery(`
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

### Authenticated Requests

Mutations and the `viewer` query require an access token. Add the `Authorization` header:

```javascript
async function authenticatedQuery(query, variables = {}) {
  const token = sessionStorage.getItem("access_token");

  const response = await fetch("https://yourapp.slices.network/graphql", {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      "Authorization": `Bearer ${token}`,
    },
    body: JSON.stringify({ query, variables }),
  });

  const result = await response.json();
  return result.data;
}
```

### The Viewer Query

Get the authenticated user's identity:

```javascript
const data = await authenticatedQuery(`
  query {
    viewer {
      did
      handle
    }
  }
`);

console.log(data.viewer.handle); // "alice.bsky.social"
```

Join to profile data from other collections:

```javascript
const data = await authenticatedQuery(`
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
```

## 4. Create Records with Mutations

Mutations require authentication and create records in the user's AT Protocol repository:

```javascript
const data = await authenticatedQuery(`
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

## 5. Confidential Clients

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

## Next Steps

- [Authentication Reference](/authentication) — OAuth endpoints and token details
- [Queries](/queries) — Filtering, sorting, and pagination
- [Mutations](/mutations) — Creating, updating, and deleting records
- [Subscriptions](/subscriptions) — Real-time updates via WebSocket
