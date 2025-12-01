# Authentication

Queries are public and require no authentication. Mutations require a valid access token. The `viewer` query returns the authenticated user's information, or `null` if not authenticated.

## OAuth Flow

Quickslice uses OAuth 2.0 with PKCE for authentication. Users authenticate via their AT Protocol identity (Bluesky account).

### Prerequisites

You need an OAuth client ID. Create one in the quickslice admin UI at `/settings` or via the GraphQL API.

### 1. Generate PKCE Values

Generate a code verifier and challenge for the PKCE flow:

```javascript
function base64UrlEncode(buffer) {
  const bytes = new Uint8Array(buffer);
  let binary = '';
  for (let i = 0; i < bytes.length; i++) {
    binary += String.fromCharCode(bytes[i]);
  }
  return btoa(binary)
    .replace(/\+/g, '-')
    .replace(/\//g, '_')
    .replace(/=+$/, '');
}

async function generateCodeVerifier() {
  const randomBytes = new Uint8Array(32);
  crypto.getRandomValues(randomBytes);
  return base64UrlEncode(randomBytes);
}

async function generateCodeChallenge(verifier) {
  const encoder = new TextEncoder();
  const data = encoder.encode(verifier);
  const hash = await crypto.subtle.digest('SHA-256', data);
  return base64UrlEncode(hash);
}

function generateState() {
  const randomBytes = new Uint8Array(16);
  crypto.getRandomValues(randomBytes);
  return base64UrlEncode(randomBytes);
}
```

### 2. Redirect to Authorization

Build the authorization URL and redirect the user:

```javascript
const codeVerifier = await generateCodeVerifier();
const codeChallenge = await generateCodeChallenge(codeVerifier);
const state = generateState();

// Store these for the callback
sessionStorage.setItem('code_verifier', codeVerifier);
sessionStorage.setItem('oauth_state', state);

const params = new URLSearchParams({
  client_id: 'your-client-id',
  redirect_uri: 'http://localhost:3000/callback',
  response_type: 'code',
  code_challenge: codeChallenge,
  code_challenge_method: 'S256',
  state: state,
  login_hint: 'alice.bsky.social'  // User's handle
});

window.location.href = `http://localhost:8080/oauth/authorize?${params}`;
```

### 3. Handle the Callback

After the user authenticates, they're redirected back with a `code` parameter:

```javascript
const params = new URLSearchParams(window.location.search);
const code = params.get('code');
const state = params.get('state');

// Verify state matches
if (state !== sessionStorage.getItem('oauth_state')) {
  throw new Error('State mismatch - possible CSRF attack');
}

const codeVerifier = sessionStorage.getItem('code_verifier');
```

### 4. Exchange Code for Tokens

Exchange the authorization code for access and refresh tokens:

```javascript
const response = await fetch('http://localhost:8080/oauth/token', {
  method: 'POST',
  headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
  body: new URLSearchParams({
    grant_type: 'authorization_code',
    code: code,
    redirect_uri: 'http://localhost:3000/callback',
    client_id: 'your-client-id',
    code_verifier: codeVerifier
  })
});

const tokens = await response.json();
// tokens.access_token - Use this for authenticated requests
// tokens.refresh_token - Use to get new access tokens
// tokens.sub - The user's DID
```

## Using Bearer Tokens

Include the access token in the `Authorization` header:

```javascript
const response = await fetch('http://localhost:8080/graphql', {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json',
    'Authorization': `Bearer ${accessToken}`
  },
  body: JSON.stringify({
    query: '{ viewer { did handle } }'
  })
});
```

Or with curl:

```bash
curl -X POST http://localhost:8080/graphql \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer YOUR_ACCESS_TOKEN" \
  -d '{"query": "{ viewer { did handle } }"}'
```

## Viewer Query

The `viewer` query returns information about the authenticated user:

```graphql
query {
  viewer {
    did
    handle
    appBskyActorProfileByDid {
      displayName
      description
      avatar { url }
    }
  }
}
```

### Fields

- `did` - The user's decentralized identifier (e.g., `did:plc:abc123...`)
- `handle` - The user's handle (e.g., `alice.bsky.social`)
- `appBskyActorProfileByDid` - The user's profile record, joined by DID

### Behavior

- Returns the user object when authenticated
- Returns `null` when not authenticated (no error)
- Useful for confirming authentication and fetching user info in one request

### Example Response

```json
{
  "data": {
    "viewer": {
      "did": "did:plc:abc123xyz",
      "handle": "alice.bsky.social",
      "appBskyActorProfileByDid": {
        "displayName": "Alice",
        "description": "Hello world!",
        "avatar": {
          "url": "https://cdn.bsky.app/..."
        }
      }
    }
  }
}
```

## Complete Example

Here's an end-to-end example showing login, fetching the viewer, and creating a record:

```javascript
// === Configuration ===
const GRAPHQL_URL = 'http://localhost:8080/graphql';
const OAUTH_AUTHORIZE_URL = 'http://localhost:8080/oauth/authorize';
const OAUTH_TOKEN_URL = 'http://localhost:8080/oauth/token';
const CLIENT_ID = 'your-client-id';
const REDIRECT_URI = window.location.origin + '/callback';

// === GraphQL helper ===
async function graphql(query, variables = {}, token = null) {
  const headers = { 'Content-Type': 'application/json' };
  if (token) {
    headers['Authorization'] = `Bearer ${token}`;
  }

  const response = await fetch(GRAPHQL_URL, {
    method: 'POST',
    headers,
    body: JSON.stringify({ query, variables })
  });

  const result = await response.json();
  if (result.errors?.length) {
    throw new Error(result.errors[0].message);
  }
  return result.data;
}

// === Login ===
async function login(handle) {
  const codeVerifier = await generateCodeVerifier();
  const codeChallenge = await generateCodeChallenge(codeVerifier);
  const state = generateState();

  sessionStorage.setItem('code_verifier', codeVerifier);
  sessionStorage.setItem('oauth_state', state);

  const params = new URLSearchParams({
    client_id: CLIENT_ID,
    redirect_uri: REDIRECT_URI,
    response_type: 'code',
    code_challenge: codeChallenge,
    code_challenge_method: 'S256',
    state: state,
    login_hint: handle
  });

  window.location.href = `${OAUTH_AUTHORIZE_URL}?${params}`;
}

// === Handle OAuth callback ===
async function handleCallback() {
  const params = new URLSearchParams(window.location.search);
  const code = params.get('code');
  const state = params.get('state');

  if (!code) return null;

  if (state !== sessionStorage.getItem('oauth_state')) {
    throw new Error('State mismatch');
  }

  const response = await fetch(OAUTH_TOKEN_URL, {
    method: 'POST',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    body: new URLSearchParams({
      grant_type: 'authorization_code',
      code,
      redirect_uri: REDIRECT_URI,
      client_id: CLIENT_ID,
      code_verifier: sessionStorage.getItem('code_verifier')
    })
  });

  const tokens = await response.json();
  sessionStorage.setItem('access_token', tokens.access_token);

  // Clean up URL
  window.history.replaceState({}, '', window.location.pathname);

  return tokens.access_token;
}

// === Fetch current user ===
async function getViewer(token) {
  const data = await graphql(`
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
  `, {}, token);

  return data.viewer;
}

// === Create a record (example: status) ===
async function createStatus(token, emoji) {
  const data = await graphql(`
    mutation CreateStatus($status: String!, $createdAt: DateTime!) {
      createXyzStatusphereStatus(input: { status: $status, createdAt: $createdAt }) {
        uri
        status
      }
    }
  `, {
    status: emoji,
    createdAt: new Date().toISOString()
  }, token);

  return data.createXyzStatusphereStatus;
}
```

## See Also

- [examples/01-statusphere-html/](../examples/01-statusphere-html/) - Complete working example
- [Mutations](./mutations.md) - Creating, updating, and deleting records
- [Queries](./queries.md) - Fetching data without authentication
