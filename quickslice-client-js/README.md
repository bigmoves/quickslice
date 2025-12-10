# quickslice-client-js

Browser client SDK for Quickslice applications.

## Installation

### Via CDN (jsDelivr)

```html
<script src="https://cdn.jsdelivr.net/gh/bigmoves/quickslice@main/quickslice-client-js/dist/quickslice-client.min.js"></script>
```

### Via npm

```bash
npm install quickslice-client-js
```

## Usage

```javascript
// Initialize client
const client = await QuicksliceClient.createQuicksliceClient({
  server: 'https://api.example.com',
  clientId: 'client_abc123',
  redirectUri: 'https://yourapp.com/oauth/callback', // optional
});

// Handle OAuth callback (on page load)
if (window.location.search.includes('code=')) {
  await client.handleRedirectCallback();
  window.history.replaceState({}, '', '/');
}

// Check auth state
if (await client.isAuthenticated()) {
  const user = client.getUser();
  console.log(`Logged in as ${user.did}`);

  // Fetch richer profile with your own query
  const profile = await client.query(`query { viewer { handle } }`);
}

// Login
document.getElementById('login').onclick = async () => {
  await client.loginWithRedirect({ handle: 'alice.bsky.social' });
};

// Logout
document.getElementById('logout').onclick = () => {
  client.logout();
};

// GraphQL queries
const data = await client.query(`
  query {
    viewer { did handle }
  }
`);

// GraphQL mutations
await client.mutate(`
  mutation CreatePost($text: String!) {
    createPost(input: { text: $text }) { id }
  }
`, { text: 'Hello world!' });

// Public queries (no auth)
const publicData = await client.publicQuery(`
  query { posts(first: 10) { edges { node { text } } } }
`);
```

## API

### `createQuicksliceClient(options)`

Factory function to create and initialize a client.

Options:
- `server` (required): Quickslice server URL
- `clientId` (required): Pre-registered client ID
- `redirectUri` (optional): OAuth callback URL. Defaults to current page URL if omitted. Useful when you have a dedicated callback route.

### `QuicksliceClient`

#### Auth Methods

- `loginWithRedirect(options?)` - Start OAuth login flow
- `handleRedirectCallback()` - Process OAuth callback
- `logout(options?)` - Clear session and reload
- `isAuthenticated()` - Check if logged in
- `getUser()` - Get current user's DID (sync, returns `{ did }`)
- `getAccessToken()` - Get access token (auto-refreshes)

#### GraphQL Methods

- `query(query, variables?)` - Execute authenticated query
- `mutate(mutation, variables?)` - Execute authenticated mutation
- `publicQuery(query, variables?)` - Execute unauthenticated query

## Security

- PKCE for OAuth authorization code flow
- DPoP (Demonstration of Proof-of-Possession) token binding
- Non-extractable P-256 keys stored in IndexedDB
- Multi-tab token refresh coordination
- CSRF protection via state parameter

## License

MIT
