# Statusphere HTML Example

A single-file HTML example demonstrating quickslice's GraphQL API with OAuth authentication.

## Features

- OAuth PKCE authentication flow
- Post status updates (emoji)
- View recent statuses from the network
- Display user profiles

## Prerequisites

1. Quickslice server running at `http://localhost:8080`
2. A registered OAuth client

## Setup

### 1. Start Quickslice

```bash
cd /path/to/quickslice
make run
```

### 2. Register an OAuth Client

Via GraphQL mutation:

```graphql
mutation {
  createOAuthClient(input: {
    name: "Statusphere HTML Example"
    redirectUris: ["http://localhost:3000/index.html"]
    tokenEndpointAuthMethod: "none"
  }) {
    clientId
  }
}
```

Or use the quickslice admin UI.

**Important:** Set the redirect URI to match where you'll serve this HTML file.

### 3. Serve the HTML File

Option A - Python:
```bash
cd examples/01-statusphere-html
python -m http.server 3000
# Open http://localhost:3000/index.html
```

Option B - Node.js:
```bash
npx serve examples/01-statusphere-html -p 3000
# Open http://localhost:3000/index.html
```

### 4. Login

1. Enter your OAuth Client ID
2. Enter your Bluesky handle (e.g., `you.bsky.social`)
3. Click "Login with Bluesky"
4. Authorize the app on your AT Protocol PDS
5. You'll be redirected back and logged in

## Usage

- Click any emoji to set your status
- View recent statuses from the network
- Click "Logout" to clear your session

## Security Notes

- Tokens are stored in `sessionStorage` (cleared when tab closes)
- No external dependencies - all code is inline
- Uses PKCE for secure OAuth flow
- CSP header restricts connections to localhost:8080

## Troubleshooting

**"Failed to load statuses"**
- Ensure quickslice server is running at localhost:8080
- Check browser console for CORS errors

**OAuth redirect fails**
- Verify redirect URI matches exactly in OAuth client config
- Check that the client ID is correct

**Can't post status**
- Ensure you're logged in (session may have expired)
- Check browser console for error details
