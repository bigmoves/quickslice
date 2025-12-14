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

Navigate to the admin settings page at `http://localhost:8080/admin/settings` and register a new OAuth client with:

- **Name:** Statusphere HTML Example
- **Token Endpoint Auth Method:** Public
- **Redirect URIs:** `http://127.0.0.1:3000/`

**Important:** Set the redirect URI to match where you'll serve this HTML file.

### 3. Serve the HTML File

```bash
npx http-server . -p 3000
# Open http://127.0.0.1:3000
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

- Uses the local `quickslice-client-js` SDK from `../../quickslice-client-js`
- Tokens are stored in `localStorage` (shared across tabs, persisted across sessions)
- Uses DPoP (Demonstrating Proof-of-Possession) for token binding
- Uses PKCE for secure OAuth flow
- CSP header restricts script sources and connections

## Troubleshooting

**"Failed to load statuses"**
- Ensure quickslice server is running at localhost:8080

**OAuth redirect fails**
- Verify redirect URI matches exactly in OAuth client config
- Check that the client ID is correct

**Can't post status**
- Ensure you're logged in (session may have expired)
- Check browser console for error details
