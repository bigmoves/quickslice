# Redirect URI Configuration Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Allow developers to pass a custom redirect URI when creating a QuicksliceClient, instead of always deriving it from the current page URL.

**Architecture:** Add optional `redirectUri` field to `QuicksliceClientOptions`. Store it during login initiation alongside PKCE values. Use stored value in callback to ensure exact match with authorize request.

**Tech Stack:** TypeScript, browser localStorage

---

### Task 1: Add redirectUri to Storage Keys

**Files:**
- Modify: `src/storage/keys.ts`

**Step 1: Add the new key**

Open `src/storage/keys.ts` and add `redirectUri` to the `STORAGE_KEYS` object:

```typescript
export const STORAGE_KEYS = {
  accessToken: 'quickslice_access_token',
  refreshToken: 'quickslice_refresh_token',
  tokenExpiry: 'quickslice_token_expiry',
  userDid: 'quickslice_user_did',
  codeVerifier: 'quickslice_code_verifier',
  oauthState: 'quickslice_oauth_state',
  clientId: 'quickslice_client_id',
  dpopKey: 'quickslice_dpop_key',
  redirectUri: 'quickslice_redirect_uri',  // ADD THIS LINE
};
```

**Step 2: Verify build**

Run: `cd /Users/chadmiller/code/quickslice/quickslice-client-js && npm run build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add src/storage/keys.ts
git commit -m "feat: add redirectUri to storage keys"
```

---

### Task 2: Update OAuth Functions to Accept and Use redirectUri

**Files:**
- Modify: `src/auth/oauth.ts`

**Step 1: Update LoginOptions interface**

Add `redirectUri` to the `LoginOptions` interface at the top of `oauth.ts`:

```typescript
export interface LoginOptions {
  handle?: string;
  redirectUri?: string;
}
```

**Step 2: Update initiateLogin to use provided redirectUri**

Modify the `initiateLogin` function to accept and store the redirect URI:

```typescript
export async function initiateLogin(
  authorizeUrl: string,
  clientId: string,
  options: LoginOptions = {}
): Promise<void> {
  const codeVerifier = generateCodeVerifier();
  const codeChallenge = await generateCodeChallenge(codeVerifier);
  const state = generateState();

  // Build redirect URI (use provided or derive from current page)
  const redirectUri = options.redirectUri || (window.location.origin + window.location.pathname);

  // Store for callback
  storage.set(STORAGE_KEYS.codeVerifier, codeVerifier);
  storage.set(STORAGE_KEYS.oauthState, state);
  storage.set(STORAGE_KEYS.clientId, clientId);
  storage.set(STORAGE_KEYS.redirectUri, redirectUri);

  // Build authorization URL
  const params = new URLSearchParams({
    client_id: clientId,
    redirect_uri: redirectUri,
    response_type: 'code',
    code_challenge: codeChallenge,
    code_challenge_method: 'S256',
    state: state,
  });

  if (options.handle) {
    params.set('login_hint', options.handle);
  }

  window.location.href = `${authorizeUrl}?${params.toString()}`;
}
```

**Step 3: Update handleOAuthCallback to use stored redirectUri**

Modify the `handleOAuthCallback` function to retrieve the stored redirect URI:

```typescript
export async function handleOAuthCallback(tokenUrl: string): Promise<boolean> {
  const params = new URLSearchParams(window.location.search);
  const code = params.get('code');
  const state = params.get('state');
  const error = params.get('error');

  if (error) {
    throw new Error(
      `OAuth error: ${error} - ${params.get('error_description') || ''}`
    );
  }

  if (!code || !state) {
    return false; // Not a callback
  }

  // Verify state
  const storedState = storage.get(STORAGE_KEYS.oauthState);
  if (state !== storedState) {
    throw new Error('OAuth state mismatch - possible CSRF attack');
  }

  // Get stored values
  const codeVerifier = storage.get(STORAGE_KEYS.codeVerifier);
  const clientId = storage.get(STORAGE_KEYS.clientId);
  const redirectUri = storage.get(STORAGE_KEYS.redirectUri);

  if (!codeVerifier || !clientId || !redirectUri) {
    throw new Error('Missing OAuth session data');
  }

  // Exchange code for tokens with DPoP
  const dpopProof = await createDPoPProof('POST', tokenUrl);

  const tokenResponse = await fetch(tokenUrl, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/x-www-form-urlencoded',
      DPoP: dpopProof,
    },
    body: new URLSearchParams({
      grant_type: 'authorization_code',
      code: code,
      redirect_uri: redirectUri,
      client_id: clientId,
      code_verifier: codeVerifier,
    }),
  });

  if (!tokenResponse.ok) {
    const errorData = await tokenResponse.json().catch(() => ({}));
    throw new Error(
      `Token exchange failed: ${errorData.error_description || tokenResponse.statusText}`
    );
  }

  const tokens = await tokenResponse.json();

  // Store tokens
  storeTokens(tokens);

  // Clean up OAuth state
  storage.remove(STORAGE_KEYS.codeVerifier);
  storage.remove(STORAGE_KEYS.oauthState);
  storage.remove(STORAGE_KEYS.redirectUri);

  // Clear URL params
  window.history.replaceState({}, document.title, window.location.pathname);

  return true;
}
```

**Step 4: Verify build**

Run: `cd /Users/chadmiller/code/quickslice/quickslice-client-js && npm run build`
Expected: Build succeeds

**Step 5: Commit**

```bash
git add src/auth/oauth.ts
git commit -m "feat: support custom redirectUri in OAuth flow"
```

---

### Task 3: Update QuicksliceClient to Accept redirectUri Option

**Files:**
- Modify: `src/client.ts`

**Step 1: Update QuicksliceClientOptions interface**

Add `redirectUri` to the options interface:

```typescript
export interface QuicksliceClientOptions {
  server: string;
  clientId: string;
  redirectUri?: string;
}
```

**Step 2: Store redirectUri in client class**

Add a private field and store the value in the constructor:

```typescript
export class QuicksliceClient {
  private server: string;
  private clientId: string;
  private redirectUri?: string;
  private graphqlUrl: string;
  private authorizeUrl: string;
  private tokenUrl: string;
  private initialized = false;

  constructor(options: QuicksliceClientOptions) {
    this.server = options.server.replace(/\/$/, ''); // Remove trailing slash
    this.clientId = options.clientId;
    this.redirectUri = options.redirectUri;

    this.graphqlUrl = `${this.server}/graphql`;
    this.authorizeUrl = `${this.server}/oauth/authorize`;
    this.tokenUrl = `${this.server}/oauth/token`;
  }
```

**Step 3: Pass redirectUri to loginWithRedirect**

Update the `loginWithRedirect` method to pass the redirect URI:

```typescript
  /**
   * Start OAuth login flow
   */
  async loginWithRedirect(options: LoginOptions = {}): Promise<void> {
    await this.init();
    await initiateLogin(this.authorizeUrl, this.clientId, {
      ...options,
      redirectUri: options.redirectUri || this.redirectUri,
    });
  }
```

**Step 4: Verify build**

Run: `cd /Users/chadmiller/code/quickslice/quickslice-client-js && npm run build`
Expected: Build succeeds

**Step 5: Commit**

```bash
git add src/client.ts
git commit -m "feat: add redirectUri option to QuicksliceClient"
```

---

### Task 4: Update README Documentation

**Files:**
- Modify: `README.md`

**Step 1: Update the usage example**

Find the client initialization example and add the `redirectUri` option:

```typescript
const client = new QuicksliceClient({
  server: 'https://your-quickslice-server.com',
  clientId: 'your-client-id',
  redirectUri: 'https://yourapp.com/oauth/callback'  // optional
});
```

**Step 2: Add a note about the option**

Add documentation explaining:
- `redirectUri` is optional
- If omitted, defaults to current page URL
- Useful when you have a dedicated callback route

**Step 3: Commit**

```bash
git add README.md
git commit -m "docs: add redirectUri option to README"
```

---

### Task 5: Final Build and Verify

**Step 1: Clean build**

Run: `cd /Users/chadmiller/code/quickslice/quickslice-client-js && rm -rf dist && npm run build`
Expected: Build succeeds, `dist/` contains updated files

**Step 2: Check TypeScript declarations**

Run: `cat dist/client.d.ts | grep -A3 "QuicksliceClientOptions"`
Expected: Shows `redirectUri?: string` in the interface

**Step 3: Final commit (if any uncommitted changes)**

```bash
git status
# If clean, done. Otherwise commit remaining changes.
```
