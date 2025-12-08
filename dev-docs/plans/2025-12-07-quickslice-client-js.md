# quickslice-client-js Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Extract auth code from statusphere example into a reusable client SDK that can be loaded via jsDelivr CDN.

**Architecture:** High-level client SDK similar to auth0-spa-js. Single `QuicksliceClient` class wraps OAuth PKCE + DPoP authentication and GraphQL requests. All complexity (key management, token refresh, multi-tab coordination) hidden behind simple methods.

**Tech Stack:** TypeScript, esbuild (UMD + ESM builds), Web Crypto API, IndexedDB, localStorage/sessionStorage

---

## Task 1: Project Scaffolding

**Files:**
- Create: `quickslice-client-js/package.json`
- Create: `quickslice-client-js/tsconfig.json`
- Create: `quickslice-client-js/build.mjs`
- Create: `quickslice-client-js/.gitignore`

**Step 1: Create package.json**

```json
{
  "name": "quickslice-client-js",
  "version": "0.1.0",
  "description": "Quickslice client SDK for browser SPAs",
  "main": "dist/quickslice-client.js",
  "module": "dist/quickslice-client.esm.js",
  "types": "dist/index.d.ts",
  "files": [
    "dist"
  ],
  "scripts": {
    "build": "node build.mjs",
    "watch": "node build.mjs --watch"
  },
  "devDependencies": {
    "esbuild": "^0.24.0",
    "typescript": "^5.3.0"
  },
  "keywords": [
    "quickslice",
    "oauth",
    "dpop",
    "atproto"
  ],
  "license": "MIT"
}
```

**Step 2: Create tsconfig.json**

```json
{
  "compilerOptions": {
    "target": "ES2020",
    "module": "ESNext",
    "moduleResolution": "bundler",
    "lib": ["ES2020", "DOM"],
    "strict": true,
    "declaration": true,
    "declarationDir": "dist",
    "emitDeclarationOnly": true,
    "outDir": "dist",
    "rootDir": "src",
    "skipLibCheck": true,
    "esModuleInterop": true
  },
  "include": ["src/**/*"],
  "exclude": ["node_modules", "dist"]
}
```

**Step 3: Create build.mjs**

```javascript
import * as esbuild from 'esbuild';

const watch = process.argv.includes('--watch');

const sharedConfig = {
  entryPoints: ['src/index.ts'],
  bundle: true,
  sourcemap: true,
  target: ['es2020'],
};

// UMD build (for CDN/script tag)
const umdBuild = {
  ...sharedConfig,
  outfile: 'dist/quickslice-client.js',
  format: 'iife',
  globalName: 'QuicksliceClient',
};

// UMD minified
const umdMinBuild = {
  ...sharedConfig,
  outfile: 'dist/quickslice-client.min.js',
  format: 'iife',
  globalName: 'QuicksliceClient',
  minify: true,
  sourcemap: false,
};

// ESM build (for bundlers)
const esmBuild = {
  ...sharedConfig,
  outfile: 'dist/quickslice-client.esm.js',
  format: 'esm',
};

async function build() {
  if (watch) {
    const ctx = await esbuild.context(umdBuild);
    await ctx.watch();
    console.log('Watching for changes...');
  } else {
    await Promise.all([
      esbuild.build(umdBuild),
      esbuild.build(umdMinBuild),
      esbuild.build(esmBuild),
    ]);
    console.log('Build complete!');
  }
}

build().catch((err) => {
  console.error(err);
  process.exit(1);
});
```

**Step 4: Create .gitignore**

```
node_modules/
```

Note: We intentionally do NOT ignore `dist/` since we commit built files for jsDelivr.

**Step 5: Install dependencies**

Run: `cd quickslice-client-js && npm install`

**Step 6: Commit**

```bash
git add quickslice-client-js/
git commit -m "chore: scaffold quickslice-client-js package"
```

---

## Task 2: Base64 URL and Crypto Utilities

**Files:**
- Create: `quickslice-client-js/src/utils/base64url.ts`
- Create: `quickslice-client-js/src/utils/crypto.ts`

**Step 1: Create base64url.ts**

```typescript
/**
 * Base64 URL encode a buffer (Uint8Array or ArrayBuffer)
 */
export function base64UrlEncode(buffer: ArrayBuffer | Uint8Array): string {
  const bytes = buffer instanceof Uint8Array ? buffer : new Uint8Array(buffer);
  let binary = '';
  for (let i = 0; i < bytes.length; i++) {
    binary += String.fromCharCode(bytes[i]);
  }
  return btoa(binary)
    .replace(/\+/g, '-')
    .replace(/\//g, '_')
    .replace(/=+$/, '');
}

/**
 * Generate a random base64url string
 */
export function generateRandomString(byteLength: number): string {
  const bytes = new Uint8Array(byteLength);
  crypto.getRandomValues(bytes);
  return base64UrlEncode(bytes);
}
```

**Step 2: Create crypto.ts**

```typescript
import { base64UrlEncode } from './base64url';

/**
 * SHA-256 hash, returned as base64url string
 */
export async function sha256Base64Url(data: string): Promise<string> {
  const encoder = new TextEncoder();
  const hash = await crypto.subtle.digest('SHA-256', encoder.encode(data));
  return base64UrlEncode(hash);
}

/**
 * Sign a JWT with an ECDSA P-256 private key
 */
export async function signJwt(
  header: Record<string, unknown>,
  payload: Record<string, unknown>,
  privateKey: CryptoKey
): Promise<string> {
  const encoder = new TextEncoder();

  const headerB64 = base64UrlEncode(encoder.encode(JSON.stringify(header)));
  const payloadB64 = base64UrlEncode(encoder.encode(JSON.stringify(payload)));

  const signingInput = `${headerB64}.${payloadB64}`;

  const signature = await crypto.subtle.sign(
    { name: 'ECDSA', hash: 'SHA-256' },
    privateKey,
    encoder.encode(signingInput)
  );

  const signatureB64 = base64UrlEncode(signature);

  return `${signingInput}.${signatureB64}`;
}
```

**Step 3: Commit**

```bash
git add quickslice-client-js/src/utils/
git commit -m "feat(client): add base64url and crypto utilities"
```

---

## Task 3: Storage Utilities

**Files:**
- Create: `quickslice-client-js/src/storage/keys.ts`
- Create: `quickslice-client-js/src/storage/storage.ts`
- Create: `quickslice-client-js/src/storage/lock.ts`

**Step 1: Create keys.ts**

```typescript
/**
 * Storage key constants
 */
export const STORAGE_KEYS = {
  accessToken: 'quickslice_access_token',
  refreshToken: 'quickslice_refresh_token',
  tokenExpiresAt: 'quickslice_token_expires_at',
  clientId: 'quickslice_client_id',
  userDid: 'quickslice_user_did',
  codeVerifier: 'quickslice_code_verifier',
  oauthState: 'quickslice_oauth_state',
} as const;

export type StorageKey = (typeof STORAGE_KEYS)[keyof typeof STORAGE_KEYS];
```

**Step 2: Create storage.ts**

```typescript
import { STORAGE_KEYS, StorageKey } from './keys';

/**
 * Hybrid storage utility - sessionStorage for OAuth flow state,
 * localStorage for tokens (shared across tabs)
 */
export const storage = {
  get(key: StorageKey): string | null {
    // OAuth flow state stays in sessionStorage (per-tab)
    if (key === STORAGE_KEYS.codeVerifier || key === STORAGE_KEYS.oauthState) {
      return sessionStorage.getItem(key);
    }
    // Tokens go in localStorage (shared across tabs)
    return localStorage.getItem(key);
  },

  set(key: StorageKey, value: string): void {
    if (key === STORAGE_KEYS.codeVerifier || key === STORAGE_KEYS.oauthState) {
      sessionStorage.setItem(key, value);
    } else {
      localStorage.setItem(key, value);
    }
  },

  remove(key: StorageKey): void {
    sessionStorage.removeItem(key);
    localStorage.removeItem(key);
  },

  clear(): void {
    Object.values(STORAGE_KEYS).forEach((key) => {
      sessionStorage.removeItem(key);
      localStorage.removeItem(key);
    });
  },
};
```

**Step 3: Create lock.ts**

```typescript
const LOCK_TIMEOUT = 5000; // 5 seconds
const LOCK_PREFIX = 'quickslice_lock_';

function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

/**
 * Acquire a lock using localStorage for multi-tab coordination
 */
export async function acquireLock(
  key: string,
  timeout = LOCK_TIMEOUT
): Promise<string | null> {
  const lockKey = LOCK_PREFIX + key;
  const lockValue = `${Date.now()}_${Math.random()}`;
  const deadline = Date.now() + timeout;

  while (Date.now() < deadline) {
    const existing = localStorage.getItem(lockKey);

    if (existing) {
      // Check if lock is stale (older than timeout)
      const [timestamp] = existing.split('_');
      if (Date.now() - parseInt(timestamp) > LOCK_TIMEOUT) {
        // Lock is stale, remove it
        localStorage.removeItem(lockKey);
      } else {
        // Lock is held, wait and retry
        await sleep(50);
        continue;
      }
    }

    // Try to acquire
    localStorage.setItem(lockKey, lockValue);

    // Verify we got it (handle race condition)
    await sleep(10);
    if (localStorage.getItem(lockKey) === lockValue) {
      return lockValue; // Lock acquired
    }
  }

  return null; // Failed to acquire
}

/**
 * Release a lock
 */
export function releaseLock(key: string, lockValue: string): void {
  const lockKey = LOCK_PREFIX + key;
  // Only release if we still hold it
  if (localStorage.getItem(lockKey) === lockValue) {
    localStorage.removeItem(lockKey);
  }
}
```

**Step 4: Commit**

```bash
git add quickslice-client-js/src/storage/
git commit -m "feat(client): add storage utilities with multi-tab lock"
```

---

## Task 4: DPoP Implementation

**Files:**
- Create: `quickslice-client-js/src/auth/dpop.ts`

**Step 1: Create dpop.ts**

```typescript
import { base64UrlEncode, generateRandomString } from '../utils/base64url';
import { sha256Base64Url, signJwt } from '../utils/crypto';

const DB_NAME = 'quickslice-oauth';
const DB_VERSION = 1;
const KEY_STORE = 'dpop-keys';
const KEY_ID = 'dpop-key';

interface DPoPKeyData {
  id: string;
  privateKey: CryptoKey;
  publicJwk: JsonWebKey;
  createdAt: number;
}

let dbPromise: Promise<IDBDatabase> | null = null;

function openDatabase(): Promise<IDBDatabase> {
  if (dbPromise) return dbPromise;

  dbPromise = new Promise((resolve, reject) => {
    const request = indexedDB.open(DB_NAME, DB_VERSION);

    request.onerror = () => reject(request.error);
    request.onsuccess = () => resolve(request.result);

    request.onupgradeneeded = (event) => {
      const db = (event.target as IDBOpenDBRequest).result;
      if (!db.objectStoreNames.contains(KEY_STORE)) {
        db.createObjectStore(KEY_STORE, { keyPath: 'id' });
      }
    };
  });

  return dbPromise;
}

async function getDPoPKey(): Promise<DPoPKeyData | null> {
  const db = await openDatabase();
  return new Promise((resolve, reject) => {
    const tx = db.transaction(KEY_STORE, 'readonly');
    const store = tx.objectStore(KEY_STORE);
    const request = store.get(KEY_ID);

    request.onerror = () => reject(request.error);
    request.onsuccess = () => resolve(request.result || null);
  });
}

async function storeDPoPKey(
  privateKey: CryptoKey,
  publicJwk: JsonWebKey
): Promise<void> {
  const db = await openDatabase();
  return new Promise((resolve, reject) => {
    const tx = db.transaction(KEY_STORE, 'readwrite');
    const store = tx.objectStore(KEY_STORE);
    const request = store.put({
      id: KEY_ID,
      privateKey,
      publicJwk,
      createdAt: Date.now(),
    });

    request.onerror = () => reject(request.error);
    request.onsuccess = () => resolve();
  });
}

export async function getOrCreateDPoPKey(): Promise<DPoPKeyData> {
  const keyData = await getDPoPKey();

  if (keyData) {
    return keyData;
  }

  // Generate new P-256 key pair
  const keyPair = await crypto.subtle.generateKey(
    { name: 'ECDSA', namedCurve: 'P-256' },
    false, // NOT extractable - critical for security
    ['sign']
  );

  // Export public key as JWK
  const publicJwk = await crypto.subtle.exportKey('jwk', keyPair.publicKey);

  // Store in IndexedDB
  await storeDPoPKey(keyPair.privateKey, publicJwk);

  return {
    id: KEY_ID,
    privateKey: keyPair.privateKey,
    publicJwk,
    createdAt: Date.now(),
  };
}

/**
 * Create a DPoP proof JWT
 */
export async function createDPoPProof(
  method: string,
  url: string,
  accessToken: string | null = null
): Promise<string> {
  const keyData = await getOrCreateDPoPKey();

  // Strip WebCrypto-specific fields from JWK for interoperability
  const { kty, crv, x, y } = keyData.publicJwk;
  const minimalJwk = { kty, crv, x, y };

  const header = {
    alg: 'ES256',
    typ: 'dpop+jwt',
    jwk: minimalJwk,
  };

  const payload: Record<string, unknown> = {
    jti: generateRandomString(16),
    htm: method,
    htu: url,
    iat: Math.floor(Date.now() / 1000),
  };

  // Add access token hash if provided (for resource requests)
  if (accessToken) {
    payload.ath = await sha256Base64Url(accessToken);
  }

  return await signJwt(header, payload, keyData.privateKey);
}

/**
 * Clear DPoP keys from IndexedDB
 */
export async function clearDPoPKeys(): Promise<void> {
  const db = await openDatabase();
  return new Promise((resolve, reject) => {
    const tx = db.transaction(KEY_STORE, 'readwrite');
    const store = tx.objectStore(KEY_STORE);
    const request = store.clear();

    request.onerror = () => reject(request.error);
    request.onsuccess = () => resolve();
  });
}
```

**Step 2: Commit**

```bash
git add quickslice-client-js/src/auth/dpop.ts
git commit -m "feat(client): add DPoP proof generation with IndexedDB key storage"
```

---

## Task 5: PKCE Utilities

**Files:**
- Create: `quickslice-client-js/src/auth/pkce.ts`

**Step 1: Create pkce.ts**

```typescript
import { base64UrlEncode, generateRandomString } from '../utils/base64url';

/**
 * Generate a PKCE code verifier (32 random bytes, base64url encoded)
 */
export function generateCodeVerifier(): string {
  return generateRandomString(32);
}

/**
 * Generate a PKCE code challenge from a verifier (SHA-256, base64url encoded)
 */
export async function generateCodeChallenge(verifier: string): Promise<string> {
  const encoder = new TextEncoder();
  const data = encoder.encode(verifier);
  const hash = await crypto.subtle.digest('SHA-256', data);
  return base64UrlEncode(hash);
}

/**
 * Generate a random state parameter for CSRF protection
 */
export function generateState(): string {
  return generateRandomString(16);
}
```

**Step 2: Commit**

```bash
git add quickslice-client-js/src/auth/pkce.ts
git commit -m "feat(client): add PKCE code verifier/challenge utilities"
```

---

## Task 6: OAuth Token Management

**Files:**
- Create: `quickslice-client-js/src/auth/tokens.ts`

**Step 1: Create tokens.ts**

```typescript
import { storage } from '../storage/storage';
import { STORAGE_KEYS } from '../storage/keys';
import { acquireLock, releaseLock } from '../storage/lock';
import { createDPoPProof } from './dpop';

const TOKEN_REFRESH_BUFFER_MS = 60000; // 60 seconds before expiry

function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

/**
 * Refresh tokens using the refresh token
 */
async function refreshTokens(tokenUrl: string): Promise<string> {
  const refreshToken = storage.get(STORAGE_KEYS.refreshToken);
  const clientId = storage.get(STORAGE_KEYS.clientId);

  if (!refreshToken || !clientId) {
    throw new Error('No refresh token available');
  }

  const dpopProof = await createDPoPProof('POST', tokenUrl);

  const response = await fetch(tokenUrl, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/x-www-form-urlencoded',
      DPoP: dpopProof,
    },
    body: new URLSearchParams({
      grant_type: 'refresh_token',
      refresh_token: refreshToken,
      client_id: clientId,
    }),
  });

  if (!response.ok) {
    const errorData = await response.json().catch(() => ({}));
    throw new Error(
      `Token refresh failed: ${errorData.error_description || response.statusText}`
    );
  }

  const tokens = await response.json();

  // Store new tokens (rotation - new refresh token each time)
  storage.set(STORAGE_KEYS.accessToken, tokens.access_token);
  if (tokens.refresh_token) {
    storage.set(STORAGE_KEYS.refreshToken, tokens.refresh_token);
  }

  const expiresAt = Date.now() + tokens.expires_in * 1000;
  storage.set(STORAGE_KEYS.tokenExpiresAt, expiresAt.toString());

  return tokens.access_token;
}

/**
 * Get a valid access token, refreshing if necessary.
 * Uses multi-tab locking to prevent duplicate refresh requests.
 */
export async function getValidAccessToken(tokenUrl: string): Promise<string> {
  const accessToken = storage.get(STORAGE_KEYS.accessToken);
  const expiresAt = parseInt(storage.get(STORAGE_KEYS.tokenExpiresAt) || '0');

  // Check if token is still valid (with buffer)
  if (accessToken && Date.now() < expiresAt - TOKEN_REFRESH_BUFFER_MS) {
    return accessToken;
  }

  // Need to refresh - acquire lock first
  const clientId = storage.get(STORAGE_KEYS.clientId);
  const lockKey = `token_refresh_${clientId}`;
  const lockValue = await acquireLock(lockKey);

  if (!lockValue) {
    // Failed to acquire lock, another tab is refreshing
    // Wait a bit and check cache again
    await sleep(100);
    const freshToken = storage.get(STORAGE_KEYS.accessToken);
    const freshExpiry = parseInt(
      storage.get(STORAGE_KEYS.tokenExpiresAt) || '0'
    );
    if (freshToken && Date.now() < freshExpiry - TOKEN_REFRESH_BUFFER_MS) {
      return freshToken;
    }
    throw new Error('Failed to refresh token');
  }

  try {
    // Double-check after acquiring lock
    const freshToken = storage.get(STORAGE_KEYS.accessToken);
    const freshExpiry = parseInt(
      storage.get(STORAGE_KEYS.tokenExpiresAt) || '0'
    );
    if (freshToken && Date.now() < freshExpiry - TOKEN_REFRESH_BUFFER_MS) {
      return freshToken;
    }

    // Actually refresh
    return await refreshTokens(tokenUrl);
  } finally {
    releaseLock(lockKey, lockValue);
  }
}

/**
 * Store tokens from OAuth response
 */
export function storeTokens(tokens: {
  access_token: string;
  refresh_token?: string;
  expires_in: number;
  sub?: string;
}): void {
  storage.set(STORAGE_KEYS.accessToken, tokens.access_token);
  if (tokens.refresh_token) {
    storage.set(STORAGE_KEYS.refreshToken, tokens.refresh_token);
  }

  const expiresAt = Date.now() + tokens.expires_in * 1000;
  storage.set(STORAGE_KEYS.tokenExpiresAt, expiresAt.toString());

  if (tokens.sub) {
    storage.set(STORAGE_KEYS.userDid, tokens.sub);
  }
}

/**
 * Check if we have a valid session
 */
export function hasValidSession(): boolean {
  const accessToken = storage.get(STORAGE_KEYS.accessToken);
  const refreshToken = storage.get(STORAGE_KEYS.refreshToken);
  return !!(accessToken || refreshToken);
}
```

**Step 2: Commit**

```bash
git add quickslice-client-js/src/auth/tokens.ts
git commit -m "feat(client): add token management with multi-tab refresh coordination"
```

---

## Task 7: OAuth Flow (Login/Callback/Logout)

**Files:**
- Create: `quickslice-client-js/src/auth/oauth.ts`

**Step 1: Create oauth.ts**

```typescript
import { storage } from '../storage/storage';
import { STORAGE_KEYS } from '../storage/keys';
import { createDPoPProof, clearDPoPKeys } from './dpop';
import { generateCodeVerifier, generateCodeChallenge, generateState } from './pkce';
import { storeTokens } from './tokens';

export interface LoginOptions {
  handle?: string;
}

/**
 * Initiate OAuth login flow with PKCE
 */
export async function initiateLogin(
  authorizeUrl: string,
  clientId: string,
  options: LoginOptions = {}
): Promise<void> {
  const codeVerifier = generateCodeVerifier();
  const codeChallenge = await generateCodeChallenge(codeVerifier);
  const state = generateState();

  // Store for callback
  storage.set(STORAGE_KEYS.codeVerifier, codeVerifier);
  storage.set(STORAGE_KEYS.oauthState, state);
  storage.set(STORAGE_KEYS.clientId, clientId);

  // Build redirect URI (current page without query params)
  const redirectUri = window.location.origin + window.location.pathname;

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

/**
 * Handle OAuth callback - exchange code for tokens
 * Returns true if callback was handled, false if not a callback
 */
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
  const redirectUri = window.location.origin + window.location.pathname;

  if (!codeVerifier || !clientId) {
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

  // Clear URL params
  window.history.replaceState({}, document.title, window.location.pathname);

  return true;
}

/**
 * Logout - clear all stored data
 */
export async function logout(options: { reload?: boolean } = {}): Promise<void> {
  storage.clear();
  await clearDPoPKeys();

  if (options.reload !== false) {
    window.location.reload();
  }
}
```

**Step 2: Commit**

```bash
git add quickslice-client-js/src/auth/oauth.ts
git commit -m "feat(client): add OAuth login/callback/logout flows"
```

---

## Task 8: GraphQL Utilities

**Files:**
- Create: `quickslice-client-js/src/graphql.ts`

**Step 1: Create graphql.ts**

```typescript
import { createDPoPProof } from './auth/dpop';
import { getValidAccessToken } from './auth/tokens';

export interface GraphQLResponse<T = unknown> {
  data?: T;
  errors?: Array<{ message: string; path?: string[] }>;
}

/**
 * Execute a GraphQL query or mutation
 */
export async function graphqlRequest<T = unknown>(
  graphqlUrl: string,
  tokenUrl: string,
  query: string,
  variables: Record<string, unknown> = {},
  requireAuth = false
): Promise<T> {
  const headers: Record<string, string> = {
    'Content-Type': 'application/json',
  };

  if (requireAuth) {
    const token = await getValidAccessToken(tokenUrl);
    if (!token) {
      throw new Error('Not authenticated');
    }

    // Create DPoP proof bound to this request
    const dpopProof = await createDPoPProof('POST', graphqlUrl, token);

    headers['Authorization'] = `DPoP ${token}`;
    headers['DPoP'] = dpopProof;
  }

  const response = await fetch(graphqlUrl, {
    method: 'POST',
    headers,
    body: JSON.stringify({ query, variables }),
  });

  if (!response.ok) {
    throw new Error(`GraphQL request failed: ${response.statusText}`);
  }

  const result: GraphQLResponse<T> = await response.json();

  if (result.errors && result.errors.length > 0) {
    throw new Error(`GraphQL error: ${result.errors[0].message}`);
  }

  return result.data as T;
}
```

**Step 2: Commit**

```bash
git add quickslice-client-js/src/graphql.ts
git commit -m "feat(client): add GraphQL request utility with DPoP auth"
```

---

## Task 9: Error Classes

**Files:**
- Create: `quickslice-client-js/src/errors.ts`

**Step 1: Create errors.ts**

```typescript
/**
 * Base error class for Quickslice client errors
 */
export class QuicksliceError extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'QuicksliceError';
  }
}

/**
 * Thrown when authentication is required but user is not logged in
 */
export class LoginRequiredError extends QuicksliceError {
  constructor(message = 'Login required') {
    super(message);
    this.name = 'LoginRequiredError';
  }
}

/**
 * Thrown when network request fails
 */
export class NetworkError extends QuicksliceError {
  constructor(message: string) {
    super(message);
    this.name = 'NetworkError';
  }
}

/**
 * Thrown when OAuth flow fails
 */
export class OAuthError extends QuicksliceError {
  public code: string;
  public description?: string;

  constructor(code: string, description?: string) {
    super(`OAuth error: ${code}${description ? ` - ${description}` : ''}`);
    this.name = 'OAuthError';
    this.code = code;
    this.description = description;
  }
}
```

**Step 2: Commit**

```bash
git add quickslice-client-js/src/errors.ts
git commit -m "feat(client): add error classes"
```

---

## Task 10: Main Client Class

**Files:**
- Create: `quickslice-client-js/src/client.ts`

**Step 1: Create client.ts**

```typescript
import { storage } from './storage/storage';
import { STORAGE_KEYS } from './storage/keys';
import { getOrCreateDPoPKey } from './auth/dpop';
import { initiateLogin, handleOAuthCallback, logout as doLogout, LoginOptions } from './auth/oauth';
import { getValidAccessToken, hasValidSession } from './auth/tokens';
import { graphqlRequest } from './graphql';

export interface QuicksliceClientOptions {
  server: string;
  clientId: string;
}

export interface User {
  did: string;
}

export class QuicksliceClient {
  private server: string;
  private clientId: string;
  private graphqlUrl: string;
  private authorizeUrl: string;
  private tokenUrl: string;
  private initialized = false;

  constructor(options: QuicksliceClientOptions) {
    this.server = options.server.replace(/\/$/, ''); // Remove trailing slash
    this.clientId = options.clientId;

    this.graphqlUrl = `${this.server}/graphql`;
    this.authorizeUrl = `${this.server}/oauth/authorize`;
    this.tokenUrl = `${this.server}/oauth/token`;
  }

  /**
   * Initialize the client - must be called before other methods
   */
  async init(): Promise<void> {
    if (this.initialized) return;

    // Ensure DPoP key exists
    await getOrCreateDPoPKey();

    this.initialized = true;
  }

  /**
   * Start OAuth login flow
   */
  async loginWithRedirect(options: LoginOptions = {}): Promise<void> {
    await this.init();
    await initiateLogin(this.authorizeUrl, this.clientId, options);
  }

  /**
   * Handle OAuth callback after redirect
   * Returns true if callback was handled
   */
  async handleRedirectCallback(): Promise<boolean> {
    await this.init();
    return await handleOAuthCallback(this.tokenUrl);
  }

  /**
   * Logout and clear all stored data
   */
  async logout(options: { reload?: boolean } = {}): Promise<void> {
    await doLogout(options);
  }

  /**
   * Check if user is authenticated
   */
  async isAuthenticated(): Promise<boolean> {
    return hasValidSession();
  }

  /**
   * Get current user's DID (from stored token data)
   * For richer profile info, use client.query() with your own schema
   */
  getUser(): User | null {
    if (!hasValidSession()) {
      return null;
    }

    const did = storage.get(STORAGE_KEYS.userDid);
    if (!did) {
      return null;
    }

    return { did };
  }

  /**
   * Get access token (auto-refreshes if needed)
   */
  async getAccessToken(): Promise<string> {
    await this.init();
    return await getValidAccessToken(this.tokenUrl);
  }

  /**
   * Execute a GraphQL query (authenticated)
   */
  async query<T = unknown>(
    query: string,
    variables: Record<string, unknown> = {}
  ): Promise<T> {
    await this.init();
    return await graphqlRequest<T>(
      this.graphqlUrl,
      this.tokenUrl,
      query,
      variables,
      true
    );
  }

  /**
   * Execute a GraphQL mutation (authenticated)
   */
  async mutate<T = unknown>(
    mutation: string,
    variables: Record<string, unknown> = {}
  ): Promise<T> {
    return this.query<T>(mutation, variables);
  }

  /**
   * Execute a public GraphQL query (no auth)
   */
  async publicQuery<T = unknown>(
    query: string,
    variables: Record<string, unknown> = {}
  ): Promise<T> {
    await this.init();
    return await graphqlRequest<T>(
      this.graphqlUrl,
      this.tokenUrl,
      query,
      variables,
      false
    );
  }
}
```

**Step 2: Commit**

```bash
git add quickslice-client-js/src/client.ts
git commit -m "feat(client): add main QuicksliceClient class"
```

---

## Task 11: Entry Point and Exports

**Files:**
- Create: `quickslice-client-js/src/index.ts`

**Step 1: Create index.ts**

```typescript
export { QuicksliceClient, QuicksliceClientOptions, User } from './client';
export {
  QuicksliceError,
  LoginRequiredError,
  NetworkError,
  OAuthError,
} from './errors';

import { QuicksliceClient, QuicksliceClientOptions } from './client';

/**
 * Create and initialize a Quickslice client
 */
export async function createQuicksliceClient(
  options: QuicksliceClientOptions
): Promise<QuicksliceClient> {
  const client = new QuicksliceClient(options);
  await client.init();
  return client;
}
```

**Step 2: Commit**

```bash
git add quickslice-client-js/src/index.ts
git commit -m "feat(client): add entry point with createQuicksliceClient factory"
```

---

## Task 12: Build and Test

**Step 1: Build the library**

Run: `cd quickslice-client-js && npm run build`

Expected output:
```
Build complete!
```

**Step 2: Verify dist files exist**

Run: `ls -la quickslice-client-js/dist/`

Expected: Should see `quickslice-client.js`, `quickslice-client.min.js`, `quickslice-client.esm.js`

**Step 3: Generate TypeScript declarations**

Run: `cd quickslice-client-js && npx tsc`

**Step 4: Commit dist files**

```bash
git add quickslice-client-js/dist/
git commit -m "chore(client): add built distribution files"
```

---

## Task 13: Update Statusphere Example

**Files:**
- Modify: `examples/01-statusphere/index.html`

**Step 1: Replace inline auth code with library import**

Replace the entire `<script>` section (lines ~370-970) with:

```html
<script src="../../quickslice-client-js/dist/quickslice-client.min.js"></script>
<script>
  // Configuration
  const SERVER_URL = "http://localhost:8080";
  const CLIENT_ID = "client_hmO069YjiCuJg5rnogC5-A"; // TODO: Replace with actual client ID

  let client;

  // =============================================================================
  // INITIALIZATION
  // =============================================================================

  async function main() {
    client = await QuicksliceClient.createQuicksliceClient({
      server: SERVER_URL,
      clientId: CLIENT_ID,
    });

    // Handle OAuth callback
    if (window.location.search.includes("code=")) {
      try {
        await client.handleRedirectCallback();
      } catch (error) {
        console.error("OAuth callback error:", error);
        showError(error.message);
        return;
      }
    }

    // Initial render
    await renderApp();
  }

  async function renderApp() {
    if (await client.isAuthenticated()) {
      const user = client.getUser();
      // Fetch additional profile data if needed via client.query()
      renderLoggedIn(user);
      await loadStatuses();
    } else {
      renderLoggedOut();
    }
  }

  // =============================================================================
  // AUTH HANDLERS
  // =============================================================================

  async function handleLogin(event) {
    event.preventDefault();
    const handle = document.getElementById("handle").value.trim();
    if (!handle) return;

    try {
      await client.loginWithRedirect({ handle });
    } catch (error) {
      console.error("Login error:", error);
      showError(error.message);
    }
  }

  function handleLogout() {
    client.logout();
  }

  // =============================================================================
  // DATA FETCHING
  // =============================================================================

  async function loadStatuses() {
    try {
      const data = await client.publicQuery(`
        query GetStatuses {
          xyzStatusphereStatus(first: 20, sortBy: [{ field: "createdAt", direction: DESC }]) {
            edges {
              node {
                status
                createdAt
                did
                appBskyActorProfileByDid {
                  displayName
                  avatar
                }
              }
            }
          }
        }
      `);
      renderStatuses(data.xyzStatusphereStatus.edges);
    } catch (error) {
      console.error("Failed to load statuses:", error);
    }
  }

  async function postStatus(emoji) {
    try {
      await client.mutate(`
        mutation CreateStatus($status: String!, $createdAt: DateTime!) {
          createXyzStatusphereStatus(input: { status: $status, createdAt: $createdAt }) {
            status
          }
        }
      `, {
        status: emoji,
        createdAt: new Date().toISOString(),
      });
      await loadStatuses();
    } catch (error) {
      console.error("Failed to post status:", error);
      showError(error.message);
    }
  }

  // ... rest of UI rendering code stays the same ...
```

**Step 2: Keep UI rendering functions, remove auth functions**

Keep these functions from the original file:
- `renderLoggedIn()`
- `renderLoggedOut()`
- `renderStatuses()`
- `showError()`
- `escapeHtml()`
- Event listeners

Remove these (now provided by library):
- All IndexedDB code
- All DPoP code
- All lock code
- All storage code
- All PKCE code
- All OAuth functions
- `graphqlQuery()` function

**Step 3: Commit**

```bash
git add examples/01-statusphere/index.html
git commit -m "refactor(example): use quickslice-client-js library"
```

---

## Task 14: Add README

**Files:**
- Create: `quickslice-client-js/README.md`

**Step 1: Create README.md**

```markdown
# quickslice-client-js

Browser client SDK for Quickslice applications.

## Installation

### Via CDN (jsDelivr)

```html
<script src="https://cdn.jsdelivr.net/gh/yourorg/quickslice@main/quickslice-client-js/dist/quickslice-client.min.js"></script>
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
```

**Step 2: Commit**

```bash
git add quickslice-client-js/README.md
git commit -m "docs(client): add README with usage examples"
```

---

Plan complete and saved to `dev-docs/plans/2025-12-07-quickslice-client-js.md`. Two execution options:

**1. Subagent-Driven (this session)** - I dispatch fresh subagent per task, review between tasks, fast iteration

**2. Parallel Session (separate)** - Open new session with executing-plans, batch execution with checkpoints

Which approach?
