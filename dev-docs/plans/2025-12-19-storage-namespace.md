# Storage Namespace Isolation Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Prevent storage collisions when multiple apps on the same domain use quickslice-client-js by deriving a unique namespace from clientId.

**Architecture:** Compute an 8-character SHA-256 hash of clientId at client init. Thread this namespace through all storage operations: localStorage/sessionStorage keys, IndexedDB database name, and lock keys. No migration — existing users re-login once.

**Tech Stack:** TypeScript, Web Crypto API (SHA-256), localStorage, sessionStorage, IndexedDB

---

## Task 1: Add namespace hash utility

**Files:**
- Modify: `src/utils/crypto.ts:1-36`

**Step 1: Add namespace hash function**

Add after the existing `sha256Base64Url` function:

```typescript
/**
 * Generate an 8-character namespace hash from clientId
 */
export async function generateNamespaceHash(clientId: string): Promise<string> {
  const encoder = new TextEncoder();
  const hash = await crypto.subtle.digest('SHA-256', encoder.encode(clientId));
  const hashArray = Array.from(new Uint8Array(hash));
  const hashHex = hashArray.map(b => b.toString(16).padStart(2, '0')).join('');
  return hashHex.substring(0, 8);
}
```

**Step 2: Verify build passes**

Run: `npm run build`
Expected: Build succeeds with no errors

**Step 3: Commit**

```bash
git add src/utils/crypto.ts
git commit -m "feat: add namespace hash utility for storage isolation"
```

---

## Task 2: Make storage keys dynamic

**Files:**
- Modify: `src/storage/keys.ts:1-16`

**Step 1: Replace static keys with factory function**

Replace the entire file contents:

```typescript
/**
 * Storage key factory - generates namespaced keys
 */
export interface StorageKeys {
  accessToken: string;
  refreshToken: string;
  tokenExpiresAt: string;
  clientId: string;
  userDid: string;
  codeVerifier: string;
  oauthState: string;
  redirectUri: string;
}

export function createStorageKeys(namespace: string): StorageKeys {
  return {
    accessToken: `quickslice_${namespace}_access_token`,
    refreshToken: `quickslice_${namespace}_refresh_token`,
    tokenExpiresAt: `quickslice_${namespace}_token_expires_at`,
    clientId: `quickslice_${namespace}_client_id`,
    userDid: `quickslice_${namespace}_user_did`,
    codeVerifier: `quickslice_${namespace}_code_verifier`,
    oauthState: `quickslice_${namespace}_oauth_state`,
    redirectUri: `quickslice_${namespace}_redirect_uri`,
  };
}

export type StorageKey = string;
```

**Step 2: Verify build fails**

Run: `npm run build`
Expected: Build fails — other files still import `STORAGE_KEYS`

**Step 3: Commit (WIP)**

```bash
git add src/storage/keys.ts
git commit -m "wip: make storage keys dynamic with namespace"
```

---

## Task 3: Update storage module to accept keys

**Files:**
- Modify: `src/storage/storage.ts:1-37`

**Step 1: Rewrite storage module**

Replace the entire file contents:

```typescript
import { StorageKeys } from './keys';

/**
 * Create a namespaced storage interface
 */
export function createStorage(keys: StorageKeys) {
  return {
    get(key: keyof StorageKeys): string | null {
      const storageKey = keys[key];
      // OAuth flow state stays in sessionStorage (per-tab)
      if (key === 'codeVerifier' || key === 'oauthState') {
        return sessionStorage.getItem(storageKey);
      }
      // Tokens go in localStorage (shared across tabs)
      return localStorage.getItem(storageKey);
    },

    set(key: keyof StorageKeys, value: string): void {
      const storageKey = keys[key];
      if (key === 'codeVerifier' || key === 'oauthState') {
        sessionStorage.setItem(storageKey, value);
      } else {
        localStorage.setItem(storageKey, value);
      }
    },

    remove(key: keyof StorageKeys): void {
      const storageKey = keys[key];
      sessionStorage.removeItem(storageKey);
      localStorage.removeItem(storageKey);
    },

    clear(): void {
      (Object.keys(keys) as Array<keyof StorageKeys>).forEach((key) => {
        const storageKey = keys[key];
        sessionStorage.removeItem(storageKey);
        localStorage.removeItem(storageKey);
      });
    },
  };
}

export type Storage = ReturnType<typeof createStorage>;
```

**Step 2: Verify build still fails**

Run: `npm run build`
Expected: Build fails — consumers still use old imports

**Step 3: Commit (WIP)**

```bash
git add src/storage/storage.ts
git commit -m "wip: storage module accepts namespaced keys"
```

---

## Task 4: Update lock module with namespace

**Files:**
- Modify: `src/storage/lock.ts:1-57`

**Step 1: Rewrite lock module to accept namespace**

Replace the entire file contents:

```typescript
const LOCK_TIMEOUT = 5000; // 5 seconds

function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

function getLockKey(namespace: string, key: string): string {
  return `quickslice_${namespace}_lock_${key}`;
}

/**
 * Acquire a lock using localStorage for multi-tab coordination
 */
export async function acquireLock(
  namespace: string,
  key: string,
  timeout = LOCK_TIMEOUT
): Promise<string | null> {
  const lockKey = getLockKey(namespace, key);
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
export function releaseLock(namespace: string, key: string, lockValue: string): void {
  const lockKey = getLockKey(namespace, key);
  // Only release if we still hold it
  if (localStorage.getItem(lockKey) === lockValue) {
    localStorage.removeItem(lockKey);
  }
}
```

**Step 2: Verify build still fails**

Run: `npm run build`
Expected: Build fails — tokens.ts uses old lock signature

**Step 3: Commit (WIP)**

```bash
git add src/storage/lock.ts
git commit -m "wip: lock module uses namespace prefix"
```

---

## Task 5: Update DPoP module with namespaced database

**Files:**
- Modify: `src/auth/dpop.ts:1-147`

**Step 1: Change database name to use namespace**

Replace lines 1-36 with:

```typescript
import { generateRandomString } from '../utils/base64url';
import { sha256Base64Url } from '../utils/crypto';

const DB_VERSION = 1;
const KEY_STORE = 'dpop-keys';
const KEY_ID = 'dpop-key';

interface DPoPKeyData {
  id: string;
  privateKey: CryptoKey;
  publicJwk: JsonWebKey;
  createdAt: number;
}

// Cache database connections per namespace
const dbPromises = new Map<string, Promise<IDBDatabase>>();

function getDbName(namespace: string): string {
  return `quickslice-oauth-${namespace}`;
}

function openDatabase(namespace: string): Promise<IDBDatabase> {
  const existing = dbPromises.get(namespace);
  if (existing) return existing;

  const promise = new Promise<IDBDatabase>((resolve, reject) => {
    const request = indexedDB.open(getDbName(namespace), DB_VERSION);

    request.onerror = () => reject(request.error);
    request.onsuccess = () => resolve(request.result);

    request.onupgradeneeded = (event) => {
      const db = (event.target as IDBOpenDBRequest).result;
      if (!db.objectStoreNames.contains(KEY_STORE)) {
        db.createObjectStore(KEY_STORE, { keyPath: 'id' });
      }
    };
  });

  dbPromises.set(namespace, promise);
  return promise;
}
```

**Step 2: Update getDPoPKey to accept namespace**

Replace the getDPoPKey function (lines 38-48) with:

```typescript
async function getDPoPKey(namespace: string): Promise<DPoPKeyData | null> {
  const db = await openDatabase(namespace);
  return new Promise((resolve, reject) => {
    const tx = db.transaction(KEY_STORE, 'readonly');
    const store = tx.objectStore(KEY_STORE);
    const request = store.get(KEY_ID);

    request.onerror = () => reject(request.error);
    request.onsuccess = () => resolve(request.result || null);
  });
}
```

**Step 3: Update storeDPoPKey to accept namespace**

Replace the storeDPoPKey function (lines 50-68) with:

```typescript
async function storeDPoPKey(
  namespace: string,
  privateKey: CryptoKey,
  publicJwk: JsonWebKey
): Promise<void> {
  const db = await openDatabase(namespace);
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
```

**Step 4: Update getOrCreateDPoPKey to accept namespace**

Replace the getOrCreateDPoPKey function (lines 70-96) with:

```typescript
export async function getOrCreateDPoPKey(namespace: string): Promise<DPoPKeyData> {
  const keyData = await getDPoPKey(namespace);

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
  await storeDPoPKey(namespace, keyPair.privateKey, publicJwk);

  return {
    id: KEY_ID,
    privateKey: keyPair.privateKey,
    publicJwk,
    createdAt: Date.now(),
  };
}
```

**Step 5: Update createDPoPProof to accept namespace**

Replace the createDPoPProof function (lines 98-131) with:

```typescript
/**
 * Create a DPoP proof JWT
 */
export async function createDPoPProof(
  namespace: string,
  method: string,
  url: string,
  accessToken: string | null = null
): Promise<string> {
  const keyData = await getOrCreateDPoPKey(namespace);

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
```

**Step 6: Update clearDPoPKeys to accept namespace**

Replace the clearDPoPKeys function (lines 133-146) with:

```typescript
/**
 * Clear DPoP keys from IndexedDB
 */
export async function clearDPoPKeys(namespace: string): Promise<void> {
  const db = await openDatabase(namespace);
  return new Promise((resolve, reject) => {
    const tx = db.transaction(KEY_STORE, 'readwrite');
    const store = tx.objectStore(KEY_STORE);
    const request = store.clear();

    request.onerror = () => reject(request.error);
    request.onsuccess = () => resolve();
  });
}
```

**Step 7: Add signJwt import**

Add to the imports at line 2:

```typescript
import { sha256Base64Url, signJwt } from '../utils/crypto';
```

**Step 8: Verify build still fails**

Run: `npm run build`
Expected: Build fails — consumers don't pass namespace

**Step 9: Commit (WIP)**

```bash
git add src/auth/dpop.ts
git commit -m "wip: dpop module uses namespaced IndexedDB database"
```

---

## Task 6: Update tokens module

**Files:**
- Modify: `src/auth/tokens.ts:1-137`

**Step 1: Rewrite tokens module to use storage instance**

Replace the entire file contents:

```typescript
import { Storage } from '../storage/storage';
import { acquireLock, releaseLock } from '../storage/lock';
import { createDPoPProof } from './dpop';

const TOKEN_REFRESH_BUFFER_MS = 60000; // 60 seconds before expiry

function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

/**
 * Refresh tokens using the refresh token
 */
async function refreshTokens(
  storage: Storage,
  namespace: string,
  tokenUrl: string
): Promise<string> {
  const refreshToken = storage.get('refreshToken');
  const clientId = storage.get('clientId');

  if (!refreshToken || !clientId) {
    throw new Error('No refresh token available');
  }

  const dpopProof = await createDPoPProof(namespace, 'POST', tokenUrl);

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
  storage.set('accessToken', tokens.access_token);
  if (tokens.refresh_token) {
    storage.set('refreshToken', tokens.refresh_token);
  }

  const expiresAt = Date.now() + tokens.expires_in * 1000;
  storage.set('tokenExpiresAt', expiresAt.toString());

  return tokens.access_token;
}

/**
 * Get a valid access token, refreshing if necessary.
 * Uses multi-tab locking to prevent duplicate refresh requests.
 */
export async function getValidAccessToken(
  storage: Storage,
  namespace: string,
  tokenUrl: string
): Promise<string> {
  const accessToken = storage.get('accessToken');
  const expiresAt = parseInt(storage.get('tokenExpiresAt') || '0');

  // Check if token is still valid (with buffer)
  if (accessToken && Date.now() < expiresAt - TOKEN_REFRESH_BUFFER_MS) {
    return accessToken;
  }

  // Need to refresh - acquire lock first
  const lockKey = 'token_refresh';
  const lockValue = await acquireLock(namespace, lockKey);

  if (!lockValue) {
    // Failed to acquire lock, another tab is refreshing
    // Wait a bit and check cache again
    await sleep(100);
    const freshToken = storage.get('accessToken');
    const freshExpiry = parseInt(storage.get('tokenExpiresAt') || '0');
    if (freshToken && Date.now() < freshExpiry - TOKEN_REFRESH_BUFFER_MS) {
      return freshToken;
    }
    throw new Error('Failed to refresh token');
  }

  try {
    // Double-check after acquiring lock
    const freshToken = storage.get('accessToken');
    const freshExpiry = parseInt(storage.get('tokenExpiresAt') || '0');
    if (freshToken && Date.now() < freshExpiry - TOKEN_REFRESH_BUFFER_MS) {
      return freshToken;
    }

    // Actually refresh
    return await refreshTokens(storage, namespace, tokenUrl);
  } finally {
    releaseLock(namespace, lockKey, lockValue);
  }
}

/**
 * Store tokens from OAuth response
 */
export function storeTokens(
  storage: Storage,
  tokens: {
    access_token: string;
    refresh_token?: string;
    expires_in: number;
    sub?: string;
  }
): void {
  storage.set('accessToken', tokens.access_token);
  if (tokens.refresh_token) {
    storage.set('refreshToken', tokens.refresh_token);
  }

  const expiresAt = Date.now() + tokens.expires_in * 1000;
  storage.set('tokenExpiresAt', expiresAt.toString());

  if (tokens.sub) {
    storage.set('userDid', tokens.sub);
  }
}

/**
 * Check if we have a valid session
 */
export function hasValidSession(storage: Storage): boolean {
  const accessToken = storage.get('accessToken');
  const refreshToken = storage.get('refreshToken');
  return !!(accessToken || refreshToken);
}
```

**Step 2: Verify build still fails**

Run: `npm run build`
Expected: Build fails — oauth.ts and client.ts use old signatures

**Step 3: Commit (WIP)**

```bash
git add src/auth/tokens.ts
git commit -m "wip: tokens module accepts storage and namespace"
```

---

## Task 7: Update oauth module

**Files:**
- Modify: `src/auth/oauth.ts:1-141`

**Step 1: Rewrite oauth module**

Replace the entire file contents:

```typescript
import { Storage } from '../storage/storage';
import { createDPoPProof, clearDPoPKeys } from './dpop';
import { generateCodeVerifier, generateCodeChallenge, generateState } from './pkce';
import { storeTokens } from './tokens';

export interface LoginOptions {
  handle?: string;
  redirectUri?: string;
  scope?: string;
}

/**
 * Initiate OAuth login flow with PKCE
 */
export async function initiateLogin(
  storage: Storage,
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
  storage.set('codeVerifier', codeVerifier);
  storage.set('oauthState', state);
  storage.set('clientId', clientId);
  storage.set('redirectUri', redirectUri);

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

  if (options.scope) {
    params.set('scope', options.scope);
  }

  window.location.href = `${authorizeUrl}?${params.toString()}`;
}

/**
 * Handle OAuth callback - exchange code for tokens
 * Returns true if callback was handled, false if not a callback
 */
export async function handleOAuthCallback(
  storage: Storage,
  namespace: string,
  tokenUrl: string
): Promise<boolean> {
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
  const storedState = storage.get('oauthState');
  if (state !== storedState) {
    throw new Error('OAuth state mismatch - possible CSRF attack');
  }

  // Get stored values
  const codeVerifier = storage.get('codeVerifier');
  const clientId = storage.get('clientId');
  const redirectUri = storage.get('redirectUri');

  if (!codeVerifier || !clientId || !redirectUri) {
    throw new Error('Missing OAuth session data');
  }

  // Exchange code for tokens with DPoP
  const dpopProof = await createDPoPProof(namespace, 'POST', tokenUrl);

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
  storeTokens(storage, tokens);

  // Clean up OAuth state
  storage.remove('codeVerifier');
  storage.remove('oauthState');
  storage.remove('redirectUri');

  // Clear URL params
  window.history.replaceState({}, document.title, window.location.pathname);

  return true;
}

/**
 * Logout - clear all stored data
 */
export async function logout(
  storage: Storage,
  namespace: string,
  options: { reload?: boolean } = {}
): Promise<void> {
  storage.clear();
  await clearDPoPKeys(namespace);

  if (options.reload !== false) {
    window.location.reload();
  }
}
```

**Step 2: Verify build still fails**

Run: `npm run build`
Expected: Build fails — client.ts uses old signatures

**Step 3: Commit (WIP)**

```bash
git add src/auth/oauth.ts
git commit -m "wip: oauth module accepts storage and namespace"
```

---

## Task 8: Update graphql module

**Files:**
- Modify: `src/graphql.ts:1-53`

**Step 1: Rewrite graphql module**

Replace the entire file contents:

```typescript
import { createDPoPProof } from './auth/dpop';
import { getValidAccessToken } from './auth/tokens';
import { Storage } from './storage/storage';

export interface GraphQLResponse<T = unknown> {
  data?: T;
  errors?: Array<{ message: string; path?: string[] }>;
}

/**
 * Execute a GraphQL query or mutation
 */
export async function graphqlRequest<T = unknown>(
  storage: Storage,
  namespace: string,
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
    const token = await getValidAccessToken(storage, namespace, tokenUrl);
    if (!token) {
      throw new Error('Not authenticated');
    }

    // Create DPoP proof bound to this request
    const dpopProof = await createDPoPProof(namespace, 'POST', graphqlUrl, token);

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

**Step 2: Verify build still fails**

Run: `npm run build`
Expected: Build fails — client.ts uses old import

**Step 3: Commit (WIP)**

```bash
git add src/graphql.ts
git commit -m "wip: graphql module accepts storage and namespace"
```

---

## Task 9: Update client module (main integration)

**Files:**
- Modify: `src/client.ts:1-155`

**Step 1: Rewrite client module**

Replace the entire file contents:

```typescript
import { createStorageKeys } from './storage/keys';
import { createStorage, Storage } from './storage/storage';
import { getOrCreateDPoPKey } from './auth/dpop';
import { initiateLogin, handleOAuthCallback, logout as doLogout, LoginOptions } from './auth/oauth';
import { getValidAccessToken, hasValidSession } from './auth/tokens';
import { graphqlRequest } from './graphql';
import { generateNamespaceHash } from './utils/crypto';

export interface QuicksliceClientOptions {
  server: string;
  clientId: string;
  redirectUri?: string;
  scope?: string;
}

export interface User {
  did: string;
}

export class QuicksliceClient {
  private server: string;
  private clientId: string;
  private redirectUri?: string;
  private scope?: string;
  private graphqlUrl: string;
  private authorizeUrl: string;
  private tokenUrl: string;
  private initialized = false;
  private namespace: string = '';
  private storage: Storage | null = null;

  constructor(options: QuicksliceClientOptions) {
    this.server = options.server.replace(/\/$/, ''); // Remove trailing slash
    this.clientId = options.clientId;
    this.redirectUri = options.redirectUri;
    this.scope = options.scope;

    this.graphqlUrl = `${this.server}/graphql`;
    this.authorizeUrl = `${this.server}/oauth/authorize`;
    this.tokenUrl = `${this.server}/oauth/token`;
  }

  /**
   * Initialize the client - must be called before other methods
   */
  async init(): Promise<void> {
    if (this.initialized) return;

    // Generate namespace from clientId
    this.namespace = await generateNamespaceHash(this.clientId);

    // Create namespaced storage
    const keys = createStorageKeys(this.namespace);
    this.storage = createStorage(keys);

    // Ensure DPoP key exists
    await getOrCreateDPoPKey(this.namespace);

    this.initialized = true;
  }

  private getStorage(): Storage {
    if (!this.storage) {
      throw new Error('Client not initialized. Call init() first.');
    }
    return this.storage;
  }

  /**
   * Start OAuth login flow
   */
  async loginWithRedirect(options: LoginOptions = {}): Promise<void> {
    await this.init();
    await initiateLogin(this.getStorage(), this.authorizeUrl, this.clientId, {
      ...options,
      redirectUri: options.redirectUri || this.redirectUri,
      scope: options.scope || this.scope,
    });
  }

  /**
   * Handle OAuth callback after redirect
   * Returns true if callback was handled
   */
  async handleRedirectCallback(): Promise<boolean> {
    await this.init();
    return await handleOAuthCallback(this.getStorage(), this.namespace, this.tokenUrl);
  }

  /**
   * Logout and clear all stored data
   */
  async logout(options: { reload?: boolean } = {}): Promise<void> {
    await this.init();
    await doLogout(this.getStorage(), this.namespace, options);
  }

  /**
   * Check if user is authenticated
   */
  async isAuthenticated(): Promise<boolean> {
    await this.init();
    return hasValidSession(this.getStorage());
  }

  /**
   * Get current user's DID (from stored token data)
   * For richer profile info, use client.query() with your own schema
   */
  async getUser(): Promise<User | null> {
    await this.init();
    if (!hasValidSession(this.getStorage())) {
      return null;
    }

    const did = this.getStorage().get('userDid');
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
    return await getValidAccessToken(this.getStorage(), this.namespace, this.tokenUrl);
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
      this.getStorage(),
      this.namespace,
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
      this.getStorage(),
      this.namespace,
      this.graphqlUrl,
      this.tokenUrl,
      query,
      variables,
      false
    );
  }
}
```

**Step 2: Verify build passes**

Run: `npm run build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add src/client.ts
git commit -m "feat: integrate namespace throughout client for storage isolation"
```

---

## Task 10: Squash WIP commits and finalize

**Step 1: Interactive rebase to squash WIP commits**

Run: `git log --oneline -10` to see recent commits

**Step 2: Squash into a single feature commit**

```bash
git rebase -i HEAD~9
```

Mark all but the first commit as "squash", use message:

```
feat: add storage namespace isolation for multi-app support

Derive unique namespace from clientId hash to prevent storage collisions
when multiple apps use quickslice-client-js on the same domain.

Changes:
- Storage keys prefixed with 8-char SHA-256 hash of clientId
- IndexedDB database name includes namespace
- Lock keys include namespace
- Breaking: existing users will need to re-login once
```

**Step 3: Verify final build**

Run: `npm run build`
Expected: Build succeeds

---

## Summary

| Task | Description |
|------|-------------|
| 1 | Add namespace hash utility |
| 2 | Make storage keys dynamic |
| 3 | Update storage module |
| 4 | Update lock module |
| 5 | Update DPoP module |
| 6 | Update tokens module |
| 7 | Update oauth module |
| 8 | Update graphql module |
| 9 | Update client module (integration) |
| 10 | Squash commits and finalize |

**Total files modified:** 9
**Breaking change:** Users will appear logged out and need to re-login once after update.
