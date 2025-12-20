"use strict";
var QuicksliceClient = (() => {
  var __defProp = Object.defineProperty;
  var __getOwnPropDesc = Object.getOwnPropertyDescriptor;
  var __getOwnPropNames = Object.getOwnPropertyNames;
  var __hasOwnProp = Object.prototype.hasOwnProperty;
  var __export = (target, all) => {
    for (var name in all)
      __defProp(target, name, { get: all[name], enumerable: true });
  };
  var __copyProps = (to, from, except, desc) => {
    if (from && typeof from === "object" || typeof from === "function") {
      for (let key of __getOwnPropNames(from))
        if (!__hasOwnProp.call(to, key) && key !== except)
          __defProp(to, key, { get: () => from[key], enumerable: !(desc = __getOwnPropDesc(from, key)) || desc.enumerable });
    }
    return to;
  };
  var __toCommonJS = (mod) => __copyProps(__defProp({}, "__esModule", { value: true }), mod);

  // src/index.ts
  var index_exports = {};
  __export(index_exports, {
    LoginRequiredError: () => LoginRequiredError,
    NetworkError: () => NetworkError,
    OAuthError: () => OAuthError,
    QuicksliceClient: () => QuicksliceClient,
    QuicksliceError: () => QuicksliceError,
    createQuicksliceClient: () => createQuicksliceClient
  });

  // src/storage/keys.ts
  function createStorageKeys(namespace) {
    return {
      accessToken: `quickslice_${namespace}_access_token`,
      refreshToken: `quickslice_${namespace}_refresh_token`,
      tokenExpiresAt: `quickslice_${namespace}_token_expires_at`,
      clientId: `quickslice_${namespace}_client_id`,
      userDid: `quickslice_${namespace}_user_did`,
      codeVerifier: `quickslice_${namespace}_code_verifier`,
      oauthState: `quickslice_${namespace}_oauth_state`,
      redirectUri: `quickslice_${namespace}_redirect_uri`
    };
  }

  // src/storage/storage.ts
  function createStorage(keys) {
    return {
      get(key) {
        const storageKey = keys[key];
        if (key === "codeVerifier" || key === "oauthState") {
          return sessionStorage.getItem(storageKey);
        }
        return localStorage.getItem(storageKey);
      },
      set(key, value) {
        const storageKey = keys[key];
        if (key === "codeVerifier" || key === "oauthState") {
          sessionStorage.setItem(storageKey, value);
        } else {
          localStorage.setItem(storageKey, value);
        }
      },
      remove(key) {
        const storageKey = keys[key];
        sessionStorage.removeItem(storageKey);
        localStorage.removeItem(storageKey);
      },
      clear() {
        Object.keys(keys).forEach((key) => {
          const storageKey = keys[key];
          sessionStorage.removeItem(storageKey);
          localStorage.removeItem(storageKey);
        });
      }
    };
  }

  // src/utils/base64url.ts
  function base64UrlEncode(buffer) {
    const bytes = buffer instanceof Uint8Array ? buffer : new Uint8Array(buffer);
    let binary = "";
    for (let i = 0; i < bytes.length; i++) {
      binary += String.fromCharCode(bytes[i]);
    }
    return btoa(binary).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
  }
  function generateRandomString(byteLength) {
    const bytes = new Uint8Array(byteLength);
    crypto.getRandomValues(bytes);
    return base64UrlEncode(bytes);
  }

  // src/utils/crypto.ts
  async function sha256Base64Url(data) {
    const encoder = new TextEncoder();
    const hash = await crypto.subtle.digest("SHA-256", encoder.encode(data));
    return base64UrlEncode(hash);
  }
  async function generateNamespaceHash(clientId) {
    const encoder = new TextEncoder();
    const hash = await crypto.subtle.digest("SHA-256", encoder.encode(clientId));
    const hashArray = Array.from(new Uint8Array(hash));
    const hashHex = hashArray.map((b) => b.toString(16).padStart(2, "0")).join("");
    return hashHex.substring(0, 8);
  }
  async function signJwt(header, payload, privateKey) {
    const encoder = new TextEncoder();
    const headerB64 = base64UrlEncode(encoder.encode(JSON.stringify(header)));
    const payloadB64 = base64UrlEncode(encoder.encode(JSON.stringify(payload)));
    const signingInput = `${headerB64}.${payloadB64}`;
    const signature = await crypto.subtle.sign(
      { name: "ECDSA", hash: "SHA-256" },
      privateKey,
      encoder.encode(signingInput)
    );
    const signatureB64 = base64UrlEncode(signature);
    return `${signingInput}.${signatureB64}`;
  }

  // src/auth/dpop.ts
  var DB_VERSION = 1;
  var KEY_STORE = "dpop-keys";
  var KEY_ID = "dpop-key";
  var dbPromises = /* @__PURE__ */ new Map();
  function getDbName(namespace) {
    return `quickslice-oauth-${namespace}`;
  }
  function openDatabase(namespace) {
    const existing = dbPromises.get(namespace);
    if (existing) return existing;
    const promise = new Promise((resolve, reject) => {
      const request = indexedDB.open(getDbName(namespace), DB_VERSION);
      request.onerror = () => reject(request.error);
      request.onsuccess = () => resolve(request.result);
      request.onupgradeneeded = (event) => {
        const db = event.target.result;
        if (!db.objectStoreNames.contains(KEY_STORE)) {
          db.createObjectStore(KEY_STORE, { keyPath: "id" });
        }
      };
    });
    dbPromises.set(namespace, promise);
    return promise;
  }
  async function getDPoPKey(namespace) {
    const db = await openDatabase(namespace);
    return new Promise((resolve, reject) => {
      const tx = db.transaction(KEY_STORE, "readonly");
      const store = tx.objectStore(KEY_STORE);
      const request = store.get(KEY_ID);
      request.onerror = () => reject(request.error);
      request.onsuccess = () => resolve(request.result || null);
    });
  }
  async function storeDPoPKey(namespace, privateKey, publicJwk) {
    const db = await openDatabase(namespace);
    return new Promise((resolve, reject) => {
      const tx = db.transaction(KEY_STORE, "readwrite");
      const store = tx.objectStore(KEY_STORE);
      const request = store.put({
        id: KEY_ID,
        privateKey,
        publicJwk,
        createdAt: Date.now()
      });
      request.onerror = () => reject(request.error);
      request.onsuccess = () => resolve();
    });
  }
  async function getOrCreateDPoPKey(namespace) {
    const keyData = await getDPoPKey(namespace);
    if (keyData) {
      return keyData;
    }
    const keyPair = await crypto.subtle.generateKey(
      { name: "ECDSA", namedCurve: "P-256" },
      false,
      // NOT extractable - critical for security
      ["sign"]
    );
    const publicJwk = await crypto.subtle.exportKey("jwk", keyPair.publicKey);
    await storeDPoPKey(namespace, keyPair.privateKey, publicJwk);
    return {
      id: KEY_ID,
      privateKey: keyPair.privateKey,
      publicJwk,
      createdAt: Date.now()
    };
  }
  async function createDPoPProof(namespace, method, url, accessToken = null) {
    const keyData = await getOrCreateDPoPKey(namespace);
    const { kty, crv, x, y } = keyData.publicJwk;
    const minimalJwk = { kty, crv, x, y };
    const header = {
      alg: "ES256",
      typ: "dpop+jwt",
      jwk: minimalJwk
    };
    const payload = {
      jti: generateRandomString(16),
      htm: method,
      htu: url,
      iat: Math.floor(Date.now() / 1e3)
    };
    if (accessToken) {
      payload.ath = await sha256Base64Url(accessToken);
    }
    return await signJwt(header, payload, keyData.privateKey);
  }
  async function clearDPoPKeys(namespace) {
    const db = await openDatabase(namespace);
    return new Promise((resolve, reject) => {
      const tx = db.transaction(KEY_STORE, "readwrite");
      const store = tx.objectStore(KEY_STORE);
      const request = store.clear();
      request.onerror = () => reject(request.error);
      request.onsuccess = () => resolve();
    });
  }

  // src/auth/pkce.ts
  function generateCodeVerifier() {
    return generateRandomString(32);
  }
  async function generateCodeChallenge(verifier) {
    const encoder = new TextEncoder();
    const data = encoder.encode(verifier);
    const hash = await crypto.subtle.digest("SHA-256", data);
    return base64UrlEncode(hash);
  }
  function generateState() {
    return generateRandomString(16);
  }

  // src/storage/lock.ts
  var LOCK_TIMEOUT = 5e3;
  function sleep(ms) {
    return new Promise((resolve) => setTimeout(resolve, ms));
  }
  function getLockKey(namespace, key) {
    return `quickslice_${namespace}_lock_${key}`;
  }
  async function acquireLock(namespace, key, timeout = LOCK_TIMEOUT) {
    const lockKey = getLockKey(namespace, key);
    const lockValue = `${Date.now()}_${Math.random()}`;
    const deadline = Date.now() + timeout;
    while (Date.now() < deadline) {
      const existing = localStorage.getItem(lockKey);
      if (existing) {
        const [timestamp] = existing.split("_");
        if (Date.now() - parseInt(timestamp) > LOCK_TIMEOUT) {
          localStorage.removeItem(lockKey);
        } else {
          await sleep(50);
          continue;
        }
      }
      localStorage.setItem(lockKey, lockValue);
      await sleep(10);
      if (localStorage.getItem(lockKey) === lockValue) {
        return lockValue;
      }
    }
    return null;
  }
  function releaseLock(namespace, key, lockValue) {
    const lockKey = getLockKey(namespace, key);
    if (localStorage.getItem(lockKey) === lockValue) {
      localStorage.removeItem(lockKey);
    }
  }

  // src/auth/tokens.ts
  var TOKEN_REFRESH_BUFFER_MS = 6e4;
  function sleep2(ms) {
    return new Promise((resolve) => setTimeout(resolve, ms));
  }
  async function refreshTokens(storage, namespace, tokenUrl) {
    const refreshToken = storage.get("refreshToken");
    const clientId = storage.get("clientId");
    if (!refreshToken || !clientId) {
      throw new Error("No refresh token available");
    }
    const dpopProof = await createDPoPProof(namespace, "POST", tokenUrl);
    const response = await fetch(tokenUrl, {
      method: "POST",
      headers: {
        "Content-Type": "application/x-www-form-urlencoded",
        DPoP: dpopProof
      },
      body: new URLSearchParams({
        grant_type: "refresh_token",
        refresh_token: refreshToken,
        client_id: clientId
      })
    });
    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(
        `Token refresh failed: ${errorData.error_description || response.statusText}`
      );
    }
    const tokens = await response.json();
    storage.set("accessToken", tokens.access_token);
    if (tokens.refresh_token) {
      storage.set("refreshToken", tokens.refresh_token);
    }
    const expiresAt = Date.now() + tokens.expires_in * 1e3;
    storage.set("tokenExpiresAt", expiresAt.toString());
    return tokens.access_token;
  }
  async function getValidAccessToken(storage, namespace, tokenUrl) {
    const accessToken = storage.get("accessToken");
    const expiresAt = parseInt(storage.get("tokenExpiresAt") || "0");
    if (accessToken && Date.now() < expiresAt - TOKEN_REFRESH_BUFFER_MS) {
      return accessToken;
    }
    const lockKey = "token_refresh";
    const lockValue = await acquireLock(namespace, lockKey);
    if (!lockValue) {
      await sleep2(100);
      const freshToken = storage.get("accessToken");
      const freshExpiry = parseInt(storage.get("tokenExpiresAt") || "0");
      if (freshToken && Date.now() < freshExpiry - TOKEN_REFRESH_BUFFER_MS) {
        return freshToken;
      }
      throw new Error("Failed to refresh token");
    }
    try {
      const freshToken = storage.get("accessToken");
      const freshExpiry = parseInt(storage.get("tokenExpiresAt") || "0");
      if (freshToken && Date.now() < freshExpiry - TOKEN_REFRESH_BUFFER_MS) {
        return freshToken;
      }
      return await refreshTokens(storage, namespace, tokenUrl);
    } finally {
      releaseLock(namespace, lockKey, lockValue);
    }
  }
  function storeTokens(storage, tokens) {
    storage.set("accessToken", tokens.access_token);
    if (tokens.refresh_token) {
      storage.set("refreshToken", tokens.refresh_token);
    }
    const expiresAt = Date.now() + tokens.expires_in * 1e3;
    storage.set("tokenExpiresAt", expiresAt.toString());
    if (tokens.sub) {
      storage.set("userDid", tokens.sub);
    }
  }
  function hasValidSession(storage) {
    const accessToken = storage.get("accessToken");
    const refreshToken = storage.get("refreshToken");
    return !!(accessToken || refreshToken);
  }

  // src/auth/oauth.ts
  async function initiateLogin(storage, authorizeUrl, clientId, options = {}) {
    const codeVerifier = generateCodeVerifier();
    const codeChallenge = await generateCodeChallenge(codeVerifier);
    const state = generateState();
    const redirectUri = options.redirectUri || window.location.origin + window.location.pathname;
    storage.set("codeVerifier", codeVerifier);
    storage.set("oauthState", state);
    storage.set("clientId", clientId);
    storage.set("redirectUri", redirectUri);
    const params = new URLSearchParams({
      client_id: clientId,
      redirect_uri: redirectUri,
      response_type: "code",
      code_challenge: codeChallenge,
      code_challenge_method: "S256",
      state
    });
    if (options.handle) {
      params.set("login_hint", options.handle);
    }
    if (options.scope) {
      params.set("scope", options.scope);
    }
    window.location.href = `${authorizeUrl}?${params.toString()}`;
  }
  async function handleOAuthCallback(storage, namespace, tokenUrl) {
    const params = new URLSearchParams(window.location.search);
    const code = params.get("code");
    const state = params.get("state");
    const error = params.get("error");
    if (error) {
      throw new Error(
        `OAuth error: ${error} - ${params.get("error_description") || ""}`
      );
    }
    if (!code || !state) {
      return false;
    }
    const storedState = storage.get("oauthState");
    if (state !== storedState) {
      throw new Error("OAuth state mismatch - possible CSRF attack");
    }
    const codeVerifier = storage.get("codeVerifier");
    const clientId = storage.get("clientId");
    const redirectUri = storage.get("redirectUri");
    if (!codeVerifier || !clientId || !redirectUri) {
      throw new Error("Missing OAuth session data");
    }
    const dpopProof = await createDPoPProof(namespace, "POST", tokenUrl);
    const tokenResponse = await fetch(tokenUrl, {
      method: "POST",
      headers: {
        "Content-Type": "application/x-www-form-urlencoded",
        DPoP: dpopProof
      },
      body: new URLSearchParams({
        grant_type: "authorization_code",
        code,
        redirect_uri: redirectUri,
        client_id: clientId,
        code_verifier: codeVerifier
      })
    });
    if (!tokenResponse.ok) {
      const errorData = await tokenResponse.json().catch(() => ({}));
      throw new Error(
        `Token exchange failed: ${errorData.error_description || tokenResponse.statusText}`
      );
    }
    const tokens = await tokenResponse.json();
    storeTokens(storage, tokens);
    storage.remove("codeVerifier");
    storage.remove("oauthState");
    storage.remove("redirectUri");
    window.history.replaceState({}, document.title, window.location.pathname);
    return true;
  }
  async function logout(storage, namespace, options = {}) {
    storage.clear();
    await clearDPoPKeys(namespace);
    if (options.reload !== false) {
      window.location.reload();
    }
  }

  // src/graphql.ts
  async function graphqlRequest(storage, namespace, graphqlUrl, tokenUrl, query, variables = {}, requireAuth = false) {
    const headers = {
      "Content-Type": "application/json"
    };
    if (requireAuth) {
      const token = await getValidAccessToken(storage, namespace, tokenUrl);
      if (!token) {
        throw new Error("Not authenticated");
      }
      const dpopProof = await createDPoPProof(namespace, "POST", graphqlUrl, token);
      headers["Authorization"] = `DPoP ${token}`;
      headers["DPoP"] = dpopProof;
    }
    const response = await fetch(graphqlUrl, {
      method: "POST",
      headers,
      body: JSON.stringify({ query, variables })
    });
    if (!response.ok) {
      throw new Error(`GraphQL request failed: ${response.statusText}`);
    }
    const result = await response.json();
    if (result.errors && result.errors.length > 0) {
      throw new Error(`GraphQL error: ${result.errors[0].message}`);
    }
    return result.data;
  }

  // src/client.ts
  var QuicksliceClient = class {
    constructor(options) {
      this.initialized = false;
      this.namespace = "";
      this.storage = null;
      this.server = options.server.replace(/\/$/, "");
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
    async init() {
      if (this.initialized) return;
      this.namespace = await generateNamespaceHash(this.clientId);
      const keys = createStorageKeys(this.namespace);
      this.storage = createStorage(keys);
      await getOrCreateDPoPKey(this.namespace);
      this.initialized = true;
    }
    getStorage() {
      if (!this.storage) {
        throw new Error("Client not initialized. Call init() first.");
      }
      return this.storage;
    }
    /**
     * Start OAuth login flow
     */
    async loginWithRedirect(options = {}) {
      await this.init();
      await initiateLogin(this.getStorage(), this.authorizeUrl, this.clientId, {
        ...options,
        redirectUri: options.redirectUri || this.redirectUri,
        scope: options.scope || this.scope
      });
    }
    /**
     * Handle OAuth callback after redirect
     * Returns true if callback was handled
     */
    async handleRedirectCallback() {
      await this.init();
      return await handleOAuthCallback(this.getStorage(), this.namespace, this.tokenUrl);
    }
    /**
     * Logout and clear all stored data
     */
    async logout(options = {}) {
      await this.init();
      await logout(this.getStorage(), this.namespace, options);
    }
    /**
     * Check if user is authenticated
     */
    async isAuthenticated() {
      await this.init();
      return hasValidSession(this.getStorage());
    }
    /**
     * Get current user's DID (from stored token data)
     * For richer profile info, use client.query() with your own schema
     */
    async getUser() {
      await this.init();
      if (!hasValidSession(this.getStorage())) {
        return null;
      }
      const did = this.getStorage().get("userDid");
      if (!did) {
        return null;
      }
      return { did };
    }
    /**
     * Get access token (auto-refreshes if needed)
     */
    async getAccessToken() {
      await this.init();
      return await getValidAccessToken(this.getStorage(), this.namespace, this.tokenUrl);
    }
    /**
     * Execute a GraphQL query (authenticated)
     */
    async query(query, variables = {}) {
      await this.init();
      return await graphqlRequest(
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
    async mutate(mutation, variables = {}) {
      return this.query(mutation, variables);
    }
    /**
     * Execute a public GraphQL query (no auth)
     */
    async publicQuery(query, variables = {}) {
      await this.init();
      return await graphqlRequest(
        this.getStorage(),
        this.namespace,
        this.graphqlUrl,
        this.tokenUrl,
        query,
        variables,
        false
      );
    }
  };

  // src/errors.ts
  var QuicksliceError = class extends Error {
    constructor(message) {
      super(message);
      this.name = "QuicksliceError";
    }
  };
  var LoginRequiredError = class extends QuicksliceError {
    constructor(message = "Login required") {
      super(message);
      this.name = "LoginRequiredError";
    }
  };
  var NetworkError = class extends QuicksliceError {
    constructor(message) {
      super(message);
      this.name = "NetworkError";
    }
  };
  var OAuthError = class extends QuicksliceError {
    constructor(code, description) {
      super(`OAuth error: ${code}${description ? ` - ${description}` : ""}`);
      this.name = "OAuthError";
      this.code = code;
      this.description = description;
    }
  };

  // src/index.ts
  async function createQuicksliceClient(options) {
    const client = new QuicksliceClient(options);
    await client.init();
    return client;
  }
  return __toCommonJS(index_exports);
})();
//# sourceMappingURL=quickslice-client.js.map
