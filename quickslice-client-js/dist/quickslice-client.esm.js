// src/storage/keys.ts
function createStorageKeys(namespace) {
  return {
    clientId: `quickslice_${namespace}_client_id`,
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
      if (key === "codeVerifier" || key === "oauthState" || key === "redirectUri") {
        return sessionStorage.getItem(storageKey);
      }
      return localStorage.getItem(storageKey);
    },
    set(key, value) {
      const storageKey = keys[key];
      if (key === "codeVerifier" || key === "oauthState" || key === "redirectUri") {
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

// src/auth/session.ts
async function createSession(serverUrl, namespace, options) {
  const keyData = await getOrCreateDPoPKey(namespace);
  const dpopJkt = await computeJkt(keyData.publicJwk);
  const response = await fetch(`${serverUrl}/api/client/session`, {
    method: "POST",
    headers: {
      "Content-Type": "application/json"
    },
    credentials: "include",
    // Include cookies in the response
    body: JSON.stringify({
      clientId: options.clientId,
      dpopJkt,
      userDid: options.userDid,
      atpSessionId: options.atpSessionId
    })
  });
  if (!response.ok) {
    const errorData = await response.json().catch(() => ({}));
    throw new Error(
      `Failed to create session: ${errorData.message || response.statusText}`
    );
  }
  return await response.json();
}
async function getSession(serverUrl) {
  const response = await fetch(`${serverUrl}/api/client/session`, {
    method: "GET",
    credentials: "include"
    // Include session cookie
  });
  if (!response.ok) {
    return {
      authenticated: false,
      did: null,
      handle: null
    };
  }
  return await response.json();
}
async function destroySession(serverUrl) {
  await fetch(`${serverUrl}/api/client/session`, {
    method: "DELETE",
    credentials: "include"
    // Include session cookie
  });
}
async function computeJkt(jwk) {
  const canonical = JSON.stringify({
    crv: jwk.crv,
    kty: jwk.kty,
    x: jwk.x,
    y: jwk.y
  });
  return await sha256Base64Url(canonical);
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
async function handleOAuthCallback(storage, namespace, tokenUrl, serverUrl) {
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
    return null;
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
  const sessionInfo = await createSession(serverUrl, namespace, {
    clientId,
    userDid: tokens.sub,
    // DID from token response
    atpSessionId: tokens.session_id
    // ATP session ID if present
  });
  storage.remove("codeVerifier");
  storage.remove("oauthState");
  storage.remove("redirectUri");
  window.history.replaceState({}, document.title, window.location.pathname);
  return sessionInfo;
}
async function logout(storage, namespace, serverUrl, options = {}) {
  await destroySession(serverUrl);
  storage.clear();
  await clearDPoPKeys(namespace);
  if (options.reload !== false) {
    window.location.reload();
  }
}

// src/graphql.ts
async function graphqlRequest(namespace, graphqlUrl, query, variables = {}, requireAuth = false, signal) {
  const headers = {
    "Content-Type": "application/json"
  };
  if (requireAuth) {
    const dpopProof = await createDPoPProof(namespace, "POST", graphqlUrl);
    headers["DPoP"] = dpopProof;
  }
  const response = await fetch(graphqlUrl, {
    method: "POST",
    headers,
    body: JSON.stringify({ query, variables }),
    credentials: "include",
    // Include session cookie
    signal
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
    this.cachedSession = null;
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
   * Returns the session info if callback was handled, null otherwise
   */
  async handleRedirectCallback() {
    await this.init();
    const session = await handleOAuthCallback(
      this.getStorage(),
      this.namespace,
      this.tokenUrl,
      this.server
    );
    if (session) {
      this.cachedSession = session;
    }
    return session;
  }
  /**
   * Logout and clear all stored data
   */
  async logout(options = {}) {
    await this.init();
    this.cachedSession = null;
    await logout(this.getStorage(), this.namespace, this.server, options);
  }
  /**
   * Check if user is authenticated
   * Queries the server to verify session is valid
   */
  async isAuthenticated() {
    await this.init();
    const session = await getSession(this.server);
    this.cachedSession = session;
    return session.authenticated;
  }
  /**
   * Get current user info from session
   * For richer profile info, use client.query() with your own schema
   */
  async getUser() {
    await this.init();
    let session = this.cachedSession;
    if (!session) {
      session = await getSession(this.server);
      this.cachedSession = session;
    }
    if (!session.authenticated || !session.did) {
      return null;
    }
    return {
      did: session.did,
      handle: session.handle ?? void 0
    };
  }
  /**
   * Execute a GraphQL query (authenticated)
   * Uses session cookie for auth - no client-side token management
   */
  async query(query, variables = {}, options = {}) {
    await this.init();
    return await graphqlRequest(
      this.namespace,
      this.graphqlUrl,
      query,
      variables,
      true,
      options.signal
    );
  }
  /**
   * Execute a GraphQL mutation (authenticated)
   */
  async mutate(mutation, variables = {}, options = {}) {
    return this.query(mutation, variables, options);
  }
  /**
   * Execute a public GraphQL query (no auth)
   */
  async publicQuery(query, variables = {}, options = {}) {
    await this.init();
    return await graphqlRequest(
      this.namespace,
      this.graphqlUrl,
      query,
      variables,
      false,
      options.signal
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
export {
  LoginRequiredError,
  NetworkError,
  OAuthError,
  QuicksliceClient,
  QuicksliceError,
  createQuicksliceClient
};
//# sourceMappingURL=quickslice-client.esm.js.map
