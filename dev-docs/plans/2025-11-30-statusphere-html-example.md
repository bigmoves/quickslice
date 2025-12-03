# Statusphere HTML Example Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Create a single-file HTML example app that mirrors the statusphere-example-app functionality using quickslice's GraphQL API.

**Architecture:** A self-contained `index.html` with embedded CSS and JavaScript. Implements OAuth PKCE flow for authentication, GraphQL queries for fetching statuses/profiles, and mutations for posting statuses. No external dependencies.

**Tech Stack:** Vanilla HTML/CSS/JavaScript, Web Crypto API for PKCE, Fetch API for GraphQL, sessionStorage for tokens.

---

## Prerequisites

Before using this example, users must:
1. Have quickslice server running at `http://localhost:8080`
2. Register an OAuth client via quickslice admin UI or GraphQL mutation
3. Set the client's redirect URI to match where they serve the HTML file

---

## Task 1: Create Directory Structure and Base HTML

**Files:**
- Create: `examples/01-statusphere-html/index.html`

**Step 1: Create the examples directory**

```bash
mkdir -p examples/01-statusphere-html
```

**Step 2: Create base HTML structure**

Create `examples/01-statusphere-html/index.html`:

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <meta http-equiv="Content-Security-Policy" content="default-src 'self'; script-src 'self' 'unsafe-inline'; style-src 'self' 'unsafe-inline'; connect-src http://localhost:8080;">
  <title>Statusphere</title>
  <style>
    /* CSS will be added in Task 2 */
  </style>
</head>
<body>
  <div id="app">
    <header>
      <h1>Statusphere</h1>
      <p class="tagline">Set your status on the Atmosphere</p>
    </header>
    <main>
      <div id="auth-section"></div>
      <div id="emoji-picker"></div>
      <div id="status-feed"></div>
    </main>
    <div id="error-banner" class="hidden"></div>
  </div>
  <script>
    // JavaScript will be added in Tasks 3-7
  </script>
</body>
</html>
```

**Step 3: Verify file exists**

Run: `ls -la examples/01-statusphere-html/`
Expected: `index.html` listed

**Step 4: Commit**

```bash
git add examples/01-statusphere-html/index.html
git commit -m "feat(examples): add base HTML structure for statusphere example"
```

---

## Task 2: Add CSS Styling

**Files:**
- Modify: `examples/01-statusphere-html/index.html` (inside `<style>` tag)

**Step 1: Add CSS variables and reset**

Replace the `<style>` section with:

```css
/* CSS Reset */
*, *::before, *::after {
  box-sizing: border-box;
}
* {
  margin: 0;
}
body {
  line-height: 1.5;
  -webkit-font-smoothing: antialiased;
}
input, button {
  font: inherit;
}

/* CSS Variables */
:root {
  --primary-500: #0078ff;
  --primary-400: #339dff;
  --primary-600: #0060cc;
  --gray-100: #f5f5f5;
  --gray-200: #e5e5e5;
  --gray-500: #737373;
  --gray-700: #404040;
  --gray-900: #171717;
  --border-color: #e5e5e5;
  --error-bg: #fef2f2;
  --error-border: #fecaca;
  --error-text: #dc2626;
}

/* Layout */
body {
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
  background: var(--gray-100);
  color: var(--gray-900);
  min-height: 100vh;
  padding: 2rem 1rem;
}

#app {
  max-width: 600px;
  margin: 0 auto;
}

/* Header */
header {
  text-align: center;
  margin-bottom: 2rem;
}

header h1 {
  font-size: 2.5rem;
  color: var(--primary-500);
  margin-bottom: 0.25rem;
}

.tagline {
  color: var(--gray-500);
  font-size: 1rem;
}

/* Cards */
.card {
  background: white;
  border-radius: 0.5rem;
  padding: 1.5rem;
  margin-bottom: 1rem;
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
}

/* Auth Section */
.login-form {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.form-group {
  display: flex;
  flex-direction: column;
  gap: 0.25rem;
}

.form-group label {
  font-size: 0.875rem;
  font-weight: 500;
  color: var(--gray-700);
}

.form-group input {
  padding: 0.75rem;
  border: 1px solid var(--border-color);
  border-radius: 0.375rem;
  font-size: 1rem;
}

.form-group input:focus {
  outline: none;
  border-color: var(--primary-500);
  box-shadow: 0 0 0 3px rgba(0, 120, 255, 0.1);
}

.btn {
  padding: 0.75rem 1.5rem;
  border: none;
  border-radius: 0.375rem;
  font-size: 1rem;
  font-weight: 500;
  cursor: pointer;
  transition: background-color 0.15s;
}

.btn-primary {
  background: var(--primary-500);
  color: white;
}

.btn-primary:hover {
  background: var(--primary-600);
}

.btn-primary:disabled {
  background: var(--gray-200);
  color: var(--gray-500);
  cursor: not-allowed;
}

.btn-secondary {
  background: var(--gray-200);
  color: var(--gray-700);
}

.btn-secondary:hover {
  background: var(--border-color);
}

/* User Card */
.user-card {
  display: flex;
  align-items: center;
  justify-content: space-between;
}

.user-info {
  display: flex;
  align-items: center;
  gap: 0.75rem;
}

.user-avatar {
  width: 48px;
  height: 48px;
  border-radius: 50%;
  background: var(--gray-200);
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 1.5rem;
}

.user-avatar img {
  width: 100%;
  height: 100%;
  border-radius: 50%;
  object-fit: cover;
}

.user-name {
  font-weight: 600;
}

.user-handle {
  font-size: 0.875rem;
  color: var(--gray-500);
}

/* Emoji Picker */
.emoji-grid {
  display: grid;
  grid-template-columns: repeat(9, 1fr);
  gap: 0.5rem;
}

.emoji-btn {
  width: 100%;
  aspect-ratio: 1;
  font-size: 1.5rem;
  border: 2px solid var(--border-color);
  border-radius: 50%;
  background: white;
  cursor: pointer;
  transition: all 0.15s;
  display: flex;
  align-items: center;
  justify-content: center;
}

.emoji-btn:hover {
  background: rgba(0, 120, 255, 0.1);
  border-color: var(--primary-400);
}

.emoji-btn.selected {
  border-color: var(--primary-500);
  box-shadow: 0 0 0 3px rgba(0, 120, 255, 0.2);
}

.emoji-btn:disabled {
  opacity: 0.5;
  cursor: not-allowed;
}

.emoji-btn:disabled:hover {
  background: white;
  border-color: var(--border-color);
}

/* Status Feed */
.feed-title {
  font-size: 1.125rem;
  font-weight: 600;
  margin-bottom: 1rem;
  color: var(--gray-700);
}

.status-list {
  list-style: none;
  padding: 0;
}

.status-item {
  position: relative;
  padding-left: 2rem;
  padding-bottom: 1.5rem;
}

.status-item::before {
  content: "";
  position: absolute;
  left: 0.75rem;
  top: 1.5rem;
  bottom: 0;
  width: 2px;
  background: var(--border-color);
}

.status-item:last-child::before {
  display: none;
}

.status-item:last-child {
  padding-bottom: 0;
}

.status-emoji {
  position: absolute;
  left: 0;
  top: 0;
  font-size: 1.5rem;
}

.status-content {
  padding-top: 0.25rem;
}

.status-author {
  color: var(--primary-500);
  text-decoration: none;
  font-weight: 500;
}

.status-author:hover {
  text-decoration: underline;
}

.status-text {
  color: var(--gray-700);
}

.status-date {
  font-size: 0.875rem;
  color: var(--gray-500);
}

/* Error Banner */
#error-banner {
  position: fixed;
  top: 1rem;
  left: 50%;
  transform: translateX(-50%);
  background: var(--error-bg);
  border: 1px solid var(--error-border);
  color: var(--error-text);
  padding: 0.75rem 1rem;
  border-radius: 0.375rem;
  display: flex;
  align-items: center;
  gap: 0.75rem;
  max-width: 90%;
  z-index: 100;
}

#error-banner.hidden {
  display: none;
}

#error-banner button {
  background: none;
  border: none;
  color: var(--error-text);
  cursor: pointer;
  font-size: 1.25rem;
  line-height: 1;
}

/* Loading State */
.loading {
  text-align: center;
  color: var(--gray-500);
  padding: 2rem;
}

/* Responsive */
@media (max-width: 480px) {
  .emoji-grid {
    grid-template-columns: repeat(6, 1fr);
  }

  .emoji-btn {
    font-size: 1.25rem;
  }
}

/* Hidden utility */
.hidden {
  display: none !important;
}
```

**Step 2: Open in browser to verify styling**

Run: `open examples/01-statusphere-html/index.html` (macOS)
Expected: Page displays with blue header "Statusphere" and gray background

**Step 3: Commit**

```bash
git add examples/01-statusphere-html/index.html
git commit -m "feat(examples): add CSS styling matching statusphere design"
```

---

## Task 3: Add Constants and Storage Utilities

**Files:**
- Modify: `examples/01-statusphere-html/index.html` (inside `<script>` tag)

**Step 1: Add constants and storage helpers**

Replace the `<script>` section with:

```javascript
// =============================================================================
// CONSTANTS
// =============================================================================

const GRAPHQL_URL = 'http://localhost:8080/graphql';
const OAUTH_AUTHORIZE_URL = 'http://localhost:8080/oauth/authorize';
const OAUTH_TOKEN_URL = 'http://localhost:8080/oauth/token';

const EMOJIS = [
  '👍', '👎', '💙', '😧', '😤', '🙃', '😉', '😎', '🤩',
  '🥳', '😭', '😱', '🥺', '😡', '💀', '🤖', '👻', '👽',
  '🎃', '🤡', '💩', '🔥', '⭐', '🌈', '🍕', '🎉', '💯'
];

const STORAGE_KEYS = {
  accessToken: 'qs_access_token',
  refreshToken: 'qs_refresh_token',
  userDid: 'qs_user_did',
  codeVerifier: 'qs_code_verifier',
  oauthState: 'qs_oauth_state',
  clientId: 'qs_client_id'
};

// =============================================================================
// STORAGE UTILITIES
// =============================================================================

const storage = {
  get(key) {
    return sessionStorage.getItem(key);
  },
  set(key, value) {
    sessionStorage.setItem(key, value);
  },
  remove(key) {
    sessionStorage.removeItem(key);
  },
  clear() {
    Object.values(STORAGE_KEYS).forEach(key => sessionStorage.removeItem(key));
  }
};

// =============================================================================
// INITIALIZATION (more code to be added in subsequent tasks)
// =============================================================================

console.log('Statusphere loaded. EMOJIS:', EMOJIS.length);
```

**Step 2: Open in browser and check console**

Open browser DevTools console.
Expected: "Statusphere loaded. EMOJIS: 27"

**Step 3: Commit**

```bash
git add examples/01-statusphere-html/index.html
git commit -m "feat(examples): add constants and storage utilities"
```

---

## Task 4: Add PKCE and OAuth Functions

**Files:**
- Modify: `examples/01-statusphere-html/index.html` (inside `<script>` tag, after storage utilities)

**Step 1: Add PKCE helper functions**

Add after the storage utilities section:

```javascript
// =============================================================================
// PKCE UTILITIES
// =============================================================================

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

// =============================================================================
// OAUTH FUNCTIONS
// =============================================================================

async function initiateLogin(clientId, handle) {
  const codeVerifier = await generateCodeVerifier();
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
    login_hint: handle
  });

  window.location.href = `${OAUTH_AUTHORIZE_URL}?${params.toString()}`;
}

async function handleOAuthCallback() {
  const params = new URLSearchParams(window.location.search);
  const code = params.get('code');
  const state = params.get('state');
  const error = params.get('error');

  if (error) {
    throw new Error(`OAuth error: ${error} - ${params.get('error_description') || ''}`);
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

  // Exchange code for tokens
  const tokenResponse = await fetch(OAUTH_TOKEN_URL, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/x-www-form-urlencoded'
    },
    body: new URLSearchParams({
      grant_type: 'authorization_code',
      code: code,
      redirect_uri: redirectUri,
      client_id: clientId,
      code_verifier: codeVerifier
    })
  });

  if (!tokenResponse.ok) {
    const errorData = await tokenResponse.json().catch(() => ({}));
    throw new Error(`Token exchange failed: ${errorData.error_description || tokenResponse.statusText}`);
  }

  const tokens = await tokenResponse.json();

  // Store tokens
  storage.set(STORAGE_KEYS.accessToken, tokens.access_token);
  if (tokens.refresh_token) {
    storage.set(STORAGE_KEYS.refreshToken, tokens.refresh_token);
  }

  // Extract DID from token response (sub claim) or we'll fetch it later
  if (tokens.sub) {
    storage.set(STORAGE_KEYS.userDid, tokens.sub);
  }

  // Clean up OAuth state
  storage.remove(STORAGE_KEYS.codeVerifier);
  storage.remove(STORAGE_KEYS.oauthState);

  // Clear URL params
  window.history.replaceState({}, document.title, window.location.pathname);

  return true;
}

function logout() {
  storage.clear();
  window.location.reload();
}

function isLoggedIn() {
  return !!storage.get(STORAGE_KEYS.accessToken);
}

function getAccessToken() {
  return storage.get(STORAGE_KEYS.accessToken);
}

function getUserDid() {
  return storage.get(STORAGE_KEYS.userDid);
}
```

**Step 2: Test PKCE generation in console**

Open browser DevTools and run:
```javascript
generateCodeVerifier().then(v => console.log('Verifier:', v));
```
Expected: Random base64url string ~43 characters

**Step 3: Commit**

```bash
git add examples/01-statusphere-html/index.html
git commit -m "feat(examples): add PKCE and OAuth authentication functions"
```

---

## Task 5: Add GraphQL Query Functions

**Files:**
- Modify: `examples/01-statusphere-html/index.html` (inside `<script>` tag, after OAuth functions)

**Step 1: Add GraphQL utility and queries**

Add after the OAuth functions section:

```javascript
// =============================================================================
// GRAPHQL UTILITIES
// =============================================================================

async function graphqlQuery(query, variables = {}, requireAuth = false) {
  const headers = {
    'Content-Type': 'application/json'
  };

  if (requireAuth) {
    const token = getAccessToken();
    if (!token) {
      throw new Error('Not authenticated');
    }
    headers['Authorization'] = `Bearer ${token}`;
  }

  const response = await fetch(GRAPHQL_URL, {
    method: 'POST',
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

// =============================================================================
// DATA FETCHING
// =============================================================================

async function fetchStatuses() {
  const query = `
    query GetStatuses {
      xyzStatusphereStatus(
        first: 20
        sortBy: [{ field: "createdAt", direction: DESC }]
      ) {
        edges {
          node {
            uri
            did
            status
            createdAt
            appBskyActorProfileByDid {
              actorHandle
              displayName
            }
          }
        }
      }
    }
  `;

  const data = await graphqlQuery(query);
  return data.xyzStatusphereStatus?.edges?.map(e => e.node) || [];
}

async function fetchUserProfile(did) {
  const query = `
    query GetProfile($did: String!) {
      appBskyActorProfile(
        where: { did: { eq: $did } }
        first: 1
      ) {
        edges {
          node {
            did
            actorHandle
            displayName
            avatar {
              url
            }
          }
        }
      }
    }
  `;

  const data = await graphqlQuery(query, { did });
  const edges = data.appBskyActorProfile?.edges || [];
  return edges.length > 0 ? edges[0].node : null;
}

async function postStatus(emoji) {
  const mutation = `
    mutation CreateStatus($status: String!, $createdAt: DateTime!) {
      createXyzStatusphereStatus(
        input: { status: $status, createdAt: $createdAt }
      ) {
        uri
        status
        createdAt
      }
    }
  `;

  const variables = {
    status: emoji,
    createdAt: new Date().toISOString()
  };

  const data = await graphqlQuery(mutation, variables, true);
  return data.createXyzStatusphereStatus;
}
```

**Step 2: Verify syntax by checking console for errors**

Refresh the page, check DevTools console.
Expected: No JavaScript errors

**Step 3: Commit**

```bash
git add examples/01-statusphere-html/index.html
git commit -m "feat(examples): add GraphQL query and mutation functions"
```

---

## Task 6: Add UI Rendering Functions

**Files:**
- Modify: `examples/01-statusphere-html/index.html` (inside `<script>` tag, after GraphQL functions)

**Step 1: Add UI rendering functions**

Add after the data fetching section:

```javascript
// =============================================================================
// UI RENDERING
// =============================================================================

function showError(message) {
  const banner = document.getElementById('error-banner');
  banner.innerHTML = `
    <span>${escapeHtml(message)}</span>
    <button onclick="hideError()">&times;</button>
  `;
  banner.classList.remove('hidden');
}

function hideError() {
  document.getElementById('error-banner').classList.add('hidden');
}

function escapeHtml(text) {
  const div = document.createElement('div');
  div.textContent = text;
  return div.innerHTML;
}

function formatDate(dateString) {
  const date = new Date(dateString);
  const now = new Date();
  const isToday = date.toDateString() === now.toDateString();

  if (isToday) {
    return 'today';
  }

  return date.toLocaleDateString('en-US', {
    month: 'short',
    day: 'numeric',
    year: date.getFullYear() !== now.getFullYear() ? 'numeric' : undefined
  });
}

function renderLoginForm() {
  const container = document.getElementById('auth-section');
  const savedClientId = storage.get(STORAGE_KEYS.clientId) || '';

  container.innerHTML = `
    <div class="card">
      <form class="login-form" onsubmit="handleLogin(event)">
        <div class="form-group">
          <label for="client-id">OAuth Client ID</label>
          <input
            type="text"
            id="client-id"
            placeholder="your-client-id"
            value="${escapeHtml(savedClientId)}"
            required
          >
        </div>
        <div class="form-group">
          <label for="handle">Bluesky Handle</label>
          <input
            type="text"
            id="handle"
            placeholder="you.bsky.social"
            required
          >
        </div>
        <button type="submit" class="btn btn-primary">Login with Bluesky</button>
      </form>
      <p style="margin-top: 1rem; font-size: 0.875rem; color: var(--gray-500); text-align: center;">
        Don't have a Bluesky account? <a href="https://bsky.app" target="_blank">Sign up</a>
      </p>
    </div>
  `;
}

function renderUserCard(profile) {
  const container = document.getElementById('auth-section');
  const displayName = profile?.displayName || 'User';
  const handle = profile?.actorHandle || 'unknown';
  const avatarUrl = profile?.avatar?.url;

  container.innerHTML = `
    <div class="card user-card">
      <div class="user-info">
        <div class="user-avatar">
          ${avatarUrl
            ? `<img src="${escapeHtml(avatarUrl)}" alt="Avatar">`
            : '👤'}
        </div>
        <div>
          <div class="user-name">Hi, ${escapeHtml(displayName)}!</div>
          <div class="user-handle">@${escapeHtml(handle)}</div>
        </div>
      </div>
      <button class="btn btn-secondary" onclick="logout()">Logout</button>
    </div>
  `;
}

function renderEmojiPicker(currentStatus, enabled = true) {
  const container = document.getElementById('emoji-picker');

  container.innerHTML = `
    <div class="card">
      <div class="emoji-grid">
        ${EMOJIS.map(emoji => `
          <button
            class="emoji-btn ${emoji === currentStatus ? 'selected' : ''}"
            onclick="selectStatus('${emoji}')"
            ${!enabled ? 'disabled' : ''}
            title="${enabled ? 'Set status' : 'Login to set status'}"
          >
            ${emoji}
          </button>
        `).join('')}
      </div>
    </div>
  `;
}

function renderStatusFeed(statuses) {
  const container = document.getElementById('status-feed');

  if (statuses.length === 0) {
    container.innerHTML = `
      <div class="card">
        <p class="loading">No statuses yet. Be the first to post!</p>
      </div>
    `;
    return;
  }

  container.innerHTML = `
    <div class="card">
      <h2 class="feed-title">Recent Statuses</h2>
      <ul class="status-list">
        ${statuses.map(status => {
          const handle = status.appBskyActorProfileByDid?.actorHandle || status.did;
          const displayHandle = handle.startsWith('did:') ? handle.substring(0, 20) + '...' : handle;
          const profileUrl = handle.startsWith('did:')
            ? `https://bsky.app/profile/${status.did}`
            : `https://bsky.app/profile/${handle}`;

          return `
            <li class="status-item">
              <span class="status-emoji">${status.status}</span>
              <div class="status-content">
                <span class="status-text">
                  <a href="${profileUrl}" target="_blank" class="status-author">@${escapeHtml(displayHandle)}</a>
                  is feeling ${status.status}
                </span>
                <div class="status-date">${formatDate(status.createdAt)}</div>
              </div>
            </li>
          `;
        }).join('')}
      </ul>
    </div>
  `;
}

function renderLoading(container) {
  document.getElementById(container).innerHTML = `
    <div class="card">
      <p class="loading">Loading...</p>
    </div>
  `;
}
```

**Step 2: Verify syntax by checking console**

Refresh page, check DevTools console.
Expected: No JavaScript errors

**Step 3: Commit**

```bash
git add examples/01-statusphere-html/index.html
git commit -m "feat(examples): add UI rendering functions"
```

---

## Task 7: Add Event Handlers and Main Initialization

**Files:**
- Modify: `examples/01-statusphere-html/index.html` (inside `<script>` tag, replace the console.log at the end)

**Step 1: Add event handlers and main function**

Replace the `console.log('Statusphere loaded...')` line with:

```javascript
// =============================================================================
// EVENT HANDLERS
// =============================================================================

async function handleLogin(event) {
  event.preventDefault();

  const clientId = document.getElementById('client-id').value.trim();
  const handle = document.getElementById('handle').value.trim();

  if (!clientId || !handle) {
    showError('Please enter both Client ID and Handle');
    return;
  }

  try {
    await initiateLogin(clientId, handle);
  } catch (error) {
    showError(`Login failed: ${error.message}`);
  }
}

async function selectStatus(emoji) {
  if (!isLoggedIn()) {
    showError('Please login to set your status');
    return;
  }

  try {
    // Disable buttons while posting
    document.querySelectorAll('.emoji-btn').forEach(btn => btn.disabled = true);

    await postStatus(emoji);

    // Refresh the page to show new status
    window.location.reload();
  } catch (error) {
    showError(`Failed to post status: ${error.message}`);
    // Re-enable buttons
    document.querySelectorAll('.emoji-btn').forEach(btn => btn.disabled = false);
  }
}

// =============================================================================
// MAIN INITIALIZATION
// =============================================================================

async function main() {
  try {
    // Check if this is an OAuth callback
    const isCallback = await handleOAuthCallback();
    if (isCallback) {
      console.log('OAuth callback handled successfully');
    }
  } catch (error) {
    showError(`Authentication failed: ${error.message}`);
    storage.clear();
  }

  // Render auth section
  if (isLoggedIn()) {
    const did = getUserDid();
    if (did) {
      try {
        const profile = await fetchUserProfile(did);
        renderUserCard(profile);
      } catch (error) {
        console.error('Failed to fetch profile:', error);
        renderUserCard(null);
      }
    } else {
      renderUserCard(null);
    }
  } else {
    renderLoginForm();
  }

  // Render emoji picker (enabled only if logged in)
  renderEmojiPicker(null, isLoggedIn());

  // Fetch and render statuses
  renderLoading('status-feed');
  try {
    const statuses = await fetchStatuses();
    renderStatusFeed(statuses);
  } catch (error) {
    console.error('Failed to fetch statuses:', error);
    document.getElementById('status-feed').innerHTML = `
      <div class="card">
        <p class="loading" style="color: var(--error-text);">
          Failed to load statuses. Is the quickslice server running at localhost:8080?
        </p>
      </div>
    `;
  }
}

// Run on page load
main();
```

**Step 2: Test the complete page**

1. Open `examples/01-statusphere-html/index.html` in browser
2. Expected: Login form visible, emoji picker visible but disabled, status feed shows loading then either statuses or error message

**Step 3: Commit**

```bash
git add examples/01-statusphere-html/index.html
git commit -m "feat(examples): add event handlers and main initialization"
```

---

## Task 8: Add README for the Example

**Files:**
- Create: `examples/01-statusphere-html/README.md`

**Step 1: Create README**

Create `examples/01-statusphere-html/README.md`:

```markdown
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
```

**Step 2: Verify README renders correctly**

View in a markdown viewer or GitHub.
Expected: Formatted documentation with code blocks

**Step 3: Commit**

```bash
git add examples/01-statusphere-html/README.md
git commit -m "docs(examples): add README for statusphere HTML example"
```

---

## Task 9: Final Testing and Verification

**Step 1: Verify file structure**

Run: `ls -la examples/01-statusphere-html/`
Expected:
```
index.html
README.md
```

**Step 2: Verify HTML is valid**

Run: `head -50 examples/01-statusphere-html/index.html`
Expected: Valid HTML structure starting with `<!DOCTYPE html>`

**Step 3: Test in browser without server**

Open `examples/01-statusphere-html/index.html` directly in browser.
Expected:
- Page loads with "Statusphere" header
- Login form displays
- Emoji picker shows (disabled)
- Error message about server connection (expected without quickslice running)

**Step 4: Test with quickslice server (if available)**

1. Start quickslice: `make run`
2. Open `http://localhost:8080` to verify server is running
3. Serve the example: `python -m http.server 3000 -d examples/01-statusphere-html`
4. Open `http://localhost:3000/index.html`
5. Expected: Statuses load from the server

**Step 5: Final commit**

```bash
git add -A
git status
# If any uncommitted changes:
git commit -m "chore(examples): finalize statusphere HTML example"
```

---

## Summary

| Task | Description | Files |
|------|-------------|-------|
| 1 | Base HTML structure | `examples/01-statusphere-html/index.html` |
| 2 | CSS styling | Same file (style section) |
| 3 | Constants and storage | Same file (script section) |
| 4 | PKCE and OAuth | Same file (script section) |
| 5 | GraphQL functions | Same file (script section) |
| 6 | UI rendering | Same file (script section) |
| 7 | Event handlers and init | Same file (script section) |
| 8 | README documentation | `examples/01-statusphere-html/README.md` |
| 9 | Final testing | Verification only |

**Total commits:** 8 focused commits following conventional commit format.
