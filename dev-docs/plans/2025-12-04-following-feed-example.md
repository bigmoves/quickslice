# Following Feed Example Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Transform the 02-following-feed example from a Statusphere clone into a Bluesky profile posts viewer with URL-based routing.

**Architecture:** Single HTML file with client-side routing using History API. Login redirects to `/profile/{handle}`, profile pages fetch posts via GraphQL query filtering by actorHandle and excluding replies. Read-only (no posting).

**Tech Stack:** Vanilla HTML/CSS/JavaScript, GraphQL, OAuth PKCE

---

## Task 1: Update Branding and Remove Statusphere Elements

**Files:**
- Modify: `examples/02-following-feed/index.html`

**Step 1: Update HTML header and title**

Change the title and header from "Statusphere" to "Following Feed":

```html
<title>Following Feed</title>
```

```html
<header>
  <h1>Following Feed</h1>
  <p class="tagline">View posts on the Atmosphere</p>
</header>
```

**Step 2: Remove emoji picker container from HTML**

Remove this line from the main section:
```html
<div id="emoji-picker"></div>
```

**Step 3: Remove status feed container, replace with posts feed**

Change:
```html
<div id="status-feed"></div>
```
To:
```html
<div id="posts-feed"></div>
```

**Step 4: Test manually**

Open `examples/02-following-feed/index.html` in browser.
Expected: See "Following Feed" title, no emoji picker section.

**Step 5: Commit**

```bash
git add examples/02-following-feed/index.html
git commit -m "feat(example): update 02-following-feed branding, remove emoji picker"
```

---

## Task 2: Remove Emoji-Related JavaScript

**Files:**
- Modify: `examples/02-following-feed/index.html`

**Step 1: Remove EMOJIS constant**

Delete these lines from the CONSTANTS section:
```javascript
const EMOJIS = [
  '👍', '👎', '💙', '😧', '😤', '🙃', '😉', '😎', '🤩',
  '🥳', '😭', '😱', '🥺', '😡', '💀', '🤖', '👻', '👽',
  '🎃', '🤡', '💩', '🔥', '⭐', '🌈', '🍕', '🎉', '💯'
];
```

**Step 2: Remove postStatus function**

Delete the entire `postStatus` function (lines ~645-665):
```javascript
async function postStatus(emoji) {
  // ... entire function
}
```

**Step 3: Remove renderEmojiPicker function**

Delete the entire `renderEmojiPicker` function (lines ~765-784):
```javascript
function renderEmojiPicker(currentStatus, enabled = true) {
  // ... entire function
}
```

**Step 4: Remove selectStatus function**

Delete the entire `selectStatus` function (lines ~857-876):
```javascript
async function selectStatus(emoji) {
  // ... entire function
}
```

**Step 5: Test manually**

Open in browser, check console for errors.
Expected: No JavaScript errors, page loads without emoji picker.

**Step 6: Commit**

```bash
git add examples/02-following-feed/index.html
git commit -m "feat(example): remove emoji picker and status posting code"
```

---

## Task 3: Remove Statusphere Status Feed Code

**Files:**
- Modify: `examples/02-following-feed/index.html`

**Step 1: Remove fetchStatuses function**

Delete the entire `fetchStatuses` function that queries `xyzStatusphereStatus`:
```javascript
async function fetchStatuses() {
  const query = `
    query GetStatuses {
      xyzStatusphereStatus(
        // ...
      ) {
        // ...
      }
    }
  `;
  // ...
}
```

**Step 2: Remove renderStatusFeed function**

Delete the entire `renderStatusFeed` function.

**Step 3: Remove status-related CSS**

Delete these CSS blocks:
- `.status-list`
- `.status-item` and its `::before` pseudo-elements
- `.status-emoji`
- `.status-content`
- `.status-author`
- `.status-text`
- `.status-date`

**Step 4: Commit**

```bash
git add examples/02-following-feed/index.html
git commit -m "feat(example): remove statusphere feed code and styles"
```

---

## Task 4: Add Client-Side Routing

**Files:**
- Modify: `examples/02-following-feed/index.html`

**Step 1: Add router utility object**

Add after the storage utilities section:

```javascript
// =============================================================================
// ROUTING
// =============================================================================

const router = {
  getPath() {
    return window.location.pathname;
  },

  getProfileHandle() {
    const match = this.getPath().match(/^\/profile\/(.+)$/);
    return match ? decodeURIComponent(match[1]) : null;
  },

  navigateTo(path) {
    window.history.pushState({}, '', path);
    renderApp();
  },

  isProfilePage() {
    return this.getPath().startsWith('/profile/');
  }
};

// Handle browser back/forward
window.addEventListener('popstate', () => renderApp());
```

**Step 2: Commit**

```bash
git add examples/02-following-feed/index.html
git commit -m "feat(example): add client-side routing utilities"
```

---

## Task 5: Add Posts Fetching Function

**Files:**
- Modify: `examples/02-following-feed/index.html`

**Step 1: Add fetchPosts function**

Add to the DATA FETCHING section:

```javascript
async function fetchPosts(handle) {
  const query = `
    query GetPosts($handle: String!) {
      appBskyFeedPost(
        sortBy: [{direction: DESC, field: createdAt}]
        where: {
          and: [
            {actorHandle: {eq: $handle}},
            {reply: {isNull: true}}
          ]
        }
      ) {
        edges {
          node {
            uri
            text
            createdAt
            appBskyActorProfileByDid {
              displayName
              actorHandle
              avatar {
                url
              }
            }
            embed {
              ... on AppBskyEmbedImages {
                images {
                  image {
                    url
                  }
                }
              }
            }
          }
        }
      }
    }
  `;

  const data = await graphqlQuery(query, { handle }, true);
  return data.appBskyFeedPost?.edges?.map(e => e.node) || [];
}
```

**Step 2: Commit**

```bash
git add examples/02-following-feed/index.html
git commit -m "feat(example): add fetchPosts function for profile posts"
```

---

## Task 6: Add Profile Fetching Function

**Files:**
- Modify: `examples/02-following-feed/index.html`

**Step 1: Add fetchProfile function**

Add to the DATA FETCHING section:

```javascript
async function fetchProfile(handle) {
  const query = `
    query GetProfile($handle: String!) {
      appBskyActorProfile(
        where: {actorHandle: {eq: $handle}}
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

  const data = await graphqlQuery(query, { handle }, true);
  const edges = data.appBskyActorProfile?.edges || [];
  return edges.length > 0 ? edges[0].node : null;
}
```

**Step 2: Commit**

```bash
git add examples/02-following-feed/index.html
git commit -m "feat(example): add fetchProfile function"
```

---

## Task 7: Add Post Card CSS

**Files:**
- Modify: `examples/02-following-feed/index.html`

**Step 1: Add post card styles**

Add to the CSS section:

```css
/* Post Cards */
.posts-list {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.post-card {
  background: white;
  border-radius: 0.5rem;
  padding: 1rem;
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
}

.post-header {
  display: flex;
  align-items: center;
  gap: 0.75rem;
  margin-bottom: 0.75rem;
}

.post-avatar {
  width: 40px;
  height: 40px;
  border-radius: 50%;
  background: var(--gray-200);
  overflow: hidden;
  flex-shrink: 0;
}

.post-avatar img {
  width: 100%;
  height: 100%;
  object-fit: cover;
}

.post-author-info {
  flex: 1;
  min-width: 0;
}

.post-author-name {
  font-weight: 600;
  color: var(--gray-900);
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

.post-author-handle {
  font-size: 0.875rem;
  color: var(--gray-500);
}

.post-text {
  color: var(--gray-700);
  line-height: 1.5;
  white-space: pre-wrap;
  word-break: break-word;
}

.post-images {
  display: grid;
  gap: 0.5rem;
  margin-top: 0.75rem;
}

.post-images.single {
  grid-template-columns: 1fr;
}

.post-images.multiple {
  grid-template-columns: repeat(2, 1fr);
}

.post-images img {
  width: 100%;
  border-radius: 0.5rem;
  max-height: 300px;
  object-fit: cover;
}

.post-date {
  font-size: 0.75rem;
  color: var(--gray-500);
  margin-top: 0.75rem;
}

/* Profile Header */
.profile-header {
  display: flex;
  align-items: center;
  gap: 1rem;
  margin-bottom: 1.5rem;
}

.profile-avatar {
  width: 64px;
  height: 64px;
  border-radius: 50%;
  background: var(--gray-200);
  overflow: hidden;
  flex-shrink: 0;
}

.profile-avatar img {
  width: 100%;
  height: 100%;
  object-fit: cover;
}

.profile-name {
  font-size: 1.25rem;
  font-weight: 600;
}

.profile-handle {
  color: var(--gray-500);
}
```

**Step 2: Commit**

```bash
git add examples/02-following-feed/index.html
git commit -m "feat(example): add post card and profile header CSS"
```

---

## Task 8: Add renderPostsFeed Function

**Files:**
- Modify: `examples/02-following-feed/index.html`

**Step 1: Add renderPostsFeed function**

Add to the UI RENDERING section:

```javascript
function renderPostsFeed(posts) {
  const container = document.getElementById('posts-feed');

  if (posts.length === 0) {
    container.innerHTML = `
      <div class="card">
        <p class="loading">No posts found.</p>
      </div>
    `;
    return;
  }

  container.innerHTML = `
    <div class="posts-list">
      ${posts.map(post => {
        const profile = post.appBskyActorProfileByDid;
        const displayName = profile?.displayName || profile?.actorHandle || 'Unknown';
        const handle = profile?.actorHandle || '';
        const avatarUrl = profile?.avatar?.url;
        const images = post.embed?.images || [];

        return `
          <div class="post-card">
            <div class="post-header">
              <div class="post-avatar">
                ${avatarUrl ? `<img src="${escapeHtml(avatarUrl)}" alt="">` : ''}
              </div>
              <div class="post-author-info">
                <div class="post-author-name">${escapeHtml(displayName)}</div>
                <div class="post-author-handle">@${escapeHtml(handle)}</div>
              </div>
            </div>
            <div class="post-text">${escapeHtml(post.text || '')}</div>
            ${images.length > 0 ? `
              <div class="post-images ${images.length === 1 ? 'single' : 'multiple'}">
                ${images.map(img => `
                  <img src="${escapeHtml(img.image?.url || '')}" alt="" loading="lazy">
                `).join('')}
              </div>
            ` : ''}
            <div class="post-date">${formatDate(post.createdAt)}</div>
          </div>
        `;
      }).join('')}
    </div>
  `;
}
```

**Step 2: Commit**

```bash
git add examples/02-following-feed/index.html
git commit -m "feat(example): add renderPostsFeed function"
```

---

## Task 9: Add renderProfileHeader Function

**Files:**
- Modify: `examples/02-following-feed/index.html`

**Step 1: Add renderProfileHeader function**

Add to the UI RENDERING section:

```javascript
function renderProfileHeader(profile) {
  const displayName = profile?.displayName || profile?.actorHandle || 'Unknown';
  const handle = profile?.actorHandle || '';
  const avatarUrl = profile?.avatar?.url;

  return `
    <div class="profile-header">
      <div class="profile-avatar">
        ${avatarUrl ? `<img src="${escapeHtml(avatarUrl)}" alt="">` : ''}
      </div>
      <div>
        <div class="profile-name">${escapeHtml(displayName)}</div>
        <div class="profile-handle">@${escapeHtml(handle)}</div>
      </div>
    </div>
  `;
}
```

**Step 2: Commit**

```bash
git add examples/02-following-feed/index.html
git commit -m "feat(example): add renderProfileHeader function"
```

---

## Task 10: Add renderProfilePage Function

**Files:**
- Modify: `examples/02-following-feed/index.html`

**Step 1: Add renderProfilePage function**

Add to the UI RENDERING section:

```javascript
async function renderProfilePage(handle) {
  const container = document.getElementById('posts-feed');

  // Show loading state
  container.innerHTML = `
    <div class="card">
      <p class="loading">Loading profile...</p>
    </div>
  `;

  try {
    // Fetch profile and posts in parallel
    const [profile, posts] = await Promise.all([
      fetchProfile(handle),
      fetchPosts(handle)
    ]);

    if (!profile) {
      container.innerHTML = `
        <div class="card">
          <p class="loading" style="color: var(--error-text);">Profile not found: @${escapeHtml(handle)}</p>
        </div>
      `;
      return;
    }

    // Render profile header + posts
    container.innerHTML = `
      <div class="card">
        ${renderProfileHeader(profile)}
      </div>
    `;

    renderPostsFeed(posts);
  } catch (error) {
    console.error('Failed to load profile:', error);
    container.innerHTML = `
      <div class="card">
        <p class="loading" style="color: var(--error-text);">
          Failed to load profile. ${error.message}
        </p>
      </div>
    `;
  }
}
```

**Step 2: Commit**

```bash
git add examples/02-following-feed/index.html
git commit -m "feat(example): add renderProfilePage function"
```

---

## Task 11: Update renderLoginForm for Routing

**Files:**
- Modify: `examples/02-following-feed/index.html`

**Step 1: Update renderLoginForm**

Modify the existing `renderLoginForm` function to show a message when on profile page but not logged in:

```javascript
function renderLoginForm() {
  const container = document.getElementById('auth-section');
  const savedClientId = storage.get(STORAGE_KEYS.clientId) || '';
  const profileHandle = router.getProfileHandle();

  const message = profileHandle
    ? `<p style="margin-bottom: 1rem; color: var(--gray-700);">Login to view @${escapeHtml(profileHandle)}'s posts</p>`
    : '';

  container.innerHTML = `
    <div class="card">
      ${message}
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
```

**Step 2: Commit**

```bash
git add examples/02-following-feed/index.html
git commit -m "feat(example): update login form with profile context message"
```

---

## Task 12: Update renderUserCard with Profile Link

**Files:**
- Modify: `examples/02-following-feed/index.html`

**Step 1: Update renderUserCard**

Modify to make the user info clickable to their profile:

```javascript
function renderUserCard(viewer) {
  const container = document.getElementById('auth-section');
  const displayName = viewer?.appBskyActorProfileByDid?.displayName || 'User';
  const handle = viewer?.handle || 'unknown';
  const avatarUrl = viewer?.appBskyActorProfileByDid?.avatar?.url;

  container.innerHTML = `
    <div class="card user-card">
      <a href="/profile/${escapeHtml(handle)}" class="user-info" onclick="event.preventDefault(); router.navigateTo('/profile/${escapeHtml(handle)}')">
        <div class="user-avatar">
          ${avatarUrl
            ? `<img src="${escapeHtml(avatarUrl)}" alt="Avatar">`
            : '👤'}
        </div>
        <div>
          <div class="user-name">${escapeHtml(displayName)}</div>
          <div class="user-handle">@${escapeHtml(handle)}</div>
        </div>
      </a>
      <button class="btn btn-secondary" onclick="logout()">Logout</button>
    </div>
  `;
}
```

**Step 2: Add hover style for user-info link**

Add to CSS:

```css
a.user-info {
  text-decoration: none;
  color: inherit;
}

a.user-info:hover .user-name {
  color: var(--primary-500);
}
```

**Step 3: Commit**

```bash
git add examples/02-following-feed/index.html
git commit -m "feat(example): make user card link to profile page"
```

---

## Task 13: Create Main renderApp Function

**Files:**
- Modify: `examples/02-following-feed/index.html`

**Step 1: Add renderApp function**

Replace the `main` function with a new routing-aware `renderApp` function:

```javascript
// =============================================================================
// MAIN APPLICATION
// =============================================================================

let currentViewer = null;

async function renderApp() {
  const profileHandle = router.getProfileHandle();

  // Always render auth section first
  if (isLoggedIn()) {
    try {
      if (!currentViewer) {
        currentViewer = await fetchViewer();
      }
      renderUserCard(currentViewer);
    } catch (error) {
      console.error('Failed to fetch viewer:', error);
      renderUserCard(null);
    }
  } else {
    renderLoginForm();
  }

  // Clear posts feed
  document.getElementById('posts-feed').innerHTML = '';

  // Route handling
  if (profileHandle) {
    // Profile page
    if (!isLoggedIn()) {
      document.getElementById('posts-feed').innerHTML = `
        <div class="card">
          <p class="loading">Please login to view profiles.</p>
        </div>
      `;
      return;
    }
    await renderProfilePage(profileHandle);
  } else {
    // Home page
    if (isLoggedIn() && currentViewer?.handle) {
      // Redirect logged-in users to their profile
      router.navigateTo(`/profile/${currentViewer.handle}`);
    }
  }
}
```

**Step 2: Commit**

```bash
git add examples/02-following-feed/index.html
git commit -m "feat(example): add renderApp function with routing logic"
```

---

## Task 14: Update Initialization and OAuth Callback

**Files:**
- Modify: `examples/02-following-feed/index.html`

**Step 1: Replace main() with init()**

Add new initialization function that handles OAuth callback and initial render:

```javascript
async function init() {
  try {
    // Check if this is an OAuth callback
    const isCallback = await handleOAuthCallback();
    if (isCallback) {
      console.log('OAuth callback handled successfully');
      // Fetch viewer and redirect to profile
      const viewer = await fetchViewer();
      currentViewer = viewer;
      if (viewer?.handle) {
        router.navigateTo(`/profile/${viewer.handle}`);
        return;
      }
    }
  } catch (error) {
    showError(`Authentication failed: ${error.message}`);
    storage.clear();
  }

  // Render the app
  await renderApp();
}

// Run on page load
init();
```

**Step 2: Remove the old main() call**

Delete:
```javascript
// Run on page load
main();
```

And delete the old `main()` function entirely.

**Step 3: Commit**

```bash
git add examples/02-following-feed/index.html
git commit -m "feat(example): update init to handle OAuth callback and redirect to profile"
```

---

## Task 15: Update README

**Files:**
- Modify: `examples/02-following-feed/README.md`

**Step 1: Read current README**

Check current content.

**Step 2: Update README content**

```markdown
# Following Feed Example

A simple HTML example demonstrating how to view Bluesky profile posts using Quickslice's GraphQL API.

## Features

- OAuth login with PKCE flow
- Client-side routing (`/profile/{handle}`)
- View any user's posts (excluding replies)
- Display post text and embedded images

## Setup

1. Start the Quickslice server on `localhost:8080`
2. Open `index.html` in a browser
3. Enter your OAuth Client ID and Bluesky handle
4. After login, you'll be redirected to your profile page

## Routes

- `/` - Home page (redirects to profile when logged in)
- `/profile/{handle}` - View posts from a specific user

## GraphQL Query Used

```graphql
query GetPosts($handle: String!) {
  appBskyFeedPost(
    sortBy: [{direction: DESC, field: createdAt}]
    where: {
      and: [
        {actorHandle: {eq: $handle}},
        {reply: {isNull: true}}
      ]
    }
  ) {
    edges {
      node {
        text
        createdAt
        appBskyActorProfileByDid {
          displayName
          actorHandle
          avatar { url }
        }
        embed {
          ... on AppBskyEmbedImages {
            images {
              image { url }
            }
          }
        }
      }
    }
  }
}
```

## Notes

- Requires authentication to view profiles
- Posts are sorted by creation date (newest first)
- Replies are filtered out to show only original posts
```

**Step 3: Commit**

```bash
git add examples/02-following-feed/README.md
git commit -m "docs(example): update README for following feed example"
```

---

## Task 16: Manual Testing

**Step 1: Test login flow**

1. Open `examples/02-following-feed/index.html` in browser
2. Should see "Following Feed" header and login form
3. Enter client ID and handle, click login
4. Complete OAuth flow
5. Should redirect to `/profile/{your-handle}`

**Step 2: Test profile page**

1. Should see your profile header (avatar, name, handle)
2. Should see your posts (newest first)
3. Should NOT see replies
4. Posts with images should display images

**Step 3: Test viewing other profiles**

1. Manually navigate to `/profile/other-user.bsky.social`
2. Should see that user's profile and posts

**Step 4: Test logout**

1. Click logout button
2. Should return to login form
3. URL should stay on profile page
4. Should see "Login to view @handle's posts" message

---

## Summary

| Task | Description |
|------|-------------|
| 1 | Update branding, remove emoji picker HTML |
| 2 | Remove emoji-related JavaScript constants and functions |
| 3 | Remove statusphere feed code and CSS |
| 4 | Add client-side routing utilities |
| 5 | Add fetchPosts function |
| 6 | Add fetchProfile function |
| 7 | Add post card CSS |
| 8 | Add renderPostsFeed function |
| 9 | Add renderProfileHeader function |
| 10 | Add renderProfilePage function |
| 11 | Update renderLoginForm for routing context |
| 12 | Update renderUserCard with profile link |
| 13 | Create main renderApp function |
| 14 | Update initialization and OAuth callback |
| 15 | Update README |
| 16 | Manual testing |
