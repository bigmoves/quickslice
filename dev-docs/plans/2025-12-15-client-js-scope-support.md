# Add Scope Support to quickslice-client-js

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Allow passing OAuth `scope` parameter in the authorize flow.

**Architecture:** Add optional `scope` field to client options and login options. Pass through to authorize URL when provided. Server uses its default when omitted.

**Tech Stack:** TypeScript, esbuild

---

## Task 1: Add scope to LoginOptions and authorize URL

**Files:**
- Modify: `quickslice-client-js/src/auth/oauth.ts:7-10` (LoginOptions interface)
- Modify: `quickslice-client-js/src/auth/oauth.ts:34-41` (params building)

**Step 1: Add scope to LoginOptions interface**

In `quickslice-client-js/src/auth/oauth.ts`, update the `LoginOptions` interface:

```typescript
export interface LoginOptions {
  handle?: string;
  redirectUri?: string;
  scope?: string;
}
```

**Step 2: Add scope to authorize URL params**

In the `initiateLogin` function, after line 45 (`if (options.handle)`), add:

```typescript
  if (options.scope) {
    params.set('scope', options.scope);
  }
```

**Step 3: Commit**

```bash
git add quickslice-client-js/src/auth/oauth.ts
git commit -m "feat(client): add scope parameter to LoginOptions"
```

---

## Task 2: Add scope to QuicksliceClientOptions and pass through

**Files:**
- Modify: `quickslice-client-js/src/client.ts:8-12` (QuicksliceClientOptions interface)
- Modify: `quickslice-client-js/src/client.ts:18-35` (constructor and fields)
- Modify: `quickslice-client-js/src/client.ts:52-58` (loginWithRedirect)

**Step 1: Add scope to QuicksliceClientOptions**

Update the interface:

```typescript
export interface QuicksliceClientOptions {
  server: string;
  clientId: string;
  redirectUri?: string;
  scope?: string;
}
```

**Step 2: Add scope field to class**

Add after line 21 (`private redirectUri?: string;`):

```typescript
  private scope?: string;
```

**Step 3: Store scope in constructor**

Add after line 30 (`this.redirectUri = options.redirectUri;`):

```typescript
    this.scope = options.scope;
```

**Step 4: Pass scope through in loginWithRedirect**

Update the `loginWithRedirect` method to pass scope:

```typescript
  async loginWithRedirect(options: LoginOptions = {}): Promise<void> {
    await this.init();
    await initiateLogin(this.authorizeUrl, this.clientId, {
      ...options,
      redirectUri: options.redirectUri || this.redirectUri,
      scope: options.scope || this.scope,
    });
  }
```

**Step 5: Commit**

```bash
git add quickslice-client-js/src/client.ts
git commit -m "feat(client): add scope to client options with per-login override"
```

---

## Task 3: Build and verify

**Step 1: Build the library**

```bash
cd /Users/chadmiller/code/quickslice/quickslice-client-js && npm run build
```

Expected: `Build complete!`

**Step 2: Generate TypeScript declarations**

```bash
cd /Users/chadmiller/code/quickslice/quickslice-client-js && npx tsc
```

Expected: No errors

**Step 3: Commit dist files**

```bash
git add quickslice-client-js/dist/
git commit -m "chore(client): rebuild dist with scope support"
```

---

## Task 4: Update README

**Files:**
- Modify: `quickslice-client-js/README.md`

**Step 1: Add scope to usage examples**

In the Usage section, update the initialization example to show scope:

```javascript
// Initialize client with default scope
const client = await QuicksliceClient.createQuicksliceClient({
  server: 'https://api.example.com',
  clientId: 'client_abc123',
  scope: 'atproto',  // optional - server uses default if omitted
});

// Or specify scope per-login
await client.loginWithRedirect({
  handle: 'alice.bsky.social',
  scope: 'atproto transition:generic'
});
```

**Step 2: Update API docs**

In the Options section under `createQuicksliceClient`, add:
- `scope` (optional): OAuth scope string to request

In the Auth Methods section under `loginWithRedirect(options?)`, add:
- `options.scope`: Override scope for this login

**Step 3: Commit**

```bash
git add quickslice-client-js/README.md
git commit -m "docs(client): document scope parameter"
```
