# User-Agent Header Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add a `user-agent: quickslice` header to all outbound HTTP requests via a centralized helper module.

**Architecture:** Create `src/lib/http_client.gleam` that wraps `httpc.send` and `hackney.send`, automatically adding the User-Agent header before delegating. Update all call sites to use the new module.

**Tech Stack:** Gleam, gleam_httpc, gleam_hackney

---

### Task 1: Create the HTTP Client Helper Module

**Files:**
- Create: `src/lib/http_client.gleam`

**Step 1: Create the module with send function**

```gleam
import gleam/bytes_builder.{type BytesBuilder}
import gleam/hackney
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/httpc

const user_agent = "quickslice"

/// Send an HTTP request with the quickslice user-agent header.
/// Use this instead of httpc.send directly.
pub fn send(
  req: Request(String),
) -> Result(Response(String), httpc.HttpcError) {
  req
  |> request.set_header("user-agent", user_agent)
  |> httpc.send
}

/// Send an HTTP request with binary body using hackney.
/// Use this instead of hackney.send directly.
pub fn send_bits(
  req: Request(BytesBuilder),
) -> Result(Response(BitArray), hackney.Error) {
  req
  |> request.set_header("user-agent", user_agent)
  |> hackney.send
}
```

**Step 2: Verify it compiles**

Run: `gleam build`
Expected: Build succeeds with no errors

**Step 3: Commit**

```bash
git add src/lib/http_client.gleam
git commit -m "feat: add http_client module with user-agent header"
```

---

### Task 2: Update did_resolver.gleam

**Files:**
- Modify: `src/lib/oauth/atproto/did_resolver.gleam`

**Step 1: Update import**

Replace:
```gleam
import gleam/httpc
```

With:
```gleam
import lib/http_client
```

**Step 2: Update send calls**

Replace all occurrences of:
```gleam
httpc.send(req)
```

With:
```gleam
http_client.send(req)
```

**Step 3: Verify it compiles**

Run: `gleam build`
Expected: Build succeeds with no errors

**Step 4: Commit**

```bash
git add src/lib/oauth/atproto/did_resolver.gleam
git commit -m "refactor: use http_client in did_resolver"
```

---

### Task 3: Update bridge.gleam

**Files:**
- Modify: `src/lib/oauth/atproto/bridge.gleam`

**Step 1: Update import**

Replace:
```gleam
import gleam/httpc
```

With:
```gleam
import lib/http_client
```

**Step 2: Update send calls**

Replace all occurrences of:
```gleam
httpc.send(req)
```

With:
```gleam
http_client.send(req)
```

**Step 3: Verify it compiles**

Run: `gleam build`
Expected: Build succeeds with no errors

**Step 4: Commit**

```bash
git add src/lib/oauth/atproto/bridge.gleam
git commit -m "refactor: use http_client in bridge"
```

---

### Task 4: Update dpop.gleam

**Files:**
- Modify: `src/dpop.gleam`

**Step 1: Update import**

Replace:
```gleam
import gleam/httpc
```

With:
```gleam
import lib/http_client
```

**Step 2: Update send calls**

Replace all occurrences of:
```gleam
httpc.send(req)
```

With:
```gleam
http_client.send(req)
```

**Step 3: Verify it compiles**

Run: `gleam build`
Expected: Build succeeds with no errors

**Step 4: Commit**

```bash
git add src/dpop.gleam
git commit -m "refactor: use http_client in dpop"
```

---

### Task 5: Update backfill.gleam

**Files:**
- Modify: `src/backfill.gleam`

**Step 1: Update import**

Replace:
```gleam
import gleam/hackney
```

With:
```gleam
import lib/http_client
```

Note: Keep any other hackney imports if they're used for types.

**Step 2: Update send_bits calls**

In the `send_bits_with_permit` function (or similar), replace:
```gleam
hackney.send(req)
```

With:
```gleam
http_client.send_bits(req)
```

**Step 3: Verify it compiles**

Run: `gleam build`
Expected: Build succeeds with no errors

**Step 4: Commit**

```bash
git add src/backfill.gleam
git commit -m "refactor: use http_client in backfill"
```

---

### Task 6: Update authorize.gleam

**Files:**
- Modify: `src/handlers/oauth/authorize.gleam`

**Step 1: Update import**

Replace:
```gleam
import gleam/httpc
```

With:
```gleam
import lib/http_client
```

**Step 2: Update send calls**

Replace all occurrences of:
```gleam
httpc.send(req)
```

With:
```gleam
http_client.send(req)
```

**Step 3: Verify it compiles**

Run: `gleam build`
Expected: Build succeeds with no errors

**Step 4: Commit**

```bash
git add src/handlers/oauth/authorize.gleam
git commit -m "refactor: use http_client in oauth authorize handler"
```

---

### Task 7: Update admin_oauth_authorize.gleam

**Files:**
- Modify: `src/handlers/admin_oauth_authorize.gleam`

**Step 1: Update import**

Replace:
```gleam
import gleam/httpc
```

With:
```gleam
import lib/http_client
```

**Step 2: Update send calls**

Replace all occurrences of:
```gleam
httpc.send(req)
```

With:
```gleam
http_client.send(req)
```

**Step 3: Verify it compiles**

Run: `gleam build`
Expected: Build succeeds with no errors

**Step 4: Commit**

```bash
git add src/handlers/admin_oauth_authorize.gleam
git commit -m "refactor: use http_client in admin oauth handler"
```

---

### Task 8: Final Verification

**Step 1: Clean build**

Run: `gleam clean && gleam build`
Expected: Build succeeds with no errors or warnings

**Step 2: Run tests**

Run: `gleam test`
Expected: All tests pass

**Step 3: Final commit (if any cleanup needed)**

```bash
git add -A
git commit -m "chore: user-agent header implementation complete"
```
