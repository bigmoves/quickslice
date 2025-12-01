# OAuth Supported Scopes Validation Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add comprehensive validation for OAuth supported scopes - validate env var at startup, fix hardcoded metadata endpoints, and validate client-requested scopes against supported list.

**Architecture:** Validate `OAUTH_SUPPORTED_SCOPES` at startup using existing scope parser (panic on invalid). Update metadata handlers to accept scopes as parameter. Add containment validation to scope validator that checks requested scopes are in supported list.

**Tech Stack:** Gleam, gleeunit, wisp

---

## Task 1: Add Scope Containment Validator

**Files:**
- Modify: `server/src/lib/oauth/scopes/validator.gleam`
- Create: `server/test/oauth/scopes/supported_test.gleam`

**Step 1: Write the failing test**

Create `server/test/oauth/scopes/supported_test.gleam`:

```gleam
import gleeunit/should
import lib/oauth/scopes/validator

pub fn validate_scopes_supported_all_in_list_test() {
  let supported = ["atproto", "transition:generic", "repo:*"]
  validator.validate_scopes_supported("atproto repo:*", supported)
  |> should.be_ok
}

pub fn validate_scopes_supported_unsupported_scope_test() {
  let supported = ["atproto", "transition:generic"]
  validator.validate_scopes_supported("atproto account:email", supported)
  |> should.be_error
}

pub fn validate_scopes_supported_empty_request_test() {
  let supported = ["atproto", "transition:generic"]
  validator.validate_scopes_supported("", supported)
  |> should.be_ok
}

pub fn validate_scopes_supported_invalid_format_test() {
  let supported = ["atproto", "transition:generic"]
  validator.validate_scopes_supported("invalid:::", supported)
  |> should.be_error
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: FAIL with "validate_scopes_supported not defined"

**Step 3: Write implementation**

Modify `server/src/lib/oauth/scopes/validator.gleam`:

```gleam
/// OAuth scope validation
import gleam/list
import gleam/result
import gleam/string
import lib/oauth/scopes/parse_error
import lib/oauth/scopes/parser
import lib/oauth/scopes/types.{type Scope}
import lib/oauth/types/error.{type OAuthError, InvalidScope}

/// Validate scope string format and parse into structured scopes
pub fn validate_scope_format(
  scope_string: String,
) -> Result(List(Scope), OAuthError) {
  parser.parse_scopes(scope_string)
  |> result.map_error(fn(e) { InvalidScope(parse_error.to_string(e)) })
}

/// Validate that all requested scopes are in the supported scopes list
pub fn validate_scopes_supported(
  requested: String,
  supported: List(String),
) -> Result(List(Scope), OAuthError) {
  // First validate format
  use parsed <- result.try(validate_scope_format(requested))

  // Extract requested scope tokens
  let requested_tokens =
    requested
    |> string.split(" ")
    |> list.map(string.trim)
    |> list.filter(fn(s) { !string.is_empty(s) })

  // Find any unsupported scope
  let unsupported =
    list.find(requested_tokens, fn(token) {
      !list.contains(supported, token)
    })

  case unsupported {
    Ok(scope) -> Error(InvalidScope("Unsupported scope: " <> scope))
    Error(Nil) -> Ok(parsed)
  }
}
```

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
git add server/src/lib/oauth/scopes/validator.gleam server/test/oauth/scopes/supported_test.gleam
git commit -m "feat(oauth): add scope containment validation"
```

---

## Task 2: Validate Env Var at Startup

**Files:**
- Modify: `server/src/server.gleam:379-388`

**Step 1: Write the implementation**

Modify `server/src/server.gleam` lines 379-388. Add import at top:

```gleam
import lib/oauth/scopes/validator as scope_validator
import lib/oauth/types/error
```

Replace the existing env var parsing:

```gleam
  // Get OAuth supported scopes from environment variable (space-separated)
  // Validate format at startup - panic on invalid configuration
  let oauth_supported_scopes = case envoy.get("OAUTH_SUPPORTED_SCOPES") {
    Ok(scopes_str) -> {
      case scope_validator.validate_scope_format(scopes_str) {
        Ok(_) -> {
          scopes_str
          |> string.split(" ")
          |> list.map(string.trim)
          |> list.filter(fn(s) { !string.is_empty(s) })
        }
        Error(e) -> {
          let msg =
            "Invalid OAUTH_SUPPORTED_SCOPES: " <> error.error_description(e)
          logging.log(logging.Error, msg)
          panic as msg
        }
      }
    }
    Error(_) -> ["atproto", "transition:generic"]
  }
```

**Step 2: Run test to verify it compiles and tests pass**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: PASS (all existing tests still pass)

**Step 3: Commit**

```bash
git add server/src/server.gleam
git commit -m "feat(oauth): validate OAUTH_SUPPORTED_SCOPES at startup"
```

---

## Task 3: Update OAuth Server Metadata Handler

**Files:**
- Modify: `server/src/handlers/oauth/metadata.gleam:24,31`
- Modify: `server/src/server.gleam:577`

**Step 1: Update metadata handler to accept scopes parameter**

Modify `server/src/handlers/oauth/metadata.gleam`:

```gleam
/// Generate server metadata for the given base URL
pub fn generate_metadata(
  base_url: String,
  scopes_supported: List(String),
) -> ServerMetadata {
  ServerMetadata(
    issuer: base_url,
    authorization_endpoint: base_url <> "/oauth/authorize",
    token_endpoint: base_url <> "/oauth/token",
    jwks_uri: base_url <> "/.well-known/jwks.json",
    registration_endpoint: base_url <> "/oauth/register",
    scopes_supported: scopes_supported,
    response_types_supported: ["code"],
    grant_types_supported: ["authorization_code", "refresh_token"],
    token_endpoint_auth_methods_supported: [
      "client_secret_basic",
      "client_secret_post",
      "none",
    ],
    code_challenge_methods_supported: ["S256"],
    pushed_authorization_request_endpoint: base_url <> "/oauth/par",
    dpop_signing_alg_values_supported: ["ES256"],
  )
}
```

Update `handle` function:

```gleam
/// Handle GET /.well-known/oauth-authorization-server
pub fn handle(base_url: String, scopes_supported: List(String)) -> wisp.Response {
  let meta = generate_metadata(base_url, scopes_supported)
  let json_response = encode_metadata(meta)

  wisp.response(200)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text(json.to_string(json_response)))
}
```

**Step 2: Update caller in server.gleam**

Modify line 577 of `server/src/server.gleam`:

```gleam
    [".well-known", "oauth-authorization-server"] ->
      oauth_metadata_handler.handle(ctx.external_base_url, ctx.oauth_supported_scopes)
```

**Step 3: Run test to verify it compiles and tests pass**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: PASS

**Step 4: Commit**

```bash
git add server/src/handlers/oauth/metadata.gleam server/src/server.gleam
git commit -m "feat(oauth): use configured scopes in server metadata"
```

---

## Task 4: Update Client Metadata Handler

**Files:**
- Modify: `server/src/server.gleam:588`

**Step 1: Update client metadata call to use config**

Modify line 588 of `server/src/server.gleam`:

Change:
```gleam
        "atproto transition:generic",
```

To:
```gleam
        string.join(ctx.oauth_supported_scopes, " "),
```

**Step 2: Run test to verify it compiles and tests pass**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: PASS

**Step 3: Commit**

```bash
git add server/src/server.gleam
git commit -m "feat(oauth): use configured scopes in client metadata"
```

---

## Task 5: Add Integration Test for Metadata Endpoints

**Files:**
- Create: `server/test/oauth/metadata_scopes_test.gleam`

**Step 1: Write integration test**

Create `server/test/oauth/metadata_scopes_test.gleam`:

```gleam
import gleam/string
import gleeunit/should
import handlers/oauth/metadata

pub fn metadata_uses_provided_scopes_test() {
  let scopes = ["atproto", "repo:*", "account:email"]
  let meta = metadata.generate_metadata("https://example.com", scopes)

  meta.scopes_supported |> should.equal(scopes)
}

pub fn metadata_default_scopes_work_test() {
  let scopes = ["atproto", "transition:generic"]
  let meta = metadata.generate_metadata("https://example.com", scopes)

  meta.scopes_supported |> should.equal(["atproto", "transition:generic"])
}
```

**Step 2: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: PASS

**Step 3: Commit**

```bash
git add server/test/oauth/metadata_scopes_test.gleam
git commit -m "test(oauth): add metadata scopes integration tests"
```

---

## Summary

This plan implements:

1. **Task 1**: New `validate_scopes_supported` function to check requested scopes are in supported list
2. **Task 2**: Validate `OAUTH_SUPPORTED_SCOPES` env var at startup (panic on invalid)
3. **Task 3**: Update OAuth server metadata to use configured scopes instead of hardcoded values
4. **Task 4**: Update client metadata to use configured scopes
5. **Task 5**: Integration tests for metadata endpoints

Each task follows TDD: write failing test → implement → verify → commit.
