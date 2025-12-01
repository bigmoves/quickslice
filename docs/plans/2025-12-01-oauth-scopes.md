# ATProto OAuth Scopes Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement full ATProto OAuth scope parsing and validation at all OAuth touchpoints.

**Architecture:** Create a scope parser module with typed Gleam representations for all ATProto scope kinds (static, account, identity, repo, blob, rpc, include). Integrate validation at registration, authorization, token exchange, and refresh endpoints.

**Tech Stack:** Gleam, gleeunit for testing

---

## Task 1: Create Scope Types Module

**Files:**
- Create: `server/src/lib/oauth/scopes/types.gleam`
- Test: `server/test/oauth/scopes/types_test.gleam`

**Step 1: Write the failing test for static scopes**

Create `server/test/oauth/scopes/types_test.gleam`:

```gleam
import gleeunit/should
import lib/oauth/scopes/types

pub fn static_scope_atproto_test() {
  types.Atproto
  |> types.static_scope_to_string
  |> should.equal("atproto")
}

pub fn static_scope_transition_generic_test() {
  types.TransitionGeneric
  |> types.static_scope_to_string
  |> should.equal("transition:generic")
}

pub fn static_scope_transition_email_test() {
  types.TransitionEmail
  |> types.static_scope_to_string
  |> should.equal("transition:email")
}

pub fn static_scope_transition_chat_bsky_test() {
  types.TransitionChatBsky
  |> types.static_scope_to_string
  |> should.equal("transition:chat.bsky")
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: FAIL with "module lib/oauth/scopes/types not found"

**Step 3: Write minimal implementation**

Create `server/src/lib/oauth/scopes/types.gleam`:

```gleam
/// ATProto OAuth scope types
/// Reference: https://github.com/bluesky-social/atproto/tree/main/packages/oauth/oauth-scopes

/// Static ATProto scopes
pub type StaticScope {
  Atproto
  TransitionEmail
  TransitionGeneric
  TransitionChatBsky
}

/// Actions for repo and account scopes
pub type Action {
  Create
  Update
  Delete
  Read
  Manage
}

/// Account attributes
pub type AccountAttribute {
  Email
  Repo
  Status
}

/// Identity attributes
pub type IdentityAttribute {
  Handle
  All
}

/// Account scope: account:email?action=read
pub type AccountScope {
  AccountScope(attribute: AccountAttribute, action: Action)
}

/// Identity scope: identity:handle or identity:*
pub type IdentityScope {
  IdentityScope(attribute: IdentityAttribute)
}

/// Repo scope: repo:app.bsky.feed.post?action=create
pub type RepoScope {
  RepoScope(collection: String, actions: List(Action))
}

/// Blob scope: blob:image/* or blob:*/*
pub type BlobScope {
  BlobScope(mime_type: String)
}

/// RPC scope: rpc:app.bsky.feed.getFeed?aud=did:web:bsky.app
pub type RpcScope {
  RpcScope(methods: List(String), audience: String)
}

/// Include scope: include:app.bsky.feed?aud=did:web:...
pub type IncludeScope {
  IncludeScope(nsid: String, audience: Option(String))
}

import gleam/option.{type Option}

/// Union of all scope types
pub type Scope {
  Static(StaticScope)
  Account(AccountScope)
  Identity(IdentityScope)
  Repo(RepoScope)
  Blob(BlobScope)
  Rpc(RpcScope)
  Include(IncludeScope)
}

/// Convert static scope to string
pub fn static_scope_to_string(scope: StaticScope) -> String {
  case scope {
    Atproto -> "atproto"
    TransitionEmail -> "transition:email"
    TransitionGeneric -> "transition:generic"
    TransitionChatBsky -> "transition:chat.bsky"
  }
}
```

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
git add server/src/lib/oauth/scopes/types.gleam server/test/oauth/scopes/types_test.gleam
git commit -m "feat(oauth): add scope type definitions"
```

---

## Task 2: Add Scope-to-String Conversions

**Files:**
- Modify: `server/src/lib/oauth/scopes/types.gleam`
- Modify: `server/test/oauth/scopes/types_test.gleam`

**Step 1: Write the failing test for action_to_string**

Add to `server/test/oauth/scopes/types_test.gleam`:

```gleam
pub fn action_to_string_create_test() {
  types.Create
  |> types.action_to_string
  |> should.equal("create")
}

pub fn action_to_string_read_test() {
  types.Read
  |> types.action_to_string
  |> should.equal("read")
}

pub fn action_to_string_manage_test() {
  types.Manage
  |> types.action_to_string
  |> should.equal("manage")
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: FAIL with "action_to_string not found"

**Step 3: Write minimal implementation**

Add to `server/src/lib/oauth/scopes/types.gleam`:

```gleam
/// Convert action to string
pub fn action_to_string(action: Action) -> String {
  case action {
    Create -> "create"
    Update -> "update"
    Delete -> "delete"
    Read -> "read"
    Manage -> "manage"
  }
}
```

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
git add server/src/lib/oauth/scopes/types.gleam server/test/oauth/scopes/types_test.gleam
git commit -m "feat(oauth): add action_to_string conversion"
```

---

## Task 3: Create Parse Error Types

**Files:**
- Create: `server/src/lib/oauth/scopes/parse_error.gleam`
- Test: `server/test/oauth/scopes/parse_error_test.gleam`

**Step 1: Write the failing test**

Create `server/test/oauth/scopes/parse_error_test.gleam`:

```gleam
import gleeunit/should
import lib/oauth/scopes/parse_error

pub fn invalid_scope_format_to_string_test() {
  parse_error.InvalidScopeFormat("repo:", "missing collection")
  |> parse_error.to_string
  |> should.equal("Invalid scope format: 'repo:' - missing collection")
}

pub fn invalid_action_to_string_test() {
  parse_error.InvalidAction("foo")
  |> parse_error.to_string
  |> should.equal("Unknown action 'foo', expected: create, update, delete, read, manage")
}

pub fn invalid_rpc_scope_to_string_test() {
  parse_error.InvalidRpcScope("wildcard method requires specific audience")
  |> parse_error.to_string
  |> should.equal("Invalid RPC scope: wildcard method requires specific audience")
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: FAIL with "module lib/oauth/scopes/parse_error not found"

**Step 3: Write minimal implementation**

Create `server/src/lib/oauth/scopes/parse_error.gleam`:

```gleam
/// Parse error types for OAuth scope parsing

pub type ParseError {
  InvalidScopeFormat(scope: String, reason: String)
  InvalidAction(action: String)
  InvalidAttribute(attribute: String)
  InvalidMimeType(mime: String)
  InvalidRpcScope(reason: String)
}

/// Convert parse error to user-facing string
pub fn to_string(error: ParseError) -> String {
  case error {
    InvalidScopeFormat(scope, reason) ->
      "Invalid scope format: '" <> scope <> "' - " <> reason
    InvalidAction(action) ->
      "Unknown action '" <> action <> "', expected: create, update, delete, read, manage"
    InvalidAttribute(attr) ->
      "Unknown attribute '" <> attr <> "'"
    InvalidMimeType(mime) ->
      "Invalid MIME type '" <> mime <> "', expected format: type/subtype"
    InvalidRpcScope(reason) ->
      "Invalid RPC scope: " <> reason
  }
}
```

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
git add server/src/lib/oauth/scopes/parse_error.gleam server/test/oauth/scopes/parse_error_test.gleam
git commit -m "feat(oauth): add scope parse error types"
```

---

## Task 4: Create Parser Module - Static Scopes

**Files:**
- Create: `server/src/lib/oauth/scopes/parser.gleam`
- Test: `server/test/oauth/scopes/parser_test.gleam`

**Step 1: Write the failing test for parsing static scopes**

Create `server/test/oauth/scopes/parser_test.gleam`:

```gleam
import gleam/option.{None, Some}
import gleeunit/should
import lib/oauth/scopes/parser
import lib/oauth/scopes/types

pub fn parse_atproto_test() {
  parser.parse_scope("atproto")
  |> should.be_ok
  |> should.equal(types.Static(types.Atproto))
}

pub fn parse_transition_generic_test() {
  parser.parse_scope("transition:generic")
  |> should.be_ok
  |> should.equal(types.Static(types.TransitionGeneric))
}

pub fn parse_transition_email_test() {
  parser.parse_scope("transition:email")
  |> should.be_ok
  |> should.equal(types.Static(types.TransitionEmail))
}

pub fn parse_transition_chat_bsky_test() {
  parser.parse_scope("transition:chat.bsky")
  |> should.be_ok
  |> should.equal(types.Static(types.TransitionChatBsky))
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: FAIL with "module lib/oauth/scopes/parser not found"

**Step 3: Write minimal implementation**

Create `server/src/lib/oauth/scopes/parser.gleam`:

```gleam
/// ATProto OAuth scope parser

import lib/oauth/scopes/parse_error.{type ParseError, InvalidScopeFormat}
import lib/oauth/scopes/types.{
  type Scope, type StaticScope, Atproto, Static, TransitionChatBsky,
  TransitionEmail, TransitionGeneric,
}

/// Parse a single scope token
pub fn parse_scope(token: String) -> Result(Scope, ParseError) {
  case parse_static(token) {
    Ok(scope) -> Ok(Static(scope))
    Error(Nil) -> Error(InvalidScopeFormat(token, "unknown scope type"))
  }
}

/// Try to parse as static scope
fn parse_static(token: String) -> Result(StaticScope, Nil) {
  case token {
    "atproto" -> Ok(Atproto)
    "transition:email" -> Ok(TransitionEmail)
    "transition:generic" -> Ok(TransitionGeneric)
    "transition:chat.bsky" -> Ok(TransitionChatBsky)
    _ -> Error(Nil)
  }
}
```

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
git add server/src/lib/oauth/scopes/parser.gleam server/test/oauth/scopes/parser_test.gleam
git commit -m "feat(oauth): add parser for static scopes"
```

---

## Task 5: Parse Account Scopes

**Files:**
- Modify: `server/src/lib/oauth/scopes/parser.gleam`
- Modify: `server/test/oauth/scopes/parser_test.gleam`

**Step 1: Write the failing test**

Add to `server/test/oauth/scopes/parser_test.gleam`:

```gleam
pub fn parse_account_email_test() {
  parser.parse_scope("account:email")
  |> should.be_ok
  |> should.equal(types.Account(types.AccountScope(
    attribute: types.Email,
    action: types.Read,
  )))
}

pub fn parse_account_email_with_action_test() {
  parser.parse_scope("account:email?action=manage")
  |> should.be_ok
  |> should.equal(types.Account(types.AccountScope(
    attribute: types.Email,
    action: types.Manage,
  )))
}

pub fn parse_account_repo_test() {
  parser.parse_scope("account:repo")
  |> should.be_ok
  |> should.equal(types.Account(types.AccountScope(
    attribute: types.Repo,
    action: types.Read,
  )))
}

pub fn parse_account_status_test() {
  parser.parse_scope("account:status?action=manage")
  |> should.be_ok
  |> should.equal(types.Account(types.AccountScope(
    attribute: types.Status,
    action: types.Manage,
  )))
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: FAIL - account scopes not yet parsed

**Step 3: Write minimal implementation**

Update `server/src/lib/oauth/scopes/parser.gleam` to add account parsing:

```gleam
/// ATProto OAuth scope parser

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/uri
import lib/oauth/scopes/parse_error.{
  type ParseError, InvalidAction, InvalidAttribute, InvalidScopeFormat,
}
import lib/oauth/scopes/types.{
  type AccountAttribute, type Action, type Scope, type StaticScope, Account,
  AccountScope, Atproto, Create, Delete, Email, Manage, Read, Repo, Static,
  Status, TransitionChatBsky, TransitionEmail, TransitionGeneric, Update,
}

/// Parse a single scope token
pub fn parse_scope(token: String) -> Result(Scope, ParseError) {
  case parse_static(token) {
    Ok(scope) -> Ok(Static(scope))
    Error(Nil) -> parse_parameterized(token)
  }
}

/// Try to parse as static scope
fn parse_static(token: String) -> Result(StaticScope, Nil) {
  case token {
    "atproto" -> Ok(Atproto)
    "transition:email" -> Ok(TransitionEmail)
    "transition:generic" -> Ok(TransitionGeneric)
    "transition:chat.bsky" -> Ok(TransitionChatBsky)
    _ -> Error(Nil)
  }
}

/// Parse parameterized scope (prefix:value?params)
fn parse_parameterized(token: String) -> Result(Scope, ParseError) {
  case string.split_once(token, ":") {
    Ok(#("account", rest)) -> parse_account_scope(token, rest)
    Ok(#(prefix, _)) ->
      Error(InvalidScopeFormat(token, "unknown prefix: " <> prefix))
    Error(Nil) -> Error(InvalidScopeFormat(token, "missing scope prefix"))
  }
}

/// Parse account scope: account:email?action=manage
fn parse_account_scope(
  original: String,
  rest: String,
) -> Result(Scope, ParseError) {
  let #(attr_str, query) = split_query(rest)

  use attribute <- result.try(parse_account_attribute(original, attr_str))

  let action = case get_query_param(query, "action") {
    Some("read") -> Ok(Read)
    Some("manage") -> Ok(Manage)
    Some(other) -> Error(InvalidAction(other))
    None -> Ok(Read)
  }

  use act <- result.try(action)

  Ok(Account(AccountScope(attribute: attribute, action: act)))
}

/// Parse account attribute
fn parse_account_attribute(
  original: String,
  attr: String,
) -> Result(AccountAttribute, ParseError) {
  case attr {
    "email" -> Ok(Email)
    "repo" -> Ok(Repo)
    "status" -> Ok(Status)
    _ -> Error(InvalidAttribute(attr))
  }
}

/// Split value from query string
fn split_query(s: String) -> #(String, Option(String)) {
  case string.split_once(s, "?") {
    Ok(#(value, query)) -> #(value, Some(query))
    Error(Nil) -> #(s, None)
  }
}

/// Get a query parameter value
fn get_query_param(query: Option(String), key: String) -> Option(String) {
  case query {
    None -> None
    Some(q) -> {
      case uri.parse_query(q) {
        Ok(params) ->
          params
          |> list.find(fn(p) { p.0 == key })
          |> result.map(fn(p) { p.1 })
          |> option.from_result
        Error(_) -> None
      }
    }
  }
}
```

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
git add server/src/lib/oauth/scopes/parser.gleam server/test/oauth/scopes/parser_test.gleam
git commit -m "feat(oauth): add account scope parsing"
```

---

## Task 6: Parse Identity Scopes

**Files:**
- Modify: `server/src/lib/oauth/scopes/parser.gleam`
- Modify: `server/test/oauth/scopes/parser_test.gleam`

**Step 1: Write the failing test**

Add to `server/test/oauth/scopes/parser_test.gleam`:

```gleam
pub fn parse_identity_handle_test() {
  parser.parse_scope("identity:handle")
  |> should.be_ok
  |> should.equal(types.Identity(types.IdentityScope(attribute: types.Handle)))
}

pub fn parse_identity_all_test() {
  parser.parse_scope("identity:*")
  |> should.be_ok
  |> should.equal(types.Identity(types.IdentityScope(attribute: types.All)))
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: FAIL - identity scopes not yet parsed

**Step 3: Write minimal implementation**

Add to `server/src/lib/oauth/scopes/parser.gleam` in `parse_parameterized`:

```gleam
fn parse_parameterized(token: String) -> Result(Scope, ParseError) {
  case string.split_once(token, ":") {
    Ok(#("account", rest)) -> parse_account_scope(token, rest)
    Ok(#("identity", rest)) -> parse_identity_scope(token, rest)
    Ok(#(prefix, _)) ->
      Error(InvalidScopeFormat(token, "unknown prefix: " <> prefix))
    Error(Nil) -> Error(InvalidScopeFormat(token, "missing scope prefix"))
  }
}
```

Add the parsing function:

```gleam
/// Parse identity scope: identity:handle or identity:*
fn parse_identity_scope(
  original: String,
  rest: String,
) -> Result(Scope, ParseError) {
  let #(attr_str, _query) = split_query(rest)

  case attr_str {
    "handle" -> Ok(Identity(IdentityScope(attribute: Handle)))
    "*" -> Ok(Identity(IdentityScope(attribute: All)))
    _ -> Error(InvalidAttribute(attr_str))
  }
}
```

Also update imports:

```gleam
import lib/oauth/scopes/types.{
  type AccountAttribute, type Action, type Scope, type StaticScope, Account,
  AccountScope, All, Atproto, Create, Delete, Email, Handle, Identity,
  IdentityScope, Manage, Read, Repo, Static, Status, TransitionChatBsky,
  TransitionEmail, TransitionGeneric, Update,
}
```

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
git add server/src/lib/oauth/scopes/parser.gleam server/test/oauth/scopes/parser_test.gleam
git commit -m "feat(oauth): add identity scope parsing"
```

---

## Task 7: Parse Repo Scopes

**Files:**
- Modify: `server/src/lib/oauth/scopes/parser.gleam`
- Modify: `server/test/oauth/scopes/parser_test.gleam`

**Step 1: Write the failing test**

Add to `server/test/oauth/scopes/parser_test.gleam`:

```gleam
pub fn parse_repo_wildcard_test() {
  parser.parse_scope("repo:*")
  |> should.be_ok
  |> should.equal(types.Repo(types.RepoScope(
    collection: "*",
    actions: [types.Create, types.Update, types.Delete],
  )))
}

pub fn parse_repo_specific_collection_test() {
  parser.parse_scope("repo:app.bsky.feed.post")
  |> should.be_ok
  |> should.equal(types.Repo(types.RepoScope(
    collection: "app.bsky.feed.post",
    actions: [types.Create, types.Update, types.Delete],
  )))
}

pub fn parse_repo_with_single_action_test() {
  parser.parse_scope("repo:app.bsky.feed.post?action=create")
  |> should.be_ok
  |> should.equal(types.Repo(types.RepoScope(
    collection: "app.bsky.feed.post",
    actions: [types.Create],
  )))
}

pub fn parse_repo_with_multiple_actions_test() {
  parser.parse_scope("repo:*?action=create&action=delete")
  |> should.be_ok
  |> should.equal(types.Repo(types.RepoScope(
    collection: "*",
    actions: [types.Create, types.Delete],
  )))
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: FAIL - repo scopes not yet parsed

**Step 3: Write minimal implementation**

Add to `server/src/lib/oauth/scopes/parser.gleam` in `parse_parameterized`:

```gleam
fn parse_parameterized(token: String) -> Result(Scope, ParseError) {
  case string.split_once(token, ":") {
    Ok(#("account", rest)) -> parse_account_scope(token, rest)
    Ok(#("identity", rest)) -> parse_identity_scope(token, rest)
    Ok(#("repo", rest)) -> parse_repo_scope(token, rest)
    Ok(#(prefix, _)) ->
      Error(InvalidScopeFormat(token, "unknown prefix: " <> prefix))
    Error(Nil) -> Error(InvalidScopeFormat(token, "missing scope prefix"))
  }
}
```

Add the parsing function:

```gleam
/// Parse repo scope: repo:app.bsky.feed.post?action=create
fn parse_repo_scope(
  original: String,
  rest: String,
) -> Result(Scope, ParseError) {
  let #(collection, query) = split_query(rest)

  case string.is_empty(collection) {
    True -> Error(InvalidScopeFormat(original, "missing collection"))
    False -> {
      let actions = case query {
        None -> [Create, Update, Delete]
        Some(q) -> parse_repo_actions(q)
      }

      case list.is_empty(actions) {
        True -> Error(InvalidScopeFormat(original, "no valid actions"))
        False -> Ok(Repo(RepoScope(collection: collection, actions: actions)))
      }
    }
  }
}

/// Parse actions from query string for repo scope
fn parse_repo_actions(query: String) -> List(Action) {
  case uri.parse_query(query) {
    Ok(params) ->
      params
      |> list.filter_map(fn(p) {
        case p.0 {
          "action" ->
            case p.1 {
              "create" -> Ok(Create)
              "update" -> Ok(Update)
              "delete" -> Ok(Delete)
              _ -> Error(Nil)
            }
          _ -> Error(Nil)
        }
      })
    Error(_) -> []
  }
}
```

Also update imports to include `Repo` and `RepoScope`.

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
git add server/src/lib/oauth/scopes/parser.gleam server/test/oauth/scopes/parser_test.gleam
git commit -m "feat(oauth): add repo scope parsing"
```

---

## Task 8: Parse Blob Scopes

**Files:**
- Modify: `server/src/lib/oauth/scopes/parser.gleam`
- Modify: `server/test/oauth/scopes/parser_test.gleam`

**Step 1: Write the failing test**

Add to `server/test/oauth/scopes/parser_test.gleam`:

```gleam
pub fn parse_blob_wildcard_test() {
  parser.parse_scope("blob:*/*")
  |> should.be_ok
  |> should.equal(types.Blob(types.BlobScope(mime_type: "*/*")))
}

pub fn parse_blob_image_wildcard_test() {
  parser.parse_scope("blob:image/*")
  |> should.be_ok
  |> should.equal(types.Blob(types.BlobScope(mime_type: "image/*")))
}

pub fn parse_blob_specific_type_test() {
  parser.parse_scope("blob:image/png")
  |> should.be_ok
  |> should.equal(types.Blob(types.BlobScope(mime_type: "image/png")))
}

pub fn parse_blob_invalid_mime_test() {
  parser.parse_scope("blob:invalid")
  |> should.be_error
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: FAIL - blob scopes not yet parsed

**Step 3: Write minimal implementation**

Add to `parse_parameterized`:

```gleam
    Ok(#("blob", rest)) -> parse_blob_scope(token, rest)
```

Add the parsing function:

```gleam
/// Parse blob scope: blob:image/* or blob:*/*
fn parse_blob_scope(
  original: String,
  mime_type: String,
) -> Result(Scope, ParseError) {
  case validate_mime_type(mime_type) {
    True -> Ok(Blob(BlobScope(mime_type: mime_type)))
    False -> Error(InvalidMimeType(mime_type))
  }
}

/// Validate MIME type format
fn validate_mime_type(mime: String) -> Bool {
  case string.split(mime, "/") {
    [type_part, subtype] ->
      !string.is_empty(type_part) && !string.is_empty(subtype)
    _ -> False
  }
}
```

Update imports to include `Blob`, `BlobScope`, and `InvalidMimeType`.

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
git add server/src/lib/oauth/scopes/parser.gleam server/test/oauth/scopes/parser_test.gleam
git commit -m "feat(oauth): add blob scope parsing"
```

---

## Task 9: Parse RPC Scopes

**Files:**
- Modify: `server/src/lib/oauth/scopes/parser.gleam`
- Modify: `server/test/oauth/scopes/parser_test.gleam`

**Step 1: Write the failing test**

Add to `server/test/oauth/scopes/parser_test.gleam`:

```gleam
pub fn parse_rpc_specific_method_test() {
  parser.parse_scope("rpc:app.bsky.feed.getFeed?aud=did:web:bsky.app")
  |> should.be_ok
  |> should.equal(types.Rpc(types.RpcScope(
    methods: ["app.bsky.feed.getFeed"],
    audience: "did:web:bsky.app",
  )))
}

pub fn parse_rpc_wildcard_method_specific_aud_test() {
  parser.parse_scope("rpc:*?aud=did:web:api.bsky.app")
  |> should.be_ok
  |> should.equal(types.Rpc(types.RpcScope(
    methods: ["*"],
    audience: "did:web:api.bsky.app",
  )))
}

pub fn parse_rpc_missing_aud_test() {
  parser.parse_scope("rpc:app.bsky.feed.getFeed")
  |> should.be_error
}

pub fn parse_rpc_wildcard_both_test() {
  // rpc:* with aud=* is invalid
  parser.parse_scope("rpc:*?aud=*")
  |> should.be_error
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: FAIL - rpc scopes not yet parsed

**Step 3: Write minimal implementation**

Add to `parse_parameterized`:

```gleam
    Ok(#("rpc", rest)) -> parse_rpc_scope(token, rest)
```

Add the parsing function:

```gleam
/// Parse rpc scope: rpc:app.bsky.feed.getFeed?aud=did:web:bsky.app
fn parse_rpc_scope(
  original: String,
  rest: String,
) -> Result(Scope, ParseError) {
  let #(method, query) = split_query(rest)

  case string.is_empty(method) {
    True -> Error(InvalidScopeFormat(original, "missing method"))
    False -> {
      case get_query_param(query, "aud") {
        None -> Error(InvalidRpcScope("aud parameter is required"))
        Some(aud) -> {
          // Validate: can't have wildcard method with wildcard audience
          case method == "*" && aud == "*" {
            True ->
              Error(InvalidRpcScope(
                "wildcard method requires specific audience",
              ))
            False -> Ok(Rpc(RpcScope(methods: [method], audience: aud)))
          }
        }
      }
    }
  }
}
```

Update imports to include `Rpc`, `RpcScope`, and `InvalidRpcScope`.

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
git add server/src/lib/oauth/scopes/parser.gleam server/test/oauth/scopes/parser_test.gleam
git commit -m "feat(oauth): add rpc scope parsing"
```

---

## Task 10: Parse Include Scopes

**Files:**
- Modify: `server/src/lib/oauth/scopes/parser.gleam`
- Modify: `server/test/oauth/scopes/parser_test.gleam`

**Step 1: Write the failing test**

Add to `server/test/oauth/scopes/parser_test.gleam`:

```gleam
pub fn parse_include_simple_test() {
  parser.parse_scope("include:app.bsky.feed")
  |> should.be_ok
  |> should.equal(types.Include(types.IncludeScope(
    nsid: "app.bsky.feed",
    audience: None,
  )))
}

pub fn parse_include_with_aud_test() {
  parser.parse_scope("include:chat.bsky.moderation?aud=did:web:bsky.chat")
  |> should.be_ok
  |> should.equal(types.Include(types.IncludeScope(
    nsid: "chat.bsky.moderation",
    audience: Some("did:web:bsky.chat"),
  )))
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: FAIL - include scopes not yet parsed

**Step 3: Write minimal implementation**

Add to `parse_parameterized`:

```gleam
    Ok(#("include", rest)) -> parse_include_scope(token, rest)
```

Add the parsing function:

```gleam
/// Parse include scope: include:app.bsky.feed?aud=did:web:...
fn parse_include_scope(
  original: String,
  rest: String,
) -> Result(Scope, ParseError) {
  let #(nsid, query) = split_query(rest)

  case string.is_empty(nsid) {
    True -> Error(InvalidScopeFormat(original, "missing NSID"))
    False -> {
      let audience = get_query_param(query, "aud")
      Ok(Include(IncludeScope(nsid: nsid, audience: audience)))
    }
  }
}
```

Update imports to include `Include` and `IncludeScope`.

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
git add server/src/lib/oauth/scopes/parser.gleam server/test/oauth/scopes/parser_test.gleam
git commit -m "feat(oauth): add include scope parsing"
```

---

## Task 11: Add parse_scopes Function

**Files:**
- Modify: `server/src/lib/oauth/scopes/parser.gleam`
- Modify: `server/test/oauth/scopes/parser_test.gleam`

**Step 1: Write the failing test**

Add to `server/test/oauth/scopes/parser_test.gleam`:

```gleam
pub fn parse_scopes_multiple_test() {
  parser.parse_scopes("atproto repo:* account:email")
  |> should.be_ok
  |> should.equal([
    types.Static(types.Atproto),
    types.Repo(types.RepoScope(
      collection: "*",
      actions: [types.Create, types.Update, types.Delete],
    )),
    types.Account(types.AccountScope(
      attribute: types.Email,
      action: types.Read,
    )),
  ])
}

pub fn parse_scopes_empty_test() {
  parser.parse_scopes("")
  |> should.be_ok
  |> should.equal([])
}

pub fn parse_scopes_single_invalid_fails_test() {
  parser.parse_scopes("atproto invalid::: repo:*")
  |> should.be_error
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: FAIL - parse_scopes not defined

**Step 3: Write minimal implementation**

Add to `server/src/lib/oauth/scopes/parser.gleam`:

```gleam
/// Parse a space-separated scope string into list of Scope
pub fn parse_scopes(scope_string: String) -> Result(List(Scope), ParseError) {
  case string.is_empty(string.trim(scope_string)) {
    True -> Ok([])
    False -> {
      scope_string
      |> string.split(" ")
      |> list.filter(fn(s) { !string.is_empty(s) })
      |> list.try_map(parse_scope)
    }
  }
}
```

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
git add server/src/lib/oauth/scopes/parser.gleam server/test/oauth/scopes/parser_test.gleam
git commit -m "feat(oauth): add parse_scopes for space-separated scope strings"
```

---

## Task 12: Create Scope Validator Module

**Files:**
- Create: `server/src/lib/oauth/scopes/validator.gleam`
- Test: `server/test/oauth/scopes/validator_test.gleam`

**Step 1: Write the failing test**

Create `server/test/oauth/scopes/validator_test.gleam`:

```gleam
import gleam/option.{None, Some}
import gleeunit/should
import lib/oauth/scopes/validator
import lib/oauth/types/error

pub fn validate_scope_format_valid_test() {
  validator.validate_scope_format("atproto repo:* account:email")
  |> should.be_ok
}

pub fn validate_scope_format_invalid_test() {
  validator.validate_scope_format("atproto invalid:::")
  |> should.be_error
}

pub fn validate_scope_format_empty_test() {
  validator.validate_scope_format("")
  |> should.be_ok
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: FAIL - module not found

**Step 3: Write minimal implementation**

Create `server/src/lib/oauth/scopes/validator.gleam`:

```gleam
/// OAuth scope validation

import lib/oauth/scopes/parse_error
import lib/oauth/scopes/parser
import lib/oauth/scopes/types.{type Scope}
import lib/oauth/types/error.{type OAuthError, InvalidScope}

/// Validate scope string format and parse into structured scopes
pub fn validate_scope_format(scope_string: String) -> Result(List(Scope), OAuthError) {
  parser.parse_scopes(scope_string)
  |> result.map_error(fn(e) { InvalidScope(parse_error.to_string(e)) })
}

import gleam/result
```

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
git add server/src/lib/oauth/scopes/validator.gleam server/test/oauth/scopes/validator_test.gleam
git commit -m "feat(oauth): add scope format validation"
```

---

## Task 13: Integrate Scope Validation in Registration

**Files:**
- Modify: `server/src/handlers/oauth/register.gleam:75-155`
- Modify: `server/test/oauth/register_test.gleam`

**Step 1: Write the failing test**

Add to `server/test/oauth/register_test.gleam`:

```gleam
pub fn register_valid_scope_test() {
  let assert Ok(conn) = sqlight.open(":memory:")
  let assert Ok(_) = tables.create_oauth_client_table(conn)

  let body =
    json.object([
      #("client_name", json.string("Test Client")),
      #(
        "redirect_uris",
        json.array([json.string("https://example.com/callback")], fn(x) { x }),
      ),
      #("scope", json.string("atproto repo:app.bsky.feed.post account:email")),
    ])
    |> json.to_string

  let req =
    simulate.request(http.Post, "/oauth/register")
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/json")

  let response = register.handle(req, conn)

  response.status |> should.equal(201)
}

pub fn register_invalid_scope_test() {
  let assert Ok(conn) = sqlight.open(":memory:")
  let assert Ok(_) = tables.create_oauth_client_table(conn)

  let body =
    json.object([
      #("client_name", json.string("Test Client")),
      #(
        "redirect_uris",
        json.array([json.string("https://example.com/callback")], fn(x) { x }),
      ),
      #("scope", json.string("atproto invalid:::")),
    ])
    |> json.to_string

  let req =
    simulate.request(http.Post, "/oauth/register")
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/json")

  let response = register.handle(req, conn)

  response.status |> should.equal(400)
  case response.body {
    wisp.Text(body) -> {
      body |> string.contains("invalid_scope") |> should.be_true
    }
    _ -> should.fail()
  }
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: FAIL - invalid scope test fails (currently accepts any scope)

**Step 3: Write minimal implementation**

Update `server/src/handlers/oauth/register.gleam`. Add import:

```gleam
import lib/oauth/scopes/validator as scope_validator
```

Update `parse_and_register` function around line 91 (after redirect_uris validation):

```gleam
  // Validate scope format if provided
  use _ <- result.try(case req.scope {
    Some(scope_str) ->
      scope_validator.validate_scope_format(scope_str)
      |> result.map(fn(_) { Nil })
      |> result.map_error(fn(e) {
        #(400, "invalid_scope", error.error_description(e))
      })
    None -> Ok(Nil)
  })
```

Also add import for error:

```gleam
import lib/oauth/types/error
```

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
git add server/src/handlers/oauth/register.gleam server/test/oauth/register_test.gleam
git commit -m "feat(oauth): validate scopes during client registration"
```

---

## Task 14: Integrate Scope Validation in Authorization

**Files:**
- Modify: `server/src/handlers/oauth/authorize.gleam:192-226`
- Modify: `server/test/oauth/authorize_test.gleam`

**Step 1: Write the failing test**

Add to `server/test/oauth/authorize_test.gleam` (create if doesn't exist appropriate test):

```gleam
pub fn authorize_invalid_scope_test() {
  // Setup test database and client
  let assert Ok(conn) = sqlight.open(":memory:")
  // ... setup client ...

  // Test with invalid scope
  let query = "response_type=code&client_id=test&redirect_uri=https://example.com/callback&scope=invalid:::"

  // Expect error response for invalid scope format
  // The exact test depends on your existing test patterns
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: FAIL

**Step 3: Write minimal implementation**

Update `server/src/handlers/oauth/authorize.gleam`. Add import:

```gleam
import lib/oauth/scopes/validator as scope_validator
```

Update `validate_authorization_request` function around line 192. Add scope validation after PKCE validation:

```gleam
fn validate_authorization_request(
  req: AuthorizationRequest,
  client: types.OAuthClient,
) -> Result(Nil, String) {
  // ... existing validations ...

  // Validate scope format if provided
  use _ <- result.try(case req.scope {
    Some(scope_str) ->
      scope_validator.validate_scope_format(scope_str)
      |> result.map(fn(_) { Nil })
      |> result.map_error(fn(e) { error.error_description(e) })
    None -> Ok(Nil)
  })

  Ok(Nil)
}
```

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
git add server/src/handlers/oauth/authorize.gleam server/test/oauth/authorize_test.gleam
git commit -m "feat(oauth): validate scopes during authorization"
```

---

## Task 15: Integrate Scope Validation in Token Refresh

**Files:**
- Modify: `server/src/handlers/oauth/token.gleam:193-332`
- Modify: `server/test/oauth/token_test.gleam`

**Step 1: Write the failing test**

Add to `server/test/oauth/token_test.gleam`:

```gleam
pub fn refresh_token_invalid_scope_test() {
  // Setup: Create client, authorization code, tokens
  // ...

  // Try to refresh with invalid scope format
  let body = "grant_type=refresh_token&refresh_token=xxx&client_id=xxx&scope=invalid:::"

  // Expect 400 with invalid_scope error
}
```

**Step 2: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: FAIL

**Step 3: Write minimal implementation**

Update `server/src/handlers/oauth/token.gleam`. Add import:

```gleam
import lib/oauth/scopes/validator as scope_validator
import lib/oauth/types/error
```

In `handle_refresh_token` around line 248 (where scope is determined), add validation:

```gleam
// Validate scope format if new scope requested
use _ <- result.try(case requested_scope {
  Some(scope_str) ->
    case scope_validator.validate_scope_format(scope_str) {
      Ok(_) -> Ok(Nil)
      Error(e) ->
        Error(error_response(400, "invalid_scope", error.error_description(e)))
    }
  None -> Ok(Nil)
})
```

**Step 4: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: PASS

**Step 5: Commit**

```bash
git add server/src/handlers/oauth/token.gleam server/test/oauth/token_test.gleam
git commit -m "feat(oauth): validate scopes during token refresh"
```

---

## Task 16: Final Integration Test

**Files:**
- Create: `server/test/oauth/scopes/integration_test.gleam`

**Step 1: Write comprehensive integration test**

Create `server/test/oauth/scopes/integration_test.gleam`:

```gleam
/// Integration tests for OAuth scope validation across all endpoints

import database/schema/tables
import gleam/http
import gleam/json
import gleam/string
import gleeunit/should
import handlers/oauth/register
import sqlight
import wisp
import wisp/simulate

/// Test that all ATProto scope types are accepted
pub fn all_atproto_scopes_accepted_test() {
  let assert Ok(conn) = sqlight.open(":memory:")
  let assert Ok(_) = tables.create_oauth_client_table(conn)

  let all_scopes =
    "atproto transition:generic transition:email transition:chat.bsky "
    <> "account:email account:repo?action=manage account:status "
    <> "identity:handle identity:* "
    <> "repo:* repo:app.bsky.feed.post?action=create "
    <> "blob:*/* blob:image/* blob:image/png "
    <> "rpc:app.bsky.feed.getFeed?aud=did:web:bsky.app "
    <> "include:app.bsky.feed include:chat.bsky?aud=did:web:bsky.chat"

  let body =
    json.object([
      #("client_name", json.string("Full Scope Test")),
      #(
        "redirect_uris",
        json.array([json.string("https://example.com/callback")], fn(x) { x }),
      ),
      #("scope", json.string(all_scopes)),
    ])
    |> json.to_string

  let req =
    simulate.request(http.Post, "/oauth/register")
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/json")

  let response = register.handle(req, conn)

  response.status |> should.equal(201)
}
```

**Step 2: Run test to verify it passes**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: PASS

**Step 3: Commit**

```bash
git add server/test/oauth/scopes/integration_test.gleam
git commit -m "test(oauth): add comprehensive scope integration test"
```

---

## Summary

This plan implements full ATProto OAuth scope parsing and validation:

1. **Tasks 1-3**: Create type definitions and error types
2. **Tasks 4-10**: Build parser for each scope type (static, account, identity, repo, blob, rpc, include)
3. **Task 11**: Add `parse_scopes` for space-separated strings
4. **Task 12**: Create validator module bridging parser to OAuth errors
5. **Tasks 13-15**: Integrate validation at registration, authorization, and token refresh
6. **Task 16**: Comprehensive integration test

Each task follows TDD: write failing test → implement → verify → commit.
