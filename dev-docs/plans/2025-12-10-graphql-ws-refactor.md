# GraphQL WebSocket Module Refactor Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Move `graphql_ws.gleam` into the `graphql/` folder and replace manual JSON string concatenation with `gleam_json`.

**Architecture:** The protocol module (`graphql_ws.gleam`) belongs with GraphQL concerns, not as a top-level module. The handler stays in `handlers/` following existing conventions. JSON formatting uses `gleam_json` with `json.preprocessed()` for already-serialized payloads.

**Tech Stack:** Gleam, gleam_json

---

### Task 1: Create graphql/ws.gleam with gleam_json formatting

**Files:**
- Create: `server/src/graphql/ws.gleam`
- Reference: `server/src/graphql_ws.gleam` (copy and modify)

**Step 1: Create the new module**

Create `server/src/graphql/ws.gleam` with the refactored code:

```gleam
/// GraphQL-WS Protocol Implementation
///
/// Implements the graphql-ws WebSocket subprotocol for GraphQL subscriptions
/// Spec: https://github.com/enisdenjo/graphql-ws/blob/master/PROTOCOL.md
import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result

/// Client-to-server message types
/// Server-to-client message types
pub type Message {
  // Client messages
  ConnectionInit(payload: Dict(String, String))
  Subscribe(id: String, query: String, variables: Option(String))
  Complete(id: String)
  Ping
  Pong

  // Server messages
  ConnectionAck
  Next(id: String, data: String)
  ErrorMessage(id: String, message: String)
}

/// Parse a JSON string into a GraphQL-WS message
pub fn parse_message(json_str: String) -> Result(Message, String) {
  //  First parse to extract the type field
  let type_decoder = {
    use message_type <- decode.field("type", decode.string)
    decode.success(message_type)
  }

  use message_type <- result.try(
    json.parse(json_str, type_decoder)
    |> result.map_error(fn(_) { "Missing or invalid 'type' field" }),
  )

  case message_type {
    "connection_init" -> {
      // Try to extract payload, but it's optional
      let payload =
        extract_string_payload(json_str) |> option.unwrap(dict.new())
      Ok(ConnectionInit(payload))
    }

    "subscribe" -> {
      let subscribe_decoder = {
        use id <- decode.field("id", decode.string)
        use query <- decode.subfield(["payload", "query"], decode.string)
        decode.success(#(id, query))
      }

      use #(id, query) <- result.try(
        json.parse(json_str, subscribe_decoder)
        |> result.map_error(fn(_) {
          "Subscribe message missing required fields"
        }),
      )

      // Variables are optional - try to extract them
      let vars = extract_variables_from_json(json_str)

      Ok(Subscribe(id, query, vars))
    }

    "complete" -> {
      let complete_decoder = {
        use id <- decode.field("id", decode.string)
        decode.success(id)
      }

      use id <- result.try(
        json.parse(json_str, complete_decoder)
        |> result.map_error(fn(_) { "Complete message missing 'id' field" }),
      )

      Ok(Complete(id))
    }

    "ping" -> Ok(Ping)

    "pong" -> Ok(Pong)

    _ -> {
      let err_msg = "Unknown message type: " <> message_type
      Error(err_msg)
    }
  }
}

/// Format a GraphQL-WS message as JSON string
pub fn format_message(message: Message) -> String {
  case message {
    ConnectionAck ->
      json.object([#("type", json.string("connection_ack"))])
      |> json.to_string

    Next(id, data) ->
      json.object([
        #("id", json.string(id)),
        #("type", json.string("next")),
        #("payload", json.preprocessed(data)),
      ])
      |> json.to_string

    ErrorMessage(id, msg) ->
      json.object([
        #("id", json.string(id)),
        #("type", json.string("error")),
        #("payload", json.preprocessed_array([
          json.object([#("message", json.string(msg))]),
        ])),
      ])
      |> json.to_string

    Complete(id) ->
      json.object([
        #("id", json.string(id)),
        #("type", json.string("complete")),
      ])
      |> json.to_string

    Pong ->
      json.object([#("type", json.string("pong"))])
      |> json.to_string

    Ping ->
      json.object([#("type", json.string("ping"))])
      |> json.to_string

    ConnectionInit(_) ->
      json.object([#("type", json.string("connection_init"))])
      |> json.to_string

    Subscribe(id, _, _) ->
      json.object([
        #("id", json.string(id)),
        #("type", json.string("subscribe")),
      ])
      |> json.to_string
  }
}

// Helper to extract payload field as dict of strings
fn extract_string_payload(json_str: String) -> Option(Dict(String, String)) {
  let decoder = {
    use payload <- decode.field(
      "payload",
      decode.dict(decode.string, decode.string),
    )
    decode.success(payload)
  }

  json.parse(json_str, decoder)
  |> result.map(Some)
  |> result.unwrap(None)
}

// Helper to extract variables from subscribe message
fn extract_variables_from_json(json_str: String) -> Option(String) {
  let vars_decoder = {
    use vars <- decode.subfield(["payload", "variables"], decode.string)
    decode.success(vars)
  }

  json.parse(json_str, vars_decoder)
  |> result.map(Some)
  |> result.unwrap(None)
}
```

**Step 2: Build to verify no compile errors**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds (warnings about unused module are OK)

**Step 3: Commit**

```bash
git add server/src/graphql/ws.gleam
git commit -m "feat: add graphql/ws module with gleam_json formatting"
```

---

### Task 2: Move and update test file

**Files:**
- Create: `server/test/graphql/ws_test.gleam`
- Reference: `server/test/graphql_ws_test.gleam`

**Step 1: Read current test file**

Read `server/test/graphql_ws_test.gleam` to understand existing tests.

**Step 2: Create updated test file**

Create `server/test/graphql/ws_test.gleam` with updated imports:
- Change `import graphql_ws` to `import graphql/ws`
- Change all `graphql_ws.` references to `ws.`

**Step 3: Run tests to verify they pass**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: All ws_test tests pass

**Step 4: Commit**

```bash
git add server/test/graphql/ws_test.gleam
git commit -m "test: move graphql_ws tests to graphql/ws_test"
```

---

### Task 3: Update handler imports

**Files:**
- Modify: `server/src/handlers/graphql_ws.gleam`

**Step 1: Update import statement**

Change line 14:
```gleam
// From:
import graphql_ws

// To:
import graphql/ws
```

**Step 2: Update all references**

Replace all occurrences of `graphql_ws.` with `ws.`:
- `graphql_ws.format_message` → `ws.format_message`
- `graphql_ws.parse_message` → `ws.parse_message`
- `graphql_ws.ConnectionInit` → `ws.ConnectionInit`
- `graphql_ws.Subscribe` → `ws.Subscribe`
- `graphql_ws.Complete` → `ws.Complete`
- `graphql_ws.Ping` → `ws.Ping`
- `graphql_ws.Pong` → `ws.Pong`
- `graphql_ws.ConnectionAck` → `ws.ConnectionAck`
- `graphql_ws.Next` → `ws.Next`
- `graphql_ws.ErrorMessage` → `ws.ErrorMessage`

**Step 3: Build to verify**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 4: Run tests**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: All tests pass

**Step 5: Commit**

```bash
git add server/src/handlers/graphql_ws.gleam
git commit -m "refactor: update handler to use graphql/ws module"
```

---

### Task 4: Remove old files

**Files:**
- Delete: `server/src/graphql_ws.gleam`
- Delete: `server/test/graphql_ws_test.gleam`

**Step 1: Delete old source file**

```bash
rm server/src/graphql_ws.gleam
```

**Step 2: Delete old test file**

```bash
rm server/test/graphql_ws_test.gleam
```

**Step 3: Build and test to verify everything still works**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build && gleam test`
Expected: Build succeeds, all tests pass

**Step 4: Commit**

```bash
git add -A
git commit -m "chore: remove old graphql_ws files"
```

---

### Task 5: Final verification

**Step 1: Run full test suite**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: All tests pass

**Step 2: Verify no references to old module remain**

Run: `grep -r "import graphql_ws" /Users/chadmiller/code/quickslice/server/src/`
Expected: No output (no remaining imports)

**Step 3: Verify new structure**

Run: `ls -la /Users/chadmiller/code/quickslice/server/src/graphql/`
Expected: Shows `ws.gleam` alongside `admin/`, `lexicon/`, `where_converter.gleam`
