# Mutation Subscription Events Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Publish pubsub events after GraphQL mutations so WebSocket subscriptions receive updates immediately.

**Architecture:** Add `pubsub.publish()` calls after database operations in create/update/delete mutation resolvers. Export existing `microseconds_to_iso8601` from event_handler and add a `current_timestamp_us` helper.

**Tech Stack:** Gleam, Erlang FFI, existing pubsub module

---

## Task 1: Export timestamp functions from event_handler

**Files:**
- Modify: `server/src/event_handler.gleam:54-57`

**Step 1: Make microseconds_to_iso8601 public and add current_timestamp_us**

Replace lines 54-57:

```gleam
/// Convert microseconds since Unix epoch to ISO8601 format
/// Uses the event's original timestamp for accurate indexedAt values
@external(erlang, "event_handler_ffi", "microseconds_to_iso8601")
fn microseconds_to_iso8601(time_us: Int) -> String
```

With:

```gleam
/// Convert microseconds since Unix epoch to ISO8601 format
@external(erlang, "event_handler_ffi", "microseconds_to_iso8601")
pub fn microseconds_to_iso8601(time_us: Int) -> String

/// Get current timestamp in microseconds
@external(erlang, "os", "system_time")
fn system_time_native() -> Int

/// Get current time as ISO8601 string
pub fn current_iso8601() -> String {
  // os:system_time() returns nanoseconds, convert to microseconds
  microseconds_to_iso8601(system_time_native() / 1000)
}
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add server/src/event_handler.gleam
git commit -m "feat: export timestamp helpers from event_handler"
```

---

## Task 2: Add imports to mutations.gleam

**Files:**
- Modify: `server/src/graphql/lexicon/mutations.gleam:1-25` (add imports)

**Step 1: Add event_handler and pubsub imports**

In `server/src/graphql/lexicon/mutations.gleam`, after line 21 (`import honk/errors`), add:

```gleam
import event_handler
import pubsub
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add server/src/graphql/lexicon/mutations.gleam
git commit -m "feat: add event_handler and pubsub imports to mutations"
```

---

## Task 3: Publish event after create mutation

**Files:**
- Modify: `server/src/graphql/lexicon/mutations.gleam:490-513`

**Step 1: Add pubsub.publish after records.insert**

Replace lines 490-513 with:

```gleam
    // Index the created record in the database
    use _ <- result.try(
      records.insert(
        ctx.db,
        uri,
        cid,
        auth.user_info.did,
        collection,
        record_json_string,
      )
      |> result.map_error(fn(_) { "Failed to index record in database" }),
    )

    // Publish event for GraphQL subscriptions
    pubsub.publish(pubsub.RecordEvent(
      uri: uri,
      cid: cid,
      did: auth.user_info.did,
      collection: collection,
      value: record_json_string,
      indexed_at: event_handler.current_iso8601(),
      operation: pubsub.Create,
    ))

    Ok(
      value.Object([
        #("uri", value.String(uri)),
        #("cid", value.String(cid)),
        #("did", value.String(auth.user_info.did)),
        #("collection", value.String(collection)),
        #("indexedAt", value.String("")),
        #("value", input),
      ]),
    )
  }
}
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add server/src/graphql/lexicon/mutations.gleam
git commit -m "feat: publish subscription event after create mutation"
```

---

## Task 4: Publish event after update mutation

**Files:**
- Modify: `server/src/graphql/lexicon/mutations.gleam:610-626`

**Step 1: Add pubsub.publish after records.update**

Replace lines 610-626 with:

```gleam
    // Update the record in the database
    use _ <- result.try(
      records.update(ctx.db, uri, cid, record_json_string)
      |> result.map_error(fn(_) { "Failed to update record in database" }),
    )

    // Publish event for GraphQL subscriptions
    pubsub.publish(pubsub.RecordEvent(
      uri: uri,
      cid: cid,
      did: auth.user_info.did,
      collection: collection,
      value: record_json_string,
      indexed_at: event_handler.current_iso8601(),
      operation: pubsub.Update,
    ))

    Ok(
      value.Object([
        #("uri", value.String(uri)),
        #("cid", value.String(cid)),
        #("did", value.String(auth.user_info.did)),
        #("collection", value.String(collection)),
        #("indexedAt", value.String("")),
        #("value", input),
      ]),
    )
  }
}
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add server/src/graphql/lexicon/mutations.gleam
git commit -m "feat: publish subscription event after update mutation"
```

---

## Task 5: Publish event after delete mutation

**Files:**
- Modify: `server/src/graphql/lexicon/mutations.gleam:678-685`

**Step 1: Add pubsub.publish after records.delete**

Replace lines 678-685 with:

```gleam
    // Delete the record from the database
    use _ <- result.try(
      records.delete(ctx.db, uri)
      |> result.map_error(fn(_) { "Failed to delete record from database" }),
    )

    // Publish event for GraphQL subscriptions
    pubsub.publish(pubsub.RecordEvent(
      uri: uri,
      cid: "",
      did: auth.user_info.did,
      collection: collection,
      value: "",
      indexed_at: event_handler.current_iso8601(),
      operation: pubsub.Delete,
    ))

    Ok(value.Object([#("uri", value.String(uri))]))
  }
}
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add server/src/graphql/lexicon/mutations.gleam
git commit -m "feat: publish subscription event after delete mutation"
```

---

## Task 6: Manual test

**Step 1: Start the server**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam run`

**Step 2: Test subscription receives mutation events**

1. Open GraphQL playground in browser
2. Start a subscription for a collection
3. In another tab, run a create mutation
4. Verify the subscription receives the event immediately (not delayed by Jetstream round-trip)

**Step 3: Final commit (if any fixes needed)**

If tests reveal issues, fix and commit with appropriate message.
