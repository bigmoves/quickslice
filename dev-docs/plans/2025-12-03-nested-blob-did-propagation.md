# Nested Blob DID Propagation Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix `embed.images[].image.url` returning null by propagating `did` through nested object resolution.

**Architecture:** Inject `did` into nested objects at record-level field resolution (database.gleam), then propagate and enrich blob fields in nested object types (object_builder.gleam). The `did` flows from record level through intermediate objects to nested blobs.

**Tech Stack:** Gleam, swell (GraphQL library), lexicon_graphql package

---

### Task 1: Add `did` injection helper to database.gleam

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/schema/database.gleam`

**Step 1: Add helper function after `extract_blob_data` (around line 2094)**

```gleam
/// Inject DID into a value for nested blob resolution
/// Recursively adds "did" field to objects that might contain blobs
fn inject_did_into_value(val: value.Value, did: String) -> value.Value {
  case val {
    value.Object(fields) -> {
      // Add did if not already present
      let has_did = list.any(fields, fn(f) { f.0 == "did" })
      case has_did {
        True -> val
        False -> value.Object(list.append(fields, [#("did", value.String(did))]))
      }
    }
    value.Array(items) -> {
      value.Array(list.map(items, fn(item) { inject_did_into_value(item, did) }))
    }
    _ -> val
  }
}
```

**Step 2: Run build to verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`
Expected: Compiles with no errors (warning about unused function is OK)

**Step 3: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/schema/database.gleam
git commit -m "feat(database): add inject_did_into_value helper for nested blob resolution"
```

---

### Task 2: Use `did` injection in record field resolver

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/schema/database.gleam:1278-1285`

**Step 1: Update the non-blob field resolver branch**

Find this code (around line 1278-1285):
```gleam
_ -> {
  // Try to extract field from the value object in context
  // Use the type-safe version that preserves Int, Float, Boolean types
  case get_nested_field_value_from_context(ctx, "value", name) {
    Ok(val) -> Ok(val)
    Error(_) -> Ok(value.Null)
  }
}
```

Replace with:
```gleam
_ -> {
  // Try to extract field from the value object in context
  // Use the type-safe version that preserves Int, Float, Boolean types
  case get_nested_field_value_from_context(ctx, "value", name) {
    Ok(val) -> {
      // Inject did into nested objects for blob URL resolution
      let did = case get_field_from_context(ctx, "did") {
        Ok(d) -> d
        Error(_) -> ""
      }
      Ok(inject_did_into_value(val, did))
    }
    Error(_) -> Ok(value.Null)
  }
}
```

**Step 2: Run build to verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`
Expected: Compiles with no errors

**Step 3: Run tests**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`
Expected: All tests pass

**Step 4: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/schema/database.gleam
git commit -m "feat(database): inject did into nested objects at record level"
```

---

### Task 3: Add blob enrichment helper to object_builder.gleam

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam`

**Step 1: Add helper function at the end of the file (before closing)**

```gleam
/// Enrich a blob value with did for URL generation
/// Handles both raw blob format and AT Protocol $link format
fn enrich_blob_with_did(val: value.Value, did: String) -> value.Value {
  case val {
    value.Object(fields) -> {
      // Extract ref - handle nested $link format from AT Protocol
      let ref = case list.key_find(fields, "ref") {
        Ok(value.Object(ref_obj)) -> {
          case list.key_find(ref_obj, "$link") {
            Ok(value.String(cid)) -> cid
            _ -> ""
          }
        }
        Ok(value.String(cid)) -> cid
        _ -> ""
      }

      let mime_type = case list.key_find(fields, "mimeType") {
        Ok(value.String(mt)) -> mt
        _ -> "image/jpeg"
      }

      let size = case list.key_find(fields, "size") {
        Ok(value.Int(s)) -> s
        _ -> 0
      }

      value.Object([
        #("ref", value.String(ref)),
        #("mime_type", value.String(mime_type)),
        #("size", value.Int(size)),
        #("did", value.String(did)),
      ])
    }
    _ -> val
  }
}
```

**Step 2: Run build to verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`
Expected: Compiles with no errors (warning about unused function is OK)

**Step 3: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam
git commit -m "feat(object_builder): add enrich_blob_with_did helper"
```

---

### Task 4: Add `did` propagation helper to object_builder.gleam

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam`

**Step 1: Add helper function after enrich_blob_with_did**

```gleam
/// Propagate did through nested objects and arrays
fn propagate_did(val: value.Value, did: String) -> value.Value {
  case val {
    value.Object(fields) -> {
      let has_did = list.any(fields, fn(f) { f.0 == "did" })
      case has_did {
        True -> val
        False -> value.Object(list.append(fields, [#("did", value.String(did))]))
      }
    }
    value.Array(items) -> {
      value.Array(list.map(items, fn(item) { propagate_did(item, did) }))
    }
    _ -> val
  }
}
```

**Step 2: Run build to verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`
Expected: Compiles with no errors (warning about unused function is OK)

**Step 3: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam
git commit -m "feat(object_builder): add propagate_did helper"
```

---

### Task 5: Update field resolver to handle blob fields and propagate did

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam:67-77`

**Step 1: Update the field resolver**

Find this code (around line 67-77):
```gleam
// Create field with a resolver that extracts the value from context
schema.field(name, field_type, "Field from object definition", fn(ctx) {
  case ctx.data {
    option.Some(value.Object(fields)) -> {
      case list.key_find(fields, name) {
        Ok(val) -> Ok(val)
        Error(_) -> Ok(value.Null)
      }
    }
    _ -> Ok(value.Null)
  }
})
```

Replace with:
```gleam
// Create field with a resolver that extracts the value from context
// For blob fields, enrich with did; for nested objects, propagate did
schema.field(name, field_type, "Field from object definition", fn(ctx) {
  case ctx.data {
    option.Some(value.Object(fields)) -> {
      // Get did from parent if available (propagated from record level)
      let parent_did = case list.key_find(fields, "did") {
        Ok(value.String(d)) -> option.Some(d)
        _ -> option.None
      }

      case list.key_find(fields, name) {
        Ok(val) -> {
          case type_, parent_did {
            // For blob fields, ensure did is injected
            "blob", option.Some(did) -> Ok(enrich_blob_with_did(val, did))
            // For nested objects/arrays, propagate did
            _, option.Some(did) -> Ok(propagate_did(val, did))
            // No did available, return as-is
            _, option.None -> Ok(val)
          }
        }
        Error(_) -> Ok(value.Null)
      }
    }
    _ -> Ok(value.Null)
  }
})
```

**Step 2: Run build to verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`
Expected: Compiles with no errors

**Step 3: Run tests**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`
Expected: All tests pass

**Step 4: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam
git commit -m "feat(object_builder): handle blob fields and propagate did through nested objects"
```

---

### Task 6: Build server and verify fix

**Files:**
- None (verification only)

**Step 1: Build server**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles with no errors

**Step 2: Test via MCP query**

After restarting the server, run this query:
```graphql
query {
  appBskyFeedPost(first: 5) {
    edges {
      node {
        text
        embed {
          ... on AppBskyEmbedImages {
            images {
              image {
                url
                ref
              }
            }
          }
        }
      }
    }
  }
}
```

Expected: `image.url` returns valid CDN URLs like `https://cdn.bsky.app/img/feed_fullsize/plain/did:plc:.../bafyrei...@jpeg`

**Step 3: Final commit**

```bash
git add -A
git commit -m "test: verify nested blob URL resolution works"
```

---

## Verification Query

After implementation, this query should work without errors:

```graphql
query MyQuery {
  appBskyFeedPost {
    edges {
      node {
        text
        embed {
          ... on AppBskyEmbedImages {
            images {
              aspectRatio
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
```

The `image.url` field should return valid CDN URLs instead of `null`.
