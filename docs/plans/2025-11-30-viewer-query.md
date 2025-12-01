# Viewer Query Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add a `viewer` GraphQL query that returns the authenticated user's DID, handle, and profile via reverse join.

**Architecture:** The viewer query extracts the user DID from the Bearer token, queries the actors table for the handle, and returns a Viewer object. The Viewer type includes `appBskyActorProfileByDid` using the same reverse join pattern as other record types.

**Tech Stack:** Gleam, swell/schema, sqlight, existing batch fetcher infrastructure.

---

## Task 1: Add Viewer Type and Query to Schema Builder

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/schema/database.gleam`

**Step 1: Add viewer_fetcher type alias after existing type definitions (around line 80)**

Add after the `AggregateFetcher` type:

```gleam
/// Fetches viewer info (did, handle) from auth token
pub type ViewerFetcher =
  fn(String) -> Result(#(String, option.Option(String)), String)
```

**Step 2: Update build_schema_with_subscriptions signature to accept viewer_fetcher**

Modify the function signature (line 183) to add the viewer_fetcher parameter:

```gleam
pub fn build_schema_with_subscriptions(
  lexicons: List(types.Lexicon),
  fetcher: RecordFetcher,
  batch_fetcher: option.Option(dataloader.BatchFetcher),
  paginated_batch_fetcher: option.Option(dataloader.PaginatedBatchFetcher),
  create_factory: option.Option(mutation_builder.ResolverFactory),
  update_factory: option.Option(mutation_builder.ResolverFactory),
  delete_factory: option.Option(mutation_builder.ResolverFactory),
  upload_blob_factory: option.Option(mutation_builder.UploadBlobResolverFactory),
  aggregate_fetcher: option.Option(AggregateFetcher),
  viewer_fetcher: option.Option(ViewerFetcher),
) -> Result(schema.Schema, String) {
```

**Step 3: Pass viewer_fetcher to build_query_type**

Update the call to build_query_type (around line 206):

```gleam
let query_type =
  build_query_type(
    record_types,
    object_types,
    fetcher,
    aggregate_fetcher,
    field_type_registry,
    batch_fetcher,
    viewer_fetcher,
  )
```

**Step 4: Update build_query_type to accept and use viewer_fetcher**

Modify build_query_type signature (line 1460):

```gleam
fn build_query_type(
  record_types: List(RecordType),
  object_types: dict.Dict(String, schema.Type),
  fetcher: RecordFetcher,
  aggregate_fetcher: option.Option(AggregateFetcher),
  field_type_registry: dict.Dict(String, dict.Dict(String, FieldType)),
  batch_fetcher: option.Option(dataloader.BatchFetcher),
  viewer_fetcher: option.Option(ViewerFetcher),
) -> schema.Type {
```

**Step 5: Build viewer field and add to all_query_fields (before line 1564)**

Add after `let aggregate_query_fields = ...` block:

```gleam
// Build viewer query field if viewer_fetcher provided
let viewer_field = case viewer_fetcher {
  option.Some(vf) -> {
    // Build Viewer object type with did, handle, and profile join
    let viewer_base_fields = [
      schema.field("did", schema.non_null(schema.string()), "User DID", fn(ctx) {
        case ctx.parent {
          option.Some(value.Object(fields)) ->
            case list.key_find(fields, "did") {
              Ok(v) -> Ok(v)
              Error(_) -> Ok(value.Null)
            }
          _ -> Ok(value.Null)
        }
      }),
      schema.field("handle", schema.string(), "User handle", fn(ctx) {
        case ctx.parent {
          option.Some(value.Object(fields)) ->
            case list.key_find(fields, "handle") {
              Ok(v) -> Ok(v)
              Error(_) -> Ok(value.Null)
            }
          _ -> Ok(value.Null)
        }
      }),
    ]

    // Add appBskyActorProfileByDid if that type exists
    let profile_field = case dict.get(object_types, "app.bsky.actor.profile") {
      Ok(profile_type) -> {
        [schema.field(
          "appBskyActorProfileByDid",
          profile_type,
          "User's profile record",
          fn(ctx) {
            case ctx.parent {
              option.Some(value.Object(fields)) ->
                case list.key_find(fields, "did") {
                  Ok(value.String(did)) -> {
                    case batch_fetcher {
                      option.Some(bf) -> {
                        case dataloader.batch_fetch_by_did([did], "app.bsky.actor.profile", bf) {
                          Ok(results) ->
                            case dict.get(results, did) {
                              Ok([first, ..]) -> Ok(first)
                              _ -> Ok(value.Null)
                            }
                          Error(_) -> Ok(value.Null)
                        }
                      }
                      option.None -> Ok(value.Null)
                    }
                  }
                  _ -> Ok(value.Null)
                }
              _ -> Ok(value.Null)
            }
          },
        )]
      }
      Error(_) -> []
    }

    let viewer_fields = list.append(viewer_base_fields, profile_field)
    let viewer_type = schema.object_type("Viewer", "Authenticated user", viewer_fields)

    // Create the viewer query field
    [schema.field(
      "viewer",
      viewer_type,
      "The currently authenticated user, or null if not authenticated",
      fn(ctx) {
        // Extract auth_token from context
        case ctx.data {
          option.Some(value.Object(fields)) ->
            case list.key_find(fields, "auth_token") {
              Ok(value.String(token)) -> {
                case vf(token) {
                  Ok(#(did, handle_opt)) -> {
                    let handle_value = case handle_opt {
                      option.Some(h) -> value.String(h)
                      option.None -> value.Null
                    }
                    Ok(value.Object([
                      #("did", value.String(did)),
                      #("handle", handle_value),
                    ]))
                  }
                  Error(_) -> Ok(value.Null)
                }
              }
              _ -> Ok(value.Null)
            }
          _ -> Ok(value.Null)
        }
      },
    )]
  }
  option.None -> []
}

// Combine all query fields
let all_query_fields = list.flatten([query_fields, aggregate_query_fields, viewer_field])
```

**Step 6: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/schema/database.gleam
git commit -m "feat(graphql): add viewer query type to schema builder"
```

---

## Task 2: Update Public API to Accept ViewerFetcher

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql.gleam`

**Step 1: Re-export ViewerFetcher type**

Add to the exports (around line 10):

```gleam
pub type ViewerFetcher =
  db_schema_builder.ViewerFetcher
```

**Step 2: Update build_schema_with_subscriptions to pass viewer_fetcher**

Update the function (around line 46) to accept and forward viewer_fetcher:

```gleam
pub fn build_schema_with_subscriptions(
  lexicons: List(types.Lexicon),
  fetcher: db_schema_builder.RecordFetcher,
  batch_fetcher: option.Option(dataloader.BatchFetcher),
  paginated_batch_fetcher: option.Option(dataloader.PaginatedBatchFetcher),
  create_factory: option.Option(mutation_builder.ResolverFactory),
  update_factory: option.Option(mutation_builder.ResolverFactory),
  delete_factory: option.Option(mutation_builder.ResolverFactory),
  upload_blob_factory: option.Option(mutation_builder.UploadBlobResolverFactory),
  aggregate_fetcher: option.Option(db_schema_builder.AggregateFetcher),
  viewer_fetcher: option.Option(ViewerFetcher),
) -> Result(schema.Schema, String) {
  db_schema_builder.build_schema_with_subscriptions(
    lexicons,
    fetcher,
    batch_fetcher,
    paginated_batch_fetcher,
    create_factory,
    update_factory,
    delete_factory,
    upload_blob_factory,
    aggregate_fetcher,
    viewer_fetcher,
  )
}
```

**Step 3: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql.gleam
git commit -m "feat(graphql): expose ViewerFetcher in public API"
```

---

## Task 3: Implement ViewerFetcher in Server

**Files:**
- Modify: `server/src/graphql_gleam.gleam`

**Step 1: Add import for actors repository**

Add to imports (around line 10):

```gleam
import database/repositories/actors
```

**Step 2: Create viewer_fetcher function**

Add after existing helper functions:

```gleam
/// Creates a viewer fetcher that validates token and returns user info
fn create_viewer_fetcher(
  conn: sqlight.Connection,
) -> fn(String) -> Result(#(String, option.Option(String)), String) {
  fn(token: String) {
    // Verify token and get user DID
    case atproto_auth.verify_token(conn, token) {
      Error(_) -> Error("Invalid or expired token")
      Ok(user_info) -> {
        // Get handle from actors table
        let handle = case actors.get(conn, user_info.did) {
          Ok([actor, ..]) -> option.Some(actor.handle)
          _ -> option.None
        }
        Ok(#(user_info.did, handle))
      }
    }
  }
}
```

**Step 3: Pass viewer_fetcher when building schema**

Find where `build_schema_with_subscriptions` is called and add the viewer_fetcher parameter:

```gleam
let viewer_fetcher = option.Some(create_viewer_fetcher(conn))

lexicon_graphql.build_schema_with_subscriptions(
  lexicons,
  fetcher,
  batch_fetcher,
  paginated_batch_fetcher,
  create_factory,
  update_factory,
  delete_factory,
  upload_blob_factory,
  aggregate_fetcher,
  viewer_fetcher,
)
```

**Step 4: Commit**

```bash
git add server/src/graphql_gleam.gleam
git commit -m "feat(server): implement viewer fetcher with token validation"
```

---

## Task 4: Update All Schema Builder Call Sites

**Files:**
- Modify: Any test files or other files that call `build_schema_with_subscriptions`

**Step 1: Find all call sites**

Run: `grep -r "build_schema_with_subscriptions" --include="*.gleam"`

**Step 2: Update each call site to pass `option.None` for viewer_fetcher**

For test files, add `option.None` as the last parameter:

```gleam
db_schema_builder.build_schema_with_subscriptions(
  lexicons,
  fetcher,
  batch_fetcher,
  paginated_batch_fetcher,
  create_factory,
  update_factory,
  delete_factory,
  upload_blob_factory,
  aggregate_fetcher,
  option.None,  // viewer_fetcher - not needed in tests
)
```

**Step 3: Verify build passes**

Run: `gleam build`
Expected: Build succeeds with no errors

**Step 4: Commit**

```bash
git add -A
git commit -m "chore: update schema builder call sites for viewer_fetcher param"
```

---

## Task 5: Update HTML Example to Use Viewer Query

**Files:**
- Modify: `examples/01-statusphere-html/index.html`

**Step 1: Replace fetchUserProfile with fetchViewer**

Find the `fetchUserProfile` function and replace with:

```javascript
async function fetchViewer() {
  const query = `
    query {
      viewer {
        did
        handle
        appBskyActorProfileByDid {
          displayName
          avatar { url }
        }
      }
    }
  `;

  const data = await graphqlQuery(query, {}, true);
  return data?.viewer;
}
```

**Step 2: Update main() to use fetchViewer**

Replace the profile fetching logic in main():

```javascript
// Render auth section
if (isLoggedIn()) {
  try {
    const viewer = await fetchViewer();
    if (viewer) {
      const profile = {
        did: viewer.did,
        actorHandle: viewer.handle,
        displayName: viewer.appBskyActorProfileByDid?.displayName,
        avatar: viewer.appBskyActorProfileByDid?.avatar,
      };
      renderUserCard(profile);
    } else {
      renderUserCard(null);
    }
  } catch (error) {
    console.error('Failed to fetch viewer:', error);
    renderUserCard(null);
  }
} else {
  renderLoginForm();
}
```

**Step 3: Remove unused getUserDid and storage of userDid**

Remove references to `STORAGE_KEYS.userDid` and `getUserDid()` function if no longer needed.

**Step 4: Commit**

```bash
git add examples/01-statusphere-html/index.html
git commit -m "feat(examples): use viewer query for authenticated user info"
```

---

## Task 6: Verify End-to-End

**Step 1: Build the project**

Run: `gleam build`
Expected: No errors

**Step 2: Start the server**

Run: `gleam run` or `make run`

**Step 3: Test viewer query without auth**

```bash
curl -X POST http://localhost:8080/graphql \
  -H "Content-Type: application/json" \
  -d '{"query": "{ viewer { did handle } }"}'
```

Expected: `{"data": {"viewer": null}}`

**Step 4: Test viewer query with auth**

```bash
curl -X POST http://localhost:8080/graphql \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer <valid-token>" \
  -d '{"query": "{ viewer { did handle appBskyActorProfileByDid { displayName } } }"}'
```

Expected: `{"data": {"viewer": {"did": "did:plc:...", "handle": "user.bsky.social", "appBskyActorProfileByDid": {...}}}}`

**Step 5: Test HTML example**

1. Start HTTP server: `npx serve examples/01-statusphere-html -p 3000`
2. Open http://127.0.0.1:3000
3. Login with OAuth
4. Verify user card shows correct name and handle

**Step 6: Final commit if any fixes needed**

```bash
git add -A
git commit -m "fix: address viewer query issues found in testing"
```

---

## Summary

| Task | Description | Files |
|------|-------------|-------|
| 1 | Add Viewer type and query to schema builder | `lexicon_graphql/.../database.gleam` |
| 2 | Update public API to accept ViewerFetcher | `lexicon_graphql/src/lexicon_graphql.gleam` |
| 3 | Implement ViewerFetcher in server | `server/src/graphql_gleam.gleam` |
| 4 | Update all schema builder call sites | Various test files |
| 5 | Update HTML example to use viewer query | `examples/01-statusphere-html/index.html` |
| 6 | End-to-end verification | N/A |

**Total commits:** 5-6 focused commits following conventional commit format.
