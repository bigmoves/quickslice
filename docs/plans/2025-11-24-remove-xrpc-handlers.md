# Remove XRPC Handlers Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove XRPC REST-like endpoints and associated code, keeping GraphQL mutations as the sole API for record CRUD operations.

**Architecture:** XRPC handlers provide REST endpoints at `/xrpc/[nsid].[method]` for CRUD operations. These are being replaced by GraphQL mutations in `mutation_resolvers.gleam` which already provide full functionality. The removal involves deleting handler/router files, updating server routing, removing error messages referencing XRPC, and cleaning up imports.

**Tech Stack:** Gleam, Wisp (web framework)

---

## Task 1: Remove XRPC route handling from server

**Files:**
- Modify: `server/src/server.gleam:578-632`

**Step 1: Read the server.gleam file to understand current routing**

Read the file to see the exact XRPC routing code:

```bash
# Already done in analysis phase
```

**Step 2: Remove XRPC imports from server.gleam**

Remove these import statements from the top of `server/src/server.gleam`:

```gleam
import xrpc_handlers
import xrpc_router
```

Expected location: Lines 34-35 in server/src/server.gleam

**Step 3: Remove XRPC route handling from handle_request function**

In `server/src/server.gleam`, remove the entire XRPC routing case from the `handle_request` function (lines 578-632).

Remove this entire block:

```gleam
    ["xrpc", _] -> {
      // Try to parse the XRPC route
      case xrpc_router.parse_xrpc_path(segments) {
        option.Some(route) -> {
          // Check if lexicon exists for this NSID
          case xrpc_router.validate_nsid(ctx.db, route.nsid) {
            True -> {
              // Route to the appropriate handler based on method
              case xrpc_router.parse_method(route.method) {
                xrpc_router.CreateRecord ->
                  xrpc_handlers.handle_create_record(
                    req,
                    ctx.db,
                    route.nsid,
                    ctx.auth_base_url,
                  )
                xrpc_router.UpdateRecord ->
                  xrpc_handlers.handle_update_record(req, ctx.db, route.nsid)
                xrpc_router.DeleteRecord ->
                  xrpc_handlers.handle_delete_record(req, ctx.db, route.nsid)
                xrpc_router.GetRecord ->
                  xrpc_handlers.handle_get_record(req, ctx.db, route.nsid)
                xrpc_router.UnknownMethod -> {
                  wisp.response(404)
                  |> wisp.set_header("content-type", "application/json")
                  |> wisp.set_body(wisp.Text(
                    "{\"error\": \"MethodNotSupported\", \"message\": \"Unknown XRPC method: "
                    <> route.method
                    <> "\"}",
                  ))
                }
              }
            }
            False -> {
              // No lexicon found for this NSID
              wisp.response(404)
              |> wisp.set_header("content-type", "application/json")
              |> wisp.set_body(wisp.Text(
                "{\"error\": \"LexiconNotFound\", \"message\": \"No lexicon found for collection: "
                <> route.nsid
                <> "\"}",
              ))
            }
          }
        }
        option.None -> {
          // Invalid XRPC path format
          wisp.response(400)
          |> wisp.set_header("content-type", "application/json")
          |> wisp.set_body(wisp.Text(
            "{\"error\": \"InvalidRequest\", \"message\": \"Invalid XRPC path format\"}",
          ))
        }
      }
    }
```

After removal, the routing should go directly from the `["upload"]` case to the fallback case `_ -> index_route(req, ctx)`.

**Step 4: Build the project to verify no compilation errors**

Run the build command:

```bash
cd server
gleam build
```

Expected: Build should succeed with no errors (the removed modules will show as unused, which is expected).

**Step 5: Commit the server routing changes**

```bash
git add server/src/server.gleam
git commit -m "refactor: remove XRPC route handling from server"
```

---

## Task 2: Delete XRPC source files

**Files:**
- Delete: `server/src/xrpc_handlers.gleam`
- Delete: `server/src/xrpc_router.gleam`

**Step 1: Delete xrpc_handlers.gleam**

```bash
rm server/src/xrpc_handlers.gleam
```

Expected: File removed from filesystem.

**Step 2: Delete xrpc_router.gleam**

```bash
rm server/src/xrpc_router.gleam
```

Expected: File removed from filesystem.

**Step 3: Build the project to verify clean removal**

```bash
cd server
gleam build
```

Expected: Build should succeed with no errors. The build system will clean up any stale build artifacts.

**Step 4: Commit the deletions**

```bash
git add server/src/xrpc_handlers.gleam server/src/xrpc_router.gleam
git commit -m "refactor: delete XRPC handler and router source files"
```

---

## Task 3: Delete XRPC test files

**Files:**
- Delete: `server/test/xrpc_handlers_test.gleam`
- Delete: `server/test/xrpc_router_test.gleam`

**Step 1: Delete xrpc_handlers_test.gleam**

```bash
rm server/test/xrpc_handlers_test.gleam
```

Expected: File removed from filesystem.

**Step 2: Delete xrpc_router_test.gleam**

```bash
rm server/test/xrpc_router_test.gleam
```

Expected: File removed from filesystem.

**Step 3: Run tests to verify test suite still works**

```bash
cd server
gleam test
```

Expected: All remaining tests should pass. The test suite should run without trying to compile the deleted test files.

**Step 4: Commit the test deletions**

```bash
git add server/test/xrpc_handlers_test.gleam server/test/xrpc_router_test.gleam
git commit -m "test: remove XRPC handler and router tests"
```

---

## Task 4: Remove XRPC references from mutation_builder.gleam

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/mutation_builder.gleam:153-157, 208-212, 252-256`

**Step 1: Read mutation_builder.gleam to locate XRPC error messages**

```bash
cd lexicon_graphql
```

The file contains three stub resolver error messages that mention "Use XRPC endpoint instead" at:
- Line 153-157 (create mutation)
- Line 208-212 (update mutation)
- Line 252-256 (delete mutation)

**Step 2: Update create mutation stub error message**

In `lexicon_graphql/src/lexicon_graphql/mutation_builder.gleam`, change line 154-156 from:

```gleam
      Error(
        "Create mutation for "
        <> collection
        <> " not yet implemented. Use XRPC endpoint instead.",
      )
```

To:

```gleam
      Error(
        "Create mutation for "
        <> collection
        <> " not yet implemented.",
      )
```

**Step 3: Update update mutation stub error message**

Change line 209-211 from:

```gleam
      Error(
        "Update mutation for "
        <> collection
        <> " not yet implemented. Use XRPC endpoint instead.",
      )
```

To:

```gleam
      Error(
        "Update mutation for "
        <> collection
        <> " not yet implemented.",
      )
```

**Step 4: Update delete mutation stub error message**

Change line 253-255 from:

```gleam
      Error(
        "Delete mutation for "
        <> collection
        <> " not yet implemented. Use XRPC endpoint instead.",
      )
```

To:

```gleam
      Error(
        "Delete mutation for "
        <> collection
        <> " not yet implemented.",
      )
```

**Step 5: Build lexicon_graphql to verify changes**

```bash
cd lexicon_graphql
gleam build
```

Expected: Build should succeed with no errors.

**Step 6: Commit the mutation_builder changes**

```bash
git add lexicon_graphql/src/lexicon_graphql/mutation_builder.gleam
git commit -m "refactor: remove XRPC references from mutation error messages"
```

---

## Task 5: Clean build artifacts and verify final state

**Files:**
- None (cleanup task)

**Step 1: Clean server build directory**

```bash
cd server
rm -rf build/
```

Expected: Build directory removed.

**Step 2: Rebuild server from clean state**

```bash
gleam build
```

Expected: Clean build completes successfully with no references to xrpc modules.

**Step 3: Run server tests from clean state**

```bash
gleam test
```

Expected: All tests pass.

**Step 4: Clean lexicon_graphql build directory**

```bash
cd ../lexicon_graphql
rm -rf build/
```

Expected: Build directory removed.

**Step 5: Rebuild lexicon_graphql from clean state**

```bash
gleam build
```

Expected: Clean build completes successfully.

**Step 6: Verify no remaining XRPC references in codebase**

```bash
cd ..
grep -r "xrpc" --include="*.gleam" server/src server/test lexicon_graphql/src
```

Expected: No matches found (grep returns no results or exit code 1).

Note: This will find references to `/xrpc/com.atproto.repo.*` URLs in `mutation_resolvers.gleam` which are legitimate AT Protocol API calls, NOT our removed XRPC handlers.

**Step 7: Final commit for cleanup verification**

```bash
git add -A
git status
```

Expected: Working tree should be clean (no uncommitted changes). If there are untracked build artifacts, add them to .gitignore:

```bash
# Only if needed:
echo "server/build/" >> .gitignore
echo "lexicon_graphql/build/" >> .gitignore
git add .gitignore
git commit -m "chore: update gitignore for build artifacts"
```

---

## Task 6: Update documentation

**Files:**
- Check: `docs/mutations.md` (verify it doesn't reference XRPC)
- Check: `README.md` (verify it doesn't reference XRPC endpoints)

**Step 1: Check if mutations.md references XRPC**

```bash
grep -i "xrpc" docs/mutations.md
```

Expected: No matches found. The mutations documentation should only reference GraphQL API.

**Step 2: Check if README.md references XRPC**

```bash
grep -i "xrpc" README.md
```

Expected: No matches found. The README should only reference GraphQL API.

**Step 3: Search for any remaining XRPC references in docs**

```bash
grep -r "xrpc" --include="*.md" docs/
```

Expected: Should find reference in `docs/plans/2025-11-23-database-refactoring.md` (historical plan document, no action needed) and this current plan document.

**Step 4: Verify documentation is accurate**

No changes needed if grep results show only historical references. The current documentation already focuses on GraphQL as the primary API.

**Step 5: Final verification commit (if any doc changes were made)**

```bash
git status
```

Expected: Working tree clean (no documentation changes needed).

---

## Verification Checklist

After completing all tasks, verify:

- [ ] `server/src/server.gleam` has no XRPC imports or routing
- [ ] `server/src/xrpc_handlers.gleam` deleted
- [ ] `server/src/xrpc_router.gleam` deleted
- [ ] `server/test/xrpc_handlers_test.gleam` deleted
- [ ] `server/test/xrpc_router_test.gleam` deleted
- [ ] `lexicon_graphql/src/lexicon_graphql/mutation_builder.gleam` has no XRPC references
- [ ] `gleam build` succeeds in both `server/` and `lexicon_graphql/`
- [ ] `gleam test` passes in `server/`
- [ ] No XRPC references remain in source code (except legitimate AT Protocol API URLs)
- [ ] All changes committed to git

## Notes

- GraphQL mutations in `server/src/mutation_resolvers.gleam` remain unchanged and continue to work
- The mutations call AT Protocol endpoints like `/xrpc/com.atproto.repo.createRecord` - these are legitimate PDS API calls, not the removed XRPC handlers
- Build artifacts will be automatically cleaned up by the Gleam build system
- No database migrations needed - this is purely removing API surface area
