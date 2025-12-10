# Fix Lexicon Ref Validation in Mutations Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix GraphQL mutation validation to include all lexicons so refs can be resolved by honk.

**Architecture:** The mutation resolvers currently pass only the target collection's lexicon to honk for validation. When a lexicon has `ref` fields pointing to other lexicons (e.g., `org.atmosphereconf.profile` references `community.lexicon.location.hthree`), honk fails with "Referenced lexicon not found". The fix is to fetch all lexicons and pass them to `honk.validate_record()`.

**Tech Stack:** Gleam, honk (lexicon validation library), SQLite

---

### Task 1: Write failing test for ref validation

**Files:**
- Modify: `server/test/mutation_resolver_integration_test.gleam`

**Step 1: Add test helper for lexicon with ref**

Add after the existing `create_status_lexicon()` function (around line 72):

```gleam
// Helper to create a location lexicon (referenced by profile)
fn create_location_lexicon() -> String {
  json.object([
    #("lexicon", json.int(1)),
    #("id", json.string("community.lexicon.location.hthree")),
    #(
      "defs",
      json.object([
        #(
          "main",
          json.object([
            #("type", json.string("object")),
            #("description", json.string("A physical location in H3 format")),
            #(
              "properties",
              json.object([
                #(
                  "name",
                  json.object([
                    #("type", json.string("string")),
                  ]),
                ),
                #(
                  "value",
                  json.object([
                    #("type", json.string("string")),
                  ]),
                ),
              ]),
            ),
            #("required", json.array([json.string("value")], of: fn(x) { x })),
          ]),
        ),
      ]),
    ),
  ])
  |> json.to_string
}

// Helper to create a profile lexicon that references location
fn create_profile_with_ref_lexicon() -> String {
  json.object([
    #("lexicon", json.int(1)),
    #("id", json.string("org.atmosphereconf.profile")),
    #(
      "defs",
      json.object([
        #(
          "main",
          json.object([
            #("type", json.string("record")),
            #("key", json.string("literal:self")),
            #(
              "record",
              json.object([
                #("type", json.string("object")),
                #(
                  "properties",
                  json.object([
                    #(
                      "displayName",
                      json.object([
                        #("type", json.string("string")),
                      ]),
                    ),
                    #(
                      "homeTown",
                      json.object([
                        #("type", json.string("ref")),
                        #("ref", json.string("community.lexicon.location.hthree")),
                      ]),
                    ),
                    #(
                      "createdAt",
                      json.object([
                        #("type", json.string("string")),
                        #("format", json.string("datetime")),
                      ]),
                    ),
                  ]),
                ),
              ]),
            ),
          ]),
        ),
      ]),
    ),
  ])
  |> json.to_string
}
```

**Step 2: Add the failing test**

Add at the end of the test file (before the final closing):

```gleam
// Test: Mutation with lexicon ref should validate correctly
pub fn mutation_with_lexicon_ref_validates_test() {
  // Setup: Create in-memory database with both lexicons
  let assert Ok(db) = sqlight.open(":memory:")
  let assert Ok(_) = tables.create_lexicon_table(db)

  // Insert BOTH lexicons - the profile AND the referenced location
  let location_lexicon = create_location_lexicon()
  let assert Ok(_) =
    lexicons.insert(db, "community.lexicon.location.hthree", location_lexicon)

  let profile_lexicon = create_profile_with_ref_lexicon()
  let assert Ok(_) =
    lexicons.insert(db, "org.atmosphereconf.profile", profile_lexicon)

  let assert Ok(lexicon_records) = lexicons.get_all(db)
  let parsed_lexicons =
    lexicon_records
    |> list.filter_map(fn(lex) { lexicon_graphql.parse_lexicon(lex.json) })

  let empty_fetcher = fn(_collection, _params) {
    Ok(#([], option.None, False, False, option.None))
  }

  // Mock resolver that simulates successful validation and record creation
  let mock_create_resolver_factory = fn(_collection: String) {
    fn(_ctx: schema.Context) {
      Ok(
        value.Object([
          #("uri", value.String("at://did:plc:test/org.atmosphereconf.profile/self")),
          #("cid", value.String("bafyreimock")),
          #("did", value.String("did:plc:test")),
          #("collection", value.String("org.atmosphereconf.profile")),
          #("indexedAt", value.String("2024-01-01T00:00:00Z")),
        ]),
      )
    }
  }

  let create_factory = option.Some(mock_create_resolver_factory)

  let assert Ok(built_schema) =
    database.build_schema_with_fetcher(
      parsed_lexicons,
      empty_fetcher,
      option.None,
      option.None,
      create_factory,
      option.None,
      option.None,
      option.None,
    )

  // Execute mutation with homeTown field that uses ref type
  let mutation =
    "mutation { createOrgAtmosphereconfProfile(rkey: \"self\", input: { displayName: \"Test\", homeTown: { name: \"Portland\", value: \"8528f003fffffff\" }, createdAt: \"2024-01-01T00:00:00Z\" }) { uri } }"

  let ctx_data = value.Object([#("auth_token", value.String("mock_token_123"))])
  let ctx = schema.context(option.Some(ctx_data))
  let assert Ok(response) = executor.execute(mutation, built_schema, ctx)

  // Should have no errors - ref should be resolved correctly
  response.errors
  |> list.length
  |> should.equal(0)
}
```

**Step 3: Run test to verify it fails**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test -- --only mutation_with_lexicon_ref_validates_test`

Expected: Test should pass at GraphQL level (mock resolver), but this test validates the schema builds correctly with refs.

**Step 4: Commit**

```bash
git add server/test/mutation_resolver_integration_test.gleam
git commit -m "test: add test for lexicon ref validation in mutations"
```

---

### Task 2: Fix create mutation to use all lexicons

**Files:**
- Modify: `server/src/mutation_resolvers.gleam:158-179`

**Step 1: Update the create resolver validation logic**

Replace lines 158-179 (the validation section) with:

```gleam
    // Step 6: Validate against lexicon (fetch all lexicons to resolve refs)
    use all_lexicon_records <- result.try(
      lexicons.get_all(ctx.db)
      |> result.map_error(fn(_) { "Failed to fetch lexicons" }),
    )

    // Parse all lexicon JSON strings
    use all_lex_jsons <- result.try(
      all_lexicon_records
      |> list.try_map(fn(lex) {
        honk.parse_json_string(lex.json)
        |> result.map_error(fn(e) {
          "Failed to parse lexicon JSON: " <> errors.to_string(e)
        })
      }),
    )

    // Check that the target collection lexicon exists
    case list.find(all_lexicon_records, fn(lex) { lex.id == collection }) {
      Ok(_) -> Nil
      Error(_) -> {
        // Will be caught by honk.validate_record, but early exit is cleaner
      }
    }

    use _ <- result.try(
      honk.validate_record(all_lex_jsons, collection, record_json_value)
      |> result.map_error(fn(err) {
        "Validation failed: " <> errors.to_string(err)
      }),
    )

    {
```

Note: The opening `{` starts a new block since we removed the `case lexicon_records {` and `[lex, ..] -> {` wrapper.

**Step 2: Find and remove the closing braces**

Around line 262 (after the original code), find and remove:

```gleam
      [] -> Error("Lexicon not found for collection: " <> collection)
    }
```

**Step 3: Run tests**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`

Expected: All tests pass

**Step 4: Commit**

```bash
git add server/src/mutation_resolvers.gleam
git commit -m "fix: use all lexicons for create mutation validation

Fixes 'Referenced lexicon not found' error when validating records
with ref fields pointing to other lexicons."
```

---

### Task 3: Fix update mutation to use all lexicons

**Files:**
- Modify: `server/src/mutation_resolvers.gleam:368-389` (line numbers will shift after Task 2)

**Step 1: Update the update resolver validation logic**

Apply the same pattern as Task 2 to the update mutation. Replace the validation section:

```gleam
    // Step 6: Validate against lexicon (fetch all lexicons to resolve refs)
    use all_lexicon_records <- result.try(
      lexicons.get_all(ctx.db)
      |> result.map_error(fn(_) { "Failed to fetch lexicons" }),
    )

    // Parse all lexicon JSON strings
    use all_lex_jsons <- result.try(
      all_lexicon_records
      |> list.try_map(fn(lex) {
        honk.parse_json_string(lex.json)
        |> result.map_error(fn(e) {
          "Failed to parse lexicon JSON: " <> errors.to_string(e)
        })
      }),
    )

    use _ <- result.try(
      honk.validate_record(all_lex_jsons, collection, record_json_value)
      |> result.map_error(fn(err) {
        "Validation failed: " <> errors.to_string(err)
      }),
    )

    {
```

**Step 2: Remove the closing braces for the old case statement**

Find and remove around line 453 (adjusted):

```gleam
      [] -> Error("Lexicon not found for collection: " <> collection)
    }
```

**Step 3: Run tests**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`

Expected: All tests pass

**Step 4: Commit**

```bash
git add server/src/mutation_resolvers.gleam
git commit -m "fix: use all lexicons for update mutation validation"
```

---

### Task 4: Verify fix with manual test

**Step 1: Build and run the server**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`

Expected: Build succeeds with no errors

**Step 2: Test the original failing mutation**

If you have a running instance, test the original payload:

```json
{
  "query": "mutation ProfileSettingsPageUpdateProfileMutation($input: OrgAtmosphereconfProfileInput!) { updateOrgAtmosphereconfProfile(rkey: \"self\", input: $input) { uri } }",
  "variables": {
    "input": {
      "displayName": "Chad",
      "description": "software engineer",
      "createdAt": "2025-12-10T05:33:33.170Z",
      "homeTown": {
        "name": "Portland, Oregon, United States",
        "value": "8528f003fffffff"
      }
    }
  }
}
```

Expected: No "Referenced lexicon not found" error

**Step 3: Final commit**

```bash
git add -A
git commit -m "chore: verify lexicon ref validation fix"
```

---

## Summary

| Task | Description | Files |
|------|-------------|-------|
| 1 | Add test for lexicon ref validation | `server/test/mutation_resolver_integration_test.gleam` |
| 2 | Fix create mutation validation | `server/src/mutation_resolvers.gleam` |
| 3 | Fix update mutation validation | `server/src/mutation_resolvers.gleam` |
| 4 | Verify fix works | Manual testing |

**Root Cause:** `honk.validate_record([lex_json], ...)` only received the target collection's lexicon, but lexicons with `ref` fields need all referenced lexicons to be present for resolution.

**Fix:** Change to `honk.validate_record(all_lex_jsons, ...)` where `all_lex_jsons` contains all lexicons from the database.
