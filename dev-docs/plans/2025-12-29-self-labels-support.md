# Self-Labels Support Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Merge self-labels from record JSON with moderator labels, avoiding field name collisions.

**Architecture:** The `labels` field resolver checks if the record's JSON contains self-labels (`com.atproto.label.defs#selfLabels`), parses them, fetches moderator labels from the database, and returns the merged list. If a lexicon defines a `labels` property, we replace its resolver with our enhanced version instead of adding a duplicate field.

**Tech Stack:** Gleam, lexicon_graphql library, GraphQL schema builder

---

## Background

AT Protocol supports two label sources:
1. **Self-labels** - embedded in record JSON by the author (e.g., marking own post as adult content)
2. **Moderator labels** - applied externally by labelers, stored in our `label` table

Currently, our `labels` field only returns moderator labels. If a lexicon defines `labels` with type `com.atproto.label.defs#selfLabels`, we'd have a field collision.

## Tasks

### Task 1: Add Helper to Check for Self-Labels Property

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/schema/database.gleam`

**Step 1: Write the helper function**

Add after line 3436 (after `build_labels_field`):

```gleam
/// Check if a record type has a labels property with selfLabels ref
fn has_self_labels_property(properties: List(#(String, types.Property))) -> Bool {
  list.any(properties, fn(prop) {
    let #(name, types.Property(_, _, _, ref, _, _)) = prop
    name == "labels" && ref == option.Some("com.atproto.label.defs#selfLabels")
  })
}
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/schema/database.gleam
git commit -m "feat(lexicon_graphql): add helper to detect selfLabels property"
```

---

### Task 2: Modify build_labels_field to Accept Properties

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/schema/database.gleam`

**Step 1: Update function signature**

Change `build_labels_field` (around line 3402) from:

```gleam
fn build_labels_field(
  labels_fetcher: option.Option(LabelsFetcher),
) -> List(schema.Field) {
```

To:

```gleam
fn build_labels_field(
  labels_fetcher: option.Option(LabelsFetcher),
  properties: List(#(String, types.Property)),
) -> List(schema.Field) {
```

**Step 2: Update the call site**

Find where `build_labels_field` is called (around line 650) and change:

```gleam
let labels_field = build_labels_field(labels_fetcher)
```

To:

```gleam
let labels_field = build_labels_field(labels_fetcher, record_type.properties)
```

**Step 3: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam build`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/schema/database.gleam
git commit -m "refactor(lexicon_graphql): pass properties to build_labels_field"
```

---

### Task 3: Add Self-Labels Parsing to Fetcher

**Files:**
- Modify: `server/src/graphql/lexicon/fetchers.gleam`

**Step 1: Add helper to parse self-labels from JSON**

Add after the `filter_takedowns` function (around line 39):

```gleam
/// Parse self-labels from a record's JSON if present
/// Self-labels have format: {"$type": "com.atproto.label.defs#selfLabels", "values": [{"val": "..."}]}
fn parse_self_labels_from_json(
  json_str: String,
  uri: String,
) -> List(value.Value) {
  case json.parse(json_str, dynamic.dynamic) {
    Error(_) -> []
    Ok(dyn) -> {
      // Try to get labels field
      case dynamic.field("labels", dynamic.dynamic)(dyn) {
        Error(_) -> []
        Ok(labels_dyn) -> {
          // Check $type is selfLabels
          case dynamic.field("$type", dynamic.string)(labels_dyn) {
            Ok("com.atproto.label.defs#selfLabels") -> {
              // Parse values array
              case
                dynamic.field(
                  "values",
                  dynamic.list(dynamic.decode1(
                    fn(val) { val },
                    dynamic.field("val", dynamic.string),
                  )),
                )(labels_dyn)
              {
                Ok(vals) -> {
                  list.map(vals, fn(val) {
                    value.Object([
                      #("val", value.String(val)),
                      #("src", value.String(uri |> string.split("/") |> list.first |> result.unwrap(""))),
                      #("uri", value.String(uri)),
                      #("neg", value.Boolean(False)),
                      #("cts", value.Null),
                      #("exp", value.Null),
                      #("cid", value.Null),
                      #("id", value.Null),
                    ])
                  })
                }
                Error(_) -> []
              }
            }
            _ -> []
          }
        }
      }
    }
  }
}
```

**Step 2: Add required imports**

Add to imports at top of file:

```gleam
import gleam/json
import gleam/dynamic
```

**Step 3: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add server/src/graphql/lexicon/fetchers.gleam
git commit -m "feat(fetchers): add self-labels JSON parser"
```

---

### Task 4: Modify Labels Fetcher to Accept Record JSON

**Files:**
- Modify: `server/src/graphql/lexicon/fetchers.gleam`
- Modify: `lexicon_graphql/src/lexicon_graphql/schema/database.gleam`

**Step 1: Change fetcher type to include record JSON**

In `lexicon_graphql/src/lexicon_graphql/schema/database.gleam`, find `LabelsFetcher` type (around line 144):

```gleam
pub type LabelsFetcher =
  fn(List(String)) -> Result(dict.Dict(String, List(value.Value)), String)
```

Change to:

```gleam
pub type LabelsFetcher =
  fn(List(#(String, option.Option(String)))) ->
    Result(dict.Dict(String, List(value.Value)), String)
```

The tuple is `#(uri, optional_record_json)`.

**Step 2: Update fetcher in server**

In `server/src/graphql/lexicon/fetchers.gleam`, change `labels_fetcher` (around line 469):

```gleam
pub fn labels_fetcher(db: Executor) {
  fn(
    uris_with_json: List(#(String, option.Option(String))),
  ) -> Result(dict.Dict(String, List(value.Value)), String) {
    let uris = list.map(uris_with_json, fn(pair) { pair.0 })

    case labels.get_by_uris(db, uris) {
      Ok(label_list) -> {
        // Group moderator labels by URI
        let mod_labels =
          list.fold(label_list, dict.new(), fn(acc, label) {
            let label_value =
              value.Object([
                #("id", value.Int(label.id)),
                #("src", value.String(label.src)),
                #("uri", value.String(label.uri)),
                #("cid", case label.cid {
                  option.Some(c) -> value.String(c)
                  option.None -> value.Null
                }),
                #("val", value.String(label.val)),
                #("neg", value.Boolean(label.neg)),
                #("cts", value.String(label.cts)),
                #("exp", case label.exp {
                  option.Some(e) -> value.String(e)
                  option.None -> value.Null
                }),
              ])
            let existing = dict.get(acc, label.uri) |> result.unwrap([])
            dict.insert(acc, label.uri, [label_value, ..existing])
          })

        // Merge with self-labels from record JSON
        let merged =
          list.fold(uris_with_json, mod_labels, fn(acc, pair) {
            let #(uri, json_opt) = pair
            case json_opt {
              option.None -> acc
              option.Some(json_str) -> {
                let self_labels = parse_self_labels_from_json(json_str, uri)
                case self_labels {
                  [] -> acc
                  _ -> {
                    let existing = dict.get(acc, uri) |> result.unwrap([])
                    dict.insert(acc, uri, list.append(self_labels, existing))
                  }
                }
              }
            }
          })

        Ok(merged)
      }
      Error(_) -> Error("Failed to fetch labels")
    }
  }
}
```

**Step 3: Update call site in build_labels_field**

In `lexicon_graphql/src/lexicon_graphql/schema/database.gleam`, update the resolver (around line 3414):

```gleam
fn(ctx) {
  // Get the URI and JSON from the parent record
  case get_field_from_context(ctx, "uri") {
    Ok(uri_str) -> {
      let json_opt = case get_field_from_context(ctx, "json") {
        Ok(j) -> option.Some(j)
        Error(_) -> option.None
      }
      case fetcher([#(uri_str, json_opt)]) {
        Ok(results) -> {
          case dict.get(results, uri_str) {
            Ok(labels) -> Ok(value.List(labels))
            Error(_) -> Ok(value.List([]))
          }
        }
        Error(_) -> Ok(value.List([]))
      }
    }
    Error(_) -> Ok(value.List([]))
  }
},
```

**Step 4: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice && gleam build`
Expected: Compiles without errors

**Step 5: Commit**

```bash
git add -A
git commit -m "feat: merge self-labels from record JSON with moderator labels"
```

---

### Task 5: Handle Field Collision

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/schema/database.gleam`

**Step 1: Skip adding labels field if lexicon defines it**

Update `build_labels_field` to check for collision:

```gleam
fn build_labels_field(
  labels_fetcher: option.Option(LabelsFetcher),
  properties: List(#(String, types.Property)),
) -> List(schema.Field) {
  case labels_fetcher {
    option.None -> []
    option.Some(fetcher) -> {
      // Skip if lexicon already defines a labels field
      case has_self_labels_property(properties) {
        True -> []
        False -> {
          let label_type = build_label_type()
          [
            schema.field(
              "labels",
              schema.non_null(schema.list_type(schema.non_null(label_type))),
              "Labels applied to this record",
              fn(ctx) {
                case get_field_from_context(ctx, "uri") {
                  Ok(uri_str) -> {
                    let json_opt = case get_field_from_context(ctx, "json") {
                      Ok(j) -> option.Some(j)
                      Error(_) -> option.None
                    }
                    case fetcher([#(uri_str, json_opt)]) {
                      Ok(results) -> {
                        case dict.get(results, uri_str) {
                          Ok(labels) -> Ok(value.List(labels))
                          Error(_) -> Ok(value.List([]))
                        }
                      }
                      Error(_) -> Ok(value.List([]))
                    }
                  }
                  Error(_) -> Ok(value.List([]))
                }
              },
            ),
          ]
        }
      }
    }
  }
}
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/schema/database.gleam
git commit -m "fix(lexicon_graphql): skip labels field if lexicon defines selfLabels"
```

---

### Task 6: Override Lexicon's Labels Field Resolver

When the lexicon defines a `labels` property, we need to replace its resolver with our enhanced version that merges self-labels with moderator labels.

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql/schema/database.gleam`

**Step 1: Add function to replace labels field resolver**

Add after `has_self_labels_property`:

```gleam
/// Replace the labels field resolver with an enhanced version that merges moderator labels
fn replace_labels_field_resolver(
  fields: List(schema.Field),
  labels_fetcher: option.Option(LabelsFetcher),
) -> List(schema.Field) {
  case labels_fetcher {
    option.None -> fields
    option.Some(fetcher) -> {
      list.map(fields, fn(field) {
        case schema.field_name(field) == "labels" {
          False -> field
          True -> {
            // Replace with enhanced resolver
            schema.field(
              "labels",
              schema.field_type(field),
              schema.field_description(field),
              fn(ctx) {
                case get_field_from_context(ctx, "uri") {
                  Ok(uri_str) -> {
                    let json_opt = case get_field_from_context(ctx, "json") {
                      Ok(j) -> option.Some(j)
                      Error(_) -> option.None
                    }
                    case fetcher([#(uri_str, json_opt)]) {
                      Ok(results) -> {
                        case dict.get(results, uri_str) {
                          Ok(labels) -> Ok(value.List(labels))
                          Error(_) -> Ok(value.List([]))
                        }
                      }
                      Error(_) -> Ok(value.List([]))
                    }
                  }
                  Error(_) -> Ok(value.List([]))
                }
              },
            )
          }
        }
      })
    }
  }
}
```

**Step 2: Add accessor functions to schema module if missing**

Check if `schema.field_name`, `schema.field_type`, `schema.field_description` exist. If not, we may need to add them or use pattern matching on the Field type.

**Step 3: Apply the replacement in field building**

Update around line 652 where fields are combined:

```gleam
// Replace lexicon's labels field resolver if it exists
let enhanced_fields = case has_self_labels_property(record_type.properties) {
  True -> replace_labels_field_resolver(record_type.fields, labels_fetcher)
  False -> record_type.fields
}

// Build labels field if labels_fetcher is provided
let labels_field = build_labels_field(labels_fetcher, record_type.properties)

// Combine all fields
let all_fields =
  list.flatten([
    enhanced_fields,  // Use enhanced instead of record_type.fields
    forward_join_fields,
    reverse_join_fields,
    did_join_fields,
    viewer_fields,
    did_viewer_fields,
    labels_field,
  ])
```

**Step 4: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice && gleam build`
Expected: Compiles without errors

**Step 5: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql/schema/database.gleam
git commit -m "feat(lexicon_graphql): enhance lexicon labels field with moderator labels"
```

---

### Task 7: Write Integration Test

**Files:**
- Modify: `server/test/labels_test.gleam`

**Step 1: Add test for self-labels parsing**

```gleam
pub fn self_labels_parsing_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  let uri = "at://did:plc:user1/app.bsky.feed.post/abc123"
  let json_with_self_labels =
    "{\"text\": \"hello\", \"labels\": {\"$type\": \"com.atproto.label.defs#selfLabels\", \"values\": [{\"val\": \"porn\"}]}}"

  // Create labels fetcher
  let fetcher = fetchers.labels_fetcher(db)

  // Call fetcher with JSON
  let assert Ok(results) = fetcher([#(uri, option.Some(json_with_self_labels))])

  // Should have self-label
  let assert Ok(labels) = dict.get(results, uri)
  labels |> list.length() |> should.equal(1)
}
```

**Step 2: Add test for merged labels**

```gleam
pub fn merged_labels_test() {
  let assert Ok(db) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_moderation_tables(db)

  let uri = "at://did:plc:user1/app.bsky.feed.post/abc123"
  let json_with_self_labels =
    "{\"text\": \"hello\", \"labels\": {\"$type\": \"com.atproto.label.defs#selfLabels\", \"values\": [{\"val\": \"porn\"}]}}"

  // Add moderator label
  let assert Ok(_) =
    labels.insert(db, "did:plc:admin", uri, option.None, "spam", option.None)

  // Create labels fetcher
  let fetcher = fetchers.labels_fetcher(db)

  // Call fetcher with JSON
  let assert Ok(results) = fetcher([#(uri, option.Some(json_with_self_labels))])

  // Should have both labels (1 self + 1 moderator)
  let assert Ok(labels_list) = dict.get(results, uri)
  labels_list |> list.length() |> should.equal(2)
}
```

**Step 3: Run tests**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: All tests pass

**Step 4: Commit**

```bash
git add server/test/labels_test.gleam
git commit -m "test: add self-labels parsing and merging tests"
```

---

### Task 8: Update Documentation

**Files:**
- Modify: `docs/guides/moderation.md`

**Step 1: Add section about self-labels**

Add after the "Querying Labels" section:

```markdown
### Self-Labels

Authors can label their own content by including a `labels` field in their record with type `com.atproto.label.defs#selfLabels`. Quickslice automatically merges self-labels with moderator labels.

Example record with self-labels:

```json
{
  "text": "Adult content warning",
  "labels": {
    "$type": "com.atproto.label.defs#selfLabels",
    "values": [{"val": "porn"}]
  }
}
```

When querying, both self-labels and moderator labels appear in the `labels` field:

```graphql
query {
  xyzPosts(first: 10) {
    nodes {
      uri
      labels {
        val
        src
      }
    }
  }
}
```

Self-labels have the record author as the `src`. Moderator labels have the moderator's DID.
```

**Step 2: Commit**

```bash
git add docs/guides/moderation.md
git commit -m "docs: add self-labels documentation"
```

---

### Task 9: Final Verification

**Step 1: Run full test suite**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: All tests pass

**Step 2: Run lexicon_graphql tests**

Run: `cd /Users/chadmiller/code/quickslice/lexicon_graphql && gleam test`
Expected: All tests pass

**Step 3: Manual testing**

1. Start the server
2. Query a record type that has self-labels in the lexicon
3. Verify the `labels` field returns merged labels

---

## Summary

After completing these tasks:
1. Self-labels from record JSON are parsed and merged with moderator labels
2. No field collision when lexicons define their own `labels` property
3. Users get a unified view of all labels via the `labels` field
