# Wipe-and-Replace Lexicon Import Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Change lexicon import to be declarative - the uploaded ZIP defines the complete set of lexicons, wiping any existing ones.

**Architecture:** Modify `import_lexicons_from_directory()` to call `delete_all()` after validation passes but before inserting. Remove unused CLI import command.

**Tech Stack:** Gleam, SQLite, existing `lexicons.delete_all()` function

---

## Task 1: Add delete_all call to importer after validation

**Files:**
- Modify: `server/src/importer.gleam:83-91`

**Step 1: Add delete_all call after validation succeeds**

In `import_lexicons_from_directory()`, after the line `use _ <- result.try(validation_result)` (line 83) and before the comment `// Validation succeeded, import each lexicon` (line 85), add:

```gleam
  // Wipe existing lexicons before importing new set
  logging.log(logging.Info, "[import] Wiping existing lexicons...")
  case lexicons.delete_all(db) {
    Ok(_) -> Nil
    Error(err) -> {
      logging.log(
        logging.Error,
        "[import]   Failed to delete existing lexicons: "
          <> string.inspect(err),
      )
    }
  }
```

The full section (lines 79-91) should become:

```gleam
  logging.log(logging.Info, "")
  logging.log(logging.Info, "[import] Importing lexicons to database...")

  // If validation failed, return error immediately
  use _ <- result.try(validation_result)

  // Wipe existing lexicons before importing new set
  logging.log(logging.Info, "[import] Wiping existing lexicons...")
  case lexicons.delete_all(db) {
    Ok(_) -> Nil
    Error(err) -> {
      logging.log(
        logging.Error,
        "[import]   Failed to delete existing lexicons: "
          <> string.inspect(err),
      )
    }
  }

  // Validation succeeded, import each lexicon
  let results =
    file_contents
    |> list.map(fn(pair) {
      let #(file_path, json_content) = pair
      import_validated_lexicon(db, file_path, json_content)
    })
```

**Step 2: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds with no errors

**Step 3: Commit**

```bash
git add server/src/importer.gleam
git commit -m "feat: wipe existing lexicons before import

Import is now declarative - the uploaded set defines the complete
set of lexicons. Old lexicons not in the new set are removed."
```

---

## Task 2: Remove CLI import command from server.gleam

**Files:**
- Modify: `server/src/server.gleam:39,66-117`

**Step 1: Remove the importer import**

Delete line 39:
```gleam
import importer
```

**Step 2: Remove the import command case and function**

In the `main()` function (lines 66-72), change from:

```gleam
pub fn main() {
  // Check for CLI arguments
  case argv.load().arguments {
    ["import", directory] -> run_import_command(directory)
    ["backfill"] -> run_backfill_command()
    _ -> start_server_normally()
  }
}
```

To:

```gleam
pub fn main() {
  // Check for CLI arguments
  case argv.load().arguments {
    ["backfill"] -> run_backfill_command()
    _ -> start_server_normally()
  }
}
```

**Step 3: Delete the run_import_command function**

Delete lines 75-117 (the entire `run_import_command` function):

```gleam
fn run_import_command(directory: String) {
  logging.log(logging.Info, "Importing lexicons from: " <> directory)
  // ... entire function ...
}
```

**Step 4: Verify it compiles**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Build succeeds with no errors (may have warning about unused import if importer is used elsewhere)

**Step 5: Commit**

```bash
git add server/src/server.gleam
git commit -m "chore: remove unused CLI import command

The settings UI is the only supported way to import lexicons."
```

---

## Task 3: Verify end-to-end behavior

**Step 1: Run tests**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam test`
Expected: All tests pass

**Step 2: Manual verification (optional)**

If you have a running server:
1. Import a set of lexicons via settings UI
2. Verify they appear
3. Import a different/smaller set
4. Verify only the new set exists (old ones are gone)

**Step 3: Final commit if any cleanup needed**

If tests revealed issues, fix and commit.
