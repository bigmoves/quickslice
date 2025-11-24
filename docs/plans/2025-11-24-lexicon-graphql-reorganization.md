# Lexicon GraphQL Package Reorganization Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Reorganize the lexicon_graphql package structure from flat modules to a GraphQL-abstraction-based hierarchy (Schema/Query/Mutation/Input/Output/Scalar with internal/ for implementation details).

**Architecture:** Move 18 modules into organized directories based on GraphQL operation types and concerns. Lexicon-specific logic moves to internal/lexicon/, GraphQL utilities to internal/graphql/. Main module (src/lexicon_graphql.gleam) becomes public API gateway with re-exports. All tests updated to reference new paths.

**Tech Stack:** Gleam, file system operations, import path updates

---

## Task 1: Create Directory Structure

**Files:**
- Create: `lexicon_graphql/src/lexicon_graphql/schema/` (directory)
- Create: `lexicon_graphql/src/lexicon_graphql/query/` (directory)
- Create: `lexicon_graphql/src/lexicon_graphql/mutation/` (directory)
- Create: `lexicon_graphql/src/lexicon_graphql/input/` (directory)
- Create: `lexicon_graphql/src/lexicon_graphql/output/` (directory)
- Create: `lexicon_graphql/src/lexicon_graphql/scalar/` (directory)
- Create: `lexicon_graphql/src/lexicon_graphql/internal/` (directory)
- Create: `lexicon_graphql/src/lexicon_graphql/internal/lexicon/` (directory)
- Create: `lexicon_graphql/src/lexicon_graphql/internal/graphql/` (directory)

**Step 1: Create all directories**

Run:
```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql/src/lexicon_graphql
mkdir -p schema query mutation input output scalar internal/lexicon internal/graphql
```

Expected: Directories created successfully, no output

**Step 2: Verify directory structure**

Run:
```bash
ls -la /Users/chadmiller/code/quickslice/lexicon_graphql/src/lexicon_graphql/
```

Expected: See new directories: schema, query, mutation, input, output, scalar, internal

**Step 3: Commit**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql
git add -A
git commit -m "feat: create directory structure for GraphQL-based organization"
```

---

## Task 2: Move Schema Modules

**Files:**
- Move: `lexicon_graphql/src/lexicon_graphql/schema_builder.gleam` → `lexicon_graphql/src/lexicon_graphql/schema/builder.gleam`
- Move: `lexicon_graphql/src/lexicon_graphql/db_schema_builder.gleam` → `lexicon_graphql/src/lexicon_graphql/schema/database.gleam`

**Step 1: Move schema_builder.gleam**

Run:
```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql/src/lexicon_graphql
git mv schema_builder.gleam schema/builder.gleam
```

Expected: File moved successfully

**Step 2: Move db_schema_builder.gleam**

Run:
```bash
git mv db_schema_builder.gleam schema/database.gleam
```

Expected: File moved successfully

**Step 3: Update imports in schema/builder.gleam**

File: `lexicon_graphql/src/lexicon_graphql/schema/builder.gleam`

Find and replace all internal imports:
- `import lexicon_graphql/types` → `import lexicon_graphql/types`
- Any other module imports will be updated in later tasks after those modules move

**Step 4: Update imports in schema/database.gleam**

File: `lexicon_graphql/src/lexicon_graphql/schema/database.gleam`

Find and replace:
- `import lexicon_graphql/types` → `import lexicon_graphql/types`
- Note: Other imports will be fixed in subsequent tasks

**Step 5: Run tests to check for breakage**

Run:
```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql
gleam test
```

Expected: Tests will likely fail with import errors (expected at this stage)

**Step 6: Commit**

```bash
git add -A
git commit -m "refactor: move schema modules to schema/ directory"
```

---

## Task 3: Move Query Modules

**Files:**
- Move: `lexicon_graphql/src/lexicon_graphql/dataloader.gleam` → `lexicon_graphql/src/lexicon_graphql/query/dataloader.gleam`

**Step 1: Move dataloader.gleam**

Run:
```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql/src/lexicon_graphql
git mv dataloader.gleam query/dataloader.gleam
```

Expected: File moved successfully

**Step 2: Update imports in query/dataloader.gleam**

File: `lexicon_graphql/src/lexicon_graphql/query/dataloader.gleam`

Update internal imports (will be fixed when those modules move):
- Note: collection_meta, uri_extractor will be updated in later tasks

**Step 3: Commit**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql
git add -A
git commit -m "refactor: move dataloader to query/ directory"
```

---

## Task 4: Move Mutation Modules

**Files:**
- Move: `lexicon_graphql/src/lexicon_graphql/mutation_builder.gleam` → `lexicon_graphql/src/lexicon_graphql/mutation/builder.gleam`

**Step 1: Move mutation_builder.gleam**

Run:
```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql/src/lexicon_graphql
git mv mutation_builder.gleam mutation/builder.gleam
```

Expected: File moved successfully

**Step 2: Update imports in mutation/builder.gleam**

File: `lexicon_graphql/src/lexicon_graphql/mutation/builder.gleam`

Update imports:
- `import lexicon_graphql/types` → `import lexicon_graphql/types`
- Note: Other imports will be updated after those modules move

**Step 3: Commit**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql
git add -A
git commit -m "refactor: move mutation_builder to mutation/ directory"
```

---

## Task 5: Move Input Modules

**Files:**
- Move: `lexicon_graphql/src/lexicon_graphql/where_input.gleam` → `lexicon_graphql/src/lexicon_graphql/input/where.gleam`
- Move: `lexicon_graphql/src/lexicon_graphql/aggregate_input.gleam` → `lexicon_graphql/src/lexicon_graphql/input/aggregate.gleam`
- Move: `lexicon_graphql/src/lexicon_graphql/connection.gleam` → `lexicon_graphql/src/lexicon_graphql/input/connection.gleam`

**Step 1: Move where_input.gleam**

Run:
```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql/src/lexicon_graphql
git mv where_input.gleam input/where.gleam
```

Expected: File moved successfully

**Step 2: Move aggregate_input.gleam**

Run:
```bash
git mv aggregate_input.gleam input/aggregate.gleam
```

Expected: File moved successfully

**Step 3: Move connection.gleam**

Run:
```bash
git mv connection.gleam input/connection.gleam
```

Expected: File moved successfully

**Step 4: Commit**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql
git add -A
git commit -m "refactor: move input modules to input/ directory"
```

---

## Task 6: Move Output Modules

**Files:**
- Move: `lexicon_graphql/src/lexicon_graphql/aggregate_types.gleam` → `lexicon_graphql/src/lexicon_graphql/output/aggregate.gleam`

**Step 1: Move aggregate_types.gleam**

Run:
```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql/src/lexicon_graphql
git mv aggregate_types.gleam output/aggregate.gleam
```

Expected: File moved successfully

**Step 2: Commit**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql
git add -A
git commit -m "refactor: move aggregate_types to output/ directory"
```

---

## Task 7: Move Scalar Modules

**Files:**
- Move: `lexicon_graphql/src/lexicon_graphql/blob_type.gleam` → `lexicon_graphql/src/lexicon_graphql/scalar/blob.gleam`

**Step 1: Move blob_type.gleam**

Run:
```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql/src/lexicon_graphql
git mv blob_type.gleam scalar/blob.gleam
```

Expected: File moved successfully

**Step 2: Commit**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql
git add -A
git commit -m "refactor: move blob_type to scalar/ directory"
```

---

## Task 8: Move Lexicon-Specific Modules to Internal

**Files:**
- Move: `lexicon_graphql/src/lexicon_graphql/lexicon_parser.gleam` → `lexicon_graphql/src/lexicon_graphql/internal/lexicon/parser.gleam`
- Move: `lexicon_graphql/src/lexicon_graphql/nsid.gleam` → `lexicon_graphql/src/lexicon_graphql/internal/lexicon/nsid.gleam`
- Move: `lexicon_graphql/src/lexicon_graphql/collection_meta.gleam` → `lexicon_graphql/src/lexicon_graphql/internal/lexicon/collection_meta.gleam`
- Move: `lexicon_graphql/src/lexicon_graphql/lexicon_registry.gleam` → `lexicon_graphql/src/lexicon_graphql/internal/lexicon/registry.gleam`
- Move: `lexicon_graphql/src/lexicon_graphql/ref_resolver.gleam` → `lexicon_graphql/src/lexicon_graphql/internal/lexicon/ref_resolver.gleam`
- Move: `lexicon_graphql/src/lexicon_graphql/uri_extractor.gleam` → `lexicon_graphql/src/lexicon_graphql/internal/lexicon/uri_extractor.gleam`

**Step 1: Move lexicon_parser.gleam**

Run:
```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql/src/lexicon_graphql
git mv lexicon_parser.gleam internal/lexicon/parser.gleam
```

Expected: File moved successfully

**Step 2: Move nsid.gleam**

Run:
```bash
git mv nsid.gleam internal/lexicon/nsid.gleam
```

Expected: File moved successfully

**Step 3: Move collection_meta.gleam**

Run:
```bash
git mv collection_meta.gleam internal/lexicon/collection_meta.gleam
```

Expected: File moved successfully

**Step 4: Move lexicon_registry.gleam**

Run:
```bash
git mv lexicon_registry.gleam internal/lexicon/registry.gleam
```

Expected: File moved successfully

**Step 5: Move ref_resolver.gleam**

Run:
```bash
git mv ref_resolver.gleam internal/lexicon/ref_resolver.gleam
```

Expected: File moved successfully

**Step 6: Move uri_extractor.gleam**

Run:
```bash
git mv uri_extractor.gleam internal/lexicon/uri_extractor.gleam
```

Expected: File moved successfully

**Step 7: Commit**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql
git add -A
git commit -m "refactor: move lexicon-specific modules to internal/lexicon/"
```

---

## Task 9: Move GraphQL Utility Modules to Internal

**Files:**
- Move: `lexicon_graphql/src/lexicon_graphql/type_mapper.gleam` → `lexicon_graphql/src/lexicon_graphql/internal/graphql/type_mapper.gleam`
- Move: `lexicon_graphql/src/lexicon_graphql/object_type_builder.gleam` → `lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam`

**Step 1: Move type_mapper.gleam**

Run:
```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql/src/lexicon_graphql
git mv type_mapper.gleam internal/graphql/type_mapper.gleam
```

Expected: File moved successfully

**Step 2: Move object_type_builder.gleam**

Run:
```bash
git mv object_type_builder.gleam internal/graphql/object_builder.gleam
```

Expected: File moved successfully

**Step 3: Commit**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql
git add -A
git commit -m "refactor: move GraphQL utility modules to internal/graphql/"
```

---

## Task 10: Fix All Internal Cross-References in Moved Modules

**Files:**
- Modify: All moved modules (18 files) to update their import statements

**Context:** Now that all modules are in their final locations, we need to update all import paths throughout the package. This is a comprehensive search-and-replace task.

**Step 1: Fix imports in schema/builder.gleam**

File: `lexicon_graphql/src/lexicon_graphql/schema/builder.gleam`

Find and replace imports (preserve the rest of the file exactly):
- `import lexicon_graphql/nsid` → `import lexicon_graphql/internal/lexicon/nsid`
- `import lexicon_graphql/type_mapper` → `import lexicon_graphql/internal/graphql/type_mapper`
- `import lexicon_graphql/object_type_builder` → `import lexicon_graphql/internal/graphql/object_builder`
- `import lexicon_graphql/lexicon_registry` → `import lexicon_graphql/internal/lexicon/registry`
- `import lexicon_graphql/ref_resolver` → `import lexicon_graphql/internal/lexicon/ref_resolver`
- `import lexicon_graphql/blob_type` → `import lexicon_graphql/scalar/blob`
- `import lexicon_graphql/connection` → `import lexicon_graphql/input/connection`

**Step 2: Fix imports in schema/database.gleam**

File: `lexicon_graphql/src/lexicon_graphql/schema/database.gleam`

Find and replace imports:
- `import lexicon_graphql/schema_builder` → `import lexicon_graphql/schema/builder`
- `import lexicon_graphql/mutation_builder` → `import lexicon_graphql/mutation/builder`
- `import lexicon_graphql/nsid` → `import lexicon_graphql/internal/lexicon/nsid`
- `import lexicon_graphql/type_mapper` → `import lexicon_graphql/internal/graphql/type_mapper`
- `import lexicon_graphql/object_type_builder` → `import lexicon_graphql/internal/graphql/object_builder`
- `import lexicon_graphql/collection_meta` → `import lexicon_graphql/internal/lexicon/collection_meta`
- `import lexicon_graphql/uri_extractor` → `import lexicon_graphql/internal/lexicon/uri_extractor`
- `import lexicon_graphql/where_input` → `import lexicon_graphql/input/where`
- `import lexicon_graphql/aggregate_input` → `import lexicon_graphql/input/aggregate`
- `import lexicon_graphql/dataloader` → `import lexicon_graphql/query/dataloader`
- `import lexicon_graphql/connection` → `import lexicon_graphql/input/connection`
- `import lexicon_graphql/blob_type` → `import lexicon_graphql/scalar/blob`
- `import lexicon_graphql/aggregate_types` → `import lexicon_graphql/output/aggregate`

**Step 3: Fix imports in query/dataloader.gleam**

File: `lexicon_graphql/src/lexicon_graphql/query/dataloader.gleam`

Find and replace imports:
- `import lexicon_graphql/collection_meta` → `import lexicon_graphql/internal/lexicon/collection_meta`
- `import lexicon_graphql/uri_extractor` → `import lexicon_graphql/internal/lexicon/uri_extractor`
- `import lexicon_graphql/where_input` → `import lexicon_graphql/input/where`

**Step 4: Fix imports in mutation/builder.gleam**

File: `lexicon_graphql/src/lexicon_graphql/mutation/builder.gleam`

Find and replace imports:
- `import lexicon_graphql/nsid` → `import lexicon_graphql/internal/lexicon/nsid`
- `import lexicon_graphql/type_mapper` → `import lexicon_graphql/internal/graphql/type_mapper`
- `import lexicon_graphql/blob_type` → `import lexicon_graphql/scalar/blob`

**Step 5: Fix imports in internal/lexicon/parser.gleam**

File: `lexicon_graphql/src/lexicon_graphql/internal/lexicon/parser.gleam`

Find and replace imports:
- `import lexicon_graphql/types` → `import lexicon_graphql/types` (already correct, but verify)

**Step 6: Fix imports in internal/lexicon/collection_meta.gleam**

File: `lexicon_graphql/src/lexicon_graphql/internal/lexicon/collection_meta.gleam`

Find and replace imports:
- `import lexicon_graphql/types` → `import lexicon_graphql/types`

**Step 7: Fix imports in internal/lexicon/ref_resolver.gleam**

File: `lexicon_graphql/src/lexicon_graphql/internal/lexicon/ref_resolver.gleam`

Find and replace imports:
- `import lexicon_graphql/types` → `import lexicon_graphql/types`

**Step 8: Fix imports in internal/graphql/type_mapper.gleam**

File: `lexicon_graphql/src/lexicon_graphql/internal/graphql/type_mapper.gleam`

Find and replace imports:
- `import lexicon_graphql/blob_type` → `import lexicon_graphql/scalar/blob`
- `import lexicon_graphql/lexicon_registry` → `import lexicon_graphql/internal/lexicon/registry`
- `import lexicon_graphql/ref_resolver` → `import lexicon_graphql/internal/lexicon/ref_resolver`

**Step 9: Fix imports in internal/graphql/object_builder.gleam**

File: `lexicon_graphql/src/lexicon_graphql/internal/graphql/object_builder.gleam`

Find and replace imports:
- `import lexicon_graphql/nsid` → `import lexicon_graphql/internal/lexicon/nsid`
- `import lexicon_graphql/type_mapper` → `import lexicon_graphql/internal/graphql/type_mapper`
- `import lexicon_graphql/lexicon_registry` → `import lexicon_graphql/internal/lexicon/registry`
- `import lexicon_graphql/ref_resolver` → `import lexicon_graphql/internal/lexicon/ref_resolver`

**Step 10: Fix imports in internal/lexicon/registry.gleam**

File: `lexicon_graphql/src/lexicon_graphql/internal/lexicon/registry.gleam`

Find and replace imports:
- `import lexicon_graphql/types` → `import lexicon_graphql/types`
- `import lexicon_graphql/nsid` → `import lexicon_graphql/internal/lexicon/nsid`

**Step 11: Run build to check for import errors**

Run:
```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql
gleam build
```

Expected: Build should succeed (or reveal any remaining import issues)

**Step 12: Commit**

```bash
git add -A
git commit -m "refactor: fix all internal cross-references after module reorganization"
```

---

## Task 11: Update Main Module with Re-Exports

**Files:**
- Modify: `lexicon_graphql/src/lexicon_graphql.gleam`

**Step 1: Read current main module**

File: `lexicon_graphql/src/lexicon_graphql.gleam`

Current content (approximately 5 lines with stub main function).

**Step 2: Replace with public API re-exports**

File: `lexicon_graphql/src/lexicon_graphql.gleam`

Replace entire content with:

```gleam
//// Public API for lexicon_graphql package
////
//// This module re-exports the main public API functions and types.
//// For more specialized usage, import specific submodules:
//// - lexicon_graphql/schema/builder - Basic schema building
//// - lexicon_graphql/schema/database - Database-backed schema building
//// - lexicon_graphql/query/dataloader - Batching and pagination types
//// - lexicon_graphql/mutation/builder - Mutation operations
//// - lexicon_graphql/input/* - Input types (where, aggregate, connection)
//// - lexicon_graphql/output/* - Output types (aggregate results)
//// - lexicon_graphql/scalar/* - Custom scalar types (blob)

// Re-export core types
pub type Lexicon =
  types.Lexicon

pub type Defs =
  types.Defs

pub type Def =
  types.Def

pub type RecordDef =
  types.RecordDef

pub type ObjectDef =
  types.ObjectDef

pub type Property =
  types.Property

// Re-export main schema building functions
pub fn build_schema(lexicons: List(Lexicon)) {
  schema_builder.build_schema(lexicons)
}

pub fn build_schema_with_subscriptions(
  lexicons: List(Lexicon),
  fetcher: db_schema_builder.RecordFetcher,
  batch_fetcher: Option(dataloader.BatchFetcher),
  paginated_batch_fetcher: Option(dataloader.PaginatedBatchFetcher),
  create_factory: Option(mutation_builder.ResolverFactory),
  update_factory: Option(mutation_builder.ResolverFactory),
  delete_factory: Option(mutation_builder.ResolverFactory),
  upload_blob_factory: Option(mutation_builder.UploadBlobResolverFactory),
  aggregate_fetcher: Option(db_schema_builder.AggregateFetcher),
) {
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
  )
}

// Re-export lexicon parser
pub fn parse_lexicon(json_str: String) {
  lexicon_parser.parse_lexicon(json_str)
}

// Import statements for re-exports
import lexicon_graphql/types
import lexicon_graphql/schema/builder as schema_builder
import lexicon_graphql/schema/database as db_schema_builder
import lexicon_graphql/query/dataloader
import lexicon_graphql/mutation/builder as mutation_builder
import lexicon_graphql/internal/lexicon/parser as lexicon_parser
import gleam/option.{type Option}
```

**Step 3: Run build to verify**

Run:
```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql
gleam build
```

Expected: Build succeeds

**Step 4: Commit**

```bash
git add lexicon_graphql/src/lexicon_graphql.gleam
git commit -m "feat: add public API re-exports to main module"
```

---

## Task 12: Update Test Files

**Files:**
- Modify: All 18 test files in `lexicon_graphql/test/` directory

**Context:** Test files import from the old module paths. Each needs to be updated to import from new locations.

**Step 1: List all test files**

Run:
```bash
find /Users/chadmiller/code/quickslice/lexicon_graphql/test -name "*.gleam" -type f
```

Expected: List of approximately 18 test files

**Step 2: Update schema_builder_test.gleam**

File: `lexicon_graphql/test/lexicon_graphql/schema_builder_test.gleam`

Find and replace:
- `import lexicon_graphql/schema_builder` → `import lexicon_graphql/schema/builder as schema_builder`
- `import lexicon_graphql/lexicon_parser` → `import lexicon_graphql/internal/lexicon/parser as lexicon_parser`
- Any other old paths → new paths as needed

**Step 3: Update db_schema_builder_test.gleam**

File: `lexicon_graphql/test/lexicon_graphql/db_schema_builder_test.gleam`

Find and replace:
- `import lexicon_graphql/db_schema_builder` → `import lexicon_graphql/schema/database as db_schema_builder`
- `import lexicon_graphql/lexicon_parser` → `import lexicon_graphql/internal/lexicon/parser as lexicon_parser`
- `import lexicon_graphql/dataloader` → `import lexicon_graphql/query/dataloader`

**Step 4: Update mutation_builder_test.gleam**

File: `lexicon_graphql/test/lexicon_graphql/mutation_builder_test.gleam`

Find and replace:
- `import lexicon_graphql/mutation_builder` → `import lexicon_graphql/mutation/builder as mutation_builder`
- `import lexicon_graphql/lexicon_parser` → `import lexicon_graphql/internal/lexicon/parser as lexicon_parser`

**Step 5: Update dataloader_test.gleam**

File: `lexicon_graphql/test/lexicon_graphql/dataloader_test.gleam`

Find and replace:
- `import lexicon_graphql/dataloader` → `import lexicon_graphql/query/dataloader`

**Step 6: Update where_input_test.gleam**

File: `lexicon_graphql/test/lexicon_graphql/where_input_test.gleam`

Find and replace:
- `import lexicon_graphql/where_input` → `import lexicon_graphql/input/where as where_input`

**Step 7: Update aggregate_input_test.gleam**

File: `lexicon_graphql/test/lexicon_graphql/aggregate_input_test.gleam`

Find and replace:
- `import lexicon_graphql/aggregate_input` → `import lexicon_graphql/input/aggregate as aggregate_input`

**Step 8: Update connection_test.gleam**

File: `lexicon_graphql/test/lexicon_graphql/connection_test.gleam`

Find and replace:
- `import lexicon_graphql/connection` → `import lexicon_graphql/input/connection as connection`

**Step 9: Update blob_type_test.gleam**

File: `lexicon_graphql/test/lexicon_graphql/blob_type_test.gleam`

Find and replace:
- `import lexicon_graphql/blob_type` → `import lexicon_graphql/scalar/blob as blob_type`

**Step 10: Update lexicon_parser_test.gleam**

File: `lexicon_graphql/test/lexicon_graphql/lexicon_parser_test.gleam`

Find and replace:
- `import lexicon_graphql/lexicon_parser` → `import lexicon_graphql/internal/lexicon/parser as lexicon_parser`

**Step 11: Update nsid_test.gleam**

File: `lexicon_graphql/test/lexicon_graphql/nsid_test.gleam`

Find and replace:
- `import lexicon_graphql/nsid` → `import lexicon_graphql/internal/lexicon/nsid as nsid`

**Step 12: Update collection_meta_test.gleam**

File: `lexicon_graphql/test/lexicon_graphql/collection_meta_test.gleam`

Find and replace:
- `import lexicon_graphql/collection_meta` → `import lexicon_graphql/internal/lexicon/collection_meta as collection_meta`

**Step 13: Update type_mapper_test.gleam**

File: `lexicon_graphql/test/lexicon_graphql/type_mapper_test.gleam`

Find and replace:
- `import lexicon_graphql/type_mapper` → `import lexicon_graphql/internal/graphql/type_mapper as type_mapper`

**Step 14: Update object_type_builder_test.gleam**

File: `lexicon_graphql/test/lexicon_graphql/object_type_builder_test.gleam`

Find and replace:
- `import lexicon_graphql/object_type_builder` → `import lexicon_graphql/internal/graphql/object_builder as object_type_builder`

**Step 15: Update uri_extractor_test.gleam (if exists)**

File: `lexicon_graphql/test/lexicon_graphql/uri_extractor_test.gleam`

Find and replace:
- `import lexicon_graphql/uri_extractor` → `import lexicon_graphql/internal/lexicon/uri_extractor as uri_extractor`

**Step 16: Update ref_resolver_test.gleam (if exists)**

File: `lexicon_graphql/test/lexicon_graphql/ref_resolver_test.gleam`

Find and replace:
- `import lexicon_graphql/ref_resolver` → `import lexicon_graphql/internal/lexicon/ref_resolver as ref_resolver`

**Step 17: Update lexicon_registry_test.gleam (if exists)**

File: `lexicon_graphql/test/lexicon_graphql/lexicon_registry_test.gleam`

Find and replace:
- `import lexicon_graphql/lexicon_registry` → `import lexicon_graphql/internal/lexicon/registry as lexicon_registry`

**Step 18: Update aggregate_types_test.gleam (if exists)**

File: `lexicon_graphql/test/lexicon_graphql/aggregate_types_test.gleam`

Find and replace:
- `import lexicon_graphql/aggregate_types` → `import lexicon_graphql/output/aggregate as aggregate_types`

**Step 19: Run all tests**

Run:
```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql
gleam test
```

Expected: All tests pass

**Step 20: Commit**

```bash
git add test/
git commit -m "refactor: update all test imports to use new module paths"
```

---

## Task 13: Update Server Imports - graphql_gleam.gleam

**Files:**
- Modify: `/Users/chadmiller/code/quickslice/server/src/graphql_gleam.gleam`

**Step 1: Update imports at lines 21-24**

File: `/Users/chadmiller/code/quickslice/server/src/graphql_gleam.gleam`

Find lines 21-24:
```gleam
import lexicon_graphql/aggregate_input
import lexicon_graphql/dataloader
import lexicon_graphql/db_schema_builder
import lexicon_graphql/lexicon_parser
```

Replace with:
```gleam
import lexicon_graphql/input/aggregate as aggregate_input
import lexicon_graphql/query/dataloader
import lexicon_graphql/schema/database as db_schema_builder
import lexicon_graphql/internal/lexicon/parser as lexicon_parser
```

**Step 2: Run server build to verify**

Run:
```bash
cd /Users/chadmiller/code/quickslice/server
gleam build
```

Expected: Build succeeds

**Step 3: Commit**

```bash
cd /Users/chadmiller/code/quickslice
git add server/src/graphql_gleam.gleam
git commit -m "refactor(server): update graphql_gleam imports for new lexicon_graphql structure"
```

---

## Task 14: Update Server Imports - aggregates.gleam

**Files:**
- Modify: `/Users/chadmiller/code/quickslice/server/src/database/queries/aggregates.gleam`

**Step 1: Update import at line 12**

File: `/Users/chadmiller/code/quickslice/server/src/database/queries/aggregates.gleam`

Find line 12:
```gleam
import lexicon_graphql/aggregate_types
```

Replace with:
```gleam
import lexicon_graphql/output/aggregate as aggregate_types
```

**Step 2: Run server build to verify**

Run:
```bash
cd /Users/chadmiller/code/quickslice/server
gleam build
```

Expected: Build succeeds

**Step 3: Commit**

```bash
cd /Users/chadmiller/code/quickslice
git add server/src/database/queries/aggregates.gleam
git commit -m "refactor(server): update aggregates imports for new lexicon_graphql structure"
```

---

## Task 15: Update Server Imports - where_converter.gleam

**Files:**
- Modify: `/Users/chadmiller/code/quickslice/server/src/where_converter.gleam`

**Step 1: Update import at line 8**

File: `/Users/chadmiller/code/quickslice/server/src/where_converter.gleam`

Find line 8:
```gleam
import lexicon_graphql/where_input
```

Replace with:
```gleam
import lexicon_graphql/input/where as where_input
```

**Step 2: Run server build to verify**

Run:
```bash
cd /Users/chadmiller/code/quickslice/server
gleam build
```

Expected: Build succeeds

**Step 3: Commit**

```bash
cd /Users/chadmiller/code/quickslice
git add server/src/where_converter.gleam
git commit -m "refactor(server): update where_converter imports for new lexicon_graphql structure"
```

---

## Task 16: Final Verification - Run All Tests

**Step 1: Run lexicon_graphql tests**

Run:
```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql
gleam test
```

Expected: All tests pass (100% success)

**Step 2: Run server tests (if any)**

Run:
```bash
cd /Users/chadmiller/code/quickslice/server
gleam test
```

Expected: All tests pass

**Step 3: Build both packages**

Run:
```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql
gleam build
cd /Users/chadmiller/code/quickslice/server
gleam build
```

Expected: Both build successfully with no errors

**Step 4: Verify no orphaned files remain**

Run:
```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql/src/lexicon_graphql
ls -la *.gleam
```

Expected: Only `lexicon_graphql.gleam` and `types.gleam` should remain at the root level

**Step 5: Commit verification results (if any changes)**

If any issues were found and fixed:
```bash
cd /Users/chadmiller/code/quickslice
git add -A
git commit -m "fix: address final verification issues"
```

---

## Task 17: Documentation Update

**Files:**
- Create: `lexicon_graphql/MIGRATION.md`

**Step 1: Create migration guide**

File: `lexicon_graphql/MIGRATION.md`

```markdown
# Migration Guide: lexicon_graphql v1.x to v2.x

## Overview

The lexicon_graphql package has been reorganized to follow GraphQL abstractions (Schema/Query/Mutation/Input/Output/Scalar) with internal implementation details moved to `internal/`.

## Import Path Changes

### Schema Building
- `lexicon_graphql/schema_builder` → `lexicon_graphql/schema/builder`
- `lexicon_graphql/db_schema_builder` → `lexicon_graphql/schema/database`

### Query
- `lexicon_graphql/dataloader` → `lexicon_graphql/query/dataloader`

### Mutation
- `lexicon_graphql/mutation_builder` → `lexicon_graphql/mutation/builder`

### Input Types
- `lexicon_graphql/where_input` → `lexicon_graphql/input/where`
- `lexicon_graphql/aggregate_input` → `lexicon_graphql/input/aggregate`
- `lexicon_graphql/connection` → `lexicon_graphql/input/connection`

### Output Types
- `lexicon_graphql/aggregate_types` → `lexicon_graphql/output/aggregate`

### Scalar Types
- `lexicon_graphql/blob_type` → `lexicon_graphql/scalar/blob`

### Internal Modules (moved to internal/)
- `lexicon_graphql/lexicon_parser` → `lexicon_graphql/internal/lexicon/parser`
- `lexicon_graphql/nsid` → `lexicon_graphql/internal/lexicon/nsid`
- `lexicon_graphql/collection_meta` → `lexicon_graphql/internal/lexicon/collection_meta`
- `lexicon_graphql/lexicon_registry` → `lexicon_graphql/internal/lexicon/registry`
- `lexicon_graphql/ref_resolver` → `lexicon_graphql/internal/lexicon/ref_resolver`
- `lexicon_graphql/uri_extractor` → `lexicon_graphql/internal/lexicon/uri_extractor`
- `lexicon_graphql/type_mapper` → `lexicon_graphql/internal/graphql/type_mapper`
- `lexicon_graphql/object_type_builder` → `lexicon_graphql/internal/graphql/object_builder`

## Recommended Usage

### Option 1: Use Main Module (Simplest)

```gleam
import lexicon_graphql

// Parse lexicons
let lexicons = lexicon_graphql.parse_lexicon(json_string)

// Build schema
let schema = lexicon_graphql.build_schema(lexicons)

// Or build with database
let schema = lexicon_graphql.build_schema_with_subscriptions(
  lexicons,
  fetcher,
  // ... other args
)
```

### Option 2: Import Specific Modules

```gleam
import lexicon_graphql/internal/lexicon/parser
import lexicon_graphql/schema/database
import lexicon_graphql/query/dataloader

let lexicons = parser.parse_lexicon(json_string)
let schema = database.build_schema_with_subscriptions(/* ... */)
```

## Breaking Changes

- Modules in `internal/` are considered implementation details and may change without notice
- If you were importing internal modules directly, update to new paths or use the main module API

## Benefits

✅ Clear organization by GraphQL concepts
✅ Reduced public API surface
✅ Better separation of concerns
✅ Future-ready for subscriptions
```

**Step 2: Commit documentation**

```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql
git add MIGRATION.md
git commit -m "docs: add migration guide for v2.x reorganization"
```

---

## Task 18: Final Commit and Tag

**Step 1: Review all changes**

Run:
```bash
cd /Users/chadmiller/code/quickslice
git log --oneline --graph -20
```

Expected: See all commits from this reorganization

**Step 2: Create summary of changes**

View the file structure:
```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql
tree src/lexicon_graphql -L 3
```

Expected: See organized directory structure with schema/, query/, mutation/, input/, output/, scalar/, internal/

**Step 3: Tag the release (optional)**

If this is a new version:
```bash
cd /Users/chadmiller/code/quickslice/lexicon_graphql
git tag -a v2.0.0 -m "feat: reorganize package by GraphQL abstractions"
```

**Step 4: Done!**

Reorganization complete. The package now follows GraphQL abstractions with clear public API boundaries.

---

## Verification Checklist

- [ ] All 18 modules moved to new locations
- [ ] All internal cross-references updated
- [ ] Main module has re-exports
- [ ] All 18 test files updated and passing
- [ ] Server imports updated (3 files)
- [ ] Server builds successfully
- [ ] lexicon_graphql builds successfully
- [ ] No orphaned files at root level (except types.gleam and lexicon_graphql.gleam)
- [ ] Migration guide created
- [ ] All changes committed

---

## Notes

- **DRY:** No duplication - each module moved once
- **YAGNI:** Only moved existing modules, no new abstractions added
- **TDD:** Tests updated to verify correctness at each step
- **Frequent commits:** 18 tasks = 18 commits minimum
- **Gleam conventions:** Follows standard Gleam package organization with internal/ for implementation details