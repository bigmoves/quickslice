# Extract atproto_car Library Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Extract `server/src/car/` into a standalone Gleam package `atproto_car` at the repository root for eventual publication to Hex.

**Architecture:** Single public module `atproto_car` re-exports key types and functions. Internal modules under `atproto_car/internal/` handle implementation details (blockstore, cbor, cid, mst, varint). Erlang FFI files provide ETS and encoding support.

**Tech Stack:** Gleam, Erlang FFI, erl_cbor, base32

---

## Task 1: Create Package Skeleton

**Files:**
- Create: `atproto_car/gleam.toml`
- Create: `atproto_car/src/atproto_car.gleam`
- Create: `atproto_car/test/atproto_car_test.gleam`

**Step 1: Create directory structure**

```bash
mkdir -p atproto_car/src/atproto_car/internal
mkdir -p atproto_car/test
```

**Step 2: Create gleam.toml**

Create `atproto_car/gleam.toml`:

```toml
name = "atproto_car"
version = "0.1.0"
description = "CAR (Content Addressable aRchive) file parsing for AT Protocol"
licences = ["MIT"]
target = "erlang"

[dependencies]
gleam_stdlib = ">= 0.60.0 and < 1.0.0"
erl_cbor = ">= 2.0.0 and < 3.0.0"
base32 = ">= 1.0.0 and < 2.0.0"

[dev-dependencies]
gleeunit = ">= 1.0.0 and < 2.0.0"
```

**Step 3: Create stub main module**

Create `atproto_car/src/atproto_car.gleam`:

```gleam
//// CAR (Content Addressable aRchive) file parsing for AT Protocol
////
//// This library parses CAR files containing AT Protocol repository data,
//// including MST (Merkle Search Tree) traversal for record extraction.

pub type Placeholder {
  Placeholder
}
```

**Step 4: Create test stub**

Create `atproto_car/test/atproto_car_test.gleam`:

```gleam
import gleeunit

pub fn main() {
  gleeunit.main()
}
```

**Step 5: Verify package builds**

Run: `cd atproto_car && gleam build`
Expected: Successful build with no errors

**Step 6: Commit**

```bash
git add atproto_car/
git commit -m "feat(atproto_car): create package skeleton"
```

---

## Task 2: Move varint Module

**Files:**
- Create: `atproto_car/src/atproto_car/internal/varint.gleam`
- Reference: `server/src/car/varint.gleam`

**Step 1: Copy varint module**

Copy `server/src/car/varint.gleam` to `atproto_car/src/atproto_car/internal/varint.gleam`.

The file should be copied exactly as-is (no changes needed - it has no external dependencies).

**Step 2: Verify it compiles**

Run: `cd atproto_car && gleam build`
Expected: Successful build

**Step 3: Commit**

```bash
git add atproto_car/src/atproto_car/internal/varint.gleam
git commit -m "feat(atproto_car): add varint module"
```

---

## Task 3: Move CID Module and FFI

**Files:**
- Create: `atproto_car/src/atproto_car/internal/cid.gleam`
- Create: `atproto_car/src/atproto_car/internal/cid_ffi.erl`
- Reference: `server/src/car/cid.gleam`
- Reference: `server/src/car/cid_ffi.erl`

**Step 1: Copy cid.gleam**

Copy `server/src/car/cid.gleam` to `atproto_car/src/atproto_car/internal/cid.gleam`.

**Step 2: Update import in cid.gleam**

Change line 7:
```gleam
// Before
import car/varint

// After
import atproto_car/internal/varint
```

**Step 3: Copy cid_ffi.erl**

Copy `server/src/car/cid_ffi.erl` to `atproto_car/src/atproto_car/internal/cid_ffi.erl`.

No changes needed to the Erlang file.

**Step 4: Verify it compiles**

Run: `cd atproto_car && gleam build`
Expected: Successful build

**Step 5: Commit**

```bash
git add atproto_car/src/atproto_car/internal/cid.gleam
git add atproto_car/src/atproto_car/internal/cid_ffi.erl
git commit -m "feat(atproto_car): add cid module and FFI"
```

---

## Task 4: Move CBOR Module and FFI

**Files:**
- Create: `atproto_car/src/atproto_car/internal/cbor.gleam`
- Create: `atproto_car/src/atproto_car/internal/cbor_ffi.erl`
- Reference: `server/src/car/cbor.gleam`
- Reference: `server/src/car/cbor_ffi.erl`

**Step 1: Copy cbor.gleam**

Copy `server/src/car/cbor.gleam` to `atproto_car/src/atproto_car/internal/cbor.gleam`.

No changes needed (no internal imports).

**Step 2: Copy cbor_ffi.erl**

Copy `server/src/car/cbor_ffi.erl` to `atproto_car/src/atproto_car/internal/cbor_ffi.erl`.

No changes needed.

**Step 3: Verify it compiles**

Run: `cd atproto_car && gleam build`
Expected: Successful build

**Step 4: Commit**

```bash
git add atproto_car/src/atproto_car/internal/cbor.gleam
git add atproto_car/src/atproto_car/internal/cbor_ffi.erl
git commit -m "feat(atproto_car): add cbor module and FFI"
```

---

## Task 5: Move Blockstore Module and FFI

**Files:**
- Create: `atproto_car/src/atproto_car/internal/blockstore.gleam`
- Create: `atproto_car/src/atproto_car/internal/blockstore_ffi.erl`
- Reference: `server/src/car/blockstore.gleam`
- Reference: `server/src/car/car_ffi.erl`

**Step 1: Copy blockstore.gleam**

Copy `server/src/car/blockstore.gleam` to `atproto_car/src/atproto_car/internal/blockstore.gleam`.

**Step 2: Update imports in blockstore.gleam**

Change lines 7-8:
```gleam
// Before
import car/cid.{type Cid}
import car/varint

// After
import atproto_car/internal/cid.{type Cid}
import atproto_car/internal/varint
```

**Step 3: Update FFI module reference in blockstore.gleam**

Change lines 28-35 to reference new FFI module name:
```gleam
// Before
@external(erlang, "car_ffi", "ets_new")
fn ets_new() -> EtsTable

@external(erlang, "car_ffi", "ets_insert")
fn ets_insert(table: EtsTable, key: BitArray, value: Int) -> Nil

@external(erlang, "car_ffi", "ets_get")
fn ets_get(table: EtsTable, key: BitArray) -> Result(Int, Nil)

// After
@external(erlang, "blockstore_ffi", "ets_new")
fn ets_new() -> EtsTable

@external(erlang, "blockstore_ffi", "ets_insert")
fn ets_insert(table: EtsTable, key: BitArray, value: Int) -> Nil

@external(erlang, "blockstore_ffi", "ets_get")
fn ets_get(table: EtsTable, key: BitArray) -> Result(Int, Nil)
```

**Step 4: Create blockstore_ffi.erl**

Create `atproto_car/src/atproto_car/internal/blockstore_ffi.erl`:

```erlang
-module(blockstore_ffi).
-export([ets_new/0, ets_insert/3, ets_get/2]).

%% Create a new ETS table for blockstore
%% Returns an opaque reference to the table
ets_new() ->
    ets:new(blockstore, [set, public, {read_concurrency, true}]).

%% Insert a key-value pair into ETS table
%% Key is binary (CID bytes), Value is integer (offset)
ets_insert(Table, Key, Value) ->
    ets:insert(Table, {Key, Value}),
    nil.

%% Get a value from ETS table by key
%% Returns {ok, Value} or {error, nil}
ets_get(Table, Key) ->
    case ets:lookup(Table, Key) of
        [{_, Value}] -> {ok, Value};
        [] -> {error, nil}
    end.
```

**Step 5: Verify it compiles**

Run: `cd atproto_car && gleam build`
Expected: Successful build

**Step 6: Commit**

```bash
git add atproto_car/src/atproto_car/internal/blockstore.gleam
git add atproto_car/src/atproto_car/internal/blockstore_ffi.erl
git commit -m "feat(atproto_car): add blockstore module and FFI"
```

---

## Task 6: Create MST Module (Extract from car.gleam)

**Files:**
- Create: `atproto_car/src/atproto_car/internal/mst.gleam`
- Reference: `server/src/car/car.gleam` (lines 334-596)

**Step 1: Create mst.gleam with MST types and walking logic**

Create `atproto_car/src/atproto_car/internal/mst.gleam`:

```gleam
//// MST (Merkle Search Tree) traversal for AT Protocol repositories

import atproto_car/internal/blockstore.{type BlockStore}
import atproto_car/internal/cbor
import atproto_car/internal/cid.{type Cid}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/list
import gleam/string

/// MST entry - path and CID
pub type MstEntry {
  MstEntry(path: String, cid: Cid)
}

/// Parsed MST node
type MstNode {
  MstNode(left: Result(Cid, Nil), entries: List(MstNodeEntry))
}

/// MST node entry
type MstNodeEntry {
  MstNodeEntry(
    prefix_len: Int,
    key_suffix: BitArray,
    val: Cid,
    tree: Result(Cid, Nil),
  )
}

/// Walk MST from root CID, returning all entries with their paths
pub fn walk(store: BlockStore, root_cid: Cid) -> List(MstEntry) {
  walk_node(store, root_cid, "")
}

/// Walk a single MST node recursively
fn walk_node(
  store: BlockStore,
  node_cid: Cid,
  prev_key: String,
) -> List(MstEntry) {
  case blockstore.get(store, node_cid) {
    Error(_) -> []
    Ok(node_data) -> {
      case parse_node(node_data) {
        Error(_) -> []
        Ok(node) -> {
          // Process left subtree
          let left_entries = case node.left {
            Error(_) -> []
            Ok(left_cid) -> walk_node(store, left_cid, prev_key)
          }

          // Process entries
          let #(entry_results, _) =
            list.fold(node.entries, #([], prev_key), fn(acc, entry) {
              let #(entries, last_key) = acc

              // Build full key from prefix + suffix
              let prefix = string.slice(last_key, 0, entry.prefix_len)
              let suffix = bit_array_to_string(entry.key_suffix)
              let full_key = prefix <> suffix

              // Add this entry
              let this_entry = MstEntry(path: full_key, cid: entry.val)

              // Process right subtree
              let right_entries = case entry.tree {
                Error(_) -> []
                Ok(tree_cid) -> walk_node(store, tree_cid, full_key)
              }

              #(list.flatten([entries, [this_entry], right_entries]), full_key)
            })

          list.flatten([left_entries, entry_results])
        }
      }
    }
  }
}

fn bit_array_to_string(bytes: BitArray) -> String {
  case do_bit_array_to_string(bytes) {
    Ok(s) -> s
    Error(_) -> ""
  }
}

@external(erlang, "cid_ffi", "decode_binary")
fn do_bit_array_to_string(bytes: BitArray) -> Result(String, Nil)

/// Parse MST node from CBOR data
fn parse_node(data: BitArray) -> Result(MstNode, Nil) {
  case cbor.decode(data) {
    Error(_) -> Error(Nil)
    Ok(decoded) -> {
      let left = get_optional_cid_field(decoded, "l")
      let entries = get_entries(decoded)
      Ok(MstNode(left: left, entries: entries))
    }
  }
}

/// Get optional CID field from dynamic
fn get_optional_cid_field(data: Dynamic, field: String) -> Result(Cid, Nil) {
  let decoder = {
    use val <- decode.field(field, decode.dynamic)
    decode.success(val)
  }
  case decode.run(data, decoder) {
    Ok(val) -> parse_cid_from_dynamic(val)
    Error(_) -> Error(Nil)
  }
}

/// Parse CID from dynamic (tag 42 tuple)
fn parse_cid_from_dynamic(data: Dynamic) -> Result(Cid, Nil) {
  case decode_cid_bytes(data) {
    Ok(bytes) -> {
      case cid.parse(bytes) {
        Ok(#(parsed, _)) -> Ok(parsed)
        Error(_) -> Error(Nil)
      }
    }
    Error(_) -> Error(Nil)
  }
}

@external(erlang, "cid_ffi", "decode_cid_bytes")
fn decode_cid_bytes(data: Dynamic) -> Result(BitArray, Nil)

/// Get entries array from MST node
fn get_entries(data: Dynamic) -> List(MstNodeEntry) {
  let decoder = {
    use entries <- decode.field("e", decode.list(decode.dynamic))
    decode.success(entries)
  }
  case decode.run(data, decoder) {
    Ok(entries) -> list.filter_map(entries, parse_entry)
    Error(_) -> []
  }
}

/// Parse a single MST entry
fn parse_entry(data: Dynamic) -> Result(MstNodeEntry, Nil) {
  let p_decoder = {
    use p <- decode.field("p", decode.int)
    decode.success(p)
  }

  case decode.run(data, p_decoder) {
    Error(_) -> Error(Nil)
    Ok(p) -> {
      case get_binary_field(data, "k") {
        Error(_) -> Error(Nil)
        Ok(k) -> {
          case get_optional_cid_field(data, "v") {
            Error(_) -> Error(Nil)
            Ok(v) -> {
              let t = get_optional_cid_field(data, "t")
              Ok(MstNodeEntry(prefix_len: p, key_suffix: k, val: v, tree: t))
            }
          }
        }
      }
    }
  }
}

/// Get binary field from dynamic
fn get_binary_field(data: Dynamic, field: String) -> Result(BitArray, Nil) {
  let decoder = {
    use val <- decode.field(field, decode.dynamic)
    decode.success(val)
  }
  case decode.run(data, decoder) {
    Ok(val) -> decode_binary_dyn(val)
    Error(_) -> Error(Nil)
  }
}

@external(erlang, "cid_ffi", "decode_binary")
fn decode_binary_dyn(data: Dynamic) -> Result(BitArray, Nil)
```

**Step 2: Verify it compiles**

Run: `cd atproto_car && gleam build`
Expected: Successful build

**Step 3: Commit**

```bash
git add atproto_car/src/atproto_car/internal/mst.gleam
git commit -m "feat(atproto_car): add mst module for tree traversal"
```

---

## Task 7: Create Main atproto_car Module

**Files:**
- Modify: `atproto_car/src/atproto_car.gleam`
- Reference: `server/src/car/car.gleam`

**Step 1: Write the main module**

Replace `atproto_car/src/atproto_car.gleam` with:

```gleam
//// CAR (Content Addressable aRchive) file parsing for AT Protocol
////
//// This library parses CAR files containing AT Protocol repository data,
//// including MST (Merkle Search Tree) traversal for record extraction.
////
//// ## Example
////
//// ```gleam
//// import atproto_car
////
//// let records = atproto_car.extract_records_with_paths(
////   car_bytes,
////   ["app.bsky.feed.post", "app.bsky.actor.profile"],
//// )
//// ```

import atproto_car/internal/blockstore.{type BlockStore}
import atproto_car/internal/cbor
import atproto_car/internal/cid.{type Cid}
import atproto_car/internal/mst
import atproto_car/internal/varint
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/list
import gleam/result
import gleam/string

// Re-export Cid type
pub type Cid =
  cid.Cid

/// A parsed CAR block containing CID and raw data
pub type CarBlock {
  CarBlock(cid: Cid, data: BitArray)
}

/// A record extracted from a CAR file (no path)
pub type Record {
  Record(type_: String, data: Dynamic, cid: Cid)
}

/// A record with its full path from the MST
pub type RecordWithPath {
  RecordWithPath(
    path: String,
    collection: String,
    rkey: String,
    type_: String,
    data: Dynamic,
    cid: Cid,
  )
}

/// CAR parsing errors
pub type CarError {
  InvalidHeader
  InvalidBlock
  InvalidVarint
  CborDecodeError
  CidParseError(cid.CidError)
}

/// Parse CAR header, return roots and remaining bytes
pub fn parse_header(bytes: BitArray) -> Result(#(List(Cid), BitArray), CarError) {
  use #(header_len, rest) <- result.try(
    varint.decode(bytes) |> result.replace_error(InvalidVarint),
  )

  case rest {
    <<header_bytes:bytes-size(header_len), remaining:bits>> -> {
      case cbor.decode(header_bytes) {
        Ok(_header) -> Ok(#([], remaining))
        Error(_) -> Error(CborDecodeError)
      }
    }
    _ -> Error(InvalidHeader)
  }
}

/// Parse a single block from bytes
fn parse_block(bytes: BitArray) -> Result(#(CarBlock, BitArray), CarError) {
  use #(block_len, rest) <- result.try(
    varint.decode(bytes) |> result.replace_error(InvalidVarint),
  )

  case rest {
    <<block_bytes:bytes-size(block_len), remaining:bits>> -> {
      case cid.parse(block_bytes) {
        Ok(#(parsed_cid, data)) -> {
          Ok(#(CarBlock(cid: parsed_cid, data: data), remaining))
        }
        Error(e) -> Error(CidParseError(e))
      }
    }
    _ -> Error(InvalidBlock)
  }
}

/// Fold over all blocks in a CAR file
pub fn fold_blocks(
  bytes: BitArray,
  acc: a,
  f: fn(a, CarBlock) -> a,
) -> Result(a, CarError) {
  case bytes {
    <<>> -> Ok(acc)
    _ -> {
      use #(block, rest) <- result.try(parse_block(bytes))
      fold_blocks(rest, f(acc, block), f)
    }
  }
}

/// Try to decode a block as a record
fn decode_as_record(block: CarBlock) -> Result(Record, Nil) {
  case cbor.decode(block.data) {
    Ok(decoded) -> {
      let type_decoder = {
        use type_ <- decode.field("$type", decode.string)
        decode.success(type_)
      }
      case decode.run(decoded, type_decoder) {
        Ok(type_) -> Ok(Record(type_: type_, data: decoded, cid: block.cid))
        Error(_) -> Error(Nil)
      }
    }
    Error(_) -> Error(Nil)
  }
}

/// Extract records matching given collections from CAR bytes
pub fn extract_records(
  car_bytes: BitArray,
  collections: List(String),
) -> Result(List(Record), CarError) {
  use #(_roots, blocks_bytes) <- result.try(parse_header(car_bytes))

  fold_blocks(blocks_bytes, [], fn(acc, block) {
    case decode_as_record(block) {
      Ok(record) -> {
        case list.contains(collections, record.type_) {
          True -> [record, ..acc]
          False -> acc
        }
      }
      Error(_) -> acc
    }
  })
}

/// Extract all records from CAR bytes (no collection filtering)
pub fn extract_all_records(
  car_bytes: BitArray,
) -> Result(List(Record), CarError) {
  use #(_roots, blocks_bytes) <- result.try(parse_header(car_bytes))

  fold_blocks(blocks_bytes, [], fn(acc, block) {
    case decode_as_record(block) {
      Ok(record) -> [record, ..acc]
      Error(_) -> acc
    }
  })
}

/// Extract records with their MST paths from CAR bytes
pub fn extract_records_with_paths(
  car_bytes: BitArray,
  collections: List(String),
) -> Result(List(RecordWithPath), CarError) {
  use #(roots, header_size, blocks_bytes) <- result.try(
    parse_header_with_roots(car_bytes),
  )

  use store <- result.try(
    blockstore.from_blocks_bytes(car_bytes, header_size, blocks_bytes)
    |> result.map_error(fn(_) { InvalidBlock }),
  )

  case roots {
    [] -> Ok([])
    [commit_cid, ..] -> {
      case blockstore.get(store, commit_cid) {
        Error(_) -> Ok([])
        Ok(commit_data) -> {
          case get_data_cid(commit_data) {
            Error(_) -> Ok([])
            Ok(mst_root_cid) -> {
              let mst_entries = mst.walk(store, mst_root_cid)

              let records =
                list.filter_map(mst_entries, fn(entry) {
                  case string.split_once(entry.path, "/") {
                    Error(_) -> Error(Nil)
                    Ok(#(collection, rkey)) -> {
                      case list.contains(collections, collection) {
                        False -> Error(Nil)
                        True -> {
                          case blockstore.get(store, entry.cid) {
                            Error(_) -> Error(Nil)
                            Ok(record_data) -> {
                              case cbor.decode(record_data) {
                                Error(_) -> Error(Nil)
                                Ok(decoded) -> {
                                  case get_type_field(decoded) {
                                    Error(_) -> Error(Nil)
                                    Ok(type_) ->
                                      Ok(RecordWithPath(
                                        path: entry.path,
                                        collection: collection,
                                        rkey: rkey,
                                        type_: type_,
                                        data: decoded,
                                        cid: entry.cid,
                                      ))
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                })

              Ok(records)
            }
          }
        }
      }
    }
  }
}

/// Parse header and extract root CIDs
fn parse_header_with_roots(
  bytes: BitArray,
) -> Result(#(List(Cid), Int, BitArray), CarError) {
  use #(header_len, rest) <- result.try(
    varint.decode(bytes) |> result.replace_error(InvalidVarint),
  )

  let varint_size = varint.encoded_size(header_len)
  let header_size = varint_size + header_len

  case rest {
    <<header_bytes:bytes-size(header_len), remaining:bits>> -> {
      case cbor.decode(header_bytes) {
        Ok(header) -> {
          let roots = get_roots_from_header(header)
          Ok(#(roots, header_size, remaining))
        }
        Error(_) -> Error(CborDecodeError)
      }
    }
    _ -> Error(InvalidHeader)
  }
}

/// Extract roots array from CAR header
fn get_roots_from_header(header: Dynamic) -> List(Cid) {
  let decoder = {
    use roots <- decode.field("roots", decode.list(decode.dynamic))
    decode.success(roots)
  }
  case decode.run(header, decoder) {
    Ok(roots) -> list.filter_map(roots, parse_cid_from_dynamic)
    Error(_) -> []
  }
}

/// Parse CID from dynamic (tag 42 tuple)
fn parse_cid_from_dynamic(data: Dynamic) -> Result(Cid, Nil) {
  case decode_cid_bytes(data) {
    Ok(bytes) -> {
      case cid.parse(bytes) {
        Ok(#(parsed, _)) -> Ok(parsed)
        Error(_) -> Error(Nil)
      }
    }
    Error(_) -> Error(Nil)
  }
}

@external(erlang, "cid_ffi", "decode_cid_bytes")
fn decode_cid_bytes(data: Dynamic) -> Result(BitArray, Nil)

/// Get "data" CID from commit block (MST root)
fn get_data_cid(commit_data: BitArray) -> Result(Cid, Nil) {
  case cbor.decode(commit_data) {
    Ok(decoded) -> {
      let decoder = {
        use data <- decode.field("data", decode.dynamic)
        decode.success(data)
      }
      case decode.run(decoded, decoder) {
        Ok(data) -> parse_cid_from_dynamic(data)
        Error(_) -> Error(Nil)
      }
    }
    Error(_) -> Error(Nil)
  }
}

/// Get $type field from decoded record
fn get_type_field(data: Dynamic) -> Result(String, Nil) {
  let decoder = {
    use type_ <- decode.field("$type", decode.string)
    decode.success(type_)
  }
  case decode.run(data, decoder) {
    Ok(type_) -> Ok(type_)
    Error(_) -> Error(Nil)
  }
}

/// Convert a record's data to JSON string
pub fn record_to_json(record: RecordWithPath) -> String {
  dynamic_to_json(record.data)
}

fn dynamic_to_json(value: Dynamic) -> String {
  let sanitized = sanitize_for_json(value)
  let iolist = do_json_encode(sanitized)
  iolist_to_string(iolist)
}

@external(erlang, "cbor_ffi", "sanitize_for_json")
fn sanitize_for_json(value: Dynamic) -> Dynamic

@external(erlang, "json", "encode")
fn do_json_encode(value: Dynamic) -> Dynamic

@external(erlang, "erlang", "iolist_to_binary")
fn iolist_to_binary(iolist: Dynamic) -> Dynamic

fn iolist_to_string(iolist: Dynamic) -> String {
  let binary = iolist_to_binary(iolist)
  case decode.run(binary, decode.string) {
    Ok(str) -> str
    Error(_) -> ""
  }
}

// === CID utilities ===

/// Convert CID to base32lower string representation
pub fn cid_to_string(c: Cid) -> String {
  cid.to_string(c)
}

/// Convert CID to raw bytes
pub fn cid_to_bytes(c: Cid) -> BitArray {
  cid.to_bytes(c)
}
```

**Step 2: Verify it compiles**

Run: `cd atproto_car && gleam build`
Expected: Successful build

**Step 3: Commit**

```bash
git add atproto_car/src/atproto_car.gleam
git commit -m "feat(atproto_car): implement main module with public API"
```

---

## Task 8: Run Tests in New Package

**Files:**
- Modify: `atproto_car/test/atproto_car_test.gleam`

**Step 1: Add basic smoke test**

Replace `atproto_car/test/atproto_car_test.gleam`:

```gleam
import atproto_car
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn parse_empty_returns_error_test() {
  atproto_car.parse_header(<<>>)
  |> should.be_error()
}

pub fn parse_invalid_header_returns_error_test() {
  // Invalid varint followed by garbage
  atproto_car.parse_header(<<0xFF, 0xFF, 0xFF>>)
  |> should.be_error()
}
```

**Step 2: Run tests**

Run: `cd atproto_car && gleam test`
Expected: 2 tests pass

**Step 3: Commit**

```bash
git add atproto_car/test/atproto_car_test.gleam
git commit -m "test(atproto_car): add basic smoke tests"
```

---

## Task 9: Update Server to Use New Package

**Files:**
- Modify: `server/gleam.toml`
- Modify: `server/src/backfill.gleam`

**Step 1: Add path dependency to server/gleam.toml**

Add after line 16 (after lexicon_graphql):
```toml
atproto_car = { path = "../atproto_car" }
```

**Step 2: Update imports in backfill.gleam**

Change lines 1-2:
```gleam
// Before
import car/car
import car/cid

// After
import atproto_car
```

**Step 3: Update function calls in backfill.gleam**

Search and replace throughout the file:
- `car.extract_records_with_paths` → `atproto_car.extract_records_with_paths`
- `car.record_to_json` → `atproto_car.record_to_json`
- `car.RecordWithPath` → `atproto_car.RecordWithPath`
- `cid.to_string` → `atproto_car.cid_to_string`

**Step 4: Verify server compiles**

Run: `cd server && gleam build`
Expected: Successful build (may have warnings about unused old car/ modules)

**Step 5: Commit**

```bash
git add server/gleam.toml server/src/backfill.gleam
git commit -m "refactor(server): use atproto_car package instead of internal car/"
```

---

## Task 10: Delete Old car/ Directory

**Files:**
- Delete: `server/src/car/` (entire directory)

**Step 1: Verify no other files import from car/**

Run: `grep -r "import car/" server/src/ --include="*.gleam" | grep -v backfill`
Expected: No output (only backfill.gleam should have imported it, and we fixed that)

**Step 2: Delete the old directory**

```bash
rm -rf server/src/car/
```

**Step 3: Verify server still compiles**

Run: `cd server && gleam build`
Expected: Successful build

**Step 4: Run server tests**

Run: `cd server && gleam test`
Expected: All 314 tests pass

**Step 5: Commit**

```bash
git add -A
git commit -m "refactor(server): remove old car/ directory (now in atproto_car)"
```

---

## Task 11: Final Verification

**Step 1: Run full test suite from repo root**

```bash
cd server && gleam test
cd ../atproto_car && gleam test
```

Expected: All tests pass in both packages

**Step 2: Verify clean git status**

Run: `git status`
Expected: Clean working tree

**Step 3: Review commits**

Run: `git log --oneline -10`
Expected: Clear progression of commits showing the extraction

---

## Summary

After completing all tasks:

1. `atproto_car/` exists at repo root as standalone Gleam package
2. Server depends on it via path dependency
3. Old `server/src/car/` is deleted
4. All tests pass
5. Package is ready for eventual Hex publication
