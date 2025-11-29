/// CAR (Content Addressable aRchive) file parsing
///
/// CAR format:
/// - varint(header_length) + CBOR header with {"version": 1, "roots": [CID]}
/// - Sequence of blocks: varint(cid_len + data_len) + CID bytes + data bytes
import car/blockstore.{type BlockStore}
import car/cbor
import car/cid.{type Cid}
import car/varint
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import logging

/// Opaque type for monotonic time
pub type MonotonicTime

/// Get current monotonic time for timing measurements
@external(erlang, "backfill_ffi", "monotonic_now")
fn monotonic_now() -> MonotonicTime

/// Get elapsed milliseconds since a start time
@external(erlang, "backfill_ffi", "elapsed_ms")
fn elapsed_ms(start: MonotonicTime) -> Int

/// A parsed CAR block containing CID and raw data
pub type CarBlock {
  CarBlock(cid: Cid, data: BitArray)
}

/// A record extracted from a CAR file (old style, no path)
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
  // Read header length varint
  use #(header_len, rest) <- result.try(
    varint.decode(bytes) |> result.replace_error(InvalidVarint),
  )

  // Extract header bytes and decode CBOR
  case rest {
    <<header_bytes:bytes-size(header_len), remaining:bits>> -> {
      case cbor.decode(header_bytes) {
        Ok(_header) -> {
          // Extract roots from header (we don't validate them for now)
          // Header is {"version": 1, "roots": [CID, ...]}
          // For simple extraction, we just return empty roots and remaining bytes
          Ok(#([], remaining))
        }
        Error(_) -> Error(CborDecodeError)
      }
    }
    _ -> Error(InvalidHeader)
  }
}

/// Parse a single block from bytes
/// Returns the block and remaining bytes
fn parse_block(bytes: BitArray) -> Result(#(CarBlock, BitArray), CarError) {
  // Read block length varint (includes CID + data)
  use #(block_len, rest) <- result.try(
    varint.decode(bytes) |> result.replace_error(InvalidVarint),
  )

  case rest {
    <<block_bytes:bytes-size(block_len), remaining:bits>> -> {
      // Parse CID from start of block
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
/// Records have a $type field, MST nodes and commits don't
fn decode_as_record(block: CarBlock) -> Result(Record, Nil) {
  case cbor.decode(block.data) {
    Ok(decoded) -> {
      // Try to get $type field - records have it, other blocks don't
      // Using the new decode module API
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
  // Parse header to get past it
  use #(_roots, blocks_bytes) <- result.try(parse_header(car_bytes))

  // Fold over blocks, collecting matching records
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
/// This properly walks the MST to get collection/rkey paths
/// Uses lazy blockstore - indexes CID offsets but only loads data on demand
pub fn extract_records_with_paths(
  car_bytes: BitArray,
  collections: List(String),
) -> Result(List(RecordWithPath), CarError) {
  let total_start = monotonic_now()

  // Phase 1: Parse header and get root CID
  let header_start = monotonic_now()
  use #(roots, header_size, blocks_bytes) <- result.try(parse_header_with_roots(
    car_bytes,
  ))
  let header_ms = elapsed_ms(header_start)

  logging.log(
    logging.Debug,
    "[car] Parsed header, found "
      <> string.inspect(list.length(roots))
      <> " roots",
  )

  // Phase 2: Build lazy blockstore - indexes CID -> offset without loading block data
  let index_start = monotonic_now()
  use store <- result.try(
    blockstore.from_blocks_bytes(car_bytes, header_size, blocks_bytes)
    |> result.map_error(fn(_) { InvalidBlock }),
  )
  let index_ms = elapsed_ms(index_start)
  let block_count = blockstore.size(store)

  logging.log(
    logging.Debug,
    "[car] Built block index with " <> string.inspect(block_count) <> " blocks",
  )

  // Get the commit block (first root)
  case roots {
    [] -> {
      logging.log(logging.Debug, "[car] No roots found in CAR header")
      Ok([])
    }
    [commit_cid, ..] -> {
      // Get commit block and find MST root
      logging.log(logging.Debug, "[car] Looking for commit block")
      case blockstore.get(store, commit_cid) {
        Error(_) -> {
          logging.log(
            logging.Debug,
            "[car] Commit block not found in blockstore",
          )
          Ok([])
        }
        Ok(commit_data) -> {
          logging.log(
            logging.Debug,
            "[car] Found commit block, looking for data CID",
          )
          case get_data_cid(commit_data) {
            Error(_) -> {
              logging.log(
                logging.Debug,
                "[car] Could not get data CID from commit",
              )
              Ok([])
            }
            Ok(mst_root_cid) -> {
              logging.log(logging.Debug, "[car] Found MST root, walking tree")

              // Phase 3: Walk MST to get paths (lazy - loads blocks on demand)
              let mst_start = monotonic_now()
              let mst_entries = walk_mst(store, mst_root_cid)
              let mst_ms = elapsed_ms(mst_start)
              let mst_count = list.length(mst_entries)

              logging.log(
                logging.Debug,
                "[car] MST walk returned "
                  <> string.inspect(mst_count)
                  <> " entries",
              )

              // Phase 4: For each MST entry, get the record data and filter by collection
              let extract_start = monotonic_now()
              let records =
                list.filter_map(mst_entries, fn(entry) {
                  // Split path into collection/rkey
                  case string.split_once(entry.path, "/") {
                    Error(_) -> Error(Nil)
                    Ok(#(collection, rkey)) -> {
                      // Filter by collection
                      case list.contains(collections, collection) {
                        False -> Error(Nil)
                        True -> {
                          // Get record data (lazy load from blockstore)
                          case blockstore.get(store, entry.cid) {
                            Error(_) -> Error(Nil)
                            Ok(record_data) -> {
                              case cbor.decode(record_data) {
                                Error(_) -> Error(Nil)
                                Ok(decoded) -> {
                                  // Get $type
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
              let extract_ms = elapsed_ms(extract_start)
              let total_ms = elapsed_ms(total_start)

              // Log timing breakdown
              logging.log(
                logging.Debug,
                "[car] parse timing: header="
                  <> int.to_string(header_ms)
                  <> "ms index="
                  <> int.to_string(index_ms)
                  <> "ms mst="
                  <> int.to_string(mst_ms)
                  <> "ms extract="
                  <> int.to_string(extract_ms)
                  <> "ms total="
                  <> int.to_string(total_ms)
                  <> "ms blocks="
                  <> int.to_string(block_count)
                  <> " mst_entries="
                  <> int.to_string(mst_count)
                  <> " records="
                  <> int.to_string(list.length(records)),
              )

              Ok(records)
            }
          }
        }
      }
    }
  }
}

/// MST entry - path and CID
type MstEntry {
  MstEntry(path: String, cid: Cid)
}

/// Parse header and extract root CIDs, also returns header size for blockstore
fn parse_header_with_roots(
  bytes: BitArray,
) -> Result(#(List(Cid), Int, BitArray), CarError) {
  use #(header_len, rest) <- result.try(
    varint.decode(bytes) |> result.replace_error(InvalidVarint),
  )

  // Calculate header size: varint size + header content
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
  logging.log(logging.Debug, "[car] Header data: " <> string.inspect(header))
  let decoder = {
    use roots <- decode.field("roots", decode.list(decode.dynamic))
    decode.success(roots)
  }
  case decode.run(header, decoder) {
    Ok(roots) -> {
      logging.log(
        logging.Debug,
        "[car] Found " <> string.inspect(list.length(roots)) <> " root entries",
      )
      list.filter_map(roots, fn(r) {
        logging.log(logging.Debug, "[car] Root entry: " <> string.inspect(r))
        parse_cid_from_dynamic(r)
      })
    }
    Error(e) -> {
      logging.log(
        logging.Debug,
        "[car] Failed to decode roots: " <> string.inspect(e),
      )
      []
    }
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

/// Walk MST from root CID, returning all entries with their paths
/// Uses lazy blockstore - blocks are loaded on-demand during traversal
fn walk_mst(store: BlockStore, root_cid: Cid) -> List(MstEntry) {
  walk_mst_node(store, root_cid, "")
}

/// Walk a single MST node recursively
/// Loads blocks from blockstore on-demand (lazy)
fn walk_mst_node(
  store: BlockStore,
  node_cid: Cid,
  prev_key: String,
) -> List(MstEntry) {
  case blockstore.get(store, node_cid) {
    Error(_) -> []
    Ok(node_data) -> {
      case parse_mst_node(node_data) {
        Error(_) -> []
        Ok(node) -> {
          // Process left subtree
          let left_entries = case node.left {
            Error(_) -> []
            Ok(left_cid) -> walk_mst_node(store, left_cid, prev_key)
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
                Ok(tree_cid) -> walk_mst_node(store, tree_cid, full_key)
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

/// Parse MST node from CBOR data
fn parse_mst_node(data: BitArray) -> Result(MstNode, Nil) {
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

/// Get entries array from MST node
fn get_entries(data: Dynamic) -> List(MstNodeEntry) {
  let decoder = {
    use entries <- decode.field("e", decode.list(decode.dynamic))
    decode.success(entries)
  }
  case decode.run(data, decoder) {
    Ok(entries) -> list.filter_map(entries, parse_mst_entry)
    Error(_) -> []
  }
}

/// Parse a single MST entry
fn parse_mst_entry(data: Dynamic) -> Result(MstNodeEntry, Nil) {
  // Get prefix length
  let p_decoder = {
    use p <- decode.field("p", decode.int)
    decode.success(p)
  }

  case decode.run(data, p_decoder) {
    Error(_) -> Error(Nil)
    Ok(p) -> {
      // Get key suffix
      case get_binary_field(data, "k") {
        Error(_) -> Error(Nil)
        Ok(k) -> {
          // Get value CID
          case get_optional_cid_field(data, "v") {
            Error(_) -> Error(Nil)
            Ok(v) -> {
              // Get optional tree CID
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

/// Convert a CAR record's CBOR data to JSON string
/// Sanitizes CID links (tag 42) to {$link: "base32cid"} format
pub fn record_to_json(record: RecordWithPath) -> String {
  dynamic_to_json(record.data)
}

/// Convert a Dynamic value (Erlang term) to JSON string
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
