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
