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

import atproto_car/internal/blockstore
import atproto_car/internal/cbor
import atproto_car/internal/cid
import atproto_car/internal/mst
import atproto_car/internal/varint
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/list
import gleam/result
import gleam/string

// Re-export Cid type for public API
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
  use #(roots, header_size, blocks_bytes) <- result.try(parse_header_with_roots(
    car_bytes,
  ))

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
