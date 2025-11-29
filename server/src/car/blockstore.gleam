/// Lazy blockstore for CAR files
///
/// Instead of loading all block data into memory, this stores CID -> byte offset
/// and reads block data on-demand when requested.
///
/// Uses ETS for O(1) lookups and binary CID keys (no hex conversion).
import car/cid.{type Cid}
import car/varint
import gleam/result

/// Opaque type for ETS table reference
pub type EtsTable

/// Lazy blockstore that indexes offsets but doesn't load data until requested
/// Uses ETS with binary keys for O(1) lookups
pub opaque type BlockStore {
  BlockStore(bytes: BitArray, table: EtsTable, block_count: Int)
}

/// Blockstore errors
pub type BlockStoreError {
  InvalidVarint
  InvalidBlock
  CidParseError
}

/// ETS FFI functions
@external(erlang, "car_ffi", "ets_new")
fn ets_new() -> EtsTable

@external(erlang, "car_ffi", "ets_insert")
fn ets_insert(table: EtsTable, key: BitArray, value: Int) -> Nil

@external(erlang, "car_ffi", "ets_get")
fn ets_get(table: EtsTable, key: BitArray) -> Result(Int, Nil)

/// Build a blockstore from CAR bytes (after header has been parsed)
/// Takes the full CAR bytes and the offset where blocks start
pub fn from_blocks_bytes(
  full_car_bytes: BitArray,
  header_size: Int,
  blocks_bytes: BitArray,
) -> Result(BlockStore, BlockStoreError) {
  let table = ets_new()
  use block_count <- result.try(index_blocks(
    table,
    blocks_bytes,
    header_size,
    0,
  ))
  Ok(BlockStore(bytes: full_car_bytes, table: table, block_count: block_count))
}

/// Get block data by CID bytes key - only reads data when called
pub fn get_by_bytes(
  store: BlockStore,
  cid_bytes: BitArray,
) -> Result(BitArray, Nil) {
  case ets_get(store.table, cid_bytes) {
    Error(_) -> Error(Nil)
    Ok(offset) -> read_block_at_offset(store.bytes, offset)
  }
}

/// Get block data by CID - only reads data when called
pub fn get(store: BlockStore, c: Cid) -> Result(BitArray, Nil) {
  get_by_bytes(store, cid.to_bytes(c))
}

/// Get the number of indexed blocks
pub fn size(store: BlockStore) -> Int {
  store.block_count
}

/// Index blocks: iterate through, recording CID -> offset without storing data
/// Uses ETS for O(1) inserts
fn index_blocks(
  table: EtsTable,
  bytes: BitArray,
  current_offset: Int,
  count: Int,
) -> Result(Int, BlockStoreError) {
  case bytes {
    <<>> -> Ok(count)
    _ -> {
      use #(cid_bytes, block_total_len, rest) <- result.try(
        parse_block_index_entry(bytes),
      )
      ets_insert(table, cid_bytes, current_offset)
      index_blocks(table, rest, current_offset + block_total_len, count + 1)
    }
  }
}

/// Parse just enough of a block to get CID bytes and total length (skip data)
fn parse_block_index_entry(
  bytes: BitArray,
) -> Result(#(BitArray, Int, BitArray), BlockStoreError) {
  // Read block length varint
  use #(block_len, rest) <- result.try(
    varint.decode(bytes) |> result.replace_error(InvalidVarint),
  )

  // Calculate varint size for offset tracking
  let varint_size = varint.encoded_size(block_len)

  case rest {
    <<block_bytes:bytes-size(block_len), remaining:bits>> -> {
      // Parse CID from start of block - get raw bytes for ETS key
      case cid.parse(block_bytes) {
        Ok(#(parsed_cid, _data)) -> {
          let cid_bytes = cid.to_bytes(parsed_cid)
          let total_len = varint_size + block_len
          Ok(#(cid_bytes, total_len, remaining))
        }
        Error(_) -> Error(CidParseError)
      }
    }
    _ -> Error(InvalidBlock)
  }
}

/// Read block data at a specific offset
fn read_block_at_offset(bytes: BitArray, offset: Int) -> Result(BitArray, Nil) {
  case bytes {
    <<_skip:bytes-size(offset), rest:bits>> -> {
      // Parse varint length
      case varint.decode(rest) {
        Error(_) -> Error(Nil)
        Ok(#(block_len, after_varint)) -> {
          case after_varint {
            <<block_bytes:bytes-size(block_len), _remaining:bits>> -> {
              // Parse CID to get past it to the data
              case cid.parse(block_bytes) {
                Ok(#(_cid, data)) -> Ok(data)
                Error(_) -> Error(Nil)
              }
            }
            _ -> Error(Nil)
          }
        }
      }
    }
    _ -> Error(Nil)
  }
}
