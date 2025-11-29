/// CID (Content Identifier) parsing for CAR files
///
/// CID structure:
/// - version (varint): CIDv0 or CIDv1
/// - codec (varint): Content codec (0x71 = dag-cbor for AT Protocol)
/// - multihash: hash_type (varint) + hash_len (varint) + digest (bytes)
import car/varint
import gleam/bit_array
import gleam/result

/// A parsed Content Identifier
pub type Cid {
  Cid(version: Int, codec: Int, hash_type: Int, digest: BitArray)
}

/// CID parsing errors
pub type CidError {
  InvalidVarint
  InvalidCid
  TruncatedDigest
}

/// Parse a CID from the start of a BitArray
/// Returns the CID and remaining bytes
pub fn parse(bytes: BitArray) -> Result(#(Cid, BitArray), CidError) {
  use #(version, rest) <- result.try(
    varint.decode(bytes) |> result.replace_error(InvalidVarint),
  )
  use #(codec, rest) <- result.try(
    varint.decode(rest) |> result.replace_error(InvalidVarint),
  )
  use #(hash_type, rest) <- result.try(
    varint.decode(rest) |> result.replace_error(InvalidVarint),
  )
  use #(hash_len, rest) <- result.try(
    varint.decode(rest) |> result.replace_error(InvalidVarint),
  )

  case rest {
    <<digest:bytes-size(hash_len), remaining:bits>> ->
      Ok(#(Cid(version, codec, hash_type, digest), remaining))
    _ -> Error(TruncatedDigest)
  }
}

/// Calculate the byte size of a CID
/// Needed to know where CID ends and block data begins
pub fn byte_size(cid: Cid) -> Int {
  varint.encoded_size(cid.version)
  + varint.encoded_size(cid.codec)
  + varint.encoded_size(cid.hash_type)
  + varint.encoded_size(bit_array.byte_size(cid.digest))
  + bit_array.byte_size(cid.digest)
}

/// Convert CID to its raw bytes
pub fn to_bytes(cid: Cid) -> BitArray {
  let version_bytes = varint.encode(cid.version)
  let codec_bytes = varint.encode(cid.codec)
  let hash_type_bytes = varint.encode(cid.hash_type)
  let hash_len_bytes = varint.encode(bit_array.byte_size(cid.digest))
  <<
    version_bytes:bits,
    codec_bytes:bits,
    hash_type_bytes:bits,
    hash_len_bytes:bits,
    cid.digest:bits,
  >>
}

/// Encode CID to base32lower string (e.g., "bafyreig...")
/// Uses CIDv1 encoding with multibase 'b' prefix
pub fn to_string(c: Cid) -> String {
  encode_cid(c.version, c.codec, c.hash_type, c.digest)
}

@external(erlang, "cid_ffi", "encode_cid")
fn encode_cid(
  version: Int,
  codec: Int,
  hash_type: Int,
  digest: BitArray,
) -> String
