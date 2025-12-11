-module(cid_ffi).
-export([decode_cid_bytes/1, decode_binary/1, encode_cid/4, encode_cid_from_cbor_tag/1]).

%% Decode CID bytes from a CBOR tag 42 tuple {42, Binary}
%% CBOR CID links have a leading 0x00 identity multicodec prefix that must be stripped
%% Returns {ok, CidBytes} or {error, nil}
decode_cid_bytes({42, <<0, CidBytes/binary>>}) when is_binary(CidBytes) ->
    {ok, CidBytes};
decode_cid_bytes({42, Binary}) when is_binary(Binary) ->
    %% Fallback if no identity prefix (shouldn't happen in practice)
    {ok, Binary};
decode_cid_bytes(_) ->
    {error, nil}.

%% Decode a binary from dynamic
%% Returns {ok, Binary} or {error, nil}
decode_binary(Binary) when is_binary(Binary) ->
    {ok, Binary};
decode_binary(_) ->
    {error, nil}.

%% Encode a CID to its base32lower string representation
%% Version, Codec, HashType are integers; Digest is binary
%% Returns a binary string like <<"bafyreig...">>
encode_cid(Version, Codec, HashType, Digest) ->
    %% Build the raw CID bytes: varint(version) + varint(codec) + varint(hashtype) + varint(len) + digest
    DigestLen = byte_size(Digest),
    CidBytes = <<(encode_varint(Version))/binary,
                 (encode_varint(Codec))/binary,
                 (encode_varint(HashType))/binary,
                 (encode_varint(DigestLen))/binary,
                 Digest/binary>>,
    %% Base32 encode with lowercase, no padding, and 'b' prefix
    Base32 = base32:encode(CidBytes, [lower, nopad]),
    <<"b", Base32/binary>>.

%% Encode an unsigned integer as a varint (LEB128)
encode_varint(N) when N < 128 ->
    <<N>>;
encode_varint(N) ->
    <<1:1, (N band 16#7F):7, (encode_varint(N bsr 7))/binary>>.

%% Encode CID bytes from a CBOR tag 42 to base32lower string
%% Strips the leading 0x00 identity multicodec prefix per DAG-CBOR spec
%% Returns a binary string like <<"bafyreig...">>
encode_cid_from_cbor_tag(<<0, CidBytes/binary>>) ->
    Base32 = base32:encode(CidBytes, [lower, nopad]),
    <<"b", Base32/binary>>;
encode_cid_from_cbor_tag(CidBytes) when is_binary(CidBytes) ->
    %% Fallback if no identity prefix (shouldn't happen in practice)
    Base32 = base32:encode(CidBytes, [lower, nopad]),
    <<"b", Base32/binary>>.
