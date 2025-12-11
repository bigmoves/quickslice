-module(cbor_ffi).
-export([decode/1, sanitize_for_json/1]).

%% Decode CBOR binary data using erl_cbor
%% Returns {ok, Term} or {error, Reason}
decode(Binary) ->
    case erl_cbor:decode(Binary) of
        {ok, Term, _Rest} -> {ok, Term};
        {error, Reason} -> {error, Reason}
    end.

%% Sanitize CBOR-decoded data for JSON encoding
%% Converts CBOR tag 42 (CID links) to base32-encoded $link objects
%% Recursively walks the structure to handle nested CIDs
sanitize_for_json(Term) ->
    sanitize_term(Term).

sanitize_term({42, CidBytes}) when is_binary(CidBytes) ->
    %% CBOR tag 42 is a CID link - convert to $link object for JSON
    %% Use cid_ffi to properly strip 0x00 prefix and encode
    CidString = cid_ffi:encode_cid_from_cbor_tag(CidBytes),
    #{<<"$link">> => CidString};

sanitize_term(Map) when is_map(Map) ->
    maps:map(fun(_K, V) -> sanitize_term(V) end, Map);

sanitize_term(List) when is_list(List) ->
    [sanitize_term(Item) || Item <- List];

sanitize_term({Tag, Value}) when is_integer(Tag) ->
    %% Other CBOR tags - just return the value
    sanitize_term(Value);

sanitize_term(Tuple) when is_tuple(Tuple) ->
    %% Convert tuples to lists for JSON
    list_to_tuple([sanitize_term(E) || E <- tuple_to_list(Tuple)]);

sanitize_term(Bin) when is_binary(Bin) ->
    %% Check if binary is valid UTF-8 (text string) or raw bytes
    %% ATProto spec: bytes must be encoded as {"$bytes": "<base64>"}
    case unicode:characters_to_binary(Bin) of
        Bin -> Bin;  % Valid UTF-8, pass through as string
        _ -> #{<<"$bytes">> => base64:encode(Bin)}  % Invalid UTF-8, encode as $bytes
    end;

sanitize_term(Other) ->
    %% Atoms, numbers, etc - pass through
    Other.
