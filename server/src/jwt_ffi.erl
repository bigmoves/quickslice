-module(jwt_ffi).
-export([sign_jwt/3, derive_public_did_key/1, extract_public_key_coords/1]).

%% Sign a JWT with ES256 using a multibase-encoded private key
%% Args: ClaimsJson (binary), Kid (binary), PrivateKeyMultibase (binary)
%% Returns: {ok, JWT} | {error, Reason}
sign_jwt(ClaimsJson, Kid, PrivateKeyMultibase) ->
    try
        %% Parse multibase key (z-prefixed base58btc)
        case parse_multibase_key(PrivateKeyMultibase) of
            {ok, PrivateKeyBytes} ->
                %% Generate EC key from private key bytes
                {PubX, PubY} = derive_public_coords(PrivateKeyBytes),

                %% Build JWK for signing
                JWK = jose_jwk:from_map(#{
                    <<"kty">> => <<"EC">>,
                    <<"crv">> => <<"P-256">>,
                    <<"x">> => base64url_encode(PubX),
                    <<"y">> => base64url_encode(PubY),
                    <<"d">> => base64url_encode(PrivateKeyBytes)
                }),

                %% Parse claims
                Claims = json:decode(ClaimsJson),

                %% Create JWT with header
                JWS = jose_jws:from_map(#{
                    <<"alg">> => <<"ES256">>,
                    <<"kid">> => Kid
                }),
                JWT = jose_jwt:from_map(Claims),

                %% Sign
                Signed = jose_jwt:sign(JWK, JWS, JWT),
                {_JWS2, CompactToken} = jose_jws:compact(Signed),

                {ok, CompactToken};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error:Error:Stack ->
            {error, iolist_to_binary([<<"JWT signing error: ">>,
                                       io_lib:format("~p at ~p", [Error, Stack])])}
    end.

%% Derive public did:key from private key multibase
derive_public_did_key(PrivateKeyMultibase) ->
    try
        case parse_multibase_key(PrivateKeyMultibase) of
            {ok, PrivateKeyBytes} ->
                {PubX, PubY} = derive_public_coords(PrivateKeyBytes),
                %% Compressed public key format
                CompressedPub = compress_public_key(PubX, PubY),
                %% Add multicodec prefix for P-256 public key (0x1200)
                Prefixed = <<16#80, 16#24, CompressedPub/binary>>,
                %% Encode as base58btc with 'z' prefix
                Encoded = base58_encode(Prefixed),
                {ok, <<"did:key:z", Encoded/binary>>};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:_ -> {error, <<"Failed to derive public key">>}
    end.

%% Parse multibase key (z-prefixed base58btc)
parse_multibase_key(<<"z", Rest/binary>>) ->
    try
        Decoded = base58_decode(Rest),
        %% Skip multicodec prefix (2 bytes for P-256 private key)
        <<_Prefix:2/binary, PrivateKey/binary>> = Decoded,
        {ok, PrivateKey}
    catch
        _:_ -> {error, <<"Invalid multibase key format">>}
    end;
parse_multibase_key(_) ->
    {error, <<"Unsupported multibase prefix">>}.

%% Derive public key coordinates from private key
derive_public_coords(PrivateKeyBytes) ->
    %% Use crypto to compute public key from private key
    %% crypto:generate_key returns {PublicKey, PrivateKey}
    {<<4, XY/binary>>, _Priv} = crypto:generate_key(ecdh, secp256r1, PrivateKeyBytes),
    <<X:32/binary, Y:32/binary>> = XY,
    {X, Y}.

%% Compress public key (02/03 prefix based on Y parity)
compress_public_key(X, Y) ->
    <<YLast>> = binary:part(Y, 31, 1),
    Prefix = case YLast band 1 of
        0 -> <<2>>;
        1 -> <<3>>
    end,
    <<Prefix/binary, X/binary>>.

%% Base58 Bitcoin alphabet
-define(BASE58_ALPHABET, <<"123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz">>).

base58_encode(Bin) ->
    base58_encode(binary:decode_unsigned(Bin), <<>>).

base58_encode(0, Acc) -> Acc;
base58_encode(N, Acc) ->
    Rem = N rem 58,
    Char = binary:at(?BASE58_ALPHABET, Rem),
    base58_encode(N div 58, <<Char, Acc/binary>>).

base58_decode(Bin) ->
    base58_decode(Bin, 0).

base58_decode(<<>>, Acc) -> binary:encode_unsigned(Acc);
base58_decode(<<C, Rest/binary>>, Acc) ->
    case binary:match(?BASE58_ALPHABET, <<C>>) of
        {Pos, 1} -> base58_decode(Rest, Acc * 58 + Pos);
        nomatch -> error(invalid_base58)
    end.

%% Base64 URL-safe encoding (no padding)
base64url_encode(Bin) ->
    Base64 = base64:encode(Bin),
    NoPlus = binary:replace(Base64, <<"+">>, <<"-">>, [global]),
    NoSlash = binary:replace(NoPlus, <<"/">>, <<"_">>, [global]),
    binary:replace(NoSlash, <<"=">>, <<"">>, [global]).

%% Extract public key coordinates from private key multibase
%% Takes a private key in multibase format (z42t...)
%% Returns {ok, {XCoord, YCoord}} where coordinates are base64url-encoded binaries
extract_public_key_coords(PrivateKeyMultibase) ->
    try
        case parse_multibase_key(PrivateKeyMultibase) of
            {ok, PrivateKeyBytes} ->
                {PubX, PubY} = derive_public_coords(PrivateKeyBytes),
                X = base64url_encode(PubX),
                Y = base64url_encode(PubY),
                {ok, {X, Y}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:_ -> {error, <<"Failed to extract public key coordinates">>}
    end.
