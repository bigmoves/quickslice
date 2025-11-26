-module(dpop_keygen_ffi).
-export([secp256r1_params/0, ec_key_to_jwk_json/1]).

%% Return secp256r1 (P-256) curve parameters for key generation
secp256r1_params() ->
    {namedCurve, secp256r1}.

%% Convert EC private key to JWK JSON string (includes private key 'd' component)
ec_key_to_jwk_json(ECPrivateKey) ->
    %% Extract the key components from the ECPrivateKey record
    %% ECPrivateKey = {ECPrivateKey, Version, PrivateKey, Parameters, PublicKey, Attributes}
    {_, _, PrivateKeyBin, _, PublicKeyBin, _} = ECPrivateKey,

    %% PublicKey is {0, 4, X, Y} format (uncompressed point)
    %% First byte is 0x04 indicating uncompressed format
    <<4, XY/binary>> = PublicKeyBin,
    KeySize = byte_size(XY) div 2,
    <<X:KeySize/binary, Y:KeySize/binary>> = XY,

    %% Pad private key to 32 bytes if needed
    PaddedD = pad_to_32(PrivateKeyBin),

    %% Build JWK map
    JWK = #{
        <<"kty">> => <<"EC">>,
        <<"crv">> => <<"P-256">>,
        <<"x">> => base64url_encode(X),
        <<"y">> => base64url_encode(Y),
        <<"d">> => base64url_encode(PaddedD)
    },

    %% Convert to JSON string
    json:encode(JWK).

%% Pad binary to 32 bytes (P-256 key size)
pad_to_32(Bin) when byte_size(Bin) >= 32 -> Bin;
pad_to_32(Bin) ->
    PadSize = 32 - byte_size(Bin),
    <<0:(PadSize * 8), Bin/binary>>.

%% Base64 URL-safe encoding (no padding)
base64url_encode(Bin) ->
    Base64 = base64:encode(Bin),
    NoPlus = binary:replace(Base64, <<"+">>, <<"-">>, [global]),
    NoSlash = binary:replace(NoPlus, <<"/">>, <<"_">>, [global]),
    binary:replace(NoSlash, <<"=">>, <<"">>, [global]).
