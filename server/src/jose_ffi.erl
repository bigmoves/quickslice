-module(jose_ffi).
-export([generate_dpop_proof/5, sha256_hash/1]).

%% Generate a DPoP proof JWT token
%% Args: Method (binary), URL (binary), AccessToken (binary), JWKJson (binary), ServerNonce (binary)
%% Returns: {ok, DPoPToken} | {error, Reason}
generate_dpop_proof(Method, URL, AccessToken, JWKJson, ServerNonce) ->
    try
        %% Decode JSON - OTP 27+ has json module built-in
        JWKMap = json:decode(JWKJson),
        JWK = jose_jwk:from_map(JWKMap),

        %% Get current timestamp
        Now = erlang:system_time(second),

        %% Generate a unique jti (different from the server nonce)
        Jti = base64:encode(crypto:strong_rand_bytes(16)),

        %% Hash the access token for "ath" claim (base64url of SHA-256)
        TokenHash = sha256_base64url(AccessToken),

        %% Create the DPoP header - include the public JWK
        {_, PublicJWK} = jose_jwk:to_public_map(JWK),

        %% Create the base DPoP payload
        BasePayload = #{
            <<"jti">> => Jti,
            <<"htm">> => Method,
            <<"htu">> => URL,
            <<"iat">> => Now,
            <<"ath">> => TokenHash
        },

        %% Add nonce field if ServerNonce is not empty
        Payload = case ServerNonce of
            <<>> -> BasePayload;
            _ -> maps:put(<<"nonce">>, ServerNonce, BasePayload)
        end,

        %% Sign the JWT using jose compact API
        Alg = detect_algorithm(JWK),

        %% Create JWS header with custom fields
        JWSHeader = #{
            <<"alg">> => Alg,
            <<"typ">> => <<"dpop+jwt">>,
            <<"jwk">> => PublicJWK
        },

        %% Create JWT and JWS structs
        JWT = jose_jwt:from_map(Payload),
        JWS = jose_jws:from_map(JWSHeader),

        %% Sign the JWT
        Signed = jose_jwt:sign(JWK, JWS, JWT),

        %% Compact to get the token string
        {_JWS, CompactToken} = jose_jws:compact(Signed),

        {ok, CompactToken}
    catch
        error:Reason ->
            {error, {dpop_generation_failed, Reason}};
        _:Error ->
            {error, {dpop_generation_failed, Error}}
    end.

%% Hash a string using SHA-256 and return base64 encoded result
sha256_hash(Data) when is_binary(Data) ->
    Hash = crypto:hash(sha256, Data),
    base64:encode(Hash);
sha256_hash(Data) when is_list(Data) ->
    sha256_hash(list_to_binary(Data)).

%% Internal: Hash and base64url encode for "ath" claim
sha256_base64url(Data) ->
    Hash = crypto:hash(sha256, Data),
    base64url_encode(Hash).

%% Internal: Base64 URL-safe encoding (no padding)
base64url_encode(Bin) ->
    %% Standard base64 encode, then make URL-safe
    Base64 = base64:encode(Bin),
    %% Replace + with -, / with _, and remove padding =
    NoPlus = binary:replace(Base64, <<"+">>, <<"-">>, [global]),
    NoSlash = binary:replace(NoPlus, <<"/">>, <<"_">>, [global]),
    binary:replace(NoSlash, <<"=">>, <<"">>, [global]).

%% Internal: Detect algorithm from JWK
detect_algorithm(JWK) ->
    case jose_jwk:to_map(JWK) of
        {_Kind, #{<<"kty">> := <<"EC">>}} ->
            <<"ES256">>;
        {_Kind, #{<<"kty">> := <<"RSA">>}} ->
            <<"RS256">>;
        {_Kind, #{<<"kty">> := <<"OKP">>}} ->
            <<"EdDSA">>;
        _ ->
            <<"ES256">>  %% Default to ES256
    end.

