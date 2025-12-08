-module(jose_ffi).
-export([generate_dpop_proof/5, sha256_hash/1, compute_jwk_thumbprint/1, sha256_base64url/1, verify_dpop_proof/4]).

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

        %% Create the DPoP header - include the public JWK
        {_, PublicJWK} = jose_jwk:to_public_map(JWK),

        %% Create the base DPoP payload (without ath)
        BasePayload = #{
            <<"jti">> => Jti,
            <<"htm">> => Method,
            <<"htu">> => URL,
            <<"iat">> => Now
        },

        %% Add ath only if access token is provided (not for token exchange)
        Payload1 = case AccessToken of
            <<>> -> BasePayload;
            _ -> maps:put(<<"ath">>, sha256_base64url(AccessToken), BasePayload)
        end,

        %% Add nonce field if ServerNonce is not empty
        Payload = case ServerNonce of
            <<>> -> Payload1;
            _ -> maps:put(<<"nonce">>, ServerNonce, Payload1)
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

%% Compute JWK thumbprint (SHA-256 hash of the canonical JWK)
compute_jwk_thumbprint(JWKJson) when is_binary(JWKJson) ->
    try
        case catch json:decode(JWKJson) of
            {'EXIT', Reason} ->
                {error, iolist_to_binary([<<"Invalid JWK JSON: ">>,
                                           io_lib:format("~p", [Reason])])};
            JWKMap when is_map(JWKMap) ->
                JWK = jose_jwk:from_map(JWKMap),
                {_Module, Thumbprint} = jose_jwk:thumbprint(JWK),
                {ok, Thumbprint};
            Other ->
                {error, iolist_to_binary([<<"JWK decode returned unexpected type: ">>,
                                           io_lib:format("~p", [Other])])}
        end
    catch
        error:ErrorReason:ErrorStack ->
            {error, iolist_to_binary([<<"JWK thumbprint error: ">>,
                                       io_lib:format("~p at ~p", [ErrorReason, ErrorStack])])};
        _:OtherError ->
            {error, iolist_to_binary([<<"Unexpected error: ">>,
                                       io_lib:format("~p", [OtherError])])}
    end;
compute_jwk_thumbprint(JWKJson) when is_list(JWKJson) ->
    compute_jwk_thumbprint(list_to_binary(JWKJson)).

%% Verify a DPoP proof JWT
%% Args: DPoPProof (binary), ExpectedMethod (binary), ExpectedUrl (binary), MaxAgeSeconds (integer)
%% Returns: {ok, #{jkt => Thumbprint, jti => Jti, iat => Iat}} | {error, Reason}
verify_dpop_proof(DPoPProof, ExpectedMethod, ExpectedUrl, MaxAgeSeconds) ->
    try
        %% Split JWT into parts (compact serialization: header.payload.signature)
        case binary:split(DPoPProof, <<".">>, [global]) of
            [HeaderB64, _PayloadB64, _SignatureB64] ->
                %% Decode the header (base64url)
                HeaderJson = base64:decode(HeaderB64, #{mode => urlsafe, padding => false}),
                HeaderMap = json:decode(HeaderJson),

                %% Extract the JWK from the header
                case maps:get(<<"jwk">>, HeaderMap, undefined) of
                    undefined ->
                        {error, <<"Missing jwk in DPoP header">>};
                    JWKMap ->
                        %% Verify typ is dpop+jwt
                        case maps:get(<<"typ">>, HeaderMap, undefined) of
                            <<"dpop+jwt">> ->
                                %% Reconstruct JWK for verification
                                %% jose_jwk:from_map expects base64url encoding natively
                                JWK = jose_jwk:from_map(JWKMap),

                                %% Verify the signature using jose
                                case jose_jwt:verify(JWK, DPoPProof) of
                                    {true, JWT, _JWS} ->
                                        Claims = jose_jwt:to_map(JWT),
                                        validate_dpop_claims(Claims, JWK, ExpectedMethod, ExpectedUrl, MaxAgeSeconds);
                                    {false, _, _} ->
                                        {error, <<"Invalid DPoP signature">>}
                                end;
                            Other ->
                                {error, iolist_to_binary([<<"Invalid typ: expected dpop+jwt, got ">>,
                                                           io_lib:format("~p", [Other])])}
                        end
                end;
            _ ->
                {error, <<"Invalid JWT format">>}
        end
    catch
        error:Reason:Stacktrace ->
            io:format("[DPoP] Error: ~p~nStacktrace: ~p~n", [Reason, Stacktrace]),
            {error, iolist_to_binary([<<"DPoP verification failed: ">>,
                                       io_lib:format("~p", [Reason])])};
        _:Error ->
            {error, iolist_to_binary([<<"DPoP verification error: ">>,
                                       io_lib:format("~p", [Error])])}
    end.

%% Internal: Validate DPoP claims
validate_dpop_claims({_Kind, Claims}, JWK, ExpectedMethod, ExpectedUrl, MaxAgeSeconds) ->
    Now = erlang:system_time(second),

    %% Extract required claims
    Htm = maps:get(<<"htm">>, Claims, undefined),
    Htu = maps:get(<<"htu">>, Claims, undefined),
    Jti = maps:get(<<"jti">>, Claims, undefined),
    Iat = maps:get(<<"iat">>, Claims, undefined),

    %% Validate all required claims exist
    case {Htm, Htu, Jti, Iat} of
        {undefined, _, _, _} -> {error, <<"Missing htm claim">>};
        {_, undefined, _, _} -> {error, <<"Missing htu claim">>};
        {_, _, undefined, _} -> {error, <<"Missing jti claim">>};
        {_, _, _, undefined} -> {error, <<"Missing iat claim">>};
        _ ->
            %% Validate htm matches
            case Htm =:= ExpectedMethod of
                false ->
                    {error, iolist_to_binary([<<"htm mismatch: expected ">>, ExpectedMethod,
                                               <<", got ">>, Htm])};
                true ->
                    %% Validate htu matches (normalize URLs)
                    case normalize_url(Htu) =:= normalize_url(ExpectedUrl) of
                        false ->
                            {error, iolist_to_binary([<<"htu mismatch: expected ">>, ExpectedUrl,
                                                       <<", got ">>, Htu])};
                        true ->
                            %% Validate iat is within acceptable range
                            case abs(Now - Iat) =< MaxAgeSeconds of
                                false ->
                                    {error, <<"iat outside acceptable time window">>};
                                true ->
                                    %% Compute JKT (SHA-256 thumbprint of the JWK)
                                    Thumbprint = jose_jwk:thumbprint(JWK),
                                    {ok, #{
                                        jkt => Thumbprint,
                                        jti => Jti,
                                        iat => Iat
                                    }}
                            end
                    end
            end
    end.

%% Internal: Normalize URL for comparison (remove trailing slash, fragments)
normalize_url(Url) when is_binary(Url) ->
    %% Remove fragment
    case binary:split(Url, <<"#">>) of
        [Base | _] ->
            %% Remove trailing slash
            case byte_size(Base) > 0 andalso binary:last(Base) of
                $/ -> binary:part(Base, 0, byte_size(Base) - 1);
                _ -> Base
            end;
        _ -> Url
    end.
