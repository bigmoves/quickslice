-module(dpop_validator_ffi).
-export([verify_dpop_proof/4]).

%% Bridge to jose_ffi:verify_dpop_proof with Gleam-compatible return types
verify_dpop_proof(DPoPProof, Method, Url, MaxAgeSeconds) ->
    case jose_ffi:verify_dpop_proof(DPoPProof, Method, Url, MaxAgeSeconds) of
        {ok, #{jkt := Jkt, jti := Jti, iat := Iat}} ->
            %% Return DPoPValidationResult directly (Gleam atom: d_po_p_validation_result)
            {ok, {d_po_p_validation_result, Jkt, Jti, Iat}};
        {error, Reason} when is_binary(Reason) ->
            {error, Reason};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.
