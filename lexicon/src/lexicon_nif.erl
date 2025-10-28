-module(lexicon_nif).
-export([validate_schemas/1, validate_record/3, is_valid_nsid/1]).
-nifs([validate_schemas/1, validate_record/3, is_valid_nsid/1]).
-on_load(init/0).

init() ->
    PrivDir = case code:priv_dir(lexicon) of
        {error, _} ->
            % Fallback for development
            case filelib:is_dir(filename:join(["..", priv])) of
                true -> filename:join(["..", priv]);
                _ -> "priv"
            end;
        Dir ->
            Dir
    end,
    SoName = filename:join(PrivDir, "liblexicon_nif"),
    ok = erlang:load_nif(SoName, 0).

validate_schemas(_JsonStrings) ->
    exit(nif_library_not_loaded).

validate_record(_LexiconJsons, _Collection, _RecordJson) ->
    exit(nif_library_not_loaded).

is_valid_nsid(_Nsid) ->
    exit(nif_library_not_loaded).
