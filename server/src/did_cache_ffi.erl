-module(did_cache_ffi).
-export([new_table/0, insert/4, lookup/2, delete/2, cleanup_expired/1, stats/1]).

%% Create a new ETS table for DID document caching
new_table() ->
    ets:new(did_cache, [set, public, {read_concurrency, true}]).

%% Insert a DID document with expiration timestamp
%% Args: Table, DID (binary), Document (binary), ExpiresAt (integer, unix timestamp)
insert(Table, DID, Document, ExpiresAt) ->
    true = ets:insert(Table, {DID, Document, ExpiresAt}),
    ok.

%% Lookup a DID document, checking expiration
%% Returns: {ok, Document} | {error, not_found} | {error, expired}
lookup(Table, DID) ->
    case ets:lookup(Table, DID) of
        [] ->
            {error, not_found};
        [{_DID, Document, ExpiresAt}] ->
            Now = erlang:system_time(second),
            case Now < ExpiresAt of
                true -> {ok, Document};
                false ->
                    ets:delete(Table, DID),
                    {error, expired}
            end
    end.

%% Delete a DID from the cache
delete(Table, DID) ->
    ets:delete(Table, DID),
    ok.

%% Remove all expired entries from the cache
cleanup_expired(Table) ->
    Now = erlang:system_time(second),
    %% Select and delete expired entries
    ets:select_delete(Table, [{{{'$1', '$2', '$3'}, [], [{'<', '$3', Now}]}}]),
    ok.

%% Get cache statistics
%% Returns: {EntryCount, MemoryBytes}
stats(Table) ->
    Info = ets:info(Table),
    Size = proplists:get_value(size, Info, 0),
    Memory = proplists:get_value(memory, Info, 0) * erlang:system_info(wordsize),
    {Size, Memory}.
