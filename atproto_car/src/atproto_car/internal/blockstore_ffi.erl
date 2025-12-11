-module(blockstore_ffi).
-export([ets_new/0, ets_insert/3, ets_get/2]).

%% Create a new ETS table for blockstore
%% Returns an opaque reference to the table
ets_new() ->
    ets:new(blockstore, [set, public, {read_concurrency, true}]).

%% Insert a key-value pair into ETS table
%% Key is binary (CID bytes), Value is integer (offset)
ets_insert(Table, Key, Value) ->
    ets:insert(Table, {Key, Value}),
    nil.

%% Get a value from ETS table by key
%% Returns {ok, Value} or {error, nil}
ets_get(Table, Key) ->
    case ets:lookup(Table, Key) of
        [{_, Value}] -> {ok, Value};
        [] -> {error, nil}
    end.
