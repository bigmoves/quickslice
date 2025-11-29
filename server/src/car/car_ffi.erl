-module(car_ffi).
-export([bytes_to_hex_lower/1, ets_new/0, ets_insert/3, ets_get/2, ets_size/1]).

%% Convert binary to lowercase hex string using OTP 24+ binary:encode_hex
%% This is O(n) vs O(n²) for string concatenation in a loop
bytes_to_hex_lower(Bytes) ->
    binary:encode_hex(Bytes, lowercase).

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

%% Get the number of entries in the ETS table
ets_size(Table) ->
    ets:info(Table, size).
