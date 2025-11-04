-module(test_helpers_ffi).
-export([to_dynamic/1]).

%% Convert any value to Dynamic (which is just the identity function in Erlang)
%% since all Erlang values are already "dynamic"
to_dynamic(Value) -> Value.
