-module(object_builder_ffi).
-export([identity/1]).

%% Identity function - returns value unchanged
%% In Erlang, everything is already "dynamic", so this just passes through
identity(Value) -> Value.
