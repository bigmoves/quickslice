-module(subscription_counter_ffi).
-export([get_global_counter/0, increment_global/0, decrement_global/0, get_global_count/0]).

-define(COUNTER_KEY, graphql_subscription_counter).

%% Get or create the global subscription counter
%% Uses persistent_term for efficient, shared access across all processes
get_global_counter() ->
    case persistent_term:get(?COUNTER_KEY, undefined) of
        undefined ->
            Counter = counters:new(1, [atomics]),
            persistent_term:put(?COUNTER_KEY, Counter),
            Counter;
        Counter ->
            Counter
    end.

%% Atomically increment the global counter and return the new value
increment_global() ->
    Counter = get_global_counter(),
    counters:add(Counter, 1, 1),
    counters:get(Counter, 1).

%% Atomically decrement the global counter and return the new value
decrement_global() ->
    Counter = get_global_counter(),
    counters:sub(Counter, 1, 1),
    counters:get(Counter, 1).

%% Get the current global counter value
get_global_count() ->
    Counter = get_global_counter(),
    counters:get(Counter, 1).
