-module(backfill_ffi).
-export([configure_pool/1, init_semaphore/1, acquire_permit/0, release_permit/0, rescue/1, monotonic_now/0, elapsed_ms/1]).

%% Configure hackney connection pool with specified limits
configure_pool(MaxConcurrent) ->
    %% Suppress SSL handshake error notices (TLS alerts from bad certificates)
    application:set_env(ssl, log_level, error),
    logger:set_application_level(ssl, error),

    %% Stop the default pool if it exists (ignore errors)
    _ = hackney_pool:stop_pool(default),

    %% Start pool with configured connection limits
    Options = [
        {timeout, 150000},
        {max_connections, MaxConcurrent * 2},
        {recv_timeout, 30000}
    ],

    case hackney_pool:start_pool(default, Options) of
        ok -> ok;
        {error, {already_started, _}} -> ok;
        _ -> ok
    end,

    %% Initialize the semaphore for rate limiting
    init_semaphore(MaxConcurrent),

    nil.

%% Initialize the global semaphore using atomics
init_semaphore(MaxConcurrent) ->
    %% Always recreate to pick up new limit
    Ref = atomics:new(1, [{signed, true}]),
    atomics:put(Ref, 1, MaxConcurrent),
    persistent_term:put(backfill_semaphore, Ref),
    ok.

%% Acquire a permit from the semaphore
%% Blocks (with sleep) if no permits available
acquire_permit() ->
    Ref = persistent_term:get(backfill_semaphore),
    acquire_loop(Ref).

acquire_loop(Ref) ->
    case atomics:sub_get(Ref, 1, 1) of
        N when N >= 0 ->
            %% Got a permit
            nil;
        _ ->
            %% No permit available, restore and wait
            atomics:add(Ref, 1, 1),
            timer:sleep(10),
            acquire_loop(Ref)
    end.

%% Release a permit back to the semaphore
release_permit() ->
    Ref = persistent_term:get(backfill_semaphore),
    atomics:add(Ref, 1, 1),
    nil.

%% Rescue wrapper - catches exceptions and returns Result
rescue(Fun) ->
    try
        Result = Fun(),
        {ok, Result}
    catch
        Class:Reason:Stacktrace ->
            {error, {Class, Reason, Stacktrace}}
    end.

%% Get monotonic time in native units for timing measurements
monotonic_now() ->
    erlang:monotonic_time().

%% Calculate elapsed milliseconds from a start time
elapsed_ms(Start) ->
    End = erlang:monotonic_time(),
    erlang:convert_time_unit(End - Start, native, millisecond).
