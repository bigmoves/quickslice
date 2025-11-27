-module(backfill_ffi).
-export([configure_pool/0, init_semaphore/0, acquire_permit/0, release_permit/0]).

%% Maximum concurrent HTTP requests for backfill
-define(MAX_CONCURRENT, 150).

%% Configure hackney connection pool with higher limits
configure_pool() ->
    %% Suppress SSL handshake error notices (TLS alerts from bad certificates)
    %% These clutter the logs when connecting to self-hosted PDS with bad certs
    %% Set both the ssl application log level and logger level
    application:set_env(ssl, log_level, error),
    logger:set_application_level(ssl, error),

    %% Stop the default pool if it exists (ignore errors)
    _ = hackney_pool:stop_pool(default),

    %% Start pool with increased connection limits and timeouts
    %% timeout: how long to keep connections alive in the pool (ms)
    %% max_connections: maximum number of connections in the pool
    %% recv_timeout: how long to wait for response data (ms)
    Options = [
        {timeout, 150000},
        {max_connections, 300},
        {recv_timeout, 30000}
    ],

    %% Start the pool (this will create it if it doesn't exist)
    case hackney_pool:start_pool(default, Options) of
        ok -> ok;
        {error, {already_started, _}} -> ok;
        _ -> ok
    end,

    %% Initialize the semaphore for rate limiting
    init_semaphore(),

    %% Return nil (atom 'nil' in Gleam)
    nil.

%% Initialize the global semaphore using atomics
%% Uses persistent_term for fast global access
init_semaphore() ->
    case persistent_term:get(backfill_semaphore, undefined) of
        undefined ->
            Ref = atomics:new(1, [{signed, true}]),
            atomics:put(Ref, 1, ?MAX_CONCURRENT),
            persistent_term:put(backfill_semaphore, Ref);
        _ ->
            %% Already initialized
            ok
    end.

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
