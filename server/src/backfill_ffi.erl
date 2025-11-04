-module(backfill_ffi).
-export([configure_pool/0]).

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
        {max_connections, 200},
        {recv_timeout, 30000}
    ],

    %% Start the pool (this will create it if it doesn't exist)
    case hackney_pool:start_pool(default, Options) of
        ok -> ok;
        {error, {already_started, _}} -> ok;
        _ -> ok
    end,

    %% Return nil (atom 'nil' in Gleam)
    nil.
