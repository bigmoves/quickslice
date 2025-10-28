-module(jetstream_ws_ffi).
-export([connect/2]).

%% Connect to WebSocket using gun
connect(Url, HandlerPid) ->
    %% Start gun application and dependencies
    application:ensure_all_started(ssl),
    application:ensure_all_started(gun),

    %% Parse URL using uri_string
    UriMap = uri_string:parse(Url),
    #{scheme := SchemeStr, host := Host, path := Path} = UriMap,

    %% Get query string if present and append to path
    Query = maps:get(query, UriMap, undefined),
    PathWithQuery = case Query of
        undefined -> Path;
        <<>> -> Path;
        Q -> <<Path/binary, "?", Q/binary>>
    end,

    %% Get port, use defaults if not specified
    Port = maps:get(port, uri_string:parse(Url),
                    case SchemeStr of
                        <<"wss">> -> 443;
                        <<"ws">> -> 80;
                        _ -> 443
                    end),

    %% Determine transport
    Transport = case SchemeStr of
        <<"wss">> -> tls;
        <<"ws">> -> tcp;
        _ -> tls
    end,

    %% TLS options for secure connections
    TlsOpts = [{verify, verify_none}],  %% For simplicity, disable cert verification
                                          %% In production, use proper CA certs

    %% Connection options
    Opts = case Transport of
        tls ->
            #{
                transport => tls,
                tls_opts => TlsOpts,
                protocols => [http],
                retry => 10,
                retry_timeout => 1000
            };
        tcp ->
            #{
                transport => tcp,
                protocols => [http],
                retry => 10,
                retry_timeout => 1000
            }
    end,

    %% Convert host to list if needed
    HostStr = case is_binary(Host) of
        true -> binary_to_list(Host);
        false -> Host
    end,

    %% Ensure path with query is binary
    PathBin = case is_binary(PathWithQuery) of
        true -> PathWithQuery;
        false -> list_to_binary(PathWithQuery)
    end,

    %% Open connection
    case gun:open(HostStr, Port, Opts) of
        {ok, ConnPid} ->
            %% Monitor the connection
            MRef = monitor(process, ConnPid),

            %% Wait for connection
            receive
                {gun_up, ConnPid, _Protocol} ->
                    %% Upgrade to WebSocket
                    StreamRef = gun:ws_upgrade(ConnPid, binary_to_list(PathBin), []),

                    %% Wait for upgrade
                    receive
                        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers} ->
                            %% Spawn a handler process to listen for WebSocket frames
                            spawn(fun() -> handle_messages(ConnPid, StreamRef, HandlerPid) end),
                            %% Return immediately so Gleam can continue
                            {ok, ConnPid};
                        {gun_response, ConnPid, _, _, Status, Headers} ->
                            gun:close(ConnPid),
                            {error, {upgrade_failed, Status, Headers}};
                        {gun_error, ConnPid, _StreamRef, Reason} ->
                            gun:close(ConnPid),
                            {error, {gun_error, Reason}};
                        {'DOWN', MRef, process, ConnPid, Reason} ->
                            {error, {connection_down, Reason}};
                        _Other ->
                            gun:close(ConnPid),
                            {error, unexpected_message}
                    after 30000 ->
                        gun:close(ConnPid),
                        {error, upgrade_timeout}
                    end;
                {'DOWN', MRef, process, ConnPid, Reason} ->
                    {error, {connection_failed, Reason}};
                _Other ->
                    gun:close(ConnPid),
                    {error, unexpected_message}
            after 30000 ->
                gun:close(ConnPid),
                {error, connection_timeout}
            end;
        {error, Reason} ->
            {error, {open_failed, Reason}}
    end.

%% Handle incoming WebSocket messages
handle_messages(ConnPid, StreamRef, HandlerPid) ->
    receive
        {gun_ws, ConnPid, StreamRef, {text, Text}} ->
            HandlerPid ! {ws_text, Text},
            handle_messages(ConnPid, StreamRef, HandlerPid);
        {gun_ws, ConnPid, StreamRef, {binary, Binary}} ->
            HandlerPid ! {ws_binary, Binary},
            handle_messages(ConnPid, StreamRef, HandlerPid);
        {gun_ws, ConnPid, StreamRef, close} ->
            HandlerPid ! {ws_closed, normal},
            gun:close(ConnPid);
        {gun_down, ConnPid, _Protocol, Reason, _KilledStreams} ->
            HandlerPid ! {ws_error, Reason},
            gun:close(ConnPid);
        {gun_error, ConnPid, StreamRef, Reason} ->
            HandlerPid ! {ws_error, Reason},
            handle_messages(ConnPid, StreamRef, HandlerPid);
        stop ->
            gun:close(ConnPid)
    after 30000 ->
        %% Heartbeat every 30 seconds to keep connection alive
        handle_messages(ConnPid, StreamRef, HandlerPid)
    end.
