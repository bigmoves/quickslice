-module(jetstream_ffi).
-export([receive_ws_message/0]).

%% Receive a WebSocket text message from the process mailbox
receive_ws_message() ->
    receive
        %% Handle gun_ws messages directly (they're coming to us, not the handler)
        {gun_ws, _ConnPid, _StreamRef, {text, Text}} ->
            {ok, Text};
        {gun_ws, _ConnPid, _StreamRef, {binary, _Binary}} ->
            %% Ignore binary messages, try again
            receive_ws_message();
        {gun_ws, _ConnPid, _StreamRef, close} ->
            {error, nil};
        {gun_down, _ConnPid, _Protocol, _Reason, _KilledStreams} ->
            {error, nil};
        {gun_error, _ConnPid, _StreamRef, _Reason} ->
            {error, nil};
        _Other ->
            %% Ignore unexpected messages
            receive_ws_message()
    after 60000 ->
        %% Timeout - return error to continue loop
        {error, nil}
    end.
