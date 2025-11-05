-module(websocket_ffi_impl).
-export([send_to_handler/3, send_ping/1, receive_subscription_data/0, select_subscription_data/1]).

%% Send a message to the handler process
%% The message is a plain {subscription_data, Id, Data} tuple
%% that will be matched by the selector we registered
send_to_handler(Pid, SubscriptionId, Data) ->
    Pid ! {subscription_data, SubscriptionId, Data},
    nil.

%% Send a ping to trigger handler
send_ping(Pid) ->
    Pid ! ping,
    nil.

%% Check mailbox for subscription_data messages (non-blocking)
receive_subscription_data() ->
    receive
        {subscription_data, SubscriptionId, Data} ->
            {ok, {SubscriptionId, Data}}
    after 0 ->
        {error, nil}
    end.

%% Add selector branch for {subscription_data, Id, Data} messages
%% The selector is a Gleam process selector
%% We use gleam_erlang's select function to add a handler
select_subscription_data(Selector) ->
    %% Use gleam@erlang@process:select_record to match the tuple
    %% The atom is 'subscription_data', tag position is 1 (first element)
    gleam@erlang@process:select_record(
        Selector,
        subscription_data,
        1,
        fun(Record) ->
            %% Decode the record into SubscriptionData
            %% Record is {subscription_data, Id, Data}
            Decoder = gleam@dynamic@decode:tuple3(
                fun(_) -> {ok, nil} end,  %% Skip tag
                gleam@dynamic@decode:string(),
                gleam@dynamic@decode:string()
            ),
            case gleam@dynamic@decode:run(Record, Decoder) of
                {ok, {_, Id, Data}} ->
                    %% Return SubscriptionData constructor
                    {ok, {subscription_data, Id, Data}};
                {error, _} ->
                    {error, nil}
            end
        end
    ).
