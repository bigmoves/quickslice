-module(stats_pubsub_ffi).
-export([get_registry_name/0]).

%% Get the registry name, creating it once and caching it
get_registry_name() ->
    case persistent_term:get({stats_pubsub, registry_name}, undefined) of
        undefined ->
            Name = gleam@erlang@process:new_name(<<"quickslice_stats_pubsub_registry">>),
            persistent_term:put({stats_pubsub, registry_name}, Name),
            Name;
        Name ->
            Name
    end.
