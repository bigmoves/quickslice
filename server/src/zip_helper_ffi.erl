-module(zip_helper_ffi).
-export([unzip_file/2]).

%% Extract a ZIP file to a destination directory
%% Uses Erlang's built-in :zip.unzip/2 function
unzip_file(ZipPath, Destination) ->
    %% Convert Gleam strings (binaries) to Erlang strings (lists)
    ZipPathList = binary_to_list(ZipPath),
    DestinationList = binary_to_list(Destination),
    Options = [{cwd, DestinationList}],
    case zip:unzip(ZipPathList, Options) of
        {ok, _FileList} -> {ok, nil};
        {error, Reason} -> {error, format_error(Reason)}
    end.

%% Format error reason as a binary string
format_error(Reason) when is_atom(Reason) ->
    list_to_binary(atom_to_list(Reason));
format_error(Reason) when is_list(Reason) ->
    list_to_binary(Reason);
format_error(Reason) ->
    list_to_binary(io_lib:format("~p", [Reason])).
