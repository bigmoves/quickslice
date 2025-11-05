-module(event_handler_ffi).
-export([microseconds_to_iso8601/1]).

%% Convert microseconds since Unix epoch to ISO8601 format
%% Includes millisecond precision for better accuracy
%% Uses the event's original timestamp from the Jetstream event for accurate indexedAt values
microseconds_to_iso8601(Microseconds) ->
    %% Convert microseconds to seconds and get the remainder for milliseconds
    Seconds = Microseconds div 1000000,
    Milliseconds = (Microseconds rem 1000000) div 1000,

    %% Convert Unix timestamp to datetime
    DateTime = calendar:system_time_to_universal_time(Seconds, second),
    {{Year, Month, Day}, {Hour, Minute, Second}} = DateTime,

    %% Format with leading zeros
    YearStr = io_lib:format("~4..0B", [Year]),
    MonthStr = io_lib:format("~2..0B", [Month]),
    DayStr = io_lib:format("~2..0B", [Day]),
    HourStr = io_lib:format("~2..0B", [Hour]),
    MinuteStr = io_lib:format("~2..0B", [Minute]),
    SecondStr = io_lib:format("~2..0B", [Second]),
    MillisStr = io_lib:format("~3..0B", [Milliseconds]),

    %% Concatenate and convert to binary (Gleam string)
    %% Format: YYYY-MM-DDTHH:MM:SS.sssZ
    iolist_to_binary([
        YearStr, "-", MonthStr, "-", DayStr, "T",
        HourStr, ":", MinuteStr, ":", SecondStr, ".", MillisStr, "Z"
    ]).
