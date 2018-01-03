%% @doc This module provides utility functions for formatting and getting
%% measurements using the sensor-specific Python script.
-module(util).
-export([get_measurement/1, format_time/1, format_csv/1, format_csv_line/1]).

%% @spec get_measurement(Pin::string()) -> measurement()
%% @doc Uses the Python script to read temperature and humidity on the given
%% GPIO pin and returns them as a tuple with the local time.

get_measurement(Pin) ->
    % Call the Python script with the given pin
    Out = os:cmd("python am2302.py " ++ Pin),
    % Strip whitespace (linebreaks)
    Cleaned = re:replace(Out, "(^\\s+)|(\\s+$)", "", [global, {return, list}]),
    case Cleaned =:= "failure" of
        true ->
            % Python script was unsuccessful, return accordingly
            {erlang:system_time(seconds), failure};
        false ->
            % Get individual strings from output
            [TempStr, HumStr] = string:tokens(Cleaned, ","),
            % Convert them to floats
            Temp = list_to_float(TempStr),
            Hum = list_to_float(HumStr),
            % Return measurements as tuple
            {erlang:system_time(seconds), {Temp, Hum}}
    end.

%% @spec format_time(Seconds::integer()) -> string()
%% @doc Takes seconds since 1970 (UTC) and returns the corresponding local
%% time formatted as "YYYY/MM/DD hh:mm:ss".

format_time(Seconds) ->
    Base = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    % Get UTC datetime from seconds
    TimeUTC = calendar:gregorian_seconds_to_datetime(Base + Seconds),
    % Convert UTC datetime to local datetime
    {{Year, Month, Day}, {Hour, Min, Sec}} =
        calendar:universal_time_to_local_time(TimeUTC),
    % Format
    lists:flatten(io_lib:format(
        "~4..0w/~2..0w/~2..0w ~2..0w:~2..0w:~2..0w",
        [Year, Month, Day, Hour, Min, Sec])).

%% @spec format_csv(Measurements::[weather:measurement()]) -> string()
%% @doc Accepts a list of measurements in reverse chronological order and
%% returns its data in chronological order formatted as CSV.

format_csv(Measurements) -> format_csv(Measurements, []).

%% @spec format_csv(Measurements::[weather:measurement()],
%%                  Acc::string()) -> string()
%% @doc Accepts a list of measurements in reverse chronological order and
%% returns its data in chronological order formatted as CSV. Uses an
%% accumulator to efficiently reverse the list while converting its entries.

format_csv([], Acc) ->
    Acc;
format_csv([Measurement | R], Acc) ->
    % Format the data for CSV
    Line = format_csv_line(Measurement),
    % Continue with next measurement
    format_csv(R, [Line | Acc]).

%% @spec format_csv_line(Measurement::measurement()) -> string()
%% @doc Returns a CSV formatted version of this measurement.

format_csv_line({SecondsUTC, {Temp, Hum}}) ->
    lists:flatten(io_lib:format("~p,~p,~p~n", [SecondsUTC, Temp, Hum])).
