%% @doc This module provides utility functions for formatting, reading
%% measurements from file and acquiring measurements using the
%% sensor-specific Python script.
-module(util).
-export([get_measurement/1, read_history/1,
         format_time/1, format_csv/1, format_csv_line/1,
         format_json/1, format_json_item/1]).

%% @spec get_measurement(Pin::string()) -> weather:measurement()
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

%% @spec read_history(Filename::string()) -> [weather:measurement()]
%% @doc Reads the measurements that have previously been written to file and
%% returns them in reverse chronological order.

read_history(Filename) ->
    Lines = read_lines(Filename),
    convert_lines(Lines).

%% @spec convert_lines(Lines::[string()] | no_such_file) ->
%%           [weather:measurement()]
%% @doc Deals with the result of reading the lines from CSV, converting it
%% to a reversed list of measurements.

convert_lines(no_such_file) ->
    % Return no measurements in case file doesn't exist
    [];
convert_lines(Lines) ->
    % Otherwise, start conversion with empty accumulator
    convert_lines(Lines, []).

%% @spec convert_lines(Lines::[string()], Acc::[weather:measurement()]) ->
%%           [weather:measurement()]
%% @doc Converts the CSV lines to a list of measurements, reversing the order
%% in the process by using an accumulator.

convert_lines([], Acc) ->
    Acc;
convert_lines([Line | R], Acc) ->
    [SecondsUTC, Temp, Hum] = string:tokens(Line, ","),
    convert_lines(R, [{list_to_integer(SecondsUTC),
        {list_to_float(Temp), list_to_float(Hum)}} | Acc]).

%% @spec read_lines(Filename::string()) -> [string()] | no_such_file
%% @doc Returns the list of lines from the given file.
%% The lines are trimmed, so leading and trailing whitespace, including
%% line breaks, is removed.

read_lines({error, enoent}) ->
    % File could not be found, return error.
    no_such_file;
read_lines({ok, Fd}) ->
    % File was opened, read it, close it and return the lines.
    Lines = read(Fd),
    file:close(Fd),
    Lines;
read_lines(Filename) ->
    % We were given the file name, open it
    read_lines(file:open(Filename, [read])).

%% @spec read(InputDevice::io_device()) -> [string()]
%% @doc Using the given input device, returns all lines as a list of strings
%% with trimmed lines.

read(Fd) ->
    case file:read_line(Fd) of
        {ok, Data} ->
            % Strip whitespace
            [re:replace(Data, "(^\\s+)|(\\s+$)", "", [global, {return, list}])
                | read(Fd)];
        eof -> []
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

%% @spec format_csv(Measurements::[weather:measurement()]) -> list()
%% @doc Accepts a list of measurements in reverse chronological order and
%% returns its data in chronological order formatted as CSV.

format_csv(Measurements) ->
    format_csv(Measurements, []).

%% @spec format_csv(Measurements::[weather:measurement()],
%%                  Acc::string()) -> list()
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

%% @spec format_csv_line(Measurement::weather:measurement()) -> list()
%% @doc Returns a CSV formatted version of this measurement.

format_csv_line({SecondsUTC, {Temp, Hum}}) ->
    lists:flatten(io_lib:format("~p,~p,~p~n", [SecondsUTC, Temp, Hum])).

%% @spec format_json(Measurements::[weather:measurement()]) -> list()
%% @doc Accepts a list of measurements in reverse chronological order and
%% returns its data in chronological order formatted as JSON.

format_json(Measurements) ->
    % Start accumulator with last character, the closing bracket
    format_json(Measurements, "]").

%% @spec format_json(Measurements::[weather:measurement()],
%%                  Acc::string()) -> list()
%% @doc Accepts a list of measurements in reverse chronological order and
%% returns its data in chronological order formatted as JSON. Uses an
%% accumulator to efficiently reverse the list while converting its entries.

format_json([], Acc) ->
    % Get first measurement
    F = lists:nth(1, Acc),
    % Delete leading comma of the measurement
    NewF = lists:delete(",", F),
    % Delete first measurement from accumulator
    NewAcc = lists:delete(F, Acc),
    % Add updated first measurement to accumulator
    FinalAcc = [NewF | NewAcc],
    % End accumulator with first character, the opening bracket
    ["[" | FinalAcc];
format_json([Measurement | R], Acc) ->
    % Format the data for CSV
    Line = ["," | format_json_item(Measurement)],
    % Continue with next measurement
    format_json(R, [Line | Acc]).

%% @spec format_json_item(Measurement::weather:measurement()) -> list()
%% @doc Returns a JSON formatted version of this measurement.

format_json_item({SecondsUTC, {Temp, Hum}}) ->
    lists:flatten(io_lib:format("[~p,~p,~p]", [SecondsUTC, Temp, Hum])).
