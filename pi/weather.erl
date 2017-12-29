%% @doc This module contains code for the Pi to which the sensor is connected
%% (the sender) and the base station.
%% The idea is to start execution on the base station, which will spawn the
%% sender process on the Pi before getting ready to receive its messages.
-module(weather).
-export([sender/2, start/2]).

%% @spec start(SenderNode::node(), Pin::string()) -> no_return()
%% @doc Spawns the sender process on the sender node and starts receiving.

start(SenderNode, Pin) ->
    % Make sure unicode works even in -noshell mode
    io:setopts(standard_io, [{encoding, unicode}]),
    % Spawn sender process on the node with the sensor (Raspberry Pi)
    spawn(SenderNode, ?MODULE, sender, [self(), Pin]),
    % Start receiving messages from sender
    receiver().

%% @spec receiver() -> no_return()
%% @doc Receives messages and logs the data to file.

receiver() ->
    receive
        {Time, failure} ->
            % No measurements could be taken
            io:format("~s: Failure~n", [Time]);
        {Time, {Temp, Hum}} ->
            % Print data to output nicely
            io:format("~s: ~p \x{b0}C, ~p%~n", [Time, Temp, Hum]),
            % Format the data for CSV
            Line = lists:flatten(
                io_lib:format("~s,~p,~p~n", [Time, Temp, Hum])),
            % Append the line to the CSV file
            file:write_file("history.csv", Line, [append])
    end,
    receiver().

%% @spec sender(Receiver::pid(), Pin::string()) -> no_return()
%% @doc Reads data from sensor in some interval and sends the measurements
%% to the receiver process.

sender(Receiver, Pin) ->
    % Get measurements and time
    Data = get_data(Pin),
    Time = get_datetime(),
    % Send both to receiver
    Receiver ! {Time, Data},
    % Wait for some time before taking the next measurements
    timer:sleep(60000),
    sender(Receiver, Pin).

%% @spec get_data(Pin::string()) -> {float(), float()} | failure
%% @doc Uses the Python script to read temperature and humidity on the given
%% GPIO pin and returns them as a tuple.

get_data(Pin) ->
    % Call the Python script with the given pin
    Out = os:cmd("python am2302.py " ++ Pin),
    % Strip whitespace (linebreaks)
    Cleaned = re:replace(Out, "(^\\s+)|(\\s+$)", "", [global, {return, list}]),
    case Cleaned =:= "failure" of
        true ->
            % Python script was unsuccessful, return accordingly
            failure;
        false ->
            % Get individual strings from output
            [TempStr, HumStr] = string:tokens(Cleaned, ","),
            % Convert them to floats
            Temp = list_to_float(TempStr),
            Hum = list_to_float(HumStr),
            % Return measurements as tuple
            {Temp, Hum}
    end.

%% @spec get_datetime() -> string()
%% @doc Returns local time as "YYYY/MM/DD hh:mm:ss".

get_datetime() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = erlang:localtime(),
    lists:flatten(io_lib:format(
        "~4..0w/~2..0w/~2..0w ~2..0w:~2..0w:~2..0w",
        [Year, Month, Day, Hour, Min, Sec])).
