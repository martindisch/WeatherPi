%% @doc This module contains code for the Pi to which the sensor is connected
%% (the sender) and the base station.
%% The idea is to start execution on the base station, which will spawn the
%% sender process on the Pi before getting ready to receive its messages.
%%
%% @type measurement() = {Time::string(), {Temperature::float(),
%%                        Humidity::float()} | failure}.
%% A temperature and humidity measurement.
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
        {Sender, Measurements} ->
            % Format measurements for CSV file
            Lines = format_measurements(Measurements),
            % Append the lines to file
            file:write_file("history.csv", Lines, [append])
    end,
    % Send acknowledgement to sender
    Sender ! ack,
    receiver().

%% @spec format_measurements(Measurements::[measurement()]) -> string()
%% @doc Iterates over the measurements, shows them in stdout and returns
%% formatted lines for the CSV file.

format_measurements([]) ->
    [];
format_measurements([{Time, failure} | R]) ->
    io:format("~s: Failure~n", [Time]),
    % Don't add failure to CSV, continue with next measurement
    format_measurements(R);
format_measurements([{Time, {Temp, Hum}} | R]) ->
    % Print data to output nicely
    io:format("~s: ~p \x{b0}C, ~p%~n", [Time, Temp, Hum]),
    % Format the data for CSV
    Line = lists:flatten(
        io_lib:format("~s,~p,~p~n", [Time, Temp, Hum])),
    % Continue with next measurement
    [Line | format_measurements(R)].

%% @spec sender(Receiver::pid(), Pin::string()) -> no_return()
%% @doc Reads data from sensor in some interval and sends the measurements
%% to the receiver process.

sender(Receiver, Pin) ->
    sender(Receiver, Pin, []).

%% @spec sender(Receiver::pid(), Pin::string(), Queue::[measurement()]) ->
%%           no_return()
%% @doc Reads data from sensor in some interval and sends the measurements
%% to the receiver process. In case the receiver can't be reached, the
%% measurements are kept in the queue to be sent later.

sender(Receiver, Pin, Queue) ->
    % Get measurement
    Measurement = get_measurement(Pin),
    % Append it to the existing measurements
    NewQueue = Queue ++ [Measurement],
    % Send the whole queue to the receiver
    Receiver ! {self(), NewQueue},
    % Wait for acknowledgement
    receive
        ack ->
            % Empty the queue
            SendQueue = []
    after
        1000 ->
            % Network likely broke, keep existing queue
            SendQueue = NewQueue
    end,
    % Wait for some time before taking the next measurements
    timer:sleep(60000),
    sender(Receiver, Pin, SendQueue).

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
            {get_datetime(), failure};
        false ->
            % Get individual strings from output
            [TempStr, HumStr] = string:tokens(Cleaned, ","),
            % Convert them to floats
            Temp = list_to_float(TempStr),
            Hum = list_to_float(HumStr),
            % Return measurements as tuple
            {get_datetime(), {Temp, Hum}}
    end.

%% @spec get_datetime() -> string()
%% @doc Returns local time as "YYYY/MM/DD hh:mm:ss".

get_datetime() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = erlang:localtime(),
    lists:flatten(io_lib:format(
        "~4..0w/~2..0w/~2..0w ~2..0w:~2..0w:~2..0w",
        [Year, Month, Day, Hour, Min, Sec])).
