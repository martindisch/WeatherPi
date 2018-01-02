%% @doc This module contains code for the Pi to which the sensor is connected
%% (the sender) and the base station.
%% The idea is to start execution on the base station, which will spawn the
%% sender process on the Pi before getting ready to receive its messages.
%%
%% @type measurement() = {Time::integer(), {Temperature::float(),
%%                        Humidity::float()} | failure}.
%% A temperature and humidity measurement with a seconds timestamp in UTC.
-module(weather).
-export([sender/2, start/2, server/0, latest/3, history/3]).

%% @spec start(SenderNode::node(), Pin::string()) -> no_return()
%% @doc Spawns the sender process on the sender node and starts receiving.

start(SenderNode, Pin) ->
    % Make sure unicode works even in -noshell mode
    io:setopts(standard_io, [{encoding, unicode}]),

    % Start and register server process
    register(weatherserver, spawn(?MODULE, server, [])),
    % Start inets httpd server for mod_esi
    inets:start(),
    inets:start(
        httpd, [{port, 8099}, {server_name, "weather"}, {document_root, "."},
        {modules, [mod_esi]}, {server_root, "."},
        {erl_script_alias, {"/esi", [weather]}}]
    ),

    % Spawn sender process on the node with the sensor (Raspberry Pi)
    spawn(SenderNode, ?MODULE, sender, [self(), Pin]),
    % Start receiving messages from sender
    receiver().

%% @spec sender(Receiver::pid(), Pin::string()) -> no_return()
%% @doc Reads data from sensor in some interval and sends the measurements
%% to the receiver process.

sender(Receiver, Pin) ->
    sender(Receiver, Pin, []).

%% @spec sender(Receiver::pid(), Pin::string(), Queue::[measurement()]) ->
%%           no_return()
%% @doc Reads data from sensor in some interval and sends the measurements
%% to the receiver process. In case the receiver can't be reached, the
%% measurements are kept in the queue to be sent later. The queue is in reverse
%% chronological order, because new measurements are appended to the head to
%% prevent having to iterate over a possibly very long list every time.

sender(Receiver, Pin, Queue) ->
    % Check if previous send has been acknowledged
    receive
        ack ->
            % If so, empty the queue
            CurrentQueue = []
    after
        1000 ->
            % If not, keep the queue to send its contents again
            CurrentQueue = Queue
    end,
    % Take measurement and add it to the head of the queue
    NewQueue = [get_measurement(Pin) | CurrentQueue],
    % Send the whole queue to the receiver
    Receiver ! {self(), NewQueue},
    % Wait for some time before taking the next measurements
    timer:sleep(60000),
    sender(Receiver, Pin, NewQueue).

%% @spec receiver() -> no_return()
%% @doc Receives messages and logs the data to file.

receiver() ->
    receiver(0).

%% @spec receiver(LastTime::integer()) -> no_return()
%% @doc Receives messages and logs the data to file. `LastTime' is the
%% timestamp of the last measurement that has been received and is used
%% to ensure that no duplicates are logged, even if there has been duplication
%% due to network issues.

receiver(LastTime) ->
    receive
        {Sender, Measurements} ->
            % Format measurements for CSV file
            Lines = format_measurements(
                lists:reverse(Measurements), LastTime),
            % Append the lines to file
            file:write_file("history.csv", Lines, [append])
    end,
    %% Keep timestamp of latest measurement
    {NewLast, _} = lists:nth(1, Measurements),
    % Send acknowledgement to sender
    Sender ! ack,
    receiver(NewLast).

%% @spec format_measurements(Measurements::[measurement()],
%%                           LastTime::integer) -> string()
%% @doc Iterates over the measurements, shows them in stdout and returns
%% formatted lines for the CSV file. `LastTime' is used to make sure that
%% no duplicates are logged, even if there has been duplication due to
%% network issues.

format_measurements([], _) ->
    [];
format_measurements([{SecondsUTC, failure} | R], LastTime)
    when SecondsUTC > LastTime ->
    io:format("~s: Failure~n", [format_time(SecondsUTC)]),
    % Don't add failure to CSV, continue with next measurement
    format_measurements(R, LastTime);
format_measurements([{SecondsUTC, {Temp, Hum}} | R], LastTime)
    when SecondsUTC > LastTime ->
    % Send to server
    weatherserver ! {SecondsUTC, {Temp, Hum}},
    % Get human-readable datetime
    DateTime = format_time(SecondsUTC),
    % Print data to output nicely
    io:format("~s: ~p \x{b0}C, ~p%~n", [DateTime, Temp, Hum]),
    % Format the data for CSV
    Line = lists:flatten(
        io_lib:format("~s,~p,~p~n", [DateTime, Temp, Hum])),
    % Continue with next measurement
    [Line | format_measurements(R, LastTime)];
format_measurements([_ | R], LastTime) ->
    % This happens when the measurement is a duplicate, meaning that
    % its time is less than the time of the latest measurement and has
    % therefore been logged already. Continue with the next one.
    format_measurements(R, LastTime).

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

%% @spec server() -> no_return()
%% @doc Starts the weatherserver, which will be used by the ESI functions
%% to get the data that the client requested.

server() ->
    server([]).

%% @spec server(Measurements::[measurement()]) -> no_return()
%% @doc The weatherserver, which will be used by the ESI functions
%% to get the data that the client requested. At some point it will read all
%% existing measurements from file and keep them in its measurements list.
%% Whenever a new measurement is received, it will get a message containing it
%% to add it to its own list of measurements. The measurements list is in
%% reverse chronological order to quickly add new items to the head. This is
%% also better for looking up measurements, because the recent data is most
%% often requested.

server(Measurements) ->
    receive
        {latest, PID} ->
            % Send the most recent measurement to the requesting process
            PID ! lists:nth(1, Measurements),
            UpMeasurements = Measurements;
        Measurement ->
            % Received new measurement, add it to list
            UpMeasurements = [Measurement | Measurements]
    end,
    server(UpMeasurements).

%% @spec latest(Sid::term(), Env::env(), Inp::string()) -> ok | {error, Reason}
%% @doc Responds to a HTTP request with the most recent measurement.

latest(Sid, _, _) ->
    % Request latest measurement
    weatherserver ! {latest, self()},
    receive
        {SecondsUTC, {Temp, Hum}} ->
            % Get human-readable datetime
            DateTime = format_time(SecondsUTC),
            % Format CSV line
            Line = lists:flatten(
                io_lib:format("~s,~p,~p~n", [DateTime, Temp, Hum])),
            % Send response
            mod_esi:deliver(Sid, [Line])
    end.

%% @spec history(Sid::term(), Env::env(), Inp::string()) ->
%%           ok | {error, Reason}
%% @doc Responds to a HTTP request with all measurements between some requested
%% point in time and now.

history(Sid, _, _) ->
    mod_esi:deliver(Sid, ["TODO"]).
