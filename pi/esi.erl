%% @doc This module contains the ESI functions providing users with either
%% the most recent measurement or all measurements since some point in time.
-module(esi).
-export([latest/3, history/3]).

%% @spec latest(Sid::term(), Env::env(), Inp::string()) -> ok | {error, Reason}
%% @doc Responds to a HTTP request with the most recent measurement.

latest(Sid, _, _) ->
    % Request latest measurement
    weatherserver ! {latest, self()},
    receive
        {SecondsUTC, {Temp, Hum}} ->
            % Format CSV line
            Line = lists:flatten(
                io_lib:format("~p,~p,~p~n", [SecondsUTC, Temp, Hum])),
            % Send response
            mod_esi:deliver(Sid, [Line])
    end.

%% @spec history(Sid::term(), Env::env(), Inp::string()) ->
%%           ok | {error, Reason}
%% @doc Responds to a HTTP request with all measurements between some requested
%% point in time and now.

history(Sid, _, Inp) ->
    try list_to_integer(Inp) of
        _ ->
            % Get seconds
            SecondsUTC = list_to_integer(Inp),
            % Request measurements since then
            weatherserver ! {history, SecondsUTC, self()},
            receive
                Measurements ->
                    % Return a formatted CSV string
                    Out = format_csv(Measurements),
                    mod_esi:deliver(Sid, [Out])
            after
                5000 ->
                    % Timeout
                    mod_esi:deliver(Sid, ["Failure\n"])
            end
    catch
        _:_ ->
            % Most likely wrong type of argument
            mod_esi:deliver(Sid, ["Failure\n"])
    end.

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
format_csv([{SecondsUTC, {Temp, Hum}} | R], Acc) ->
    % Format the data for CSV
    Line = lists:flatten(
        io_lib:format("~p,~p,~p~n", [SecondsUTC, Temp, Hum])),
    % Continue with next measurement
    format_csv(R, [Line | Acc]).
