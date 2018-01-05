%% @doc This module contains the ESI functions providing users with either
%% the most recent measurement or all measurements since some point in time.
-module(esi).
-export([latest/3, history/3]).
-import(util, [format_json/1, format_json_item/1]).

%% @spec latest(Sid::term(), Env::env(), Inp::string()) -> ok | {error, Reason}
%% @doc Responds to a HTTP request with the most recent measurement.

latest(Sid, _, _) ->
    % Request latest measurement
    weatherserver ! {latest, self()},
    receive
        Measurement ->
            % Format measurement as JSON
            Out = format_json_item(Measurement),
            % Send response
            mod_esi:deliver(Sid, [Out])
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
                    % Return a formatted JSON string
                    Out = format_json(Measurements),
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
