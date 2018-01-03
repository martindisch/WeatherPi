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
            % Get human-readable datetime
            DateTime = weather:format_time(SecondsUTC),
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
