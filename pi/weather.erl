-module(weather).
-export([sender/2, start/2]).

start(SenderNode, Pin) ->
    % Make sure unicode works even in -noshell mode
    io:setopts(standard_io, [{encoding, unicode}]),
    spawn(SenderNode, ?MODULE, sender, [self(), Pin]),
    receiver().

receiver() ->
    receive
        {Time, failure} ->
            io:format("~s: Failure~n", [Time]);
        {Time, {Temp, Hum}} ->
            io:format("~s: ~p \x{b0}C, ~p%~n", [Time, Temp, Hum]),
            Line = lists:flatten(
                io_lib:format("~s,~p,~p~n", [Time, Temp, Hum])),
            file:write_file("history.csv", Line, [append])
    end,
    receiver().

sender(Receiver, Pin) ->
    Data = get_data(Pin),
    Time = get_datetime(),
    Receiver ! {Time, Data},
    timer:sleep(60000),
    sender(Receiver, Pin).

get_data(Pin) ->
    Out = os:cmd("python am2302.py " ++ Pin),
    Cleaned = re:replace(Out, "(^\\s+)|(\\s+$)", "", [global, {return, list}]),
    case Cleaned =:= "failure" of
        true ->
            failure;
        false ->
            [TempStr, HumStr] = string:tokens(Cleaned, ","),
            Temp = list_to_float(TempStr),
            Hum = list_to_float(HumStr),
            {Temp, Hum}
    end.

get_datetime() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = erlang:localtime(),
    lists:flatten(io_lib:format(
        "~4..0w/~2..0w/~2..0w ~2..0w:~2..0w:~2..0w",
        [Year, Month, Day, Hour, Min, Sec])).
