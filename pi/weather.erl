-module(weather).
-export([sender/1, start/1]).

start(SenderNode) ->
    % Make sure unicode works even in -noshell mode
    io:setopts(standard_io, [{encoding, unicode}]),
    spawn(SenderNode, ?MODULE, sender, [self()]),
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

sender(Receiver) ->
    Data = get_data(),
    Time = get_datetime(),
    Receiver ! {Time, Data},
    timer:sleep(60000),
    sender(Receiver).

get_data() ->
    Out = os:cmd("python am2302.py 4"),
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
