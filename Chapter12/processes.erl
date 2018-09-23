-module(processes).
-export([max/1]).

%% create N processes then destroy them. See how much time this takes

max(N) ->
    Max = erlang:system_info(process_limit),
    io:format("Max allowed processes: ~p~n", [Max]),
    statistics(runtime),
    statistics(wall_clock),
    Processes = [spawn(fun() -> wait() end) || _X <- lists:seq(1,N)],
    {_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),
    lists:foreach(fun(Pid) -> Pid ! die end, Processes),
    U1 = Time1 * 1000 / N,
    U2 = Time2 * 1000 / N,
    io:format("Process spawn time= ~p (~p) microseconds~n", [U1, U2]).

wait() ->
    receive
        die ->
            void
    end.