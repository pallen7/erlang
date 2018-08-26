-module(func_exec).
-compile(export_all).

% Need to pass in a list of paramaters in the format
%% [{function1, paramlist1}, {function2, paramlist2}...]

execute(Funs) ->
    do_execute(Funs, 0, []).

do_execute([], FunCount, PidList) ->
    Results = receiver(FunCount, [], PidList),
    io:format("All done. Results ~p~n", [Results]),
    ok;

do_execute([{Fun, Params} | Funs], FunCount, PidList) ->
    Self = self(),
    FunToSpawn = fun() ->
        Result = erlang:apply(Fun, Params),
        % todo: change signature
        Self ! {fun_finished, self(), Result} end,

    % todo: change to spawn link
    Pid = spawn(FunToSpawn),
    do_execute(Funs, FunCount+1, [Pid|PidList]).

receiver(0, Results, _PidList) ->
    io:format("Normal termination~n"),
    Results;
receiver(FunCount, Results, PidList) -> 
    receive
        {fun_finished, Pid, Result} ->
            case lists:member(Pid, PidList) of
                true ->
                    io:format("Function from PidList completed. Pid: ~p Result:~p~n", [Pid, Result]),
                    receiver(FunCount-1, [Result | Results], PidList);
                false ->
                    io:format("Function completed not in PidList. Pid: ~p Result:~p~n", [Pid, Result]),
                    receiver(FunCount, Results, PidList)
            end;
        UnrecognisedMessage ->
            io:format("Unrecognised message: ~p~n", [UnrecognisedMessage]),
            receiver(FunCount, Results, PidList)
    after 6000 ->
        io:format("Terminated~n"),
        timeout
    end.


%% Funs to test with:
function0() ->
    io:format("Sleep0 completed~n").

function1(Sleep1) ->
    timer:sleep(Sleep1),
    io:format("Sleep1 ~p completed~n", [Sleep1]).

function2(Sleep1, Sleep2) ->
    timer:sleep(Sleep1),
    io:format("Sleep2 ~p completed~n", [Sleep1]),
    timer:sleep(Sleep2),
    io:format("Sleep2 ~p completed~n", [Sleep2]).