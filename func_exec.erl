-module(func_exec).
-compile(export_all).

execute(Funs) ->
    c:flush(),
    do_execute(Funs, 0).

do_execute([], FunCount) ->
    Results = receiver(FunCount, []),
    io:format("All done. Results ~p~n", [Results]),
    ok;

do_execute([Fun | Funs], FunCount) ->
    Self = self(),
    FunToSpawn = fun() -> Result = Fun(), Self ! {fun_finished, Result} end,
    spawn(FunToSpawn),
    do_execute(Funs, FunCount+1).

receiver(0, Results) ->
    io:format("Normal termination~n"),
    Results;
receiver(FunCount, Results) ->
    receive
        {fun_finished, Result} ->
            io:format("Function completed: ~p~n", [Result]),
            receiver(FunCount-1, [Result | Results]);
        AnythingElse ->
            io:format("AnythingElse: ~p~n", [AnythingElse]),
            receiver(FunCount, Results)
    after 6000 ->
        io:format("Terminated~n"),
        timeout
    end.
