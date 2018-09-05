-module(concurrency_errors).
-compile(export_all).

my_proc() ->
    timer:sleep(5000),
    exit(reason).

chain(0) ->
    receive
        _ -> ok
    after
        10000 -> exit("chain dies here")
    end;
chain(N) ->
    Pid = spawn(fun() -> chain(N-1) end),
    io:format("Spawned: ~p~n", [Pid]),
    link(Pid),
    receive
        _ -> ok
    end.