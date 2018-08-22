-module(concurrency).
-compile(export_all).

dolphin() ->
    receive % waits until there is a message in the processes mailbox to process
        do_a_flip ->
            io:format("How about no?~n");
        fish ->
            io:format("So long and thanks for all the fish~n");
        _ ->
            io:format("Heh - we're smarter than humans eh?~n")
    end.

dolphin2() ->
    receive
        {From, do_a_flip} ->
            From ! "How about no?";
        {From, fish} ->
            From ! "So long and thanks for all the fish";
        _ ->
            io:format("Heh - we're smarter than humans eh?~n")
    end.

dolphin3() ->
    receive
        {From, do_a_flip} ->
            From ! "How about no?",
            dolphin3();
        {From, fish} ->
            From ! "So long and thanks for all the fish";
        _ ->
            io:format("Heh - we're smarter than humans eh?~n"),
            dolphin3()
    end.

fridge1(FoodList) ->
    receive
        {From, {store, Food}} ->
            From ! {self(), ok},
            fridge1([Food|FoodList]);
        {From, {take, Food}} ->
            case lists:member(Food, FoodList) of
                true ->
                    From ! {self(), {ok, Food}},
                    fridge1(lists:delete(Food, FoodList));
                false ->
                    From ! {self(), not_found},
                    fridge1(FoodList)
            end;
        terminate ->
            ok
    end.

start(FoodList) ->
    spawn(?MODULE, fridge1, [FoodList]).

store(Pid, Food) ->
    Pid ! {self(), {store, Food}},
    receive
        {Pid, Msg} -> Msg
    after 3000 ->
        timeout
    end.

take(Pid, Food) ->
    Pid ! {self(), {take, Food}},
    receive
        {Pid, Msg} -> Msg
    after 3000 ->
        timeout
    end.

sleep(T) ->
    receive
    after T ->
        ok
    end.
