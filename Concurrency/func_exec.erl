-module(func_exec).
-compile(export_all).

-define(MILLISECONDS, 1000).
-define(NOW_IN_MILLISECONDS, erlang:system_time(?MILLISECONDS)).

% todo: sort out the types - add types and type specs:
-record(state, {
    function_list = [],
    function_count = 0,
    timeout,
    start_time
}).

% function = {id, fx, [param]}
% result = {id, result}

initialise(Timeout) ->
    #state{
        timeout = Timeout
    }.

add_function(State, Id, Fx, Params) ->
    NewFunctionCount = State#state.function_count + 1,
    State#state{
        function_list = [{Id, Fx, Params} | State#state.function_list],
        function_count = NewFunctionCount
    }.

execute(State) ->
    StateWithStartTime = State#state{
        start_time = ?NOW_IN_MILLISECONDS
    },
    do_execute(StateWithStartTime, State#state.function_list, []).

get_result(Id, ResultList) ->
    [Result] = [element(2, Result) || Result <- ResultList, element(1,Result) =:= Id],
    Result.


%%%%%%%%%%%%%%%%%% ------>>>
%%%% INTERNAL %%%% ------>>>
%%%%%%%%%%%%%%%%%% ------>>>
do_execute(#state{timeout = Timeout, start_time = StartTime, function_count = Count, function_list = Functions}, [], PidList) ->
    FunctionIds = [element(1, Function) || Function <- Functions],
    Results = receiver(Count, [], PidList, FunctionIds, Timeout, StartTime),
    io:format("All done.~nResults: ~p~n", [Results]),
    Results;

do_execute(State, [Function | FunctionList], PidList) ->
    {Id, Fx, Params} = Function,
    Self = self(),
    FunToSpawn = fun() ->
        Result = erlang:apply(Fx, Params),
        Self ! {Id, self(), Result} end,

    % todo: change to spawn link
    Pid = spawn_link(FunToSpawn),
    do_execute(State, FunctionList, [Pid|PidList]).


receiver(0, Results, _PidList, _IdList, _Remaining, _LastStartTime) ->
    io:format("Normal termination~n"),
    Results;
receiver(FunCount, Results, PidList, IdList, Delay, LastStartTimeMs) -> 
    receive
        {FunctionId, Pid, Result} ->

            DelayRemaining = Delay - (?NOW_IN_MILLISECONDS - LastStartTimeMs),
            io:format("DelayRemaining: ~p~n", [DelayRemaining]),

            case lists:member(Pid, PidList) andalso lists:member(FunctionId, IdList) of
                true ->
                    io:format("Function from PidList completed. Pid: ~p Result:~p~n", [Pid, Result]),
                    receiver(FunCount-1, [{FunctionId, Result} | Results], PidList, IdList, DelayRemaining, ?NOW_IN_MILLISECONDS);
                false ->
                    io:format("Function completed not in PidList. Pid: ~p Result:~p~n", [Pid, Result]),
                    receiver(FunCount, Results, PidList, IdList, DelayRemaining, ?NOW_IN_MILLISECONDS)
            end;
        UnrecognisedMessage ->
            % Need to calculate delay here too...
            DelayRemaining = Delay - (?NOW_IN_MILLISECONDS - LastStartTimeMs),
            io:format("Unrecognised message: ~p~n", [UnrecognisedMessage]),
            receiver(FunCount, Results, PidList, IdList, DelayRemaining, ?NOW_IN_MILLISECONDS)
    after Delay ->
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