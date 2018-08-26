-module(func_exec).
-compile(export_all).

%% Add an opaque type in here that is a record to control the spawn process
%% Then we'll pass this state record to and fro

% todo: sort out the types - add types and type specs:
-record(state, {
    function_list = [],
    timeout
}).

initialise(Timeout) ->
    #state{
        timeout = Timeout
    }.

add_function(State, Fun, Params) ->
    State#state{
        function_list = [{Fun, Params} | State#state.function_list]
    }.

execute(State) ->
    StartTime = erlang:system_time(State#state.timeout),
    do_execute(State#state.function_list, 0, [], StartTime).


%%%%%%%%%%%%%%%%%% ------>>>
%%%% INTERNAL %%%% ------>>>
%%%%%%%%%%%%%%%%%% ------>>>
do_execute([], FunCount, PidList, StartTime) ->
    Results = receiver(FunCount, [], PidList, 16000, StartTime),
    io:format("All done. Results ~p~n", [Results]),
    ok;

do_execute([FunAndParams | Funs], FunCount, PidList, StartTime) ->
    {Fun, Params} = FunAndParams,
    Self = self(),
    FunToSpawn = fun() ->
        Result = erlang:apply(Fun, Params),
        % todo: change signature
        Self ! {fun_finished, self(), Result} end,

    % todo: change to spawn link
    Pid = spawn(FunToSpawn),
    do_execute(Funs, FunCount+1, [Pid|PidList], StartTime).


receiver(0, Results, _PidList, _Remaining, _TimeInMs) ->
    io:format("Normal termination~n"),
    Results;
receiver(FunCount, Results, PidList, Delay, TimeInMs) -> 
    receive
        {fun_finished, Pid, Result} ->

            DelayRemaining = Delay - (erlang:system_time(1000) - TimeInMs),
            io:format("DelayRemaining: ~p~n", [DelayRemaining]),

            case lists:member(Pid, PidList) of
                true ->
                    io:format("Function from PidList completed. Pid: ~p Result:~p~n", [Pid, Result]),
                    receiver(FunCount-1, [Result | Results], PidList, DelayRemaining, erlang:system_time(1000));
                false ->
                    io:format("Function completed not in PidList. Pid: ~p Result:~p~n", [Pid, Result]),
                    receiver(FunCount, Results, PidList, DelayRemaining, erlang:system_time(1000))
            end;
        UnrecognisedMessage ->
            % Need to calculate delay here too...
            DelayRemaining = Delay - (erlang:system_time(1000) - TimeInMs),
            io:format("Unrecognised message: ~p~n", [UnrecognisedMessage]),
            receiver(FunCount, Results, PidList, DelayRemaining, erlang:system_time(1000))
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