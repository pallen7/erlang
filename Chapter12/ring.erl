-module(ring).
-export([process/2, loop/1]).

process(_M, N) ->
    Processes = create_processes(N, []),
    LastProcess = hd(lists:reverse(Processes),
    build_process_ring(LastProcess, Processes).

build_process_ring(_, []) -> ok;
build_process_ring(Process, [H,T], LinkedProcesses) ->
    Process:

create_processes(0, Processes) -> Processes;
create_processes(N, Processes) -> create_processes(N-1, [spawn(ring, loop, [{N-1, undefined}]) | Processes]).




loop({ProcessIndex, NextProcess}) ->
    receive
        {link_process, Process} ->
            loop({ProcessIndex, NextProcess});
        Any ->
            io:format("unexpteced: ~p", [Any])
    after 2000 ->
        io:format("~p-~p finished~n", [self(), ProcessIndex])
    end.