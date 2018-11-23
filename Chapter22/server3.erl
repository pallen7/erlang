-module(server3).
-export([start/2, rpc/2, swap_code/2]).
start(Name, Mod) ->
    register(Name, spawn(fun() -> loop(Name, Mod, Mod:init()) end)).

swap_code(Name, Mod) -> rpc(Name, {swap_code, Mod}).
rpc(Name, Request) ->
    Name ! {self(), Request},
    receive
        {Name, Response} -> Response
    end.

loop(Name, Mod, State) ->
    receive
        {From, {swap_code, NewCallBackMod}} ->
            From ! {Name, ack},
            loop(Name, NewCallBackMod, State);
        {From, Request} ->
            {Response, NewState} = Mod:handle(Request, State),
            From ! {Name, Response},
            loop(Name, Mod, NewState)
    end.