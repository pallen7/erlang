-module(area_server).
-export([loop/0, area/2, start/0]).

start() -> spawn(area_server, loop, []).

area(Pid, Request) ->
    rpc(Pid, Request).

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} ->
            Response
    end.

loop() ->
    receive
        {From, {rectangle, Width, Height}} ->
            From ! {self(), Width * Height},
            loop();
        {From, {square, Side}} ->
            From ! {self(), Side * Side},
            loop();
        {From, Other} ->
            From ! {self(), {error, Other}},
            loop()
    end.