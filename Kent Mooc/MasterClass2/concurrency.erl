-module(concurrency).

-export([promise/2, yield/1, area/0]).

%% Tag = promise(Pid, request) send a request to be processed
%% Do some computations
%% yield(Tag) get the result of the request
% AreaServer = spawn(concurrency, area, []).
% Tag1 = concurrency:promise(AreaServer, {square, 5}).
% concurrency:yield(Tag1).

% Make a remote procedure call (this looks like a local call akin to rpc)
promise(Pid, Request) ->
    Tag = erlang:make_ref(),        %% Create a unique tag (unique among connected notes)
    Pid ! {self(), Tag, Request},
    Tag.

% Get the response of a remote procedure call
yield(Tag) ->
    receive
        {Tag, Response} ->
            Response
        after 5000 ->
            fail
    end.

area() ->
    receive
        {Pid, Tag, {square, X}} ->
            Pid ! {Tag, X*X};
        {Pid, Tag, {rectangle, X, Y}} ->
            Pid ! {Tag, X*Y}
    after 15000 ->
        timeout
    end,
    area().