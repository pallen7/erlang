-module(lib_misc).
-export([sleep/1, flush_buffer/0, priority_receive/0]).

sleep(N) ->
    receive
    after N ->
        true
    end.

% test in erlang shell with:
% self() ! hello.
% self() ! world.
flush_buffer() ->
    receive
        Message ->
            io:format("Unprocessed message: ~p~n", [Message]),
            flush_buffer()
    after 0 ->
        true
    end.

% If there is not a message matching {top_priority, X} then this is processed first
% self() ! hello1.
% self() ! hello2.
% self() ! hello3.
% self() ! hello4.
% self() ! {top_priority, 1}.
% self() ! {top_priority, 2}.
% lib_misc:priority_receive().
priority_receive() ->
    receive
        {top_priority, Message} ->
            io:format("Top priority: ~p~n", [Message])
    after 0 ->
        receive
            Message ->
                io:format("Other priority: ~p~n", [Message])
        end
    end.
    