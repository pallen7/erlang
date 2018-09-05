-module(ets_test).
-compile(export_all).

% ETS -> Erlang Term Storage. Stores key/value pairs in memory (Transient)
% DETS -> Same but on disk (Persistent)

start() ->
    lists:foreach(fun test_ets/1,
                    [set,               % Unique Keys
                     ordered_set,       % Ordered
                     bag,               % Duplicate Keys
                     duplicate_bag]).   % Duplicate Keys and tuples

test_ets(Mode) ->
    TableId = ets:new(test, [Mode]),
    ets:insert(TableId, {a, 1}),
    ets:insert(TableId, {b, 2}),
    ets:insert(TableId, {a, 1}),
    ets:insert(TableId, {a, 3}),
    List = ets:tab2list(TableId),
    io:format("Mode: ~p => ~p~n", [Mode, List]),
    ets:delete(TableId).