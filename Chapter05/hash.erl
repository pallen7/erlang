-module(hash).
-export([new/0, new/1, get/2, put/3, delete/2]).

% Exercise 3. Module of methods to represent the Ruby implementation of hash
% Not worrying about the fact that Ruby hashes are enumerated in the order they were created

% would make this opaque
-record(hash, {
    default,
    map
}).

new() ->
    new(nil).

new(Default) ->
    #hash{
        default=Default,
        map = maps:new() % equivalent of #{}
    }.

get(Key, #hash{map = Map, default = Default}) ->
    maps:get(Key, Map, Default).

put(Key, Value, #hash{map = Map} = Hash) ->
    Hash#hash{map = maps:put(Key, Value, Map)}.

delete(Key, #hash{map = Map} = Hash) ->
    Hash#hash{map = maps:remove(Key, Map)}.

delete_if(Pred, #hash{map = Map} = Hash) ->
    I = maps:iterator(Map).